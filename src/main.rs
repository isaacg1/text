#![allow(unknown_lints)]
#![warn(clippy_pedantic)]
#![allow(print_stdout, missing_docs_in_private_items, filter_map, string_add)]
extern crate termios;
extern crate libc;
extern crate colored;

use colored::*;

use std::io::{self, Read, Write, BufWriter};
use std::fs::File;
use std::path::Path;
use std::env;

use std::iter::repeat;

use std::panic::catch_unwind;

use std::time::Instant;

use std::collections::HashMap;

use std::cmp::min;

use std::ascii::AsciiExt;

use std::mem;

use std::os::raw::c_int;
use termios::Termios;

/// * utility **

const IED_VERSION: &'static str = "0.3.0";

const TAB_STOP: usize = 4;

const STDIN: c_int = 1;
const STDOUT: c_int = 2;

const CURSOR_TOP_RIGHT: &'static str = "\x1b[H";

const HIDE_CURSOR: &'static str = "\x1b[?25l";
const SHOW_CURSOR: &'static str = "\x1b[?25h";

const CLEAR_RIGHT: &'static str = "\x1b[K";
const CLEAR_SCREEN: &'static str = "\x1b[2J";

const DONT_EDIT_FOLDS: &'static str = "Folded lines can't be edited. Ctrl-Space to unfold.";

/// * data **
struct EditorConfig<'syntax, T: Read, W: Write> {
    core: EditorCore<'syntax>,
    screen_rows: usize,
    screen_cols: usize,
    row_offset: usize,
    col_offset: usize,
    filename: Option<String>,
    status: Status,
    quit_times: usize,
    input_source: T,
    output_buffer: W,
    current_match: Option<(usize, usize, usize)>,
    saved_search: String,
    paste_mode: bool,
}

struct EditorCore<'syntax> {
    cursor_x: usize,
    cursor_y: usize,
    rows: Vec<Row>,
    syntax: Option<EditorSyntax<'syntax>>,
    folds: HashMap<usize, (usize, usize)>,
    modified: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum EditorHighlight {
    Normal,
    Number,
    Match,
    String,
    Comment,
    Keyword1,
    Keyword2,
    Keyword3,
    Keyword4,
}

impl EditorHighlight {
    fn apply_to(&self, to_color: ColoredString) -> ColoredString {
        match *self {
            EditorHighlight::Normal => to_color,
            EditorHighlight::Number => to_color.red(),
            EditorHighlight::Match => to_color.on_blue(),
            EditorHighlight::String => to_color.magenta().bold(),
            EditorHighlight::Comment => to_color.cyan(),
            EditorHighlight::Keyword1 => to_color.yellow(),
            EditorHighlight::Keyword2 => to_color.green(),
            EditorHighlight::Keyword3 => to_color.green().bold(),
            EditorHighlight::Keyword4 => to_color.yellow().bold(),
        }
    }
    const KEYWORDS: [Self; 4] = [
        EditorHighlight::Keyword1,
        EditorHighlight::Keyword2,
        EditorHighlight::Keyword3,
        EditorHighlight::Keyword4,
    ];
}

#[derive(Copy, Clone, PartialEq)]
enum EditorKey {
    Verbatim(char),
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    PageUp,
    PageDown,
    Home,
    End,
    Delete,
}

fn ctrl_key(k: char) -> char {
    (k as u8 & 0x1f) as char
}

#[derive(Clone)]
struct Row {
    text: String,
}

impl Row {
    fn new() -> Row {
        Row { text: String::new() }
    }
    fn make_highlights(
        &self,
        syntax: &Option<EditorSyntax>,
        prev_open_quote: Option<char>,
    ) -> (Vec<EditorHighlight>, Option<char>) {
        if let Some(syntax) = syntax.as_ref() {
            let text: Vec<char> = self.text.chars().collect();
            let mut new_hls = vec![];
            let mut open_quote = if text.is_empty() {
                prev_open_quote
            } else {
                None
            };
            while new_hls.len() < text.len() {
                let prev_is_sep = new_hls.is_empty() || is_separator(text[new_hls.len() - 1]);
                if new_hls.is_empty() && prev_open_quote.is_some() ||
                    syntax.quotes.contains(text[new_hls.len()])
                {
                    let active_quote = if new_hls.is_empty() && prev_open_quote.is_some() {
                        prev_open_quote.expect("Just checked_it")
                    } else {
                        let start_quote = text[new_hls.len()];
                        new_hls.push(EditorHighlight::String);
                        start_quote
                    };
                    open_quote = Some(active_quote);
                    while new_hls.len() < text.len() {
                        if text[new_hls.len()] == active_quote {
                            new_hls.push(EditorHighlight::String);
                            open_quote = None;
                            break;
                        }
                        if text[new_hls.len()] == '\\' && new_hls.len() + 1 < text.len() {
                            new_hls.push(EditorHighlight::String);
                        }
                        new_hls.push(EditorHighlight::String);
                    }
                } else if syntax.has_digits && text[new_hls.len()].is_digit(10) && prev_is_sep {
                    while new_hls.len() < text.len() && text[new_hls.len()].is_digit(10) {
                        new_hls.push(EditorHighlight::Number);
                    }
                } else if self.text[new_hls.len()..].starts_with(syntax.singleline_comment) {
                    let chars_remaining = text.len() - new_hls.len();
                    new_hls.extend(repeat(EditorHighlight::Comment).take(chars_remaining));
                } else if prev_is_sep {
                    let following_string: String = self.text[new_hls.len()..].to_string();
                    let key_and_highlight: Vec<(usize, EditorHighlight)> = syntax
                        .keywords
                        .iter()
                        .enumerate()
                        .flat_map(|(highlight_index, keywords)| {
                            keywords
                                .iter()
                                .filter(|&keyword| {
                                    following_string.starts_with(keyword) &&
                                        (keyword.len() + new_hls.len() == text.len() ||
                                             is_separator(text[keyword.len() + new_hls.len()]))
                                })
                                .map(move |keyword| {
                                    (keyword.len(), EditorHighlight::KEYWORDS[highlight_index])
                                })
                        })
                        .collect();
                    assert!(key_and_highlight.len() <= 1);
                    if let Some(&(keyword_len, highlight)) = key_and_highlight.first() {
                        new_hls.extend(repeat(highlight).take(keyword_len));
                    } else {
                        new_hls.push(EditorHighlight::Normal);
                    }
                } else {
                    new_hls.push(EditorHighlight::Normal);
                }
            }
            (new_hls, open_quote)
        } else {
            (
                repeat(EditorHighlight::Normal)
                    .take(self.text.len())
                    .collect(),
                None,
            )
        }
    }
}

struct Status {
    message: String,
    time: Instant,
}

impl Status {
    fn set_message(&mut self, message: &str) {
        self.message = message.to_string();
        self.time = Instant::now();
    }
}

/// * filetypes **
struct EditorSyntax<'a> {
    filetype: &'a str,
    extensions: Vec<&'a str>,
    has_digits: bool,
    quotes: &'a str,
    singleline_comment: &'a str,
    keywords: [Vec<&'a str>; 4],
}

impl<'a> EditorSyntax<'a> {
    fn for_filename(filename: &str) -> Option<EditorSyntax<'static>> {
        let syntax_database = vec![
            EditorSyntax {
                filetype: "rust",
                extensions: vec![".rs"],
                has_digits: true,
                quotes: "\"",
                singleline_comment: "//",
                keywords: [
                    vec![
                        "alignof",
                        "as",
                        "break",
                        "continue",
                        "crate",
                        "else",
                        "extern",
                        "fn",
                        "for",
                        "if",
                        "impl",
                        "in",
                        "let",
                        "loop",
                        "macro",
                        "match",
                        "mod",
                        "offsetof",
                        "pub",
                        "return",
                        "sizeof",
                        "trait",
                        "typeof",
                        "unsafe",
                        "use",
                        "where",
                        "while",
                        "yield",
                    ],
                    vec![
                        "box",
                        "const",
                        "enum",
                        "move",
                        "mut",
                        "ref",
                        "static",
                        "struct",
                        "type",
                    ],
                    vec!["false", "self", "Self", "super", "true"],
                    vec![
                        "bool",
                        "char",
                        "i8",
                        "i16",
                        "i32",
                        "i64",
                        "isize",
                        "f32",
                        "f64",
                        "str",
                        "u8",
                        "u16",
                        "u32",
                        "u64",
                        "usize",
                    ],
                ],
            },
            EditorSyntax {
                filetype: "c",
                extensions: vec![".c", ".h", ".cpp"],
                has_digits: true,
                quotes: "\"'",
                singleline_comment: "//",
                keywords: [vec![], vec![], vec![], vec![]],
            },
            EditorSyntax {
                filetype: "py",
                extensions: vec![".py"],
                has_digits: true,
                quotes: "\"'",
                singleline_comment: "#",
                keywords: [
                    vec![
                        "break",
                        "continue",
                        "def",
                        "elif",
                        "else",
                        "for",
                        "from",
                        "if",
                        "import",
                        "in",
                        "return",
                        "while",
                    ],
                    vec!["any", "abs", "input", "int", "len", "range", "print", "zip"],
                    vec!["False", "True"],
                    vec!["and", "not", "or"],
                ],
            },
        ];
        syntax_database.into_iter().find(|entry| {
            entry.extensions.iter().any(|extension| {
                filename.ends_with(extension)
            })
        })
    }
}

/// * terminal **
fn enable_raw_mode() -> io::Result<Termios> {
    use termios::*;

    let orig_termios = Termios::from_fd(STDIN)?;

    let mut termios = orig_termios;

    termios.c_cflag |= CS8;
    termios.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    termios.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    termios.c_oflag &= !(OPOST);
    termios.c_cc[VMIN] = 0; // Return on any characters read;
    termios.c_cc[VTIME] = 1; //Wait for 0.1 seconds.

    tcsetattr(STDIN, TCSAFLUSH, &termios)?;

    Ok(orig_termios)
}

fn restore_orig_mode(orig_termios: &Termios) -> io::Result<()> {
    termios::tcsetattr(STDIN, termios::TCSAFLUSH, orig_termios)
}


/// * syntax highlighting **

fn is_separator(c: char) -> bool {
    c.is_whitespace() || "&{}'\",.()+-/*=~%<>[];:!".contains(c)
}

fn whitespace_depth(row: &Row) -> usize {
    row.text
        .chars()
        .position(|cell| !cell.is_whitespace())
        .unwrap_or_else(|| row.text.len())
}

/// * row operations **

fn string_to_row(s: &str) -> Row {
    Row { text: s.to_string() }
}

/// * i/o **

fn read_key(input_source: &mut Read) -> EditorKey {
    let mut buffer: [u8; 1] = [0];
    while input_source.read(&mut buffer).expect("Read failure") == 0 {}
    let c = buffer[0] as char;
    if c == '\x1b' {
        let mut escape_buf: [u8; 3] = [0; 3];
        match input_source.read(&mut escape_buf).expect(
            "Read failure during escape sequence",
        ) {
            2 | 3 => {
                match escape_buf[0] as char {
                    '[' => {
                        if escape_buf[2] as char == '~' {
                            match escape_buf[1] as char {
                                '1' | '7' => Some(EditorKey::Home),
                                '3' => Some(EditorKey::Delete),
                                '4' | '8' => Some(EditorKey::End),
                                '5' => Some(EditorKey::PageUp),
                                '6' => Some(EditorKey::PageDown),
                                _ => None,
                            }
                        } else {
                            match escape_buf[1] as char {
                                'A' => Some(EditorKey::ArrowUp),
                                'B' => Some(EditorKey::ArrowDown),
                                'C' => Some(EditorKey::ArrowRight),
                                'D' => Some(EditorKey::ArrowLeft),
                                'H' => Some(EditorKey::Home),
                                'F' => Some(EditorKey::End),
                                _ => None,
                            }
                        }
                    }
                    'O' => {
                        match escape_buf[1] as char {
                            'H' => Some(EditorKey::Home),
                            'F' => Some(EditorKey::End),
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    } else {
        None
    }.unwrap_or_else(|| EditorKey::Verbatim(c))
}

/// * core operations - rows, cursor, folds, syntax **
impl<'syntax> EditorCore<'syntax> {
    fn check_consistency(&self) -> Option<&'static str> {
        let cursor_position_failure = if self.cursor_y > self.rows.len() {
            Some("Cursor y position out of bounds.")
        } else if self.cursor_x > self.current_row_len() {
            Some("Cursor x position is out of bounds")
        } else {
            None
        };
        let mut fold_failure = None;
        for (&start, &(end, _)) in &self.folds {
            if start < self.cursor_y && self.cursor_y <= end {
                fold_failure = Some("Cursor is inside a fold");
                break;
            }
            if self.rows.len() <= end {
                fold_failure = Some("Fold goes past end of file");
                break;
            }
        }
        let mut fold_fold_failure = None;
        for (&start1, &(end1, _)) in &self.folds {
            for (&start2, &(end2, _)) in &self.folds {
                if (start1 <= start2 && start2 <= end1 && end1 <= end2) &&
                    !(start1 == start2 && end1 == end2)
                {
                    fold_fold_failure = Some("Two folds overlap");
                    break;
                }
            }
        }
        cursor_position_failure.or(fold_failure).or(
            fold_fold_failure,
        )
    }

    fn current_row_len(&self) -> usize {
        self.rows.get(self.cursor_y).map_or(0, |row| row.text.len())
    }
    fn toggle_fold(&mut self) {
        if self.folds.contains_key(&self.cursor_y) {
            self.folds.remove(&self.cursor_y);
        } else if self.cursor_y < self.rows.len() &&
                   whitespace_depth(&self.rows[self.cursor_y]) == 0
        {
            let saved_cursor_y = self.cursor_y;
            self.cursor_y = 0;
            while self.cursor_y < self.rows.len() {
                if let Some(&(end, _)) = self.folds.get(&self.cursor_y) {
                    self.cursor_y = end + 1;
                } else if whitespace_depth(&self.rows[self.cursor_y]) > 0 {
                    self.create_fold();
                } else {
                    self.cursor_y += 1;
                }
            }
            self.cursor_y = saved_cursor_y;
        } else {
            self.create_fold();
        }
    }
    fn create_fold(&mut self) {
        if self.cursor_y < self.rows.len() {
            let fold_depth = whitespace_depth(&self.rows[self.cursor_y]);
            let is_not_in_fold = |row| whitespace_depth(row) < fold_depth && !row.text.is_empty();
            let start = self.rows
                .iter()
                .rev()
                .skip(self.rows.len() - self.cursor_y)
                .position(&is_not_in_fold)
                .map_or(0, |reverse_offset| self.cursor_y - reverse_offset);
            let end = self.rows
                .iter()
                .skip(self.cursor_y + 1)
                .position(&is_not_in_fold)
                .map_or(self.rows.len() - 1, |offset| self.cursor_y + offset);
            self.folds.insert(start, (end, fold_depth));
            self.cursor_y = start;
        }
    }
    fn open_folds(&mut self) {
        let to_remove: Vec<_> = self.folds
            .iter()
            .filter_map(|(&start, &(end, _depth))| if start <= self.cursor_y &&
                self.cursor_y <= end
            {
                Some(start)
            } else {
                None
            })
            .collect();
        for start in to_remove {
            self.folds.remove(&start);
        }
    }

    fn one_row_forward(&self, index: usize) -> usize {
        min(
            self.rows.len(),
            self.folds.get(&index).map_or(index, |&(end, _)| end) + 1,
        )
    }

    fn one_row_back(&self, index: usize) -> usize {
        let prev_index = index.saturating_sub(1);
        if let Some((&start, _end_and_depth)) =
            self.folds.iter().find(|&(_start, &(end, _depth))| {
                end == prev_index
            })
        {
            start
        } else {
            prev_index
        }
    }
    /// * row operations **

    // Assumes that index is past the open quote, returns final position of index, whether
    // the string is still open.

    fn insert_char(&mut self, c: char) {
        if self.cursor_y == self.rows.len() {
            self.rows.push(Row::new());
        }
        self.rows[self.cursor_y].text.insert(self.cursor_x, c);
        self.cursor_x += 1;
        self.modified = true;
    }

    fn insert_tab(&mut self) {
        if self.cursor_y == self.rows.len() {
            self.rows.push(Row::new());
        }
        for _ in 0..4 - self.cursor_x % 4 {
            self.rows[self.cursor_y].text.insert(self.cursor_x, ' ');
            self.cursor_x += 1;
        }
        self.modified = true;
    }
    fn insert_newline(&mut self, paste_mode: bool) {
        if self.cursor_y < self.rows.len() {
            let depth = if paste_mode {
                0
            } else {
                whitespace_depth(&self.rows[self.cursor_y])
            };
            // If in the whitespace, insert blank line.
            if depth >= self.cursor_x {
                self.rows.insert(self.cursor_y, Row::new());
            } else {
                let text_end = self.rows[self.cursor_y].text.split_off(self.cursor_x);
                let next_row = {
                    let next_row_text = self.rows[self.cursor_y].text[..depth].to_string();
                    string_to_row(&(next_row_text + &text_end))
                };
                self.rows.insert(self.cursor_y + 1, next_row);
            }
            for row_index in (self.cursor_y..self.rows.len()).rev() {
                if let Some((end, depth)) = self.folds.remove(&row_index) {
                    self.folds.insert(row_index + 1, (end + 1, depth));
                }
            }
            self.cursor_x = depth;
        } else {
            self.rows.push(Row::new())
        }
        self.cursor_y += 1;
        self.modified = true;
    }
    fn shift_folds_back(&mut self) {
        for row_index in self.cursor_y..self.rows.len() + 1 {
            if let Some((end, depth)) = self.folds.remove(&row_index) {
                self.folds.insert(row_index - 1, (end - 1, depth));
            }
        }
    }

    fn delete_row(&mut self) {
        if self.cursor_y < self.rows.len() {
            if self.cursor_x == 0 {
                self.rows.remove(self.cursor_y);
                self.shift_folds_back();
                self.modified = true;
            } else if self.cursor_x == self.current_row_len() {
                self.cursor_y += 1;
                self.cursor_x = 0;
            } else {
                self.rows[self.cursor_y].text.truncate(self.cursor_x);
                self.modified = true;
            }
        }
    }

    fn delete_char(&mut self) -> Result<(), String> {
        if let Some(prev_x) = self.cursor_x.checked_sub(1) {
            self.rows[self.cursor_y].text.remove(prev_x);
            self.cursor_x = prev_x;
            self.modified = true;
            Ok(())
        } else if 0 < self.cursor_y && self.cursor_y < self.rows.len() {
            if self.folds.values().any(
                |&(end, _)| end == self.cursor_y - 1,
            )
            {
                Err(DONT_EDIT_FOLDS.to_owned())
            } else {
                self.cursor_x = self.rows[self.cursor_y - 1].text.len();
                let moved_line = self.rows.remove(self.cursor_y);
                let line_to_append = &moved_line.text[whitespace_depth(&moved_line)..];
                self.rows[self.cursor_y - 1].text.push_str(line_to_append);
                self.shift_folds_back();
                self.cursor_y -= 1;
                self.modified = true;
                Ok(())
            }
        } else {
            Ok(())
        }
    }
    fn load_text(&mut self, text: &str) {
        self.rows = vec![];
        self.folds = HashMap::new();

        let mut row_buffer = String::new();
        for byte in text.bytes() {
            let c = byte as char;
            if c == '\n' {
                let row = string_to_row(&row_buffer);
                self.rows.push(row);
                row_buffer.truncate(0);
            } else {
                row_buffer.push(c);
            }
        }
        let row = string_to_row(&row_buffer);
        self.rows.push(row);

        self.cursor_y = min(self.cursor_y, self.rows.len());
        self.cursor_x = 0;
    }

    fn all_text(&self) -> String {
        self.rows
            .iter()
            .map(|row| row.text.clone())
            .collect::<Vec<_>>()
            .join("\n")
    }
}


impl<'syntax, T, W> EditorConfig<'syntax, T, W>
where
    T: Read,
    W: Write,
{
    fn new() -> EditorConfig<'syntax, io::Stdin, io::BufWriter<io::Stdout>> {
        let mut ws = libc::winsize {
            ws_row: 0,
            ws_col: 0,
            ws_xpixel: 0,
            ws_ypixel: 0,
        };
        let res;
        unsafe {
            res = libc::ioctl(STDOUT, libc::TIOCGWINSZ, &mut ws);
        }
        if res == -1 || ws.ws_col == 0 {
            panic!("Editor config failed.");
        } else {
            EditorConfig {
                core: EditorCore {
                    cursor_x: 0,
                    cursor_y: 0,
                    syntax: None,
                    rows: vec![],
                    folds: HashMap::new(),
                    modified: false,
                },
                filename: None,
                screen_rows: ws.ws_row.checked_sub(2).expect("Need at least 2 rows") as usize,
                screen_cols: ws.ws_col as usize,
                row_offset: 0,
                col_offset: 0,
                status: Status {
                    message: String::new(),
                    time: Instant::now(),
                },
                quit_times: 3,
                input_source: io::stdin(),
                output_buffer: BufWriter::new(io::stdout()),
                current_match: None,
                saved_search: String::new(),
                paste_mode: false,
            }
        }
    }

    fn warn_consistency(&mut self) {
        let failure = {
            self.core.check_consistency()
        };
        if let Some(message) = failure {
            self.status.set_message(message);
        }
    }

    fn toggle_paste_mode(&mut self) {
        self.paste_mode = !self.paste_mode;
    }


    fn activate_syntax(&mut self) {
        self.core.syntax = self.filename.as_ref().and_then(|filename| {
            EditorSyntax::for_filename(filename)
        });
    }

    /// * file i/o **
    fn open(&mut self) -> io::Result<()> {
        self.activate_syntax();
        let mut file = {
            let filename = self.filename.as_ref().expect(
                "To open, filename must be set.",
            );

            if !Path::new(&filename).exists() {
                File::create(&filename)?;
            }
            File::open(&filename)?
        };
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        self.core.load_text(&string);
        self.scroll();
        Ok(())
    }

    fn save(&mut self) -> io::Result<()> {
        if self.filename.is_none() {
            match self.prompt("Save as: ", "", None) {
                None => {
                    self.status.set_message("Save aborted");
                    return Ok(());
                }
                Some(filename) => {
                    self.filename = Some(filename);
                    self.activate_syntax();
                }
            }
        };

        let mut file = File::create(self.filename.as_ref().expect("Just set it"))?;
        let text = self.core.all_text();
        file.write_all(text.as_bytes())?;
        self.core.modified = false;
        self.status.set_message(
            &format!("{} bytes written to disk", text.len()),
        );
        Ok(())
    }

    /// * macro movement **

    fn find_callback(&mut self, query: &str, key: EditorKey) {
        if key != EditorKey::Verbatim('\r') && key != EditorKey::Verbatim('\x1b') {
            let mut matches: Vec<(usize, usize)> = self.core
                .rows
                .iter()
                .enumerate()
                .flat_map(|(row_index, row)| {
                    row.text.match_indices(query).map(move |(char_index, _)| {
                        (row_index, char_index)
                    })
                })
                .collect();
            // We sort by ! of whether the row index is forward,
            // So that the matches behind the current location are cyclically moved to the end.
            matches.sort_by_key(|&(row_index, col_index)| {
                !{
                    row_index > self.core.cursor_y ||
                        row_index == self.core.cursor_y && col_index >= self.core.cursor_x
                }
            });
            let best_match = if key == EditorKey::ArrowRight || key == EditorKey::ArrowDown {
                // Skip the match at the current location
                matches.get(1).or_else(|| matches.get(0))
            } else if key == EditorKey::ArrowLeft || key == EditorKey::ArrowUp {
                matches.last()
            } else {
                matches.get(0)
            };
            if let Some(&(row_index, col_index)) = best_match {
                self.core.cursor_y = row_index;
                self.core.open_folds();
                self.core.cursor_x = col_index;
                self.row_offset = row_index.checked_sub(self.screen_rows / 2).unwrap_or(0);
                self.current_match = Some((row_index, col_index, query.len()));
            }
        }
    }

    fn find(&mut self) {
        let saved_cursor_x = self.core.cursor_x;
        let saved_cursor_y = self.core.cursor_y;
        let saved_col_offset = self.col_offset;
        let saved_row_offset = self.row_offset;
        let saved_folds = self.core.folds.clone();

        let search_start = mem::replace(&mut self.saved_search, "".to_owned());

        let query = self.prompt(
            "Search (ESC/Arrows/Enter): ",
            &search_start,
            Some(&|ed, query, key| ed.find_callback(query, key)),
        );
        self.current_match = None;

        match query {
            None => {
                self.core.cursor_x = saved_cursor_x;
                self.core.cursor_y = saved_cursor_y;
                self.col_offset = saved_col_offset;
                self.row_offset = saved_row_offset;
                self.core.folds = saved_folds;
            }
            Some(search) => {
                self.saved_search = search;
            }
        }
    }

    fn go_to(&mut self) {
        if let Some(response) = self.prompt("Go to line: ", "", None) {
            match response.parse::<usize>() {
                Ok(line) => {
                    if 0 < line && line - 1 <= self.core.rows.len() {
                        self.core.cursor_y = line - 1;
                        self.core.cursor_x = 0;
                    } else {
                        self.status.set_message(&format!(
                            "Line {} outside of range of file",
                            line
                        ));
                    }
                }
                Err(_) => self.status.set_message("Line was not numeric"),
            }
        }
    }

    fn display_help(&mut self) {
        self.status.set_message(
            "C-s save, C-q quit, C-f find, C-' ' fold, \
             C-e refresh, C-k del row, C-g go to, C-p paste mode, C-h help.",
        )
    }

    /// * output **

    fn scroll(&mut self) {
        self.row_offset = min(self.row_offset, self.core.cursor_y);
        while self.screen_y() >= self.screen_rows {
            self.row_offset = self.core.one_row_forward(self.row_offset)
        }
        self.col_offset = min(self.col_offset, self.core.cursor_x);
        while self.screen_x() >= self.screen_cols {
            self.col_offset += 1;
        }
    }

    fn draw_rows(&mut self) -> io::Result<()> {
        let tab = &" ".repeat(TAB_STOP);
        let mut screen_y = 0;
        let mut file_row = self.row_offset;
        let mut prev_open_quote = None;
        while screen_y < self.screen_rows {
            if let Some(&(fold_end, fold_depth)) = self.core.folds.get(&file_row) {
                let text = &self.core.rows[file_row].text;
                let fold_white_cells = if fold_depth < text.len() {
                    text[..fold_depth].to_string()
                } else {
                    " ".repeat(fold_depth)
                };
                let mut fold_white_str = fold_white_cells.replace("\t", tab);
                let fold_msg = format!("{} lines folded.", fold_end - file_row + 1);
                fold_white_str.truncate(self.screen_cols.saturating_sub(fold_msg.len()));
                let remaining_width = self.screen_cols.saturating_sub(fold_white_str.len());
                let padded_fold_msg = format!("{:width$}", fold_msg, width = remaining_width);
                write!(
                    self.output_buffer,
                    "{}{}",
                    fold_white_str,
                    padded_fold_msg.reversed()
                )?;
                file_row = fold_end + 1;
            } else if file_row < self.core.rows.len() {
                let current_row = &self.core.rows[file_row];
                if self.col_offset < current_row.text.len() {
                    let mut chars_written = 0;
                    let (highlights, next_open_quote) =
                        current_row.make_highlights(&self.core.syntax, prev_open_quote);
                    prev_open_quote = next_open_quote;
                    assert_eq!(highlights.len(), current_row.text.len());
                    for (index, (chr, &hl)) in
                        current_row
                            .text
                            .chars()
                            .zip(highlights.iter())
                            .enumerate()
                            .skip(self.col_offset)
                    {
                        chars_written += if chr == '\t' { TAB_STOP } else { 1 };
                        if chars_written > self.screen_cols {
                            break;
                        }
                        let to_write = if chr == '\t' {
                            tab.normal()
                        } else if chr.is_control() {
                            if chr.is_ascii() && chr as u8 <= 26 {
                                (64 + (chr as u8)) as char
                            } else {
                                '?'
                            }.to_string()
                                .reversed()
                        } else {
                            chr.to_string().normal()
                        };
                        let top_hl = if let Some((row, col, len)) = self.current_match {
                            if row == file_row && col <= index && col + len > index {
                                EditorHighlight::Match
                            } else {
                                hl
                            }
                        } else {
                            hl
                        };
                        write!(self.output_buffer, "{}", top_hl.apply_to(to_write))?;
                    }
                }
                file_row += 1;
            } else if self.core.rows.is_empty() && screen_y == self.screen_rows / 3 {
                let welcome = format!("Isaac's editor -- version {}", IED_VERSION);
                write!(
                    self.output_buffer,
                    "~{:^width$}",
                    welcome,
                    width = self.screen_cols.saturating_sub(1)
                )?;
                file_row += 1;
            } else {
                write!(self.output_buffer, "~")?;
                file_row += 1;
            };
            write!(self.output_buffer, "{}\r\n", CLEAR_RIGHT)?;
            screen_y += 1;
        }
        Ok(())
    }

    fn draw_status_bar(&mut self) -> io::Result<()> {
        let mut output_buffer = &mut self.output_buffer;
        let name = self.filename.clone().unwrap_or_else(
            || "[No Name]".to_string(),
        );
        let dirty = if self.core.modified { "(modified)" } else { "" };
        let paste = if self.paste_mode { "(paste)" } else { "" };
        let mut status = format!("{:.20} {} {}", name, dirty, paste);
        status.truncate(self.screen_cols);

        let filetype = match self.core.syntax {
            None => "no ft",
            Some(ref syntax) => syntax.filetype,
        };
        let right_status = format!(
            "{} | r: {}/{}, c: {}/{}",
            filetype,
            self.core.cursor_y + 1,
            self.core.rows.len(),
            self.core.cursor_x + 1,
            self.core.rows.get(self.core.cursor_y).map_or(
                0,
                |row| row.text.len(),
            )
        );
        let room_remaining = self.screen_cols - status.len();
        let to_write = format!(
            "{}{:>width$.size_cap$}",
            status,
            right_status,
            size_cap = self.screen_cols.saturating_sub(status.len() + 1),
            width = room_remaining
        );
        write!(output_buffer, "{}\r\n", to_write.reversed())?;
        Ok(())
    }

    fn draw_message_bar(&mut self) -> io::Result<()> {
        let mut output_buffer = &mut self.output_buffer;
        write!(output_buffer, "{}", CLEAR_RIGHT)?;
        if self.status.time.elapsed().as_secs() < 5 {
            write!(
                output_buffer,
                "{:.space$}",
                self.status.message,
                space = self.screen_cols
            )?;
        }
        Ok(())
    }

    fn refresh_screen(&mut self) -> io::Result<()> {
        self.scroll();
        write!(self.output_buffer, "{}", HIDE_CURSOR)?;
        write!(self.output_buffer, "{}", CURSOR_TOP_RIGHT)?;

        self.draw_rows()?;
        self.draw_status_bar()?;
        self.draw_message_bar()?;

        let y_index = self.screen_y() + 1;
        let x_index = self.screen_x() + 1;
        write!(self.output_buffer, "\x1b[{};{}H", y_index, x_index)?;
        write!(self.output_buffer, "{}", SHOW_CURSOR)?;
        self.output_buffer.flush()?;
        Ok(())
    }

    /// * input **

    fn prompt(
        &mut self,
        prompt: &str,
        initial_response: &str,
        callback: Option<&Fn(&mut EditorConfig<T, W>, &str, EditorKey) -> ()>,
    ) -> Option<String> {
        let mut response: String = initial_response.to_owned();
        loop {
            self.status.set_message(&format!("{}{}", prompt, response));
            self.refresh_screen().expect(
                "Screen should refresh successfully",
            );

            let c = read_key(&mut self.input_source);
            macro_rules! maybe_callback {
                () => {
                    if let Some(callback) = callback {
                        callback(self, &response, c)
                    }
                }
            }
            match c {
                EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('q') => {
                    self.status.set_message("");
                    maybe_callback!();
                    return None;
                }
                EditorKey::Verbatim(chr) if chr == '\r' => {
                    if !response.is_empty() {
                        self.status.set_message("");
                        maybe_callback!();
                        return Some(response);
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('h') || chr == 127 as char => {
                    if !response.is_empty() {
                        response.pop();
                    }
                }
                EditorKey::Delete => {
                    if !response.is_empty() {
                        response.pop();
                    }
                }
                EditorKey::Verbatim(chr) if chr as usize >= 32 && (chr as usize) < 128 => {
                    response.push(chr)
                }
                _ => (),
            };
            maybe_callback!();
        }
    }

    fn screen_x(&self) -> usize {
        self.core.rows.get(self.core.cursor_y).map_or(0, |row| {
            row.text
                .chars()
                .take(self.core.cursor_x)
                .skip(self.col_offset)
                .map(|chr| if chr == '\t' { TAB_STOP } else { 1 })
                .sum()
        })
    }

    fn screen_y(&self) -> usize {
        let mut file_y = self.row_offset;
        let mut screen_y = 0;
        while file_y < self.core.cursor_y {
            file_y = self.core.one_row_forward(file_y);
            screen_y += 1;
        }
        screen_y
    }

    fn move_cursor(&mut self, key: EditorKey) {
        // Smaller values are up and left
        match key {
            EditorKey::ArrowUp => {
                if self.core.cursor_y > 0 {
                    self.core.cursor_y = self.core.one_row_back(self.core.cursor_y);
                }
            }
            EditorKey::ArrowDown => {
                if self.core.cursor_y < self.core.rows.len() {
                    self.core.cursor_y = self.core.one_row_forward(self.core.cursor_y);
                }
            }
            EditorKey::ArrowLeft => {
                if let Some(prev_x) = self.core.cursor_x.checked_sub(1) {
                    self.core.cursor_x = prev_x
                } else if self.core.cursor_y > 0 {
                    self.core.cursor_y = self.core.one_row_back(self.core.cursor_y);
                    self.core.cursor_x = self.core.current_row_len();
                }
            }
            EditorKey::ArrowRight => {
                if self.core.cursor_x < self.core.current_row_len() {
                    self.core.cursor_x += 1
                } else if self.core.cursor_x == self.core.current_row_len() {
                    self.core.cursor_y = self.core.one_row_forward(self.core.cursor_y);
                    self.core.cursor_x = 0
                }
            }
            EditorKey::PageUp => {
                for _ in 0..self.screen_rows.saturating_sub(1) {
                    self.move_cursor(EditorKey::ArrowUp)
                }
            }
            EditorKey::PageDown => {
                for _ in 0..self.screen_rows.saturating_sub(1) {
                    self.move_cursor(EditorKey::ArrowDown)
                }
            }
            EditorKey::Home => self.core.cursor_x = 0,
            EditorKey::End => self.core.cursor_x = self.core.current_row_len(),

            _ => panic!("Editor move cursor received non moving character"),
        };
        self.core.cursor_x = min(self.core.cursor_x, self.core.current_row_len());
    }

    // Return value indicates whether we should continue processing keypresses.
    fn process_keypress(&mut self, c: EditorKey) -> bool {
        if c == EditorKey::Verbatim(ctrl_key('q')) {
            if self.core.modified && self.quit_times > 0 {
                let quit_times = self.quit_times;
                self.status.set_message(&format!(
                    "Warning: File has unsaved changes. \
                     Ctrl-S to save, or press Ctrl-Q \
                     {} more times to quit.",
                    quit_times
                ));
                self.quit_times -= 1;
                true
            } else {
                self.quit_times = 3;
                false
            }
        } else {
            match c {
                EditorKey::ArrowUp | EditorKey::ArrowDown | EditorKey::ArrowLeft |
                EditorKey::ArrowRight | EditorKey::PageUp | EditorKey::PageDown |
                EditorKey::Home | EditorKey::End => self.move_cursor(c),
                EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('l') => (),
                EditorKey::Verbatim(chr) if chr == ctrl_key('e') => {
                    if self.core.modified {
                        self.status.set_message(
                            "File has unsaved changed, and \
                             cannot be refreshed. Quit and \
                             reopen to discard changes.",
                        );
                    } else if self.filename.is_some() {
                        self.open().expect("Refreshing file failed")
                    } else {
                        self.status.set_message("No file to refresh")
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('s') => {
                    match self.save() {
                        Ok(()) => (),
                        Err(e) => {
                            self.status.set_message(
                                &format!("Saving failed with {}", e),
                            )
                        }
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('f') => self.find(),
                EditorKey::Verbatim(chr) if chr == ctrl_key('g') => self.go_to(),
                EditorKey::Verbatim(chr) if chr == ctrl_key(' ') => self.core.toggle_fold(),
                EditorKey::Verbatim(chr) if chr == ctrl_key('p') => self.toggle_paste_mode(),
                EditorKey::Verbatim(chr) if chr == ctrl_key('h') => self.display_help(),
                // Editing commands
                EditorKey::Delete |
                EditorKey::Verbatim(_) if self.core.folds.contains_key(&self.core.cursor_y) => {
                    self.status.set_message(DONT_EDIT_FOLDS);
                }
                EditorKey::Verbatim('\r') => self.core.insert_newline(self.paste_mode),
                EditorKey::Delete => {
                    if self.core.folds.contains_key(&(self.core.cursor_y + 1)) &&
                        self.core.cursor_x == self.core.rows[self.core.cursor_y].text.len()
                    {
                        self.status.set_message(DONT_EDIT_FOLDS);
                    } else {
                        self.move_cursor(EditorKey::ArrowRight);
                        let delete_result = self.core.delete_char();
                        if let Err(reason) = delete_result {
                            assert_eq!(reason, DONT_EDIT_FOLDS);
                            self.status.set_message(DONT_EDIT_FOLDS);
                        }
                    }
                }
                EditorKey::Verbatim(chr) if chr as usize == 127 => {
                    let delete_result = self.core.delete_char();
                    if let Err(reason) = delete_result {
                        assert_eq!(reason, DONT_EDIT_FOLDS);
                        self.status.set_message(DONT_EDIT_FOLDS);
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('k') => self.core.delete_row(),
                EditorKey::Verbatim('\t') => self.core.insert_tab(),
                EditorKey::Verbatim(chr) => self.core.insert_char(chr),
            };
            self.quit_times = 3;
            true
        }
    }
}

/// * init **

fn run() {
    let mut editor_config: EditorConfig<io::Stdin, io::BufWriter<io::Stdout>> =
        EditorConfig::<io::Stdin, io::BufWriter<io::Stdout>>::new();
    print!("{}", CLEAR_SCREEN);
    if let Some(filename) = env::args().nth(1) {
        editor_config.filename = Some(filename);
        editor_config.open().expect("Opening file failed")
    }
    editor_config.display_help();
    loop {
        editor_config.warn_consistency();
        editor_config.refresh_screen().expect(
            "Screen should refresh successfully",
        );
        let keypress = read_key(&mut editor_config.input_source);
        let to_continue = editor_config.process_keypress(keypress);
        if !to_continue {
            break;
        }
    }
}

fn main() {
    let orig_termios = enable_raw_mode().expect("Enabling raw mode failed");

    // Main program is run in such away that we can catch panics and recover.
    // Panics should not be normal control flow, but this is nicer in the face of bugs.
    let run_result = catch_unwind(run);
    restore_orig_mode(&orig_termios).expect("Disabling raw mode failed");
    run_result.expect("Editor loop panicked");

    print!("{}", CLEAR_SCREEN);
    print!("{}", CURSOR_TOP_RIGHT);
    io::stdout().flush().expect("Flushing stdout failed");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mock_editor(input: Option<&str>) -> EditorConfig<Box<Read>, io::Sink> {
        EditorConfig {
            core: EditorCore {
                rows: vec![],
                cursor_x: 0,
                cursor_y: 0,
                folds: HashMap::new(),
                syntax: None,
                modified: false,
            },

            filename: None,
            screen_rows: 10,
            screen_cols: 10,
            row_offset: 0,
            col_offset: 0,
            status: Status {
                message: String::new(),
                time: Instant::now(),
            },
            quit_times: 3,
            input_source: input.map_or(Box::new(io::empty()), |text| {
                Box::new(FakeStdin::new(text.as_bytes()))
            }),
            output_buffer: io::sink(),
            saved_search: String::new(),
            current_match: None,
            paste_mode: false,
        }
    }

    struct FakeStdin {
        backward_contents: Vec<u8>,
    }

    impl Read for FakeStdin {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            for (i, elem) in buf.iter_mut().enumerate() {
                match self.backward_contents.pop() {
                    Some(b) => *elem = b,
                    None => return Ok(i),
                }
            }
            Ok(buf.len())
        }
    }

    impl FakeStdin {
        fn new(contents: &[u8]) -> FakeStdin {
            let mut contents = contents.to_owned();
            contents.reverse();
            FakeStdin { backward_contents: contents }
        }
    }

    #[test]
    fn empty_text() {
        let mock = mock_editor(None);
        let text = mock.core.all_text();
        assert_eq!(text, "");
        assert_eq!(None, mock.core.check_consistency())
    }

    #[test]
    fn line_roundtrip() {
        let mut mock = mock_editor(None);
        let line = "Hello, world";

        mock.core.load_text(line);
        let text = mock.core.all_text();

        assert_eq!(line, text);
        assert_eq!(None, mock.core.check_consistency())
    }

    #[test]
    fn lines_roundtrip() {
        let mut mock = mock_editor(None);
        let lines = "This\n    might\n    or might not\n    work.\n    \n";

        mock.core.load_text(lines);
        let text = mock.core.all_text();

        assert_eq!(lines, text);
        assert_eq!(None, mock.core.check_consistency())
    }

    #[test]
    fn simple_typing() {
        let typed_text = "Hello, world!";

        let mut mock = mock_editor(None);
        for c in typed_text.chars() {
            mock.process_keypress(EditorKey::Verbatim(c));
        }

        assert_eq!(typed_text, mock.core.all_text());
        assert_eq!(None, mock.core.check_consistency())
    }

    #[test]
    fn reversed_typing() {
        let typed_text = "Hello, world!";

        let mut mock = mock_editor(None);
        for c in typed_text.chars() {
            mock.process_keypress(EditorKey::Verbatim(c));
            mock.process_keypress(EditorKey::ArrowLeft);
        }

        let reversed_text = typed_text.chars().rev().collect::<String>();

        assert_eq!(reversed_text, mock.core.all_text());
        assert_eq!(None, mock.core.check_consistency())
    }

    #[test]
    fn newline_typing() {
        let text = "Hello, world!";

        let mut mock = mock_editor(None);
        mock.core.load_text(text);

        for _ in 0..3 {
            for _ in 0..6 {
                mock.process_keypress(EditorKey::ArrowRight);
            }
            mock.process_keypress(EditorKey::Verbatim('\r'));
        }
        assert_eq!(mock.core.all_text(), "Hello,\n world\n !\n");
    }

    #[test]
    fn moving_around() {
        let mut mock = mock_editor(None);

        mock.process_keypress(EditorKey::ArrowUp);
        assert_eq!(None, mock.core.check_consistency());
        mock.process_keypress(EditorKey::ArrowLeft);
        assert_eq!(None, mock.core.check_consistency());
        mock.process_keypress(EditorKey::ArrowDown);
        assert_eq!(None, mock.core.check_consistency());
        mock.process_keypress(EditorKey::ArrowRight);
        assert_eq!(None, mock.core.check_consistency());
    }

    #[test]
    fn hello_world_highlight() {
        let text = "fn main() {
    println!(\"Hello, world!\\\"\");
    123 // 123
}";
        let mut mock = mock_editor(None);
        mock.filename = Some("main.rs".to_string());
        mock.activate_syntax();

        mock.core.load_text(text);

        assert_eq!(mock.core.rows.len(), 4);
        let (h0, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        assert!(h0[..2].iter().all(|&hl| hl == EditorHighlight::Keyword1));
        assert!(h0[2..].iter().all(|&hl| hl == EditorHighlight::Normal));
        let (h1, oq) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        assert!(h1[..13].iter().all(|&hl| hl == EditorHighlight::Normal));
        assert!(h1[13..30].iter().all(|&hl| hl == EditorHighlight::String));
        assert!(h1[30..].iter().all(|&hl| hl == EditorHighlight::Normal));
        let (h2, oq) = mock.core.rows[2].make_highlights(&mock.core.syntax, oq);
        assert!(h2[..4].iter().all(|&hl| hl == EditorHighlight::Normal));
        assert!(h2[4..7].iter().all(|&hl| hl == EditorHighlight::Number));
        assert!(h2[7..8].iter().all(|&hl| hl == EditorHighlight::Normal));
        assert!(h2[8..].iter().all(|&hl| hl == EditorHighlight::Comment));
        let (h3, _) = mock.core.rows[3].make_highlights(&mock.core.syntax, oq);
        assert!(h3.iter().all(|&hl| hl == EditorHighlight::Normal));
    }
    #[test]
    fn multiline_string_highlight() {
        let text = "Outside \"Inside
    Middle
    End\" Done";

        let mut mock = mock_editor(None);
        mock.filename = Some("main.rs".to_string());
        mock.activate_syntax();

        mock.core.load_text(text);

        assert_eq!(mock.core.rows.len(), 3);
        let (h0, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        assert!(h0[..8].iter().all(|&hl| hl == EditorHighlight::Normal));
        assert!(h0[8..].iter().all(|&hl| hl == EditorHighlight::String));
        let (h1, oq) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        assert!(h1.iter().all(|&hl| hl == EditorHighlight::String));
        let (h2, _) = mock.core.rows[2].make_highlights(&mock.core.syntax, oq);
        assert!(h2[..8].iter().all(|&hl| hl == EditorHighlight::String));
        assert!(h2[8..].iter().all(|&hl| hl == EditorHighlight::Normal));
    }

    #[test]
    fn temporary_multiline_string() {
        let text = "\"\na";

        let mut mock = mock_editor(None);
        mock.filename = Some("main.rs".to_string());
        mock.activate_syntax();

        mock.core.load_text(text);
        let (_, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        let (h1, _) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        assert_eq!(h1[0], EditorHighlight::String);
        mock.process_keypress(EditorKey::Delete);
        let (_, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        let (h1, _) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        assert_eq!(h1[0], EditorHighlight::Normal);
    }

    #[test]
    fn multiline_string_with_blank_line() {
        let text = "\"\n\na";

        let mut mock = mock_editor(None);
        mock.filename = Some("main.rs".to_string());
        mock.activate_syntax();

        mock.core.load_text(text);

        let (_, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        let (_, oq) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        let (h2, _) = mock.core.rows[2].make_highlights(&mock.core.syntax, oq);
        assert_eq!(h2[0], EditorHighlight::String);
    }

    #[test]
    fn string_ending_at_eol() {
        let text = "\"Hi!\"\na";

        let mut mock = mock_editor(None);
        mock.filename = Some("main.rs".to_string());
        mock.activate_syntax();
        mock.core.load_text(text);

        let (_, oq) = mock.core.rows[0].make_highlights(&mock.core.syntax, None);
        let (h1, _) = mock.core.rows[1].make_highlights(&mock.core.syntax, oq);
        assert_eq!(h1[0], EditorHighlight::Normal);
    }

    #[test]
    fn fold_last_row_delete_char() {
        let text = "a\nb\n c";

        let mut mock = mock_editor(None);
        mock.core.load_text(text);

        assert_eq!(mock.core.rows.len(), 3);

        mock.process_keypress(EditorKey::ArrowDown);
        mock.process_keypress(EditorKey::ArrowDown);
        assert_eq!(mock.core.cursor_y, 2);
        mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock.process_keypress(EditorKey::ArrowUp);
        mock.process_keypress(EditorKey::ArrowUp);
        mock.process_keypress(EditorKey::ArrowRight);
        assert_eq!(mock.core.cursor_y, 0);
        assert_eq!(mock.core.cursor_x, 1);
        mock.process_keypress(EditorKey::Delete);
        assert_eq!(mock.core.rows.len(), 2);
        mock.refresh_screen().unwrap();
    }

    #[test]
    fn fold_last_row_delete_row() {
        let text = "a\n b";

        let mut mock = mock_editor(None);
        mock.core.load_text(text);

        mock.process_keypress(EditorKey::ArrowDown);
        mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock.process_keypress(EditorKey::ArrowUp);
        mock.process_keypress(EditorKey::Verbatim(ctrl_key('k')));
        mock.refresh_screen().unwrap();
        assert_eq!(" b", mock.core.all_text());
    }

    #[test]
    fn fold_next_to_empty_line() {
        let text = "\na\n";
        let mut mock = mock_editor(None);
        mock.core.load_text(text);

        mock.process_keypress(EditorKey::ArrowDown);
        mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock.refresh_screen().unwrap();
    }

    #[test]
    fn fold_wraparound() {
        let text = "a\n b\n c\nd";

        let mut mock = mock_editor(None);
        mock.core.load_text(text);

        mock.process_keypress(EditorKey::ArrowDown);
        mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock.process_keypress(EditorKey::ArrowRight);
        mock.process_keypress(EditorKey::ArrowRight);
        mock.process_keypress(EditorKey::ArrowRight);
        assert_eq!(None, mock.core.check_consistency());
        assert_eq!(0, mock.core.cursor_x);
        assert_eq!(3, mock.core.cursor_y);
        mock.process_keypress(EditorKey::ArrowLeft);
        assert_eq!(None, mock.core.check_consistency());
        assert_eq!(2, mock.core.cursor_x);
        assert_eq!(1, mock.core.cursor_y);
    }

    #[test]
    fn fold_all() {
        let text = "a\n  b\n c\nd\n e\n f";

        let mut mock1 = mock_editor(None);
        mock1.core.load_text(text);
        mock1.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));

        let mut mock2 = mock_editor(None);
        mock2.core.load_text(text);
        mock2.process_keypress(EditorKey::ArrowDown);
        mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock2.process_keypress(EditorKey::ArrowDown);
        mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
        mock2.process_keypress(EditorKey::ArrowDown);
        mock2.process_keypress(EditorKey::ArrowDown);
        mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));

        assert_eq!(mock1.core.folds, mock2.core.folds);
    }
    #[test]
    fn read_key_escapes() {
        let keys = "Hi!\x1b[1~I say \x1b[4~\rOk, bye.\x1b[5~\x1b[7~\x1b[3~We\x11";
        let mut mock = mock_editor(Some(keys));
        while mock.quit_times == 3 {
            let keypress = read_key(&mut mock.input_source);
            mock.process_keypress(keypress);
        }

        assert_eq!("We say Hi!\nOk, bye.", mock.core.all_text())
    }

    #[test]
    fn find_no_text() {
        let keys = "\x06me\r";

        let mut mock = mock_editor(Some(keys));
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);
    }

    #[test]
    fn find() {
        let text = "Hi\nmy\nname\n\nis\ntext.";

        let keys = "\x06me\r";

        let mut mock = mock_editor(Some(keys));

        mock.core.load_text(text);
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);

        // Indicates the position of the match.
        assert_eq!(2, mock.core.cursor_y);
        assert_eq!(2, mock.core.cursor_x);

        // Highlighting should have been cleared.
        assert!(
            mock.core
                .rows
                .iter()
                .flat_map(|row| row.make_highlights(&mock.core.syntax, None).0)
                .all(|hl| hl == EditorHighlight::Normal)
        );
    }

    #[test]
    fn find_one_highlight() {
        let text = "This text contains burble once.";

        let keys = "\x06burble\r";

        let mut mock = mock_editor(Some(keys));

        // Search for "burble"
        mock.core.load_text(text);
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);

        assert_eq!(0, mock.core.cursor_y);
        assert_eq!(19, mock.core.cursor_x);

        let saved = &mock.saved_search.clone();
        mock.find_callback(saved, EditorKey::ArrowDown);
        assert_eq!(Some((0, 19, 6)), mock.current_match);
    }

    #[test]
    fn goto() {
        let text = "F\n    i\n    l\n    l\n    e\n    r";

        // Ctrl-g 4 Enter
        let keys = "\x074\r";

        let mut mock = mock_editor(Some(keys));

        mock.core.load_text(text);
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);

        assert_eq!(3, mock.core.cursor_y);
        assert_eq!(0, mock.core.cursor_x);
    }

    #[test]
    fn insert_tab() {
        let keys = "\t\x1b[C a\t";
        let mut mock = mock_editor(Some(keys));

        for _ in 0..4 {
            let keypress = read_key(&mut mock.input_source);
            mock.process_keypress(keypress);
        }

        let out_test = "    \na   ";
        assert_eq!(out_test, mock.core.all_text());
    }

    #[test]
    fn clear_find_after_esc() {
        let text = "a\nab\nc\nabc";
        // Ctrl-f, ab, enter, Ctrl-f, ArrowRight, Esc, Ctrl-f, c, enter
        let keys = "\x06ab\r\x06\x1b[C \x1b   \x06c\r";
        let mut mock = mock_editor(Some(keys));
        mock.core.load_text(text);

        // Search for ab
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);
        assert_eq!(1, mock.core.cursor_y);
        assert_eq!("ab", mock.saved_search);

        // Clear the search
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);
        assert_eq!(1, mock.core.cursor_y);
        assert_eq!("", mock.saved_search);

        // Search for c
        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);
        assert_eq!(2, mock.core.cursor_y);

        assert_eq!(mock.core.all_text(), text);
    }

    #[test]
    fn scroll_left_at_start_of_file() {
        let text = "Hi!";
        let keys = "\x1b[D ";
        let mut mock = mock_editor(Some(keys));
        mock.core.load_text(text);

        let keypress = read_key(&mut mock.input_source);
        mock.process_keypress(keypress);
        assert_eq!(0, mock.core.cursor_x);
        assert_eq!(0, mock.core.cursor_y);
    }
}
