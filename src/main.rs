#![allow(unknown_lints)]
#![warn(clippy_pedantic)]
#![allow(print_stdout, missing_docs_in_private_items, string_add)]
extern crate termios;
extern crate libc;

use std::io::{self, Read, Write};
use std::fs::File;
use std::path::Path;
use std::env;

use std::panic::catch_unwind;

use std::time::Instant;

use std::collections::HashMap;

use std::cmp::min;

use std::ascii::AsciiExt;

use std::os::raw::c_int;
use termios::Termios;

/// * testing **

mod tests;

/// * utility **

const IED_VERSION: &'static str = "0.2.0";

const TAB_STOP: usize = 4;

const STDIN: c_int = 1;
const STDOUT: c_int = 2;

const INVERT_COLORS: &'static str = "\x1b[7m";
const REVERT_COLORS: &'static str = "\x1b[m";

const RED: &'static str = "\x1b[31m\x1b[49m";
const MAGENTA: &'static str = "\x1b[1m\x1b[35m\x1b[49m";
const CYAN: &'static str = "\x1b[36m\x1b[49m";
const YELLOW: &'static str = "\x1b[33m\x1b[49m";
const BRIGHT_YELLOW: &'static str = "\x1b[1m\x1b[33m\x1b[49m";
const GREEN: &'static str = "\x1b[32m\x1b[49m";
const BRIGHT_GREEN: &'static str = "\x1b[1m\x1b[32m\x1b[49m";

const BACK_BLUE: &'static str = "\x1b[39m\x1b[44m";

const CURSOR_TOP_RIGHT: &'static str = "\x1b[H";

const HIDE_CURSOR: &'static str = "\x1b[?25l";
const SHOW_CURSOR: &'static str = "\x1b[?25h";

const CLEAR_RIGHT: &'static str = "\x1b[K";
const CLEAR_SCREEN: &'static str = "\x1b[2J";

const DONT_EDIT_FOLDS: &'static str = "Folded lines can't be edited. Ctrl-Space to unfold.";

/// * data **
#[derive(Clone)]
struct Row {
    cells: Vec<Cell>,
    open_quote: Option<char>,
}

struct EditorConfig<T: Read> {
    cursor_x: usize,
    cursor_y: usize,
    screen_rows: usize,
    screen_cols: usize,
    rows: Vec<Row>,
    row_offset: usize,
    col_offset: usize,
    filename: Option<String>,
    syntax: Option<EditorSyntax>,
    status_message: String,
    status_message_time: Instant,
    modified: bool,
    quit_times: usize,
    folds: HashMap<usize, (usize, usize)>,
    input_source: T,
    saved_search: String,
}

#[derive(Copy, Clone)]
struct Cell {
    chr: char,
    hl: EditorHighlight,
}

#[derive(Copy, Clone, PartialEq)]
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
    fn color(&self) -> String {
        REVERT_COLORS.to_owned() +
        match *self {
            EditorHighlight::Normal => "",
            EditorHighlight::Number => RED,
            EditorHighlight::Match => BACK_BLUE,
            EditorHighlight::String => MAGENTA,
            EditorHighlight::Comment => CYAN,
            EditorHighlight::Keyword1 => YELLOW,
            EditorHighlight::Keyword2 => GREEN,
            EditorHighlight::Keyword3 => BRIGHT_GREEN,
            EditorHighlight::Keyword4 => BRIGHT_YELLOW,
        }
    }
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

/// * filetypes **
struct EditorSyntax {
    filetype: String,
    extensions: Vec<String>,
    has_digits: bool,
    quotes: String,
    singleline_comment: String,
    keywords: [Vec<String>; 4],
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
    c.is_whitespace() || "&{}'\",.()+-/*=~%<>[];:".contains(c)
}

fn whitespace_depth(row: &Row) -> usize {
    row.cells
        .iter()
        .position(|cell| !cell.chr.is_whitespace())
        .unwrap_or_else(|| row.cells.len())
}

/// * row operations **

fn row_to_string(row: &Row) -> String {
    cells_to_string(&row.cells)
}

fn cells_to_string(cells: &[Cell]) -> String {
    cells.iter().map(|&cell| cell.chr).collect::<String>()
}

fn string_to_row(s: &str) -> Row {
    Row {
        cells: s.chars()
            .map(|c| {
                     Cell {
                         chr: c,
                         hl: EditorHighlight::Normal,
                     }
                 })
            .collect(),
        open_quote: None,
    }
}

impl<T> EditorConfig<T>
    where T: io::Read
{
    fn new() -> EditorConfig<io::Stdin> {
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
                filename: None,
                screen_rows: ws.ws_row.checked_sub(2).expect("Need at least 2 rows") as usize,
                screen_cols: ws.ws_col as usize,
                rows: vec![],
                row_offset: 0,
                col_offset: 0,
                cursor_x: 0,
                cursor_y: 0,
                status_message: String::new(),
                status_message_time: Instant::now(),
                modified: false,
                quit_times: 3,
                syntax: None,
                folds: HashMap::new(),
                input_source: io::stdin(),
                saved_search: String::new(),
            }
        }
    }
    fn warn_consistency(&mut self) {
        let failure = {
            self.check_consistency()
        };
        if let Some(message) = failure {
            self.set_status_message(message);
        }
    }

    /// * debug & testing **

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
                   !(start1 == start2 && end1 == end2) {
                    fold_fold_failure = Some("Two folds overlap");
                    break;
                }
            }
        }
        cursor_position_failure
            .or(fold_failure)
            .or(fold_fold_failure)
    }


    /// * folding **

    fn toggle_fold(&mut self) {
        if self.folds.contains_key(&self.cursor_y) {
            self.folds.remove(&self.cursor_y);
        } else if self.cursor_y < self.rows.len() &&
                  whitespace_depth(&self.rows[self.cursor_y]) == 0 {
            let mut index = 0;
            while index < self.rows.len() {
                if let Some(&(end, _)) = self.folds.get(&index) {
                    index = end + 1;
                } else if whitespace_depth(&self.rows[index]) > 0 {
                    let depth = whitespace_depth(&self.rows[index]);
                    let end = self.rows
                        .iter()
                        .enumerate()
                        .skip(index + 1)
                        .position(|(oth_index, row)| {
                                      whitespace_depth(row) < depth && !row.cells.is_empty() ||
                                      self.folds.contains_key(&oth_index)
                                  })
                        .map_or(self.rows.len() - 1, |offset| index + offset);
                    self.folds.insert(index, (end, depth));
                    index = end + 1;
                } else {
                    index += 1;
                }
            }
        } else {
            self.create_fold();
        }
    }
    fn create_fold(&mut self) {
        if self.cursor_y < self.rows.len() {
            let fold_depth = whitespace_depth(&self.rows[self.cursor_y]);
            let start = self.rows
                .iter()
                .rev()
                .skip(self.rows.len() - self.cursor_y)
                .position(|row| whitespace_depth(row) < fold_depth && !row.cells.is_empty())
                .map_or(0, |reverse_offset| self.cursor_y - reverse_offset);
            let end = self.rows
                .iter()
                .skip(self.cursor_y + 1)
                .position(|row| whitespace_depth(row) < fold_depth && !row.cells.is_empty())
                .map_or(self.rows.len() - 1, |offset| self.cursor_y + offset);
            self.folds.insert(start, (end, fold_depth));
            self.cursor_y = start;
        }
    }

    fn open_folds(&mut self) {
        for (start, (end, _depth)) in self.folds.clone() {
            if start <= self.cursor_y && self.cursor_y <= end {
                self.folds.remove(&start);
            }
        }
    }

    fn one_row_forward(&self, index: usize) -> usize {
        min(self.rows.len(),
            self.folds.get(&index).map_or(index, |&(end, _)| end) + 1)
    }

    fn one_row_back(&self, index: usize) -> usize {
        if let Some(prev_index) = index.checked_sub(1) {
            if let Some((&start, _end_and_depth)) =
                self.folds
                    .iter()
                    .find(|&(_start, &(end, _depth))| end == prev_index) {
                start
            } else {
                prev_index
            }
        } else {
            0
        }
    }

    /// * row operations **

    fn current_row_len(&self) -> usize {
        self.rows
            .get(self.cursor_y)
            .map_or(0, |row| row.cells.len())
    }

    // Assumes that index is past the open quote, returns final position of index,
    // Which is past the last cell iff the string is still open.
    fn update_highlights_string(cells: &mut [Cell], start_quote: char, index: usize) -> usize {
        let mut index = index;
        macro_rules! update_and_advance {
            ($highlight_expression:expr) => {
                 cells[index].hl = $highlight_expression;
                 index += 1;
            }
        }

        while index < cells.len() {
            if cells[index].chr == start_quote {
                update_and_advance!(EditorHighlight::String);
                break;
            }
            if cells[index].chr == '\\' && index + 1 < cells.len() {
                update_and_advance!(EditorHighlight::String);
            }
            update_and_advance!(EditorHighlight::String);
        }
        index
    }

    fn update_row_highlights(&mut self, row_index: usize) {
        let maybe_open_quote: Option<char> = row_index
            .checked_sub(1)
            .and_then(|prev_index| self.rows.get(prev_index))
            .and_then(|prev_row| prev_row.open_quote);
        if let Some(mut row) = self.rows.get_mut(row_index) {
            if let Some(ref syntax) = self.syntax {
                row.open_quote = None;
                let mut cells = &mut row.cells;
                let mut index = 0;
                macro_rules! update_and_advance {
                    ($highlight_expression:expr) => {
                        cells[index].hl = $highlight_expression;
                        index += 1;
                    }
                }
                'outer: while index < cells.len() {
                    let prev_is_sep = index == 0 || is_separator(cells[index - 1].chr);
                    if index == 0 && maybe_open_quote.is_some() {
                        let open_quote = maybe_open_quote.expect("Just checked_it");
                        index =
                            EditorConfig::<T>::update_highlights_string(cells, open_quote, index);
                        if index >= cells.len() {
                            row.open_quote = Some(open_quote)
                        }
                    } else if syntax.quotes.contains(cells[index].chr) {
                        let start_quote = cells[index].chr;
                        update_and_advance!(EditorHighlight::String);
                        index =
                            EditorConfig::<T>::update_highlights_string(cells, start_quote, index);
                        if index >= cells.len() {
                            row.open_quote = Some(start_quote)
                        }
                    } else if syntax.has_digits && cells[index].chr.is_digit(10) && prev_is_sep {
                        while index < cells.len() && cells[index].chr.is_digit(10) {
                            update_and_advance!(EditorHighlight::Number);
                        }
                    } else if cells_to_string(&cells[index..].to_vec())
                                  .starts_with(&syntax.singleline_comment) {
                        while index < cells.len() {
                            update_and_advance!(EditorHighlight::Comment);
                        }
                    } else {
                        if index == 0 || is_separator(cells[index - 1].chr) {
                            let following_string: String = cells_to_string(&cells[index..]
                                                                                .to_vec());
                            for (keywords, &highlight) in
                                syntax
                                    .keywords
                                    .iter()
                                    .zip([EditorHighlight::Keyword1,
                                          EditorHighlight::Keyword2,
                                          EditorHighlight::Keyword3,
                                          EditorHighlight::Keyword4]
                                                 .iter()) {
                                for keyword in keywords {
                                    let keyword_end = index + keyword.len();
                                    if following_string.starts_with(keyword) &&
                                       (keyword_end == cells.len() ||
                                        is_separator(cells[keyword_end].chr)) {
                                        while index < keyword_end {
                                            update_and_advance!(highlight);
                                        }
                                        continue 'outer;
                                    }
                                }
                            }
                        }
                        update_and_advance!(EditorHighlight::Normal);
                    }
                }
            } else {
                for cell in row.cells.iter_mut() {
                    cell.hl = EditorHighlight::Normal
                }
            }
        }
        if row_index < self.rows.len() && self.rows[row_index].open_quote.is_some() {
            self.update_row_highlights(row_index + 1);
        }
    }

    fn select_syntax(&mut self) {
        self.syntax = self.filename
            .as_ref()
            .and_then(|filename| {
                let syntax_database =
                    vec![EditorSyntax {
                             filetype: "rust".to_string(),
                             extensions: vec![".rs".to_string()],
                             has_digits: true,
                             quotes: "\"".to_string(),
                             singleline_comment: "//".to_string(),
                             keywords: [vec!["alignof", "as", "break", "continue", "crate",
                                             "else", "extern", "fn", "for", "if", "impl", "in",
                                             "let", "loop", "macro", "match", "mod", "offsetof",
                                             "pub", "return", "sizeof", "trait", "typeof",
                                             "unsafe", "use", "where", "while", "yield"]
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<_>>(),
                                        vec!["box", "mut", "const", "enum", "ref", "static",
                                             "struct", "type"]
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<_>>(),
                                        vec!["false", "self", "Self", "super", "true"]
                                            .iter()
                                            .map(|x| x.to_string())
                                            .collect::<Vec<_>>(),
                                        vec!["bool", "char", "i8", "i16", "i32", "i64", "isize",
                                             "f32", "f64", "str", "u8", "u16", "u32", "u64",
                                             "usize"]
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<_>>()],
                         },
                         EditorSyntax {
                             filetype: "c".to_string(),
                             extensions: vec![".c".to_string(),
                                              ".h".to_string(),
                                              ".cpp".to_string()],
                             has_digits: true,
                             quotes: "\"'".to_string(),
                             singleline_comment: "//".to_string(),
                             keywords: [vec![], vec![], vec![], vec![]],
                         },
                         EditorSyntax {
                             filetype: "py".to_string(),
                             extensions: vec![".py".to_string()],
                             has_digits: true,
                             quotes: "\"'".to_string(),
                             singleline_comment: "#".to_string(),
                             keywords: [vec!["break", "continue", "def", "elif", "else", "for",
                                             "from", "if", "import", "in", "return", "while"]
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<_>>(),
                                        vec!["any", "abs", "input", "int", "len", "range",
                                             "print", "zip"]
                                                .iter()
                                                .map(|x| x.to_string())
                                                .collect::<Vec<_>>(),
                                        vec!["False", "True"]
                                            .iter()
                                            .map(|x| x.to_string())
                                            .collect::<Vec<_>>(),
                                        vec!["and", "not", "or"]
                                            .iter()
                                            .map(|x| x.to_string())
                                            .collect::<Vec<_>>()],
                         }];
                syntax_database
                    .into_iter()
                    .find(|entry| {
                              entry
                                  .extensions
                                  .iter()
                                  .any(|extension| filename.ends_with(extension))
                          })
            });
        for index in 0..self.rows.len() {
            self.update_row_highlights(index);
        }
    }


    /// * editor operations **

    fn insert_char(&mut self, c: char) {
        if self.cursor_y == self.rows.len() {
            self.rows
                .push(Row {
                          cells: Vec::new(),
                          open_quote: None,
                      });
        }
        self.rows[self.cursor_y]
            .cells
            .insert(self.cursor_x,
                    Cell {
                        chr: c,
                        hl: EditorHighlight::Normal,
                    });
        let index = self.cursor_y;
        self.update_row_highlights(index);
        self.cursor_x += 1;
        self.modified = true;
    }

    fn insert_newline(&mut self) {
        let open_quote = if let Some(prev_y) = self.cursor_y.checked_sub(1) {
            self.rows[prev_y].open_quote
        } else {
            None
        };

        if self.cursor_y < self.rows.len() {
            let depth = whitespace_depth(&self.rows[self.cursor_y]);
            // If in the whitespace, insert blank line.
            if depth >= self.cursor_x {
                self.rows
                    .insert(self.cursor_y,
                            Row {
                                cells: vec![],
                                open_quote: open_quote,
                            });
            } else {
                let cells_end = self.rows[self.cursor_y].cells.split_off(self.cursor_x);
                let mut next_row = self.rows[self.cursor_y].clone();

                next_row.cells = next_row.cells[..depth].to_vec();
                next_row.cells.extend(cells_end);
                self.rows.insert(self.cursor_y + 1, next_row);
            }
            for row_index in (self.cursor_y..self.rows.len()).rev() {
                if let Some((end, depth)) = self.folds.remove(&row_index) {
                    self.folds.insert(row_index + 1, (end + 1, depth));
                }
            }
            let index = self.cursor_y;
            self.update_row_highlights(index);
            self.update_row_highlights(index + 1);

            self.cursor_x = depth;
        } else {
            self.rows
                .push(Row {
                          cells: vec![],
                          open_quote: open_quote,
                      })
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
            } else {
                self.rows[self.cursor_y].cells.truncate(self.cursor_x);
                let index = self.cursor_y;
                self.update_row_highlights(index);
            }
            self.modified = true
        }
    }

    fn delete_char(&mut self) {
        if let Some(prev_x) = self.cursor_x.checked_sub(1) {
            self.rows[self.cursor_y].cells.remove(prev_x);
            let index = self.cursor_y;
            self.update_row_highlights(index);
            self.cursor_x = prev_x;
            self.modified = true
        } else if 0 < self.cursor_y && self.cursor_y < self.rows.len() {
            if self.folds
                   .values()
                   .any(|&(end, _)| end == self.cursor_y - 1) {
                self.set_status_message(DONT_EDIT_FOLDS);
            } else {
                self.cursor_x = self.rows[self.cursor_y - 1].cells.len();
                let moved_line = self.rows.remove(self.cursor_y);
                let line_to_append = &moved_line.cells[whitespace_depth(&moved_line)..];
                self.rows[self.cursor_y - 1].cells.extend(line_to_append);
                self.rows[self.cursor_y - 1].open_quote = moved_line.open_quote;
                self.shift_folds_back();
                self.cursor_y -= 1;
                let index = self.cursor_y;
                self.update_row_highlights(index);
                self.modified = true
            }
        }
    }

    /// * file i/o **
    fn open(&mut self, filename: &str) -> io::Result<()> {
        self.filename = Some(filename.to_string());
        self.select_syntax();

        if !Path::new(filename).exists() {
            File::create(filename)?;
        }
        let mut file = File::open(filename)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        self.load_text(&string);
        Ok(())
    }

    fn load_text(&mut self, text: &str) {
        self.rows = vec![];
        self.folds = HashMap::new();

        let mut row_buffer = String::new();
        for byte in text.bytes() {
            let c = byte as char;
            if c == '\n' {
                let row = string_to_row(&row_buffer);
                let update_index = self.rows.len();
                self.rows.push(row);
                self.update_row_highlights(update_index);
                row_buffer.truncate(0);
            } else {
                row_buffer.push(c);
            }
        }
        let row = string_to_row(&row_buffer);
        let update_index = self.rows.len();
        self.rows.push(row);
        self.update_row_highlights(update_index);

        self.cursor_y = min(self.cursor_y, self.rows.len());
        self.cursor_x = 0;
        self.scroll();
    }

    fn all_text(&self) -> String {
        self.rows
            .iter()
            .map(|row| row_to_string(row))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn save(&mut self) -> io::Result<()> {
        let filename = if let Some(ref filename) = self.filename {
            filename.clone()
        } else {
            match self.prompt("Save as: ", "", None) {
                None => {
                    self.set_status_message("Save aborted");
                    return Ok(());
                }
                Some(filename) => {
                    self.filename = Some(filename.clone());
                    self.select_syntax();
                    filename
                }
            }
        };

        let mut file = File::create(filename)?;
        let text = self.all_text();
        file.write_all(text.as_bytes())?;
        self.modified = false;
        self.set_status_message(&format!("{} bytes written to disk", text.len()));
        Ok(())
    }

    /// * macro movement **

    fn find_callback(&mut self, query: &str, key: EditorKey) {
        if self.cursor_y < self.rows.len() {
            let index = self.cursor_y;
            self.update_row_highlights(index)
        }
        if key != EditorKey::Verbatim('\r') && key != EditorKey::Verbatim('\x1b') {
            let match_line = {
                let find_predicate = &|row: &Row| row_to_string(row).contains(query);
                if key == EditorKey::ArrowRight || key == EditorKey::ArrowDown {
                    let potential_match = if self.cursor_y + 1 < self.rows.len() {
                        self.rows
                            .iter()
                            .skip(self.cursor_y + 1)
                            .position(find_predicate)
                            .map(|offset| offset + self.cursor_y + 1)
                    } else {
                        None
                    };
                    potential_match.or_else(|| self.rows.iter().position(find_predicate))
                } else if key == EditorKey::ArrowLeft || key == EditorKey::ArrowUp {
                    let potential_match = self.rows[..self.cursor_y]
                        .iter()
                        .rposition(find_predicate);
                    potential_match.or_else(|| self.rows.iter().rposition(find_predicate))
                } else {
                    let potential_match = if self.cursor_y < self.rows.len() {
                        self.rows
                            .iter()
                            .skip(self.cursor_y)
                            .position(find_predicate)
                            .map(|offset| offset + self.cursor_y)
                    } else {
                        None
                    };
                    potential_match.or_else(|| self.rows.iter().position(find_predicate))
                }
            };
            if let Some(match_line) = match_line {
                let match_index = row_to_string(&self.rows[match_line])
                    .find(query)
                    .expect("We just checked the row contained the string");
                self.cursor_y = match_line;
                self.open_folds();
                self.cursor_x = match_index;
                self.row_offset = self.rows.len();
                for cell in self.rows[match_line]
                        .cells
                        .iter_mut()
                        .skip(match_index)
                        .take(query.len()) {
                    cell.hl = EditorHighlight::Match
                }
            }
        }
    }

    fn find(&mut self) {
        let saved_cursor_x = self.cursor_x;
        let saved_cursor_y = self.cursor_y;
        let saved_col_offset = self.col_offset;
        let saved_row_offset = self.row_offset;
        let saved_folds = self.folds.clone();

        let saved_search = self.saved_search.to_owned();

        let query = self.prompt("Search (ESC/Arrows/Enter): ",
                                &saved_search,
                                Some(&|ed, query, key| ed.find_callback(query, key)));

        match query {
            None => {
                self.cursor_x = saved_cursor_x;
                self.cursor_y = saved_cursor_y;
                self.col_offset = saved_col_offset;
                self.row_offset = saved_row_offset;
                self.folds = saved_folds;
            }
            Some(search) => {
                self.saved_search = search.clone();
            }
        }
    }

    fn go_to(&mut self) {
        if let Some(response) = self.prompt("Go to line: ", "", None) {
            match response.parse::<usize>() {
                Ok(line) => {
                    if 0 < line && line - 1 <= self.rows.len() {
                        self.cursor_y = line - 1;
                        self.cursor_x = 0;
                    } else {
                        self.set_status_message(&format!("Line {} outside of range of file", line));
                    }
                }
                Err(_) => self.set_status_message("Line was not numeric"),
            }
        }
    }

    /// * output **

    fn scroll(&mut self) {
        self.row_offset = min(self.row_offset, self.cursor_y);
        while self.screen_y() >= self.screen_rows {
            self.row_offset = self.one_row_forward(self.row_offset)
        }
        self.col_offset = min(self.col_offset, self.cursor_x);
        while self.screen_x() >= self.screen_cols {
            self.col_offset += 1;
        }
    }

    fn draw_rows(&self, append_buffer: &mut String) {
        let tab = &" ".repeat(TAB_STOP);
        let mut screen_y = 0;
        let mut file_row = self.row_offset;
        while screen_y < self.screen_rows {
            if let Some(&(fold_end, fold_depth)) = self.folds.get(&file_row) {
                let cells = &self.rows[file_row].cells;
                let fold_white = if fold_depth < cells.len() {
                    Row {
                        cells: cells[..fold_depth].to_vec(),
                        open_quote: None,
                    }
                } else {
                    string_to_row(&" ".repeat(fold_depth))
                };
                let fold_white_visible = Row {
                    cells: if self.col_offset < fold_white.cells.len() {
                        fold_white.cells[self.col_offset..].to_vec()
                    } else {
                        vec![]
                    },
                    open_quote: None,
                };
                let mut fold_white_str = row_to_string(&fold_white_visible).replace("\t", tab);
                let fold_msg = format!("{} lines folded.", fold_end - file_row + 1);
                fold_white_str.truncate(self.screen_cols.saturating_sub(fold_msg.len()));
                append_buffer.push_str(&fold_white_str);
                append_buffer.push_str(INVERT_COLORS);
                append_buffer.push_str(&fold_msg);
                let padding = self.screen_cols
                    .saturating_sub(fold_msg.len() + fold_white_str.len());
                append_buffer.push_str(&" ".repeat(padding));
                append_buffer.push_str(REVERT_COLORS);
                file_row = fold_end + 1;
            } else if file_row < self.rows.len() {
                let current_cells = &self.rows[file_row].cells;
                if self.col_offset < current_cells.len() {
                    let mut current_hl = EditorHighlight::Normal;
                    let mut chars_written = 0;
                    for &Cell { chr, hl } in current_cells.iter().skip(self.col_offset) {
                        if hl != current_hl {
                            current_hl = hl;
                            append_buffer.push_str(&hl.color());
                        }
                        chars_written += if chr == '\t' { TAB_STOP } else { 1 };
                        if chars_written > self.screen_cols {
                            break;
                        }
                        if chr == '\t' {
                            append_buffer.push_str(tab);
                        } else if chr.is_control() {
                            append_buffer.push_str(INVERT_COLORS);
                            let sym = if chr.is_ascii() && chr as u8 <= 26 {
                                (64 + (chr as u8)) as char
                            } else {
                                '?'
                            };
                            append_buffer.push(sym);
                            append_buffer.push_str(REVERT_COLORS);
                            append_buffer.push_str(&hl.color());
                        } else {
                            append_buffer.push(chr);
                        }
                    }
                    append_buffer.push_str(REVERT_COLORS);
                }
                file_row += 1;
            } else if self.rows.is_empty() && screen_y == self.screen_rows / 3 {
                let welcome = format!("Isaac's editor -- version {}", IED_VERSION);
                let padding = self.screen_cols.saturating_sub(welcome.len()) / 2;
                if padding > 0 {
                    append_buffer.push('~');
                    append_buffer.push_str(&" ".repeat(padding - 1));
                }
                append_buffer.push_str(&welcome);
                file_row += 1;
            } else {
                append_buffer.push('~');
                file_row += 1;
            };
            append_buffer.push_str(CLEAR_RIGHT);
            append_buffer.push_str("\r\n");
            screen_y += 1;
        }
    }

    fn draw_status_bar(&self, append_buffer: &mut String) {
        append_buffer.push_str(INVERT_COLORS);
        let mut name = self.filename
            .clone()
            .unwrap_or_else(|| "[No Name]".to_string());
        name.truncate(20);
        let dirty = if self.modified { "(modified)" } else { "" };
        let mut status = format!("{} {}", name, dirty);
        status.truncate(self.screen_cols);
        append_buffer.push_str(&status);
        let filetype = match self.syntax {
            None => "no ft".to_string(),
            Some(ref syntax) => syntax.filetype.clone(),
        };
        let mut right_status = format!("{} | r: {}/{}, c: {}/{}",
                                       filetype,
                                       self.cursor_y + 1,
                                       self.rows.len(),
                                       self.cursor_x + 1,
                                       self.rows
                                           .get(self.cursor_y)
                                           .map_or(0, |row| row.cells.len()));
        right_status.truncate(self.screen_cols.saturating_sub(status.len() + 1));
        append_buffer.push_str(&" ".repeat(self.screen_cols
                                               .saturating_sub(status.len() +
                                                               right_status.len())));
        append_buffer.push_str(&right_status);
        append_buffer.push_str(REVERT_COLORS);
        append_buffer.push_str("\r\n");
    }

    fn draw_message_bar(&self, append_buffer: &mut String) {
        append_buffer.push_str(CLEAR_RIGHT);
        if self.status_message_time.elapsed().as_secs() < 5 {
            let mut message = self.status_message.clone();
            message.truncate(self.screen_cols);
            append_buffer.push_str(&message);
        }
    }

    fn refresh_screen(&mut self) {
        self.scroll();
        let mut append_buffer: String = String::new();
        append_buffer.push_str(HIDE_CURSOR);
        append_buffer.push_str(CURSOR_TOP_RIGHT);

        self.draw_rows(&mut append_buffer);
        self.draw_status_bar(&mut append_buffer);
        self.draw_message_bar(&mut append_buffer);

        let cursor_control = format!("\x1b[{};{}H", self.screen_y() + 1, self.screen_x() + 1);
        append_buffer.push_str(&cursor_control);
        append_buffer.push_str(SHOW_CURSOR);
        print!("{}", append_buffer);
        io::stdout()
            .flush()
            .expect("Flushing to stdout should work");
    }

    fn set_status_message(&mut self, message: &str) {
        self.status_message = message.to_string();
        self.status_message_time = Instant::now();
    }

    /// * input **

    fn prompt(&mut self,
              prompt: &str,
              initial_response: &str,
              callback: Option<&Fn(&mut EditorConfig<T>, &str, EditorKey) -> ()>)
              -> Option<String> {
        let mut response: String = initial_response.to_owned();
        loop {
            self.set_status_message(&format!("{}{}", prompt, response));
            self.refresh_screen();

            let c = self.read_key();
            macro_rules! maybe_callback {
            () => {
                if let Some(callback) = callback {
                    callback(self, &response, c)
                }
            }
        }
            match c {
                EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('q') => {
                    self.set_status_message("");
                    maybe_callback!();
                    return None;
                }
                EditorKey::Verbatim(chr) if chr == '\r' => {
                    if !response.is_empty() {
                        self.set_status_message("");
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
        self.rows
            .get(self.cursor_y)
            .map_or(0, |row| {
                row.cells
                    .iter()
                    .take(self.cursor_x)
                    .skip(self.col_offset)
                    .map(|cell| if cell.chr == '\t' { TAB_STOP } else { 1 })
                    .sum()
            })
    }

    fn screen_y(&self) -> usize {
        let mut file_y = self.row_offset;
        let mut screen_y = 0;
        while file_y < self.cursor_y {
            file_y = self.one_row_forward(file_y);
            screen_y += 1;
        }
        screen_y
    }

    fn move_cursor(&mut self, key: EditorKey) {
        // Smaller values are up and left
        match key {
            EditorKey::ArrowUp => {
                if self.cursor_y > 0 {
                    self.cursor_y = self.one_row_back(self.cursor_y);
                }
            }
            EditorKey::ArrowDown => {
                if self.cursor_y < self.rows.len() {
                    self.cursor_y = self.one_row_forward(self.cursor_y);
                }
            }
            EditorKey::ArrowLeft => {
                if let Some(prev_x) = self.cursor_x.checked_sub(1) {
                    self.cursor_x = prev_x
                } else {
                    self.cursor_y = self.one_row_back(self.cursor_y);
                    self.cursor_x = self.current_row_len();
                }
            }
            EditorKey::ArrowRight => {
                if self.cursor_x < self.current_row_len() {
                    self.cursor_x += 1
                } else if self.cursor_x == self.current_row_len() {
                    self.cursor_y = self.one_row_forward(self.cursor_y);
                    self.cursor_x = 0
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
            EditorKey::Home => self.cursor_x = 0,
            EditorKey::End => self.cursor_x = self.current_row_len(),

            _ => panic!("Editor move cursor received non moving character"),
        };
        self.cursor_x = min(self.cursor_x, self.current_row_len());
    }

    fn read_key(&mut self) -> EditorKey {
        let mut buffer: [u8; 1] = [0];
        while self.input_source
                  .read(&mut buffer)
                  .expect("Read failure") == 0 {}
        let c = buffer[0] as char;
        if c == '\x1b' {
                let mut escape_buf: [u8; 3] = [0; 3];
                match self.input_source
                          .read(&mut escape_buf)
                          .expect("Read failure during escape sequence") {
                    2 | 3 => {
                        if escape_buf[0] as char == '[' {
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
                        } else if escape_buf[0] as char == 'O' {
                            match escape_buf[1] as char {
                                'H' => Some(EditorKey::Home),
                                'F' => Some(EditorKey::End),
                                _ => None,
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            } else {
                None
            }
            .unwrap_or_else(|| EditorKey::Verbatim(c))
    }
    // Return value indicates whether we should continue processing keypresses.
    fn process_keypress(&mut self, c: EditorKey) -> bool {
        if c == EditorKey::Verbatim(ctrl_key('q')) {
            if self.modified && self.quit_times > 0 {
                let quit_times = self.quit_times;
                self.set_status_message(&format!("Warning: File has unsaved changes. \
                                                       Ctrl-S to save, or press Ctrl-Q \
                                                       {} more times to quit.",
                                                 quit_times));
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
                    if self.modified {
                        self.set_status_message("File has unsaved changed, and \
                                                      cannot be refreshed. Quit and \
                                                      reopen to discard changes.");
                    } else if let Some(filename) = self.filename.clone() {
                        self.open(&filename).expect("Refreshing file failed")
                    } else {
                        self.set_status_message("No file to refresh")
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('s') => {
                    match self.save() {
                        Ok(()) => (),
                        Err(e) => self.set_status_message(&format!("Saving failed with {}", e)),
                    }
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('f') => self.find(),
                EditorKey::Verbatim(chr) if chr == ctrl_key('g') => self.go_to(),
                EditorKey::Verbatim(chr) if chr == ctrl_key(' ') => self.toggle_fold(),
                // Editing commands
                EditorKey::Delete |
                EditorKey::Verbatim(_) if self.folds.contains_key(&self.cursor_y) => {
                    self.set_status_message(DONT_EDIT_FOLDS);
                }
                EditorKey::Verbatim(chr) if chr == '\r' => self.insert_newline(),
                EditorKey::Delete => {
                    if self.folds.contains_key(&(self.cursor_y + 1)) &&
                       self.cursor_x == self.rows[self.cursor_y].cells.len() {
                        self.set_status_message(DONT_EDIT_FOLDS);
                    } else {
                        self.move_cursor(EditorKey::ArrowRight);
                        self.delete_char();
                    }
                }
                EditorKey::Verbatim(chr) if chr as usize == 127 || chr == ctrl_key('h') => {
                    self.delete_char()
                }
                EditorKey::Verbatim(chr) if chr == ctrl_key('k') => self.delete_row(),
                EditorKey::Verbatim(chr) => self.insert_char(chr),
            };
            self.quit_times = 3;
            true
        }
    }
}

/// * init **

fn run() {
    let mut editor_config: EditorConfig<io::Stdin> = EditorConfig::<io::Stdin>::new();
    print!("{}", CLEAR_SCREEN);
    if let Some(filename) = env::args().nth(1) {
        editor_config
            .open(&filename)
            .expect("Opening file failed")
    }
    editor_config.set_status_message("Help: C-s save, C-q quit, C-f find, \
                       C-' ' fold, C-e refresh, C-k del row, C-g go to.");
    loop {
        editor_config.warn_consistency();
        editor_config.refresh_screen();
        let keypress = editor_config.read_key();
        let to_continue = editor_config.process_keypress(keypress);
        if !to_continue {
            break;
        }
    }
}

fn main() {
    let orig_termios = enable_raw_mode().expect("Enabling raw mode failed");

    // Main program is run in a separate thread so that we can recover well from errors.
    let run_result = catch_unwind(run);
    restore_orig_mode(&orig_termios).expect("Disabling raw mode failed");
    run_result.expect("Editor loop panicked");

    print!("{}", CLEAR_SCREEN);
    print!("{}", CURSOR_TOP_RIGHT);
    io::stdout().flush().expect("Flushing stdout failed");
}
