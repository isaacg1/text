#![allow(unknown_lints)]
#![warn(clippy_pedantic)]
#![allow(print_stdout, missing_docs_in_private_items, string_add)]
extern crate termios;
extern crate libc;

use std::io::{self, Read, Write};
use std::fs::File;
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
type Row = Vec<Cell>;

struct EditorConfig {
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
}

impl EditorConfig {
    fn new() -> EditorConfig {
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
            }
        }
    }
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

/// * debug & testing **

fn check_consistency(editor_config_mut: &mut EditorConfig) {
    let failure: Option<&str> = {
        let editor_config = &editor_config_mut;
        let cursor_position_failure = if editor_config.cursor_y > editor_config.rows.len() {
            Some("Cursor y position out of bounds.")
        } else if editor_config.cursor_x >
                  current_row_len(editor_config) {
            Some("Cursor x position is out of bounds")
        } else {
            None
        };
        let mut fold_failure = None;
        for (&start, &(end, _)) in &editor_config.folds {
            if start < editor_config.cursor_y && editor_config.cursor_y <= end {
                fold_failure = Some("Cursor is inside a fold");
                break;
            }
            if editor_config.rows.len() <= end {
                fold_failure = Some("Fold goes past end of file");
                break;
            }
        }
        let mut fold_fold_failure = None;
        for (&start1, &(end1, _)) in &editor_config.folds {
            for (&start2, &(end2, _)) in &editor_config.folds {
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
    };
    if let Some(message) = failure {
        set_status_message(editor_config_mut, message);
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

fn read_key() -> EditorKey {
    let mut buffer: [u8; 1] = [0];
    while io::stdin().read(&mut buffer).expect("Read failure") == 0 {}
    let c = buffer[0] as char;
    if c == '\x1b' {
            let mut escape_buf: [u8; 3] = [0; 3];
            match io::stdin()
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

/// * folding **

fn toggle_fold(editor_config: &mut EditorConfig) {
    if editor_config.folds.contains_key(&editor_config.cursor_y) {
        editor_config.folds.remove(&editor_config.cursor_y);
    } else if editor_config.cursor_y < editor_config.rows.len() &&
              whitespace_depth(&editor_config.rows[editor_config.cursor_y]) == 0 {
        let mut index = 0;
        while index < editor_config.rows.len() {
            if let Some(&(end, _)) = editor_config.folds.get(&index) {
                index = end + 1;
            } else if whitespace_depth(&editor_config.rows[index]) > 0 {
                let depth = whitespace_depth(&editor_config.rows[index]);
                let end = editor_config
                    .rows
                    .iter()
                    .enumerate()
                    .skip(index + 1)
                    .position(|(oth_index, row)| {
                                  whitespace_depth(row) < depth && !row.is_empty() ||
                                  editor_config.folds.contains_key(&oth_index)
                              })
                    .map_or(editor_config.rows.len() - 1, |offset| index + offset);
                editor_config.folds.insert(index, (end, depth));
                index = end + 1;
            } else {
                index += 1;
            }
        }
    } else {
        create_fold(editor_config);
    }
}
fn create_fold(editor_config: &mut EditorConfig) {
    if editor_config.cursor_y < editor_config.rows.len() {
        let fold_depth = whitespace_depth(&editor_config.rows[editor_config.cursor_y]);
        let start = editor_config
            .rows
            .iter()
            .rev()
            .skip(editor_config.rows.len() - editor_config.cursor_y)
            .position(|row| whitespace_depth(row) < fold_depth && !row.is_empty())
            .map_or(0, |reverse_offset| editor_config.cursor_y - reverse_offset);
        let end = editor_config
            .rows
            .iter()
            .skip(editor_config.cursor_y + 1)
            .position(|row| whitespace_depth(row) < fold_depth && !row.is_empty())
            .map_or(editor_config.rows.len() - 1,
                    |offset| editor_config.cursor_y + offset);
        editor_config.folds.insert(start, (end, fold_depth));
        editor_config.cursor_y = start;
    }
}

fn open_folds(editor_config: &mut EditorConfig) {
    for (start, (end, _depth)) in editor_config.folds.clone() {
        if start <= editor_config.cursor_y && editor_config.cursor_y <= end {
            editor_config.folds.remove(&start);
        }
    }
}

fn one_row_forward(editor_config: &EditorConfig, index: usize) -> usize {
    editor_config
        .folds
        .get(&index)
        .map_or(index, |&(end, _)| end) + 1
}

fn one_row_back(editor_config: &EditorConfig, index: usize) -> usize {
    if let Some(prev_index) = index.checked_sub(1) {
        if let Some((&start, _end_and_depth)) =
            editor_config
                .folds
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

/// * syntax highlighting **

fn is_separator(c: char) -> bool {
    c.is_whitespace() || "&{}'\",.()+-/*=~%<>[];".contains(c)
}

fn whitespace_depth(row: &[Cell]) -> usize {
    row.iter()
        .position(|cell| !cell.chr.is_whitespace())
        .unwrap_or_else(|| row.len())
}

/// * row operations **

fn current_row_len(editor_config: &EditorConfig) -> usize {
    editor_config
        .rows
        .get(editor_config.cursor_y)
        .map_or(0, |row| row.len())
}

fn row_to_string(row: &[Cell]) -> String {
    row.iter().map(|&cell| cell.chr).collect::<String>()
}

fn string_to_row(s: &str) -> Row {
    s.chars()
        .map(|c| {
                 Cell {
                     chr: c,
                     hl: EditorHighlight::Normal,
                 }
             })
        .collect()
}

fn update_row_highlights(editor_config: &mut EditorConfig, row_index: usize) {
    let row = &mut editor_config.rows[row_index];
    if let Some(ref syntax) = editor_config.syntax {
        let mut index = 0;
        macro_rules! update_and_advance {
            ($highlight_expression:expr) => {
                row[index].hl = $highlight_expression;
                index += 1;
            }
        }
        'outer: while index < row.len() {
            let prev_is_sep = index == 0 || is_separator(row[index - 1].chr);
            if syntax.has_digits && row[index].chr.is_digit(10) && prev_is_sep {
                while index < row.len() && row[index].chr.is_digit(10) {
                    update_and_advance!(EditorHighlight::Number);
                }
            } else if syntax.quotes.contains(row[index].chr) {
                let start_quote = row[index].chr;
                update_and_advance!(EditorHighlight::String);
                while index < row.len() {
                    if row[index].chr == start_quote {
                        update_and_advance!(EditorHighlight::String);
                        break;
                    };
                    if row[index].chr == '\\' && index + 1 < row.len() {
                        update_and_advance!(EditorHighlight::String);
                    }
                    update_and_advance!(EditorHighlight::String);
                }
            } else if row_to_string(&row[index..].to_vec())
                          .starts_with(&syntax.singleline_comment) {
                while index < row.len() {
                    update_and_advance!(EditorHighlight::Comment);
                }
            } else {
                if index == 0 || is_separator(row[index - 1].chr) {
                    let following_string: String = row_to_string(&row[index..].to_vec());
                    for (kind, keywords) in syntax.keywords.iter().enumerate() {
                        let highlight = match kind {
                            0 => EditorHighlight::Keyword1,
                            1 => EditorHighlight::Keyword2,
                            2 => EditorHighlight::Keyword3,
                            3 => EditorHighlight::Keyword4,
                            _ => panic!("There should only be four things in the list."),
                        };
                        for keyword in keywords {
                            let keyword_end = index + keyword.len();
                            if following_string.starts_with(keyword) &&
                               (keyword_end == row.len() || is_separator(row[keyword_end].chr)) {
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
        for cell in row.iter_mut() {
            cell.hl = EditorHighlight::Normal
        }
    }
}

fn select_syntax(editor_config: &mut EditorConfig) {
    editor_config.syntax = editor_config
        .filename
        .as_ref()
        .and_then(|filename| {
            let syntax_database =
                vec![EditorSyntax {
                         filetype: "rust".to_string(),
                         extensions: vec![".rs".to_string()],
                         has_digits: true,
                         quotes: "\"".to_string(),
                         singleline_comment: "//".to_string(),
                         keywords: [vec!["alignof", "as", "break", "continue", "crate", "else",
                                         "extern", "fn", "for", "if", "impl", "in", "let",
                                         "loop", "macro", "match", "mod", "offsetof", "pub",
                                         "return", "sizeof", "trait", "typeof", "unsafe", "use",
                                         "where", "while", "yield"]
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
                                         "f32", "f64", "str", "u8", "u16", "u32", "u64", "usize"]
                                            .iter()
                                            .map(|x| x.to_string())
                                            .collect::<Vec<_>>()],
                     },
                     EditorSyntax {
                         filetype: "c".to_string(),
                         extensions: vec![".c".to_string(), ".h".to_string(), ".cpp".to_string()],
                         has_digits: true,
                         quotes: "\"'".to_string(),
                         singleline_comment: "//".to_string(),
                         keywords: [vec![], vec![], vec![], vec![]],
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
    for index in 0..editor_config.rows.len() {
        update_row_highlights(editor_config, index);
    }
}


/// * editor operations **

fn insert_char(editor_config: &mut EditorConfig, c: char) {
    if editor_config.cursor_y == editor_config.rows.len() {
        editor_config.rows.push(Vec::new());
    }
    editor_config.rows[editor_config.cursor_y].insert(editor_config.cursor_x,
                                                      Cell {
                                                          chr: c,
                                                          hl: EditorHighlight::Normal,
                                                      });
    let index = editor_config.cursor_y;
    update_row_highlights(editor_config, index);
    editor_config.cursor_x += 1;
    editor_config.modified = true;
}

fn insert_newline(editor_config: &mut EditorConfig) {
    if editor_config.cursor_y < editor_config.rows.len() {
        let depth = whitespace_depth(&editor_config.rows[editor_config.cursor_y]);
        // If in the whitespace, insert blank line.
        if depth >= editor_config.cursor_x {
            editor_config.rows.insert(editor_config.cursor_y, vec![]);
        } else {
            let mut next_row = editor_config.rows[editor_config.cursor_y][..depth].to_vec();

            let row_end = editor_config.rows[editor_config.cursor_y]
                .split_off(editor_config.cursor_x);
            next_row.extend(row_end);
            editor_config
                .rows
                .insert(editor_config.cursor_y + 1, next_row);
        }
        for row_index in (editor_config.cursor_y..editor_config.rows.len()).rev() {
            if let Some((end, depth)) = editor_config.folds.remove(&row_index) {
                editor_config
                    .folds
                    .insert(row_index + 1, (end + 1, depth));
            }
        }
        let index = editor_config.cursor_y;
        update_row_highlights(editor_config, index);
        update_row_highlights(editor_config, index + 1);

        editor_config.cursor_x = depth;
    } else {
        editor_config.rows.push(Vec::new());
        editor_config.cursor_x = 0;
    }
    editor_config.cursor_y += 1;
    editor_config.modified = true;
}

fn delete_row(editor_config: &mut EditorConfig) {
    editor_config.rows[editor_config.cursor_y].truncate(editor_config.cursor_x);
    let index = editor_config.cursor_y;
    update_row_highlights(editor_config, index);
    editor_config.modified = true
}

fn delete_char(editor_config: &mut EditorConfig) {
    if let Some(prev_x) = editor_config.cursor_x.checked_sub(1) {
        editor_config.rows[editor_config.cursor_y].remove(prev_x);
        let index = editor_config.cursor_y;
        update_row_highlights(editor_config, index);
        editor_config.cursor_x = prev_x;
        editor_config.modified = true
    } else if 0 < editor_config.cursor_y && editor_config.cursor_y < editor_config.rows.len() {
        if editor_config
               .folds
               .values()
               .any(|&(end, _)| end == editor_config.cursor_y - 1) {
            set_status_message(editor_config, DONT_EDIT_FOLDS);
        } else {
            editor_config.cursor_x = editor_config.rows[editor_config.cursor_y - 1].len();
            let moved_line = editor_config.rows.remove(editor_config.cursor_y);
            let line_to_append = &moved_line[whitespace_depth(&moved_line)..];
            editor_config.rows[editor_config.cursor_y - 1].extend(line_to_append);
            for row_index in editor_config.cursor_y..editor_config.rows.len() {
                if let Some((end, depth)) = editor_config.folds.remove(&row_index) {
                    editor_config
                        .folds
                        .insert(row_index - 1, (end - 1, depth));
                }
            }
            editor_config.cursor_y -= 1;
            let index = editor_config.cursor_y;
            update_row_highlights(editor_config, index);
            editor_config.modified = true
        }
    }
}

/// * file i/o **
fn open(editor_config: &mut EditorConfig, filename: &str) -> io::Result<()> {
    editor_config.filename = Some(filename.to_string());
    select_syntax(editor_config);
    let mut file = File::open(filename)?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    load_text(editor_config, &string);
    Ok(())
}

fn load_text(editor_config: &mut EditorConfig, text: &str) {
    editor_config.rows = vec![];
    editor_config.folds = HashMap::new();

    let mut row_buffer = String::new();
    for byte in text.bytes() {
        let c = byte as char;
        if c == '\n' {
            let row = string_to_row(&row_buffer);
            let update_index = editor_config.rows.len();
            editor_config.rows.push(row);
            update_row_highlights(editor_config, update_index);
            row_buffer.truncate(0);
        } else {
            row_buffer.push(c);
        }
    }
    let row = string_to_row(&row_buffer);
    let update_index = editor_config.rows.len();
    editor_config.rows.push(row);
    update_row_highlights(editor_config, update_index);

    editor_config.cursor_y = min(editor_config.cursor_y, editor_config.rows.len());
    editor_config.cursor_x = 0;
    scroll(editor_config);
}

fn all_text(editor_config: &EditorConfig) -> String {
    editor_config
        .rows
        .iter()
        .map(|row| row_to_string(row.as_slice()))
        .collect::<Vec<_>>()
        .join("\n")
}

fn save(editor_config: &mut EditorConfig) -> io::Result<()> {
    let filename = if let Some(ref filename) = editor_config.filename {
        filename.clone()
    } else {
        match prompt(editor_config, "Save as: ", None) {
            None => {
                set_status_message(editor_config, "Save aborted");
                return Ok(());
            }
            Some(filename) => {
                editor_config.filename = Some(filename.clone());
                select_syntax(editor_config);
                filename
            }
        }
    };

    let mut file = File::create(filename)?;
    let text = all_text(editor_config);
    file.write_all(text.as_bytes())?;
    editor_config.modified = false;
    set_status_message(editor_config,
                       &format!("{} bytes written to disk", text.len()));
    Ok(())
}

/// * macro movement **

fn find_callback(editor_config: &mut EditorConfig, query: &str, key: EditorKey) {
    if editor_config.cursor_y < editor_config.rows.len() {
        let index = editor_config.cursor_y;
        update_row_highlights(editor_config, index)
    }
    if key != EditorKey::Verbatim('\r') && key != EditorKey::Verbatim('\x1b') {
        let match_line = {
            let find_predicate = &|row: &Row| row_to_string(row).contains(query);
            if key == EditorKey::ArrowRight || key == EditorKey::ArrowDown {
                let potential_match = if editor_config.cursor_y + 1 < editor_config.rows.len() {
                    editor_config
                        .rows
                        .iter()
                        .skip(editor_config.cursor_y + 1)
                        .position(find_predicate)
                        .map(|offset| offset + editor_config.cursor_y + 1)
                } else {
                    None
                };
                potential_match.or_else(|| editor_config.rows.iter().position(find_predicate))
            } else if key == EditorKey::ArrowLeft || key == EditorKey::ArrowUp {
                let potential_match = editor_config.rows[..editor_config.cursor_y]
                    .iter()
                    .rposition(find_predicate);
                potential_match.or_else(|| editor_config.rows.iter().rposition(find_predicate))
            } else {
                let potential_match = if editor_config.cursor_y < editor_config.rows.len() {
                    editor_config
                        .rows
                        .iter()
                        .skip(editor_config.cursor_y)
                        .position(find_predicate)
                        .map(|offset| offset + editor_config.cursor_y)
                } else {
                    None
                };
                potential_match.or_else(|| editor_config.rows.iter().position(find_predicate))
            }
        };
        if let Some(match_line) = match_line {
            let match_index = row_to_string(&editor_config.rows[match_line])
                .find(query)
                .expect("We just checked the row contained the string");
            editor_config.cursor_y = match_line;
            open_folds(editor_config);
            editor_config.cursor_x = match_index;
            editor_config.row_offset = editor_config.rows.len();
            for cell in editor_config.rows[match_line]
                    .iter_mut()
                    .skip(match_index)
                    .take(query.len()) {
                cell.hl = EditorHighlight::Match
            }
        }
    }
}

fn find(editor_config: &mut EditorConfig) {
    let saved_cursor_x = editor_config.cursor_x;
    let saved_cursor_y = editor_config.cursor_y;
    let saved_col_offset = editor_config.col_offset;
    let saved_row_offset = editor_config.row_offset;
    let saved_folds = editor_config.folds.clone();

    let query = prompt(editor_config,
                       "Search (ESC/Arrows/Enter): ",
                       Some(&find_callback));

    if query.is_none() {
        editor_config.cursor_x = saved_cursor_x;
        editor_config.cursor_y = saved_cursor_y;
        editor_config.col_offset = saved_col_offset;
        editor_config.row_offset = saved_row_offset;
        editor_config.folds = saved_folds;
    }
}

fn go_to(editor_config: &mut EditorConfig) {
    if let Some(response) = prompt(editor_config, "Go to line: ", None) {
        match response.parse::<usize>() {
            Ok(line) => {
                if 0 < line && line - 1 <= editor_config.rows.len() {
                    editor_config.cursor_y = line - 1;
                    editor_config.cursor_x = 0;
                } else {
                    set_status_message(editor_config,
                                       &format!("Line {} outside of range of file", line));
                }
            }
            Err(_) => set_status_message(editor_config, "Line was not numeric"),
        }
    }
}

/// * output **

fn scroll(editor_config: &mut EditorConfig) {
    editor_config.row_offset = min(editor_config.row_offset, editor_config.cursor_y);
    while screen_y(editor_config) >= editor_config.screen_rows {
        editor_config.row_offset = one_row_forward(editor_config, editor_config.row_offset)
    }
    editor_config.col_offset = min(editor_config.col_offset, editor_config.cursor_x);
    while screen_x(editor_config) >= editor_config.screen_cols {
        editor_config.col_offset += 1;
    }
}

fn draw_rows(editor_config: &EditorConfig, append_buffer: &mut String) {
    let tab = &" ".repeat(TAB_STOP);
    let mut screen_y = 0;
    let mut file_row = editor_config.row_offset;
    while screen_y < editor_config.screen_rows {
        if let Some(&(fold_end, fold_depth)) = editor_config.folds.get(&file_row) {
            let fold_white = &editor_config.rows[file_row][..fold_depth];
            let fold_white_visible = if editor_config.col_offset < fold_white.len() {
                fold_white[editor_config.col_offset..].to_vec()
            } else {
                vec![]
            };
            let mut fold_white_str = row_to_string(&fold_white_visible).replace("\t", tab);
            let fold_msg = format!("{} lines folded.", fold_end - file_row + 1);
            fold_white_str.truncate(editor_config.screen_cols.saturating_sub(fold_msg.len()));
            append_buffer.push_str(&fold_white_str);
            append_buffer.push_str(INVERT_COLORS);
            append_buffer.push_str(&fold_msg);
            let padding = editor_config
                .screen_cols
                .saturating_sub(fold_msg.len() + fold_white_str.len());
            append_buffer.push_str(&" ".repeat(padding));
            append_buffer.push_str(REVERT_COLORS);
            file_row = fold_end + 1;
        } else if file_row < editor_config.rows.len() {
            let current_row = &editor_config.rows[file_row];
            if editor_config.col_offset < current_row.len() {
                let mut current_hl = EditorHighlight::Normal;
                let mut chars_written = 0;
                for &Cell { chr, hl } in current_row.iter().skip(editor_config.col_offset) {
                    if hl != current_hl {
                        current_hl = hl;
                        append_buffer.push_str(&hl.color());
                    }
                    chars_written += if chr == '\t' { TAB_STOP } else { 1 };
                    if chars_written > editor_config.screen_cols {
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
        } else if editor_config.rows.is_empty() && screen_y == editor_config.screen_rows / 3 {
            let welcome = format!("Isaac's editor -- version {}", IED_VERSION);
            let padding = editor_config.screen_cols.saturating_sub(welcome.len()) / 2;
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

fn draw_status_bar(editor_config: &EditorConfig, append_buffer: &mut String) {
    append_buffer.push_str(INVERT_COLORS);
    let mut name = editor_config
        .filename
        .clone()
        .unwrap_or_else(|| "[No Name]".to_string());
    name.truncate(20);
    let dirty = if editor_config.modified {
        "(modified)"
    } else {
        ""
    };
    let mut status = format!("{} {}", name, dirty);
    status.truncate(editor_config.screen_cols);
    append_buffer.push_str(&status);
    let filetype = match editor_config.syntax {
        None => "no ft".to_string(),
        Some(ref syntax) => syntax.filetype.clone(),
    };
    let mut right_status = format!("{} | r: {}/{}, c: {}/{}",
                                   filetype,
                                   editor_config.cursor_y + 1,
                                   editor_config.rows.len(),
                                   editor_config.cursor_x + 1,
                                   editor_config
                                       .rows
                                       .get(editor_config.cursor_y)
                                       .map_or(0, |row| row.len()));
    right_status.truncate(editor_config
                              .screen_cols
                              .saturating_sub(status.len() + 1));
    append_buffer.push_str(&" ".repeat(editor_config
                                           .screen_cols
                                           .saturating_sub(status.len() + right_status.len())));
    append_buffer.push_str(&right_status);
    append_buffer.push_str(REVERT_COLORS);
    append_buffer.push_str("\r\n");
}

fn draw_message_bar(editor_config: &EditorConfig, append_buffer: &mut String) {
    append_buffer.push_str(CLEAR_RIGHT);
    if editor_config.status_message_time.elapsed().as_secs() < 5 {
        let mut message = editor_config.status_message.clone();
        message.truncate(editor_config.screen_cols);
        append_buffer.push_str(&message);
    }
}

fn refresh_screen(editor_config: &mut EditorConfig) {
    scroll(editor_config);
    let mut append_buffer: String = String::new();
    append_buffer.push_str(HIDE_CURSOR);
    append_buffer.push_str(CURSOR_TOP_RIGHT);

    draw_rows(editor_config, &mut append_buffer);
    draw_status_bar(editor_config, &mut append_buffer);
    draw_message_bar(editor_config, &mut append_buffer);

    let cursor_control = format!("\x1b[{};{}H",
                                 screen_y(editor_config) + 1,
                                 screen_x(editor_config) + 1);
    append_buffer.push_str(&cursor_control);
    append_buffer.push_str(SHOW_CURSOR);
    print!("{}", append_buffer);
    io::stdout()
        .flush()
        .expect("Flushing to stdout should work");
}

fn set_status_message(editor_config: &mut EditorConfig, message: &str) {
    editor_config.status_message = message.to_string();
    editor_config.status_message_time = Instant::now();
}

/// * input **

fn prompt(editor_config: &mut EditorConfig,
          prompt: &str,
          callback: Option<&Fn(&mut EditorConfig, &str, EditorKey) -> ()>)
          -> Option<String> {
    let mut response = String::new();
    loop {
        set_status_message(editor_config, &format!("{}{}", prompt, response));
        refresh_screen(editor_config);

        let c = read_key();
        macro_rules! maybe_callback {
            () => {
                if let Some(callback) = callback {
                    callback(editor_config, &response, c)
                }
            }
        }
        match c {
            EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('q') => {
                set_status_message(editor_config, "");
                maybe_callback!();
                return None;
            }
            EditorKey::Verbatim(chr) if chr == '\r' => {
                if !response.is_empty() {
                    set_status_message(editor_config, "");
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

fn screen_x(editor_config: &EditorConfig) -> usize {
    if editor_config.cursor_y < editor_config.rows.len() {
        editor_config.rows[editor_config.cursor_y]
            .iter()
            .take(editor_config.cursor_x)
            .skip(editor_config.col_offset)
            .map(|cell| if cell.chr == '\t' { TAB_STOP } else { 1 })
            .sum()
    } else {
        0
    }
}

fn screen_y(editor_config: &EditorConfig) -> usize {
    let mut file_y = editor_config.row_offset;
    let mut screen_y = 0;
    while file_y < editor_config.cursor_y {
        file_y = one_row_forward(editor_config, file_y);
        screen_y += 1;
    }
    screen_y
}

fn move_cursor(editor_config: &mut EditorConfig, key: EditorKey) {
    // Smaller values are up and left
    match key {
        EditorKey::ArrowUp => {
            if editor_config.cursor_y > 0 {
                editor_config.cursor_y = one_row_back(editor_config, editor_config.cursor_y);
            }
        }
        EditorKey::ArrowDown => {
            if editor_config.cursor_y < editor_config.rows.len() {
                editor_config.cursor_y = one_row_forward(editor_config, editor_config.cursor_y);
            }
        }
        EditorKey::ArrowLeft => {
            if let Some(prev_x) = editor_config.cursor_x.checked_sub(1) {
                editor_config.cursor_x = prev_x
            } else if let Some(prev_y) = editor_config.cursor_y.checked_sub(1) {
                editor_config.cursor_y = prev_y;
                editor_config.cursor_x = current_row_len(editor_config);
            }
        }
        EditorKey::ArrowRight => {
            if editor_config.cursor_x < current_row_len(editor_config) {
                editor_config.cursor_x += 1
            } else if editor_config.cursor_x == current_row_len(editor_config) &&
                      editor_config.cursor_y < editor_config.rows.len() {
                editor_config.cursor_y += 1;
                editor_config.cursor_x = 0
            }
        }
        EditorKey::PageUp => {
            for _ in 0..editor_config.screen_rows.saturating_sub(1) {
                move_cursor(editor_config, EditorKey::ArrowUp)
            }
        }
        EditorKey::PageDown => {
            for _ in 0..editor_config.screen_rows.saturating_sub(1) {
                move_cursor(editor_config, EditorKey::ArrowDown)
            }
        }
        EditorKey::Home => editor_config.cursor_x = 0,
        EditorKey::End => editor_config.cursor_x = current_row_len(editor_config),

        _ => panic!("Editor move cursor received non moving character"),
    };
    editor_config.cursor_x = min(editor_config.cursor_x, current_row_len(editor_config));
}

// Return value indicates whether we should continue processing keypresses.
fn process_keypress(editor_config: &mut EditorConfig) -> bool {
    let c = read_key();

    if c == EditorKey::Verbatim(ctrl_key('q')) {
        if editor_config.modified && editor_config.quit_times > 0 {
            let quit_times = editor_config.quit_times;
            set_status_message(editor_config,
                               &format!("Warning: File has unsaved changes. Ctrl-S to \
                                                save, or press Ctrl-Q {} more times to quit.",
                                        quit_times));
            editor_config.quit_times -= 1;
            true
        } else {
            editor_config.quit_times = 3;
            false
        }
    } else {
        match c {
            EditorKey::ArrowUp | EditorKey::ArrowDown | EditorKey::ArrowLeft |
            EditorKey::ArrowRight | EditorKey::PageUp | EditorKey::PageDown | EditorKey::Home |
            EditorKey::End => move_cursor(editor_config, c),
            EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('l') => (),
            EditorKey::Verbatim(chr) if chr == ctrl_key('e') => {
                if editor_config.modified {
                    set_status_message(editor_config,
                                       "File has unsaved changed, and cannot be refreshed. \
                                        Quit and reopen to discard changes.");
                } else if let Some(filename) = editor_config.filename.clone() {
                    open(editor_config, &filename).expect("Refreshing file failed")
                } else {
                    set_status_message(editor_config, "No file to refresh")
                }
            }
            EditorKey::Verbatim(chr) if chr == ctrl_key('s') => {
                match save(editor_config) {
                    Ok(()) => (),
                    Err(e) => {
                        set_status_message(editor_config, &format!("Saving failed with {}", e))
                    }
                }
            }
            EditorKey::Verbatim(chr) if chr == ctrl_key('f') => find(editor_config),
            EditorKey::Verbatim(chr) if chr == ctrl_key('g') => go_to(editor_config),
            EditorKey::Verbatim(chr) if chr == ctrl_key(' ') => toggle_fold(editor_config),
            // Editing commands
            EditorKey::Delete |
            EditorKey::Verbatim(_) if editor_config.folds.contains_key(&editor_config.cursor_y) => {
                set_status_message(editor_config, DONT_EDIT_FOLDS);
            }
            EditorKey::Verbatim(chr) if chr == '\r' => insert_newline(editor_config),
            EditorKey::Delete => {
                if !(editor_config
                         .folds
                         .contains_key(&(editor_config.cursor_y + 1)) &&
                     editor_config.cursor_x == editor_config.rows[editor_config.cursor_y].len()) {
                    move_cursor(editor_config, EditorKey::ArrowRight);
                    delete_char(editor_config);
                }
            }
            EditorKey::Verbatim(chr) if chr as usize == 127 || chr == ctrl_key('h') => {
                delete_char(editor_config)
            }
            EditorKey::Verbatim(chr) if chr == ctrl_key('k') => delete_row(editor_config),
            EditorKey::Verbatim(chr) => insert_char(editor_config, chr),
        };
        editor_config.quit_times = 3;
        true
    }
}

/// * init **

fn run() {
    let mut editor_config: EditorConfig = EditorConfig::new();
    print!("{}", CLEAR_SCREEN);
    if let Some(filename) = env::args().nth(1) {
        open(&mut editor_config, &filename).expect("Opening file failed")
    }
    set_status_message(&mut editor_config,
                       "Help: C-s save, C-q quit, C-f find, \
                       C-' ' fold, C-e refresh, C-k del row, C-g go to.");
    loop {
        check_consistency(&mut editor_config);
        refresh_screen(&mut editor_config);
        let to_continue = process_keypress(&mut editor_config);
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
