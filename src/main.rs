#![allow(dead_code)]
extern crate termios;
extern crate libc;

use std::io::{self, Read, Write};
use std::fs::File;
use std::env;

use std::time::Instant;

use std::collections::HashMap;

use std::os::raw::c_int;
use termios::*;

/// * utility **

const IED_VERSION: &'static str = "0.0.1";

const TAB_STOP: usize = 4;

const STDIN: c_int = 1;
const STDOUT: c_int = 2;

fn ctrl_key(k: char) -> char {
    (k as u8 & 0x1f) as char
}

const INVERT_COLORS: &'static str = "\x1b[7m";
const REVERT_COLORS: &'static str = "\x1b[m";

const RED: &'static str = "\x1b[31m\x1b[49m";
const MAGENTA: &'static str = "\x1b[1m\x1b[35m\x1b[49m";
const WHITE: &'static str = "\x1b[37m\x1b[49m";
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

/// * data **

struct EditorConfig {
    cursor_x: usize,
    cursor_y: usize,
    screen_rows: usize,
    screen_cols: usize,
    rows: Vec<Vec<(char, EditorHighlight)>>,
    row_offset: usize,
    col_offset: usize,
    orig_termios: Termios,
    filename: Option<String>,
    syntax: Option<EditorSyntax>,
    status_message: String,
    status_message_time: Instant,
    modified: bool,
    quit_times: usize,
    folds: HashMap<usize, (usize, usize)>,
}

impl EditorConfig {
    fn new(orig_termios: Termios) -> EditorConfig {
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
                orig_termios: orig_termios,
                filename: None,
                screen_rows: (ws.ws_row - 2) as usize,
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
    fn color(&self) -> &str {
        match *self {
            EditorHighlight::Normal => REVERT_COLORS,
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

#[derive(PartialEq)]
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

/// * filetypes **
#[derive(Clone)]
struct EditorSyntax {
    filetype: String,
    extensions: Vec<String>,
    has_digits: bool,
    quotes: String,
    singleline_comment: String,
    keyword1s: Vec<String>,
    keyword2s: Vec<String>,
    keyword3s: Vec<String>,
    keyword4s: Vec<String>,
}

/// * terminal **
fn enable_raw_mode() -> io::Result<Termios> {
    let orig_termios = Termios::from_fd(STDIN)?;

    let mut termios = orig_termios.clone();

    termios.c_cflag |= CS8;
    termios.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    termios.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    termios.c_oflag &= !(OPOST);
    termios.c_cc[VMIN] = 0; // Return on any characters read;
    termios.c_cc[VTIME] = 1; //Wait for 0.1 seconds.

    tcsetattr(STDIN, TCSAFLUSH, &mut termios)?;

    Ok(orig_termios)
}

fn restore_orig_mode(editor_config: &mut EditorConfig) -> io::Result<()> {
    tcsetattr(STDIN, TCSAFLUSH, &mut editor_config.orig_termios)
}

fn editor_read_key() -> EditorKey {
    let mut buffer: [u8; 1] = [0];
    loop {
        match io::stdin().read(&mut buffer) {
            Ok(0) => (),
            Ok(_) => break,
            Err(e) => panic!("Read failure: {}", e),
        };
    }
    let c = buffer[0] as char;
    if c == '\x1b' {
        let mut escape_buf: [u8; 3] = [0; 3];
        match io::stdin().read(&mut escape_buf) {
            Ok(2) | Ok(3) => {
                if escape_buf[0] as char == '[' {
                    if escape_buf[2] as char == '~' {
                        match escape_buf[1] as char {
                            '1' => return EditorKey::Home,
                            '3' => return EditorKey::Delete,
                            '4' => return EditorKey::End,
                            '5' => return EditorKey::PageUp,
                            '6' => return EditorKey::PageDown,
                            '7' => return EditorKey::Home,
                            '8' => return EditorKey::End,
                            _ => (),
                        }
                    } else {
                        match escape_buf[1] as char {
                            'A' => return EditorKey::ArrowUp,
                            'B' => return EditorKey::ArrowDown,
                            'C' => return EditorKey::ArrowRight,
                            'D' => return EditorKey::ArrowLeft,
                            'H' => return EditorKey::Home,
                            'F' => return EditorKey::End,
                            _ => (),
                        }
                    }
                } else if escape_buf[0] as char == 'O' {
                    match escape_buf[1] as char {
                        'H' => return EditorKey::Home,
                        'F' => return EditorKey::End,
                        _ => (),
                    }
                }
            }
            Ok(_) => return EditorKey::Verbatim('\x1b'),
            Err(e) => panic!("Read failure during escape: {}", e),
        };
    };
    EditorKey::Verbatim(c)
}

/// * folding **

fn toggle_fold(editor_config: &mut EditorConfig) {
    if editor_config.folds.contains_key(&editor_config.cursor_y) {
        editor_config.folds.remove(&editor_config.cursor_y);
    } else {
        create_fold(editor_config);
    }
}
fn create_fold(editor_config: &mut EditorConfig) {
    if editor_config.cursor_y < editor_config.rows.len() {
        let fold_depth = whitespace_depth(&editor_config.rows[editor_config.cursor_y]);
        let mut back_index = editor_config.cursor_y;
        let start;
        loop {
            let depth = whitespace_depth(&editor_config.rows[back_index]);
            if depth < fold_depth && editor_config.rows[back_index].len() > 0 {
                start = back_index + 1;
                break;
            }
            if back_index == 0 {
                start = 0;
                break;
            }
            back_index -= 1;
        }
        let mut forward_index = editor_config.cursor_y;
        let end;
        loop {
            let depth = whitespace_depth(&editor_config.rows[forward_index]);
            if depth < fold_depth && editor_config.rows[forward_index].len() > 0 {
                end = forward_index - 1;
                break;
            }
            if forward_index == editor_config.rows.len() - 1 {
                end = editor_config.rows.len() - 1;
                break;
            }
            forward_index += 1;
        }
        editor_config.folds.insert(start, (end, fold_depth));
        editor_config.cursor_y = start;
    }
}

fn open_folds(editor_config: &mut EditorConfig) {
    for (&start, &(end, _depth)) in editor_config.folds.clone().iter() {
        if start <= editor_config.cursor_y && editor_config.cursor_y <= end {
            editor_config.folds.remove(&start);
        }
    }
}

fn one_row_forward(editor_config: &EditorConfig, index: usize) -> usize {
    match editor_config.folds.get(&index) {
        None => index + 1,
        Some(&(end, _depth)) => end + 1,
    }
}

fn one_row_back(editor_config: &EditorConfig, index: usize) -> usize {
    if index > 0 {
        if let Some((&start, _end_and_depth)) =
            editor_config
                .folds
                .iter()
                .filter(|&(_start, &(end, _depth))| end == editor_config.cursor_y - 1)
                .next() {
            start
        } else {
            index - 1
        }
    } else {
        0
    }
}

/// * syntax highlighting **

fn is_separator(c: char) -> bool {
    c.is_whitespace() || "&{}'\",.()+-/*=~%<>[];".contains(c)
}

fn whitespace_depth(row: &Row) -> usize {
    for index in 0..row.len() {
        if !row[index].0.is_whitespace() {
            return index;
        }
    }
    return row.len();
}

/// * row operations **


type Row = Vec<(char, EditorHighlight)>;

fn row_to_string(row: &Row) -> String {
    row.iter().map(|&(c, _)| c).collect::<String>()
}

fn string_to_row(s: &str) -> Row {
    s.chars()
        .map(|c| (c, EditorHighlight::Normal))
        .collect()
}

fn update_row(editor_config: &mut EditorConfig, row_index: usize) {
    if let Some(syntax) = editor_config.syntax.clone() {
        let ref mut row = editor_config.rows[row_index];
        let mut index = 0;
        let keyword_groups = vec![syntax.keyword1s,
                                  syntax.keyword2s,
                                  syntax.keyword3s,
                                  syntax.keyword4s];
        'outer: while index < row.len() {
            let prev_is_digit_or_sep = index == 0 || is_separator(row[index - 1].0) ||
                                       row[index - 1].1 == EditorHighlight::Number;
            if syntax.has_digits && row[index].0.is_digit(10) && prev_is_digit_or_sep {
                row[index].1 = EditorHighlight::Number
            } else if syntax.quotes.contains(row[index].0) {
                let start_quote = row[index].0;
                row[index].1 = EditorHighlight::String;
                index += 1;
                while index < row.len() {
                    row[index].1 = EditorHighlight::String;
                    if row[index].0 == start_quote {
                        break;
                    };
                    if row[index].0 == '\\' && index + 1 < row.len() {
                        index += 1;
                        row[index].1 = EditorHighlight::String;
                    }
                    index += 1;
                }
            } else if row_to_string(&row[index..].to_vec())
                          .starts_with(&syntax.singleline_comment) {
                for comment_index in index..row.len() {
                    row[comment_index].1 = EditorHighlight::Comment;
                }
                break;
            } else {
                if index == 0 || is_separator(row[index - 1].0) {
                    let following_string: String = row_to_string(&row[index..].to_vec());
                    for (kind, keywords) in keyword_groups.iter().enumerate() {
                        let highlight = match kind {
                            0 => EditorHighlight::Keyword1,
                            1 => EditorHighlight::Keyword2,
                            2 => EditorHighlight::Keyword3,
                            3 => EditorHighlight::Keyword4,
                            _ => panic!("There should only be four things in the list."),
                        };
                        for keyword in keywords {
                            if following_string.starts_with(keyword) &&
                               (index + keyword.len() >= row.len() ||
                                is_separator(row[index + keyword.len()].0)) {
                                let keyword_end = index + keyword.len();
                                while index < keyword_end {
                                    row[index].1 = highlight;
                                    index += 1;
                                }
                                continue 'outer;
                            }
                        }
                    }
                }
                row[index].1 = EditorHighlight::Normal
            };
            index += 1;
        }
    }
}

fn editor_select_syntax(editor_config: &mut EditorConfig) {
    match editor_config.filename.clone() {
        None => editor_config.syntax = None,
        Some(filename) => {
            let syntax_database =
                vec![EditorSyntax {
                         filetype: "rust".to_string(),
                         extensions: vec![".rs".to_string()],
                         has_digits: true,
                         quotes: "\"".to_string(),
                         singleline_comment: "//".to_string(),
                         keyword1s: vec!["extern", "crate", "use", "as", "impl", "fn", "let",
                                         "unsafe", "if", "else", "return", "while", "break",
                                         "continue", "loop", "match"]
                                 .iter()
                                 .map(|x| x.to_string())
                                 .collect::<Vec<_>>(),
                         keyword2s: vec!["const", "static", "struct", "mut", "enum", "ref", "type"]
                             .iter()
                             .map(|x| x.to_string())
                             .collect::<Vec<_>>(),
                         keyword3s: vec!["true", "false", "self"]
                             .iter()
                             .map(|x| x.to_string())
                             .collect::<Vec<_>>(),
                         keyword4s: vec!["str", "usize", "char", "u8", "bool"]
                             .iter()
                             .map(|x| x.to_string())
                             .collect::<Vec<_>>(), // TODO: Complete
                     },
                     EditorSyntax {
                         filetype: "c".to_string(),
                         extensions: vec![".c".to_string(), ".h".to_string(), ".cpp".to_string()],
                         has_digits: true,
                         quotes: "\"'".to_string(),
                         singleline_comment: "//".to_string(),
                         keyword1s: vec![], // TODO: Complete
                         keyword2s: vec![], // TODO: Complete
                         keyword3s: vec![], // TODO: Complete
                         keyword4s: vec![], // TODO: Complete
                     }];
            for entry in syntax_database {
                let extensions = entry.extensions.clone();
                for extension in extensions {
                    if filename.ends_with(&extension) {
                        editor_config.syntax = Some(entry);
                        for index in 0..editor_config.rows.len() {
                            update_row(editor_config, index);
                        }
                        return;
                    }
                }
            }
        }
    }
}


/// * editor operations **

fn editor_insert_char(editor_config: &mut EditorConfig, c: char) {
    if editor_config.folds.contains_key(&editor_config.cursor_y) {
        editor_set_status_message(editor_config, "Folded lines can't be edited. Ctrl-Space to unfold.")
    } else {
        if editor_config.cursor_y == editor_config.rows.len() {
            editor_config.rows.push(Vec::new());
        }
        editor_config.rows[editor_config.cursor_y].insert(editor_config.cursor_x,
                                                          (c, EditorHighlight::Normal));
        let index = editor_config.cursor_y;
        update_row(editor_config, index);
        editor_config.cursor_x += 1;
        editor_config.modified = true;
    }
}

fn editor_insert_newline(editor_config: &mut EditorConfig) {
    if editor_config.folds.contains_key(&editor_config.cursor_y) {
        editor_set_status_message(editor_config, "Folded lines can't be edited. Ctrl-Space to unfold.")
    } else {
        if editor_config.cursor_y < editor_config.rows.len() {
            let depth;
            let mut next_row;
            let will_clear_row;
            {
                let ref row = editor_config.rows[editor_config.cursor_y];
                depth = whitespace_depth(row);
                next_row = row[..depth].to_vec();
                will_clear_row = row.len() == depth && editor_config.cursor_x == row.len();
            }

            let row_end = editor_config.rows[editor_config.cursor_y].split_off(editor_config.cursor_x);
            next_row.extend(row_end);
            editor_config
                .rows
                .insert(editor_config.cursor_y + 1, next_row);

            if will_clear_row {
                editor_config.rows[editor_config.cursor_y] = Vec::new();
            }

            let index = editor_config.cursor_y;
            update_row(editor_config, index);
            update_row(editor_config, index + 1);

            editor_config.cursor_x = depth;
        } else {
            editor_config.rows.push(Vec::new());
            editor_config.cursor_x = 0;
        }
        editor_config.cursor_y += 1;
        editor_config.modified = true;
    }
}

fn editor_delete_char(editor_config: &mut EditorConfig) {
    if editor_config.folds.contains_key(&editor_config.cursor_y) {
        editor_set_status_message(editor_config, "Folded lines can't be edited. Ctrl-Space to unfold.")
    } else {
        if editor_config.cursor_x > 0 {
            editor_config.rows[editor_config.cursor_y].remove(editor_config.cursor_x - 1);
            let index = editor_config.cursor_y;
            update_row(editor_config, index);
            editor_config.cursor_x -= 1;
        } else if 0 < editor_config.cursor_y && editor_config.cursor_y < editor_config.rows.len() {
            editor_config.cursor_x = editor_config.rows[editor_config.cursor_y - 1].len();
            let append_line = editor_config.rows[editor_config.cursor_y].clone();
            let append_line = append_line[whitespace_depth(&append_line)..].to_vec();
            editor_config.rows[editor_config.cursor_y - 1].extend(&append_line);
            let index = editor_config.cursor_y - 1;
            update_row(editor_config, index);
            editor_config.rows.remove(editor_config.cursor_y);
            editor_config.cursor_y -= 1;
        };
        editor_config.modified = true
    }
}

/// * file i/o **

fn editor_open(editor_config: &mut EditorConfig, filename: &str) -> io::Result<()> {
    editor_config.filename = Some(filename.to_string());
    editor_select_syntax(editor_config);
    let file = File::open(filename)?;
    let mut contents = String::new();
    for byte in file.bytes() {
        let c = byte? as char;
        if c == '\n' {
            let row = string_to_row(&contents);
            editor_config.rows.push(row);
            let index = editor_config.rows.len() - 1;
            update_row(editor_config, index);
            contents = String::new();
        } else {
            contents.push(c);
        }
    }
    Ok(())
}

fn editor_save(editor_config: &mut EditorConfig) -> io::Result<()> {
    if editor_config.filename.is_none() {
        match editor_prompt(editor_config, "Save as: ", None) {
            None => {
                editor_set_status_message(editor_config, "Save aborted");
                return Ok(());
            }
            s @ Some(_) => {
                editor_config.filename = s;
                editor_select_syntax(editor_config)
            }
        }
    }

    let filename = editor_config.filename.clone().unwrap();
    let mut file = File::create(filename)?;
    let mut text = editor_config
        .rows
        .iter()
        .map(row_to_string)
        .collect::<Vec<_>>()
        .join("\n");
    text.push('\n');
    file.write_all(text.as_bytes())?;
    editor_config.modified = false;
    editor_set_status_message(editor_config,
                              &mut format!("{} bytes written to disk", text.len()));
    Ok(())
}

/// * find **

fn editor_find_callback(editor_config: &mut EditorConfig, query: &str, key: EditorKey) {
    if editor_config.cursor_y < editor_config.rows.len() {
        let index = editor_config.cursor_y;
        update_row(editor_config, index)
    }
    if key != EditorKey::Verbatim('\r') && key != EditorKey::Verbatim('\x1b') {
        let match_line = if key == EditorKey::ArrowRight || key == EditorKey::ArrowDown {
            let potential_match = if editor_config.cursor_y < editor_config.rows.len() - 1 {
                editor_config.rows[editor_config.cursor_y + 1..]
                    .iter()
                    .position(|row| row_to_string(row).contains(query))
                    .map(|offset| offset + editor_config.cursor_y + 1)
            } else {
                None
            };
            potential_match.or(editor_config
                                   .rows
                                   .iter()
                                   .position(|row| row_to_string(row).contains(query)))
        } else if key == EditorKey::ArrowLeft || key == EditorKey::ArrowUp {
            let potential_match = if editor_config.cursor_y > 1 {
                editor_config.rows[..editor_config.cursor_y]
                    .iter()
                    .rposition(|row| row_to_string(row).contains(query))
            } else {
                None
            };
            potential_match.or(editor_config
                                   .rows
                                   .iter()
                                   .rposition(|row| row_to_string(row).contains(query)))
        } else {
            let potential_match = if editor_config.cursor_y < editor_config.rows.len() - 1 {
                editor_config.rows[editor_config.cursor_y..]
                    .iter()
                    .position(|row| row_to_string(row).contains(query))
                    .map(|offset| offset + editor_config.cursor_y)
            } else {
                None
            };
            potential_match.or(editor_config
                                   .rows
                                   .iter()
                                   .position(|row| row_to_string(row).contains(query)))
        };
        if let Some(match_line) = match_line {
            let match_index = row_to_string(&editor_config.rows[match_line])
                .find(query)
                .expect("We just checked the row contained the string.");
            editor_config.cursor_y = match_line;
            open_folds(editor_config);
            editor_config.cursor_x = match_index;
            editor_config.row_offset = editor_config.rows.len();
            for elem in editor_config.rows[match_line][match_index..][..query.len()].iter_mut() {
                elem.1 = EditorHighlight::Match
            }
        }
    }
}

fn editor_find(editor_config: &mut EditorConfig) {
    let saved_cursor_x = editor_config.cursor_x;
    let saved_cursor_y = editor_config.cursor_y;
    let saved_col_offset = editor_config.col_offset;
    let saved_row_offset = editor_config.row_offset;

    let query = editor_prompt(editor_config,
                              "Search (ESC/Arrows/Enter): ",
                              Some(&editor_find_callback));

    if query.is_none() {
        editor_config.cursor_x = saved_cursor_x;
        editor_config.cursor_y = saved_cursor_y;
        editor_config.col_offset = saved_col_offset;
        editor_config.row_offset = saved_row_offset;
    }
}

/// * output **

fn editor_scroll(editor_config: &mut EditorConfig) {
    if editor_config.cursor_y < editor_config.row_offset {
        editor_config.row_offset = editor_config.cursor_y
    } else if screen_line_y(editor_config) >= editor_config.screen_rows {
        while screen_line_y(editor_config) >= editor_config.screen_rows {
            editor_config.row_offset = one_row_forward(editor_config, editor_config.row_offset);
        }
    }
    if editor_config.cursor_x < editor_config.col_offset {
        editor_config.col_offset = editor_config.cursor_x
    } else if editor_config.cursor_x >= editor_config.col_offset + editor_config.screen_cols {
        editor_config.col_offset = editor_config.cursor_x - editor_config.screen_cols + 1;
    }
}

fn editor_draw_rows(editor_config: &EditorConfig, append_buffer: &mut String) {
    let tab = &" ".repeat(TAB_STOP);
    let mut screen_y = 0;
    let mut file_row = editor_config.row_offset;
    while screen_y < editor_config.screen_rows {
        if let Some(&(fold_end, fold_depth)) = editor_config.folds.get(&file_row) {
            let fold_white = editor_config.rows[file_row][..fold_depth].to_vec();
            let fold_white_str = row_to_string(&fold_white).replace("\t", tab);
            append_buffer.push_str(&fold_white_str);
            append_buffer.push_str(INVERT_COLORS);
            let fold_msg = format!("{} lines folded.", fold_end - file_row + 1);
            append_buffer.push_str(&fold_msg);
            for _ in 0..editor_config.screen_cols - fold_msg.len() - fold_white_str.len() {
                append_buffer.push(' ');
            }
            append_buffer.push_str(REVERT_COLORS);
            file_row = fold_end + 1;
        } else if file_row < editor_config.rows.len() {
            let ref current_row = editor_config.rows[file_row];
            if editor_config.col_offset < current_row.len() {
                let mut current_hl = EditorHighlight::Normal;
                for &(c, hl) in current_row
                        .iter()
                        .skip(editor_config.col_offset)
                        .take(editor_config.screen_cols) {
                    if hl != current_hl {
                        current_hl = hl;
                        append_buffer.push_str(hl.color());
                    }
                    if c == '\t' {
                        append_buffer.push_str(tab);
                    } else {
                        if c.is_control() {
                            append_buffer.push_str(INVERT_COLORS);
                            let sym = if c as u8 <= 26 {
                                (64 + (c as u8)) as char
                            } else {
                                '?'
                            };
                            append_buffer.push(sym);
                            append_buffer.push_str(REVERT_COLORS);
                            append_buffer.push_str(hl.color());
                        } else {
                            append_buffer.push(c);
                        }
                    }
                }
                append_buffer.push_str(REVERT_COLORS);
            }
            file_row += 1;
        } else if editor_config.rows.len() == 0 && screen_y == editor_config.screen_rows / 3 {
            let welcome = format!("Isaac's editor -- version {}", IED_VERSION);
            let padding = if welcome.len() < editor_config.screen_cols {
                (editor_config.screen_cols - welcome.len()) / 2
            } else {
                0
            };
            if padding > 0 {
                append_buffer.push('~');
                for _ in 0..padding - 1 {
                    append_buffer.push(' ')
                }
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

fn editor_draw_status_bar(editor_config: &EditorConfig, append_buffer: &mut String) {
    append_buffer.push_str(INVERT_COLORS);
    let mut name = editor_config
        .filename
        .clone()
        .unwrap_or("[No Name]".to_string());
    name.truncate(20);
    let dirty = if editor_config.modified {
        "(modified)"
    } else {
        ""
    };
    let mut status = format!("{} - {} lines {}", name, editor_config.rows.len(), dirty);
    status.truncate(editor_config.screen_cols);
    append_buffer.push_str(&status);
    let filetype = match editor_config.syntax {
        None => "no ft".to_string(),
        Some(ref syntax) => syntax.filetype.clone(),
    };
    let mut right_status = format!("{} | {}/{}",
                                   filetype,
                                   editor_config.cursor_y + 1,
                                   editor_config.rows.len());
    right_status.truncate(editor_config.screen_cols - status.len() - 1);
    for _ in status.len()..(editor_config.screen_cols - right_status.len()) {
        append_buffer.push(' ');
    }
    append_buffer.push_str(&right_status);
    append_buffer.push_str(REVERT_COLORS);
    append_buffer.push_str("\r\n");
}

fn editor_draw_message_bar(editor_config: &EditorConfig, append_buffer: &mut String) {
    append_buffer.push_str(CLEAR_RIGHT);
    let mut message = editor_config.status_message.clone();
    message.truncate(editor_config.screen_cols);
    if editor_config.status_message_time.elapsed().as_secs() < 5 {
        append_buffer.push_str(&message);
    }
}

fn editor_refresh_screen(editor_config: &mut EditorConfig) {
    editor_scroll(editor_config);
    let mut append_buffer: String = String::new();
    append_buffer.push_str(HIDE_CURSOR);
    append_buffer.push_str(CURSOR_TOP_RIGHT);

    editor_draw_rows(editor_config, &mut append_buffer);
    editor_draw_status_bar(editor_config, &mut append_buffer);
    editor_draw_message_bar(editor_config, &mut append_buffer);

    let cursor_control = format!("\x1b[{};{}H",
                                 screen_line_y(editor_config) + 1,
                                 (render_x(editor_config) - editor_config.col_offset) + 1);
    append_buffer.push_str(&cursor_control);
    append_buffer.push_str(SHOW_CURSOR);
    print!("{}", append_buffer);
    io::stdout()
        .flush()
        .expect("Flushing to stdout should work.");
}

fn editor_set_status_message(editor_config: &mut EditorConfig, message: &str) {
    editor_config.status_message = message.to_string();
    editor_config.status_message_time = Instant::now();
}

/// * input **

fn editor_prompt(editor_config: &mut EditorConfig,
                 prompt: &str,
                 callback: Option<&Fn(&mut EditorConfig, &str, EditorKey) -> ()>)
                 -> Option<String> {
    let mut response = String::new();
    loop {
        editor_set_status_message(editor_config, &format!("{}{}", prompt, response));
        editor_refresh_screen(editor_config);

        let c = editor_read_key();
        if c == EditorKey::Verbatim('\x1b') {
            editor_set_status_message(editor_config, "");
            if let Some(callback) = callback {
                callback(editor_config, &response, c)
            };
            return None;
        } else if c == EditorKey::Verbatim('\r') {
            if response.len() > 0 {
                editor_set_status_message(editor_config, "");
                if let Some(callback) = callback {
                    callback(editor_config, &response, c)
                };
                return Some(response);
            }
        } else if c == EditorKey::Delete || c == EditorKey::Verbatim(ctrl_key('h')) ||
                  c == EditorKey::Verbatim(127 as char) {
            if response.len() > 0 {
                response.pop();
            }
        } else if let EditorKey::Verbatim(c) = c {
            if c as usize >= 32 && (c as usize) < 128 {
                response.push(c);
            }
        }
        if let Some(callback) = callback {
            callback(editor_config, &response, c)
        };
    }
}

fn render_x(editor_config: &EditorConfig) -> usize {
    editor_config.cursor_x - editor_config.col_offset +
    if editor_config.cursor_y < editor_config.rows.len() {
        let mut row = editor_config.rows[editor_config.cursor_y].clone();
        row.truncate(editor_config.cursor_x);
        row.iter().filter(|&&(c, _)| c == '\t').count() * (TAB_STOP - 1)
    } else {
        0
    }
}

fn screen_line_y(editor_config: &EditorConfig) -> usize {
    let mut file_y = editor_config.row_offset;
    let mut screen_y = 0;
    while file_y < editor_config.cursor_y {
        file_y = one_row_forward(editor_config, file_y);
        screen_y += 1;
    }
    screen_y
}

fn editor_move_cursor(editor_config: &mut EditorConfig, key: EditorKey) {
    let row_len = if editor_config.rows.len() > editor_config.cursor_y {
        editor_config.rows[editor_config.cursor_y].len()
    } else {
        0
    };
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
            if editor_config.cursor_x > 0 {
                editor_config.cursor_x -= 1
            } else if editor_config.cursor_y > 0 {
                editor_config.cursor_y -= 1;
                editor_config.cursor_x = if editor_config.cursor_y < editor_config.rows.len() {
                    editor_config.rows[editor_config.cursor_y].len()
                } else {
                    0
                };
            }
        }
        EditorKey::ArrowRight => {
            if editor_config.cursor_x < row_len {
                editor_config.cursor_x += 1
            } else if editor_config.cursor_x == row_len &&
                      editor_config.cursor_y < editor_config.rows.len() {
                editor_config.cursor_y += 1;
                editor_config.cursor_x = 0
            }
        }
        EditorKey::PageUp => {
            editor_config.cursor_y = editor_config.row_offset;
            for _ in 0..editor_config.screen_rows {
                editor_move_cursor(editor_config, EditorKey::ArrowUp)
            }
        }
        EditorKey::PageDown => {
            for _ in 0..(editor_config.screen_rows * 2 - 1) {
                editor_move_cursor(editor_config, EditorKey::ArrowDown)
            }
        }
        EditorKey::Home => editor_config.cursor_x = 0,
        EditorKey::End => editor_config.cursor_x = row_len,

        _ => panic!("Editor move cursor received non moving character"),
    };
    let row_len = if editor_config.rows.len() > editor_config.cursor_y {
        editor_config.rows[editor_config.cursor_y].len()
    } else {
        0
    };
    if editor_config.cursor_x > row_len {
        editor_config.cursor_x = row_len
    }
}

fn editor_process_keypress(editor_config: &mut EditorConfig) -> bool {
    let c = editor_read_key();

    if c == EditorKey::Verbatim(ctrl_key('q')) {
        if editor_config.modified && editor_config.quit_times > 0 {
            let quit_times = editor_config.quit_times;
            editor_set_status_message(editor_config,
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
            EditorKey::End => editor_move_cursor(editor_config, c),
            EditorKey::Verbatim(chr) if chr == '\r' => editor_insert_newline(editor_config),
            EditorKey::Delete => editor_delete_char(editor_config),
            EditorKey::Verbatim(chr) if chr as usize == 127 || chr == ctrl_key('h') => {
                editor_delete_char(editor_config)
            }
            EditorKey::Verbatim(chr) if chr == '\x1b' || chr == ctrl_key('l') => (),
            EditorKey::Verbatim(chr) if chr == ctrl_key('s') => {
                match editor_save(editor_config) {
                    Ok(()) => (),
                    Err(e) => {
                        editor_set_status_message(editor_config,
                                                  &mut format!("Saving failed with {}", e))
                    }
                }
            }
            EditorKey::Verbatim(chr) if chr == ctrl_key('f') => editor_find(editor_config),
            EditorKey::Verbatim(chr) if chr == ctrl_key(' ') => toggle_fold(editor_config),
            EditorKey::Verbatim(chr) => editor_insert_char(editor_config, chr),
        };
        editor_config.quit_times = 3;
        true
    }
}

/// * init **

fn main() {
    let orig_termios = match enable_raw_mode() {
        Ok(t) => t,
        Err(e) => panic!("Enabling raw mode failed with {}", e),
    };

    let mut editor_config: EditorConfig = EditorConfig::new(orig_termios);
    print!("{}", CLEAR_SCREEN);
    if let Some(filename) = env::args().nth(1) {
        match editor_open(&mut editor_config, &filename) {
            Ok(()) => (),
            Err(e) => panic!("Opening file failed with {}", e),
        }
    }

    editor_set_status_message(&mut editor_config,
                              "Help: Ctrl-S = save, Ctrl-Q = quit, Ctrl-F = find.");
    loop {
        editor_refresh_screen(&mut editor_config);
        let to_continue = editor_process_keypress(&mut editor_config);
        if !to_continue {
            break;
        }
    }

    match restore_orig_mode(&mut editor_config) {
        Ok(()) => (),
        Err(e) => panic!("Disabling raw mode failed with {}", e),
    };
    print!("{}", CLEAR_SCREEN);
    print!("{}", CURSOR_TOP_RIGHT);
    io::stdout()
        .flush()
        .expect("I hope flushing to stdout works now.");
}
