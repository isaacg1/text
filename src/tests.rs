#![allow(dead_code, unused_imports)]

use std::collections::HashMap;
use std::time::Instant;
use std::io;

use EditorConfig;
use EditorKey;
use ctrl_key;
use EditorHighlight;

fn mock_editor() -> EditorConfig<io::Empty> {
    EditorConfig {
        filename: None,
        screen_rows: 10,
        screen_cols: 10,
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
        input_source: io::empty(),
        saved_search: String::new(),
    }
}

struct FakeStdin {
    backward_contents: Vec<u8>,
}

impl io::Read for FakeStdin {
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

fn mock_editor_with_input(input: &str) -> EditorConfig<FakeStdin> {
    EditorConfig {
        filename: None,
        screen_rows: 10,
        screen_cols: 10,
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
        input_source: FakeStdin::new(input.as_bytes()),
        saved_search: String::new(),
    }
}

#[test]
fn empty_text() {
    let mock = mock_editor();
    let text = mock.all_text();
    assert_eq!(text, "");
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn line_roundtrip() {
    let mut mock = mock_editor();
    let line = "Hello, world";

    mock.load_text(line);
    let text = mock.all_text();

    assert_eq!(line, text);
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn lines_roundtrip() {
    let mut mock = mock_editor();
    let lines = "This
        might
        or might not
        work.
        \n";

    mock.load_text(lines);
    let text = mock.all_text();

    assert_eq!(lines, text);
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn simple_typing() {
    let typed_text = "Hello, world!";

    let mut mock = mock_editor();
    for c in typed_text.chars() {
        mock.process_keypress(EditorKey::Verbatim(c));
    }

    assert_eq!(typed_text, mock.all_text());
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn reversed_typing() {
    let typed_text = "Hello, world!";

    let mut mock = mock_editor();
    for c in typed_text.chars() {
        mock.process_keypress(EditorKey::Verbatim(c));
        mock.process_keypress(EditorKey::ArrowLeft);
    }

    let reversed_text = typed_text.chars().rev().collect::<String>();

    assert_eq!(reversed_text, mock.all_text());
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn newline_typing() {
    use whitespace_depth;
    let text = "Hello, world!";

    let mut mock = mock_editor();
    mock.load_text(text);

    for _ in 0..3 {
        for _ in 0..6 {
            mock.process_keypress(EditorKey::ArrowRight);
        }
        mock.process_keypress(EditorKey::Verbatim('\r'));
    }
    assert_eq!(mock.all_text(),
               "Hello,
 world
 !
");
}

#[test]
fn moving_around() {
    let mut mock = mock_editor();

    mock.process_keypress(EditorKey::ArrowUp);
    assert_eq!(None, mock.check_consistency());
    mock.process_keypress(EditorKey::ArrowLeft);
    assert_eq!(None, mock.check_consistency());
    mock.process_keypress(EditorKey::ArrowDown);
    assert_eq!(None, mock.check_consistency());
    mock.process_keypress(EditorKey::ArrowRight);
    assert_eq!(None, mock.check_consistency());
}

#[test]
fn hello_world_highlight() {
    let text = "fn main() {
    println!(\"Hello, world!\\\"\");
    123 // 123
}";
    let mut mock = mock_editor();
    mock.filename = Some("main.rs".to_string());
    mock.select_syntax();

    mock.load_text(text);

    assert_eq!(mock.rows.len(), 4);
    assert!(mock.rows[0].cells[..2]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Keyword1));
    assert!(mock.rows[0].cells[2..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[1].cells[..13]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[1].cells[13..30]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::String));
    assert!(mock.rows[1].cells[30..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2].cells[..4]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2].cells[4..7]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Number));
    assert!(mock.rows[2].cells[7..8]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2].cells[8..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Comment));
    assert!(mock.rows[3]
                .cells
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
}

#[test]
fn multiline_string_highlight() {
    let text = "Outside \"Inside
    Middle
    End\" Done";

    let mut mock = mock_editor();
    mock.filename = Some("main.rs".to_string());
    mock.select_syntax();

    mock.load_text(text);

    assert_eq!(mock.rows.len(), 3);
    assert!(mock.rows[0].cells[..8]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[0].cells[8..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::String));
    assert!(mock.rows[1]
                .cells
                .iter()
                .all(|cell| cell.hl == EditorHighlight::String));
    assert!(mock.rows[2].cells[..8]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::String));
    assert!(mock.rows[2].cells[8..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
}

#[test]
fn temporary_multiline_string() {
    let text = "\"
a";

    let mut mock = mock_editor();
    mock.filename = Some("main.rs".to_string());
    mock.select_syntax();

    mock.load_text(text);

    assert_eq!(mock.rows[1].cells[0].hl, EditorHighlight::String);
    mock.process_keypress(EditorKey::Delete);
    assert_eq!(mock.rows[1].cells[0].hl, EditorHighlight::Normal);
}

#[test]
fn multiline_string_with_blank_line() {
    let text = "\"

a";

    let mut mock = mock_editor();
    mock.filename = Some("main.rs".to_string());
    mock.select_syntax();

    mock.load_text(text);

    assert_eq!(mock.rows[2].cells[0].hl, EditorHighlight::String);
}

#[test]
fn string_ending_at_eol() {
    let text = "\"Hi!\"
a";

    let mut mock = mock_editor();
    mock.filename = Some("main.rs".to_string());
    mock.select_syntax();
    mock.load_text(text);

    assert_eq!(mock.rows[1].cells[0].hl, EditorHighlight::Normal);
}

#[test]
fn fold_last_row_delete_char() {
    let text = "a
b
 c";

    let mut mock = mock_editor();
    mock.load_text(text);

    assert_eq!(mock.rows.len(), 3);

    mock.process_keypress(EditorKey::ArrowDown);
    mock.process_keypress(EditorKey::ArrowDown);
    assert_eq!(mock.cursor_y, 2);
    mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock.process_keypress(EditorKey::ArrowUp);
    mock.process_keypress(EditorKey::ArrowUp);
    mock.process_keypress(EditorKey::ArrowRight);
    assert_eq!(mock.cursor_y, 0);
    assert_eq!(mock.cursor_x, 1);
    mock.process_keypress(EditorKey::Delete);
    assert_eq!(mock.rows.len(), 2);
    mock.refresh_screen();
}

#[test]
fn fold_last_row_delete_row() {
    let text = "a
 b";

    let mut mock = mock_editor();
    mock.load_text(text);

    mock.process_keypress(EditorKey::ArrowDown);
    mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock.process_keypress(EditorKey::ArrowUp);
    mock.process_keypress(EditorKey::Verbatim(ctrl_key('k')));
    mock.refresh_screen();
    assert_eq!(" b", mock.all_text());
}

#[test]
fn fold_next_to_empty_line() {
    let text = "
 a
";
    let mut mock = mock_editor();
    mock.load_text(text);

    mock.process_keypress(EditorKey::ArrowDown);
    mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock.refresh_screen();
}

#[test]
fn fold_wraparound() {
    let text = "a
 b
 c
d";

    let mut mock = mock_editor();
    mock.load_text(text);

    mock.process_keypress(EditorKey::ArrowDown);
    mock.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock.process_keypress(EditorKey::ArrowRight);
    mock.process_keypress(EditorKey::ArrowRight);
    mock.process_keypress(EditorKey::ArrowRight);
    assert_eq!(None, mock.check_consistency());
    assert_eq!(0, mock.cursor_x);
    assert_eq!(3, mock.cursor_y);
    mock.process_keypress(EditorKey::ArrowLeft);
    assert_eq!(None, mock.check_consistency());
    assert_eq!(2, mock.cursor_x);
    assert_eq!(1, mock.cursor_y);
}

#[test]
fn fold_all() {
    let text = "a
        b
    c
d
    e
    f";

    let mut mock1 = mock_editor();
    mock1.load_text(text);
    mock1.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));

    let mut mock2 = mock_editor();
    mock2.load_text(text);
    mock2.process_keypress(EditorKey::ArrowDown);
    mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock2.process_keypress(EditorKey::ArrowDown);
    mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));
    mock2.process_keypress(EditorKey::ArrowDown);
    mock2.process_keypress(EditorKey::ArrowDown);
    mock2.process_keypress(EditorKey::Verbatim(ctrl_key(' ')));

    assert_eq!(mock1.folds, mock2.folds);
}
#[test]
fn read_key_escapes() {
    let keys = "Hi!\x1b[1~I say \x1b[4~\rOk, bye.\x1b[5~\x1b[7~\x1b[3~We\x11";
    let mut mock = mock_editor_with_input(keys);
    while mock.quit_times == 3 {
        let keypress = mock.read_key();
        mock.process_keypress(keypress);
    }

    assert_eq!("We say Hi!
Ok, bye.",
               mock.all_text())
}

#[test]
fn find_no_text() {
    let keys = "\x06me\r";

    let mut mock = mock_editor_with_input(keys);

    let keypress = mock.read_key();
    mock.process_keypress(keypress);
}

#[test]
fn find() {
    let text = "Hi
my
name
is
text.";

    let keys = "\x06me\r";

    let mut mock = mock_editor_with_input(keys);

    mock.load_text(text);

    let keypress = mock.read_key();
    mock.process_keypress(keypress);

    // Indicates the position of the match.
    assert_eq!(2, mock.cursor_y);
    assert_eq!(2, mock.cursor_x);

    // Highlighting should have been cleared.
    assert!(mock.rows
                .iter()
                .flat_map(|row| row.cells.iter())
                .all(|cell| cell.hl == EditorHighlight::Normal));
}

#[test]
fn goto() {
    let text = "F
    i
    l
    l
    e
    r";

    // Ctrl-g 4 Enter
    let keys = "\x074\r";

    let mut mock = mock_editor_with_input(keys);

    mock.load_text(text);
    let keypress = mock.read_key();
    mock.process_keypress(keypress);

    assert_eq!(3, mock.cursor_y);
    assert_eq!(0, mock.cursor_x);
}
