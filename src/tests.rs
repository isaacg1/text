#![allow(dead_code, unused_imports)]

use std::collections::HashMap;
use std::time::Instant;

use EditorConfig;
use EditorKey;
use ctrl_key;
use EditorHighlight;

use process_keypress;
use refresh_screen;

fn mock_editor() -> EditorConfig {
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
        process_keypress(&mut mock, EditorKey::Verbatim(c));
    }

    assert_eq!(typed_text, mock.all_text());
    assert_eq!(None, mock.check_consistency())
}

#[test]
fn reversed_typing() {
    let typed_text = "Hello, world!";

    let mut mock = mock_editor();
    for c in typed_text.chars() {
        process_keypress(&mut mock, EditorKey::Verbatim(c));
        process_keypress(&mut mock, EditorKey::ArrowLeft);
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
            process_keypress(&mut mock, EditorKey::ArrowRight);
        }
        process_keypress(&mut mock, EditorKey::Verbatim('\r'));
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

    process_keypress(&mut mock, EditorKey::ArrowUp);
    assert_eq!(None, mock.check_consistency());
    process_keypress(&mut mock, EditorKey::ArrowLeft);
    assert_eq!(None, mock.check_consistency());
    process_keypress(&mut mock, EditorKey::ArrowDown);
    assert_eq!(None, mock.check_consistency());
    process_keypress(&mut mock, EditorKey::ArrowRight);
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
    assert!(mock.rows[0][..2]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Keyword1));
    assert!(mock.rows[0][2..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[1][..13]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[1][13..30]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::String));
    assert!(mock.rows[1][30..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2][..4]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2][4..7]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Number));
    assert!(mock.rows[2][7..8]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
    assert!(mock.rows[2][8..]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Comment));
    assert!(mock.rows[3]
                .iter()
                .all(|cell| cell.hl == EditorHighlight::Normal));
}

#[test]
fn fold_last_row_delete_char() {
    let text = "a
b
 c";

    let mut mock = mock_editor();
    mock.load_text(text);

    assert_eq!(mock.rows.len(), 3);

    process_keypress(&mut mock, EditorKey::ArrowDown);
    process_keypress(&mut mock, EditorKey::ArrowDown);
    assert_eq!(mock.cursor_y, 2);
    process_keypress(&mut mock, EditorKey::Verbatim(ctrl_key(' ')));
    process_keypress(&mut mock, EditorKey::ArrowUp);
    process_keypress(&mut mock, EditorKey::ArrowUp);
    process_keypress(&mut mock, EditorKey::ArrowRight);
    assert_eq!(mock.cursor_y, 0);
    assert_eq!(mock.cursor_x, 1);
    process_keypress(&mut mock, EditorKey::Delete);
    assert_eq!(mock.rows.len(), 2);
    refresh_screen(&mut mock);
}

#[test]
fn fold_last_row_delete_row() {
    let text = "a
 b";

    let mut mock = mock_editor();
    mock.load_text(text);

    process_keypress(&mut mock, EditorKey::ArrowDown);
    process_keypress(&mut mock, EditorKey::Verbatim(ctrl_key(' ')));
    process_keypress(&mut mock, EditorKey::ArrowUp);
    process_keypress(&mut mock, EditorKey::Verbatim(ctrl_key('k')));
    refresh_screen(&mut mock);
    assert_eq!(" b", mock.all_text());
}

#[test]
fn fold_next_to_empty_line() {
    let text = "
 a
";
    let mut mock = mock_editor();
    mock.load_text(text);

    process_keypress(&mut mock, EditorKey::ArrowDown);
    process_keypress(&mut mock, EditorKey::Verbatim(ctrl_key(' ')));
    refresh_screen(&mut mock);
}

#[test]
fn fold_wraparound() {
    let text = "a
 b
 c
d";

    let mut mock = mock_editor();
    mock.load_text(text);

    process_keypress(&mut mock, EditorKey::ArrowDown);
    process_keypress(&mut mock, EditorKey::Verbatim(ctrl_key(' ')));
    process_keypress(&mut mock, EditorKey::ArrowRight);
    process_keypress(&mut mock, EditorKey::ArrowRight);
    process_keypress(&mut mock, EditorKey::ArrowRight);
    assert_eq!(None, mock.check_consistency());
    assert_eq!(0, mock.cursor_x);
    assert_eq!(3, mock.cursor_y);
    process_keypress(&mut mock, EditorKey::ArrowLeft);
    assert_eq!(None, mock.check_consistency());
    assert_eq!(2, mock.cursor_x);
    assert_eq!(1, mock.cursor_y);
}
