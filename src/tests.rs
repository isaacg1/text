#![allow(dead_code, unused_imports)]

use std::collections::HashMap;
use std::time::Instant;

use EditorConfig;
use EditorKey;

use all_text;
use load_text;
use process_keypress;

use check_consistency;

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
    let text = all_text(&mock);
    assert_eq!(text, "");
    assert_eq!(None, check_consistency(&mock))
}

#[test]
fn line_roundtrip() {
    let mut mock = mock_editor();
    let line = "Hello, world";

    load_text(&mut mock, line);
    let text = all_text(&mock);

    assert_eq!(line, text);
    assert_eq!(None, check_consistency(&mock))
}

#[test]
fn lines_roundtrip() {
    let mut mock = mock_editor();
    let lines = "This
        might
        or might not
        work.
        \n";

    load_text(&mut mock, lines);
    let text = all_text(&mock);

    assert_eq!(lines, text);
    assert_eq!(None, check_consistency(&mock))
}

#[test]
fn simple_typing() {
    let typed_text = "Hello, world!";

    let mut mock = mock_editor();
    for c in typed_text.chars() {
        process_keypress(&mut mock, EditorKey::Verbatim(c));
    }

    assert_eq!(typed_text, all_text(&mock));
    assert_eq!(None, check_consistency(&mock))
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

    assert_eq!(reversed_text, all_text(&mock));
    assert_eq!(None, check_consistency(&mock))
}

#[test]
fn moving_around() {
    let mut mock = mock_editor();

    process_keypress(&mut mock, EditorKey::ArrowUp);
    assert_eq!(None, check_consistency(&mock));
    process_keypress(&mut mock, EditorKey::ArrowLeft);
    assert_eq!(None, check_consistency(&mock));
    process_keypress(&mut mock, EditorKey::ArrowDown);
    assert_eq!(None, check_consistency(&mock));
    process_keypress(&mut mock, EditorKey::ArrowRight);
    assert_eq!(None, check_consistency(&mock));
}
