#[allow(dead_code)]

mod tests {
    use std::collections::HashMap;
    use std::time::Instant;

    use EditorConfig;

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
        use all_text;
        
        let mock = mock_editor();
        let text = all_text(&mock);
        assert_eq!(text, "");
    }

    #[test]
    fn line_roundtrip() {
        use all_text;
        use load_text;

        let mut mock = mock_editor();
        let line = "Hello, world";

        load_text(&mut mock, line);
        let text = all_text(&mock);

        assert_eq!(line, text);
    }

    #[test]
    fn lines_roundtrip() {
        use all_text;
        use load_text;

        let mut mock = mock_editor();
        let lines = "This
        might
        or might not
        work.
        \n";

        load_text(&mut mock, lines);
        let text = all_text(&mock);

        assert_eq!(lines, text);
    }
}
