# text
A text editor, written in Rust.

It is intended to be a single mode editor with some extra commands, like Emacs,
but with no commands requiring more than one modifier (Ctrl, Alt, Super, etc.) and one normal key.
No Ctrl-Shift, and definitely no sequences.

---

`cargo build --release` to build, binary is `target/release/text`

---

## Commands:

* C-s: Save file, specify file name if needed.
* C-q: Quit. Repeat the command 3 times if changes must be discarded.
* C-f: Find. Type in search text, use arrow keys to navigate between matches.
* C-' ': Fold. Collapse whitespace delimited lines, or uncollapse. May be nested.
         When used on a line with no whitespace, collapses everything.
* C-e: Refresh. Reopen file. May only be used if file has not been modified since last save.
* C-k: Delete line.
* C-g: Go to line specified.
