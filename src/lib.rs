/// A simple text-and-position token iterator. Token has some (hopefully) useful traits.

use std::convert::AsRef;
use unicode_segmentation::{UWordBounds, UnicodeSegmentation};


/// A text item with a position.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    txt: &'a str,
    pos: Position
}

impl<'a> Token<'a> {
    /// Returns a new token struct with the given `text` and `position`.
    pub fn new(text: &'a str, position: Position) -> Token<'a> {
        Token { txt: text, pos: position }
    }

    /// Returns the Position associated with the text of the Token.
    pub fn position(&self) -> Position {
        self.pos.clone()
    }

    
    /// Returns the text.
    pub fn text(&self) -> &'a str {
        &self.txt[..]
    }
}

impl<'a> From<Token<'a>> for Position {
    fn from(t: Token<'a>) -> Position {
        t.pos.clone()
    }
}

impl<'a> From<Token<'a>> for &'a str {
    fn from(t: Token<'a>) -> &'a str {
        &t.txt[..]
    }
}

impl<'a> AsRef<str> for Token<'a> {
    fn as_ref(&self) -> &'a str {
        &self.txt[..]
    }
}



/// Iterates the Tokens of a given text.


#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum GraphemeKind {
    Whitespace,
    LineTerminator,
    Char
}

impl GraphemeKind {
    fn from_str(s: &str) -> GraphemeKind {
        use GraphemeKind as Gk;
        match s {
            " " => Gk::Whitespace,
            "\n" => Gk::LineTerminator,
            "\r\n" => Gk::LineTerminator,
            _ => Gk::Char,
        }
    }
}

pub struct TokenIter<'a> {
    it: UWordBounds<'a>,
    state: &'a str,
    pos: Position,
}

impl<'a> TokenIter<'a> {
    /// Returns an iterator over the `Token`s of the given `text`.
    pub fn new(text: &'a str) -> TokenIter<'a> {
        TokenIter {
            it: text.split_word_bounds(),
            state: "",
            pos: Position::new(),
        }
    }
}


fn split_text<'a>(txt: &'a str) -> (&'a str, &'a str) {
    let mut offset = 0;
    let mut state = None;
    let mut g_iter = txt.graphemes(true);
    while let Some(g) = g_iter.next() {
        // println!("GRAPHEME: {:?}", g);
        match state {
            None => {
                state = Some(GraphemeKind::from_str(&g[..]));
                // println!("None STATE: {:?} {:?}", g, state);
                offset += g.len();
                continue;
            },
            Some(state_kind) => {
                let g_kind = GraphemeKind::from_str(&g[..]);
                if g_kind == state_kind {
                    // println!("Same STATE: {:?} {:?} {:?}", g, state_kind, g_kind);
                    offset += g.len();
                    continue;
                }
                // println!("Diff STATE: {:?} {:?} {:?}", g, state_kind, g_kind);
                break;
            }
        }
    }
    let left = &txt[0..offset];
    let right = &txt[offset..];
    // println!("SPLIT TEXT: {:?} {:?}", left, right);
    (left, right)
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        if self.state.len() > 0 {
            let pos = self.pos.clone();
            let (txt, rest) = split_text(self.state);
            self.state = rest;
            self.pos = pos.update(&txt[..]);
            return Some(Token::new(txt, pos))
        }
        if let Some(txt) = self.it.next() {
            let pos = self.pos.clone();
            let (head, rest) = split_text(txt);
            self.pos = pos.update(&head[..]);
            self.state = rest;
            return Some(Token::new(head, pos));
        }
        return None   
    }

    
}


/// The Position of the token's text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    line: usize,
    column: usize,
    byte_offset: usize,
}

impl Position {
    /// Returns a Position as line 1, column 1, and byte_offset 0.
    fn new() -> Position {
        Position::at(1, 1, 0)
    }

    fn at(line: usize, column: usize, byte_offset: usize) -> Position {
        Position { line, column, byte_offset }
    }

    /// Given a position and some text `txt` returns a new, updated position.
    fn update(&self, txt: &str) -> Position {
        match txt {
            "\n" => {
                Position{
                    line: self.line + 1,
                    column: 1,
                    byte_offset: self.byte_offset + 1
                }
            },
            "\r\n" => {
                Position {
                    line: self.line + 1,
                    column: 1,
                    byte_offset: self.byte_offset + 2
                }
            },
            txt => {
                Position{
                    line: self.line,
                    column: self.column + txt.graphemes(true).count(),
                    byte_offset: self.byte_offset + txt.len(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works_for_ascii_correctly() {
        let text = "hello: (world)\n\n \r\n  \"thing\"";
        let mut it = TokenIter::new(text);
        assert_eq!(it.next().unwrap(), Token::new("hello", Position{line: 1, column: 1, byte_offset: 0}));
        assert_eq!(it.next().unwrap(), Token::new(":", Position{line: 1, column: 6, byte_offset: 5}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 1, column: 7, byte_offset: 6}));
        assert_eq!(it.next().unwrap(), Token::new("(", Position{line: 1, column: 8, byte_offset: 7}));
        assert_eq!(it.next().unwrap(), Token::new("world", Position{line: 1, column: 9, byte_offset: 8}));
        assert_eq!(it.next().unwrap(), Token::new(")", Position{line: 1, column: 14, byte_offset: 13}));
        assert_eq!(it.next().unwrap(), Token::new("\n", Position{line: 1, column: 15, byte_offset: 14}));
        assert_eq!(it.next().unwrap(), Token::new("\n", Position{line: 2, column: 1, byte_offset: 15}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 3, column: 1, byte_offset: 16}));
        assert_eq!(it.next().unwrap(), Token::new("\r\n", Position{line: 3, column: 2, byte_offset: 17}));
        assert_eq!(it.next().unwrap(), Token::new("  ", Position{line: 4, column: 1, byte_offset: 19}));
        assert_eq!(it.next().unwrap(), Token::new("\"", Position{line: 4, column: 3, byte_offset: 21}));
        assert_eq!(it.next().unwrap(), Token::new("thing", Position{line: 4, column: 4, byte_offset: 22}));
        assert_eq!(it.next().unwrap(), Token::new("\"", Position{line: 4, column: 9, byte_offset: 27}));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn works_when_unicode_byte_order_mark_is_first() {
        let text = "\u{FEFF} okay";
        let mut it = TokenIter::new(text);
        assert_eq!(it.next().unwrap(), Token::new("\u{FEFF}", Position{line: 1, column: 1, byte_offset: 0}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 1, column: 2, byte_offset: 3}));
        assert_eq!(it.next().unwrap(), Token::new("okay", Position{line: 1, column: 3, byte_offset: 4}));
    }

    #[test]
    fn works_when_unicode_byte_order_mark_is_not_first() {
        let text = "one \u{FEFF} two";
        let mut it = TokenIter::new(text);
        assert_eq!(it.next().unwrap(), Token::new("one", Position{line: 1, column: 1, byte_offset: 0}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 1, column: 4, byte_offset: 3}));
        assert_eq!(it.next().unwrap(), Token::new("\u{FEFF}", Position{line: 1, column: 5, byte_offset: 4}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 1, column: 6, byte_offset: 7}));
        assert_eq!(it.next().unwrap(), Token::new("two", Position{line: 1, column: 7, byte_offset: 8}));
    }

    #[test]
    fn keeps_words_intact() {
        let text = "\u{FEFF}José Valim";
        let mut it = TokenIter::new(text);
        assert_eq!(it.next().unwrap(), Token::new("\u{FEFF}", Position{line: 1, column: 1, byte_offset: 0}));
        assert_eq!(it.next().unwrap(), Token::new("José", Position{line: 1, column: 2, byte_offset: 3}));
        assert_eq!(it.next().unwrap(), Token::new(" ", Position{line: 1, column: 6, byte_offset: 8}));
        assert_eq!(it.next().unwrap(), Token::new("Valim", Position{line: 1, column: 7, byte_offset: 9}));
    }
}



