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
pub struct TokenIter<'a> {
    it: UWordBounds<'a>,
    pos: Position,
}

impl<'a> TokenIter<'a> {
    /// Returns an iterator over the `Token`s of the given `text`.
    pub fn new(text: &'a str) -> TokenIter<'a> {
        TokenIter {
            it: text.split_word_bounds(),
            pos: Position::new(),
        }
    }

}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        match self.it.next() {
            Some(txt) => {
                let pos = self.pos.clone();
                self.pos = pos.update(&txt[..]);
                Some(Token::new(txt, pos))
            },
            None => None
        }   
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
        Position { line: 1, column: 1, byte_offset: 0 }
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
    fn iterates_words_punctuation_spaces_and_newlines_correctly() {
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
}
