#[derive(Debug, PartialEq)]
pub enum Token {
    Lambda,
    Letter(char),
    Dot,
    OpenParen,
    CloseParen,
}

/// Lexes (tokenises) a string into a vec of tokens
pub fn lex_string(source: &str) -> Result<Vec<Token>, String> {
    // so there's not many symbols we need to lex:
    // lambda = \
    // letter/char
    // dot = .
    // brackets = (, )
    let mut tokens = Vec::new();

    for (i, c) in source.chars().enumerate() {
        if c.is_whitespace() {
            continue;
        }

        let token = match c {
            '\\' => Token::Lambda,
            c if c.is_alphabetic() => Token::Letter(c),
            '.' => Token::Dot,
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            _ => {
                return Err(format!("unrecognised character at column {}", i + 1));
            }
        };

        tokens.push(token);
    }

    Ok(tokens)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_single_variable() {
        let tokens = lex_string("a");

        assert_eq!(tokens.unwrap(), vec![Token::Letter('a')]);
    }

    #[test]
    fn test_single_lambda() {
        let tokens = lex_string("\\x.x");

        assert_eq!(
            tokens.unwrap(),
            vec![ Token::Lambda, Token::Letter('x'), Token::Dot, Token::Letter('x') ]
        );
    }

    #[test]
    fn test_single_application() {
        let tokens = lex_string("ab");

        assert_eq!(
            tokens.unwrap(),
            vec![ Token::Letter('a'), Token::Letter('b') ]
        );
    }

    #[test]
    #[should_panic]
    fn test_cannot_parse_illegal_token() {
        let _ = lex_string("ab&").unwrap();
    }
}
