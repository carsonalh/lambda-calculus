use crate::{lexer, lexer::Token};

/// Represents any legal lambda calculus term
#[derive(Clone, PartialEq, Eq)]
pub enum Expression {
    /// A lambda calculus variable and its corresponding name
    Variable(char),
    /// A lambda (function) definition
    Lambda(char, Box<Expression>),
    /// A lambda function application
    Application(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Expression {
    /// fmt implementation that uses the least amount of brackets feasible for maximum readability
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(x) => write!(f, "{}", x),
            Self::Lambda(x, term) => write!(f, "\\{}. {}", x, term),
            Self::Application(lhs, rhs) => {
                let lhs_is_lambda = matches!(lhs.as_ref(), Self::Lambda(_, _));
                let rhs_is_lambda = matches!(rhs.as_ref(), Self::Lambda(_, _));

                if lhs_is_lambda {
                    write!(f, "({lhs}) ")?;
                } else {
                    write!(f, "{lhs} ")?;
                }

                if let Self::Application(_, _) = rhs.as_ref() {
                    write!(f, "({rhs})")
                } else if rhs_is_lambda {
                    write!(f, "({rhs})")
                } else {
                    write!(f, "{rhs}")
                }
            },
        }
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d: &dyn std::fmt::Display = &self;
        d.fmt(f)
    }
}

/// Lexes and parses a string which outputs an expression
pub fn parse_string(source: &str) -> Option<Expression> {
    lexer::lex_string(source)
        .ok()
        .map(|tokens| parse_tokens(&tokens[..]))
        .flatten()
}

/// Parses a series of tokens into an expression.
pub fn parse_tokens(tokens: &[Token]) -> Option<Expression> {
    // matches of form (open_match, close_match)
    let mut bracket_matches = Vec::<(usize, usize)>::new();
    let mut bracket_stack = Vec::new();

    for (i, token) in tokens.iter().enumerate() {
        match token {
            Token::OpenParen => bracket_stack.push(i),
            Token::CloseParen => {
                if let Some(j) = bracket_stack.pop() {
                    bracket_matches.push((j, i));
                } else {
                    return None;
                }
            },
            _ => {},
        }
    }

    // this term will be one of: variable, lambda definition, application

    let mut skip_until: Option<usize> = None;
    let mut i = 0;

    // will store a list of lambda terms to be made into an application
    let mut terms = Vec::<Expression>::new();

    while i < tokens.len() {
        if let Some(until) = skip_until {
            if i < until {
                i += 1;
                continue;
            } else {
                skip_until = None;
            }
        }

        match tokens[i] {
            Token::Lambda => {
                match (&tokens[i + 1], &tokens[i + 2]) {
                    (Token::Letter(c), Token::Dot) => {
                        let inner = parse_tokens(&tokens[i + 3..]);
                        let lambda_def = inner.map(|term| Expression::Lambda(c.clone(), Box::new(term)));

                        if let Some(def) = lambda_def {
                            terms.push(def);

                            skip_until = Some(tokens.len());
                        } else {
                            println!("failed to parsed a lambda");
                            return None;
                        }
                    },
                    _ => {
                        println!("invalid sequence after lambda {:?} {:?}", &tokens[i + 1], &tokens[i + 2]);
                        return None;
                    }
                }
            },
            Token::OpenParen => {
                let closing_paren = bracket_matches
                    .iter()
                    .find(|(a, _)| a.clone() == i)
                    .map(|(_, b)| b)
                    .expect("error matching brackets")
                    .clone();

                let inner = parse_tokens(&tokens[i + 1..closing_paren]);

                skip_until = Some(closing_paren + 1);

                if let Some(expr) = inner {
                    terms.push(expr);
                } else {
                    return None;
                }
            },
            Token::Letter(c) => {
                terms.push(Expression::Variable(c));
            },
            _ => { return None; },
        }
    
        i += 1;
    }

    match terms.len() {
        0 => None,
        1 => Some(terms.pop().unwrap()),
        _ => {
            let second = terms.remove(1);
            let first = terms.remove(0);

            let mut multi_application = Expression::Application(Box::new(first), Box::new(second));

            while terms.len() > 0 {
                multi_application = Expression::Application(Box::new(multi_application), Box::new(terms.remove(0)));
            }

            Some(multi_application)
        },
    }
}

#[cfg(test)]
pub mod tests {
    use super::{parse_string, Expression};

    #[test]
    fn test_parses_single_variable() {
        let expression = parse_string("x").unwrap();

        assert_eq!(Expression::Variable('x'), expression);
    }

    #[test]
    fn test_parses_single_lambda() {
        let expression = parse_string("\\x.x").unwrap();

        assert_eq!(Expression::Lambda('x', Box::new(Expression::Variable('x'))), expression);
    }

    #[test]
    fn test_parses_single_application() {
        let expression = parse_string("a b").unwrap();

        assert_eq!(
            Expression::Application(
                Box::new(Expression::Variable('a')),
                Box::new(Expression::Variable('b'))
            ),
            expression
        );
    }

    #[test]
    #[should_panic]
    fn test_cannot_parse_illegal_expression() {
        let _ = parse_string("\\x.((x y)").unwrap();
    }

    #[test]
    fn test_parses_a_y_combinator() {
        let expression = parse_string("(\\f.(\\x.f(x x))(\\x.f(x x)))").unwrap();

        assert_eq!(
            Expression::Lambda(
                'f',
                Box::new(Expression::Application(
                    Box::new(Expression::Lambda(
                        'x',
                        Box::new(Expression::Application(
                            Box::new(Expression::Variable('f')),
                            Box::new(Expression::Application(
                                Box::new(Expression::Variable('x')),
                                Box::new(Expression::Variable('x'))
                            ))
                        ))
                    )),
                    Box::new(Expression::Lambda(
                        'x',
                        Box::new(Expression::Application(
                            Box::new(Expression::Variable('f')),
                            Box::new(Expression::Application(
                                Box::new(Expression::Variable('x')),
                                Box::new(Expression::Variable('x'))
                            ))
                        ))
                    ))
                ))
            ),
            expression
        );
    }
}
