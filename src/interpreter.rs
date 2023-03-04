use crate::parser::Expression;

/// Represents a program while it's running. Different to Expression as programs
/// can also have closures.
#[derive(PartialEq, Clone, Debug)]
pub enum Program {
    /// An outright lambda expression. Stores a list of all the places the variable should be
    /// substituted to,
    ///
    /// A closure is a wrapper of any other program state, but has an id. Programs can
    /// look equivalent sometimes but because they were passed in in different contexts
    /// they won't have the same meaning. For example;
    /// ```
    /// let simplified = parse_string("(\\x.\\y. (\\f.x f) (\\g.y))yz")
    ///     .map(|expr| simplify_expression(&expr))
    ///     .unwrap();
    ///
    /// let expected = parse_string("y (\\g. z)").unwrap();
    ///
    /// // a naive implementation would do the following;
    /// // (\x.\y.(\f.x f)(\g. y)) y z
    /// // (\y. (\f. y f) (\g. y)) z
    /// // (\f. z f)(\g.z)
    /// // z (\g. z)
    ///
    /// // whereas the correct reduction is:
    /// // (\x.\y.(\f.x f)(\g. y)) y z
    /// // (\y. (\f. y f) (\g. y)) z
    /// // (\f. y f)(\g.z)
    /// // y (\g. z)
    ///
    /// assert_eq!(expected, simplified);
    /// ```
    Lambda(char, u32, Box<Program>),
    /// A variable expression; stores the name of the variable along with an optional capture id.
    ///
    /// The capture id is optional because some variables are not captured by a surrounding lambda.
    Variable(char, Option<u32>),
    /// An application expression
    Application(Box<Program>, Box<Program>),
}

impl std::fmt::Display for Program {
    /// fmt implementation that uses the least amount of brackets feasible for maximum readability
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(x, _) => write!(f, "{}", *x),
            Self::Lambda(x, _, term) => write!(f, "\\{}. {}", x, term),
            Self::Application(lhs, rhs) => {
                let lhs_is_lambda = matches!(lhs.as_ref(), Self::Lambda(_, _, _));
                let rhs_is_lambda = matches!(rhs.as_ref(), Self::Lambda(_, _, _));

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
            }
        }
    }
}

// impl std::fmt::Debug for Program {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let d: &dyn std::fmt::Display = &self;
//         d.fmt(f)
//     }
// }

impl Program {
    pub fn from_expression(expression: &Expression) -> Self {
        let mut captures = Vec::new();
        let mut capture_id = 0;

        Self::from_expression_with_stack(expression, &mut captures, &mut capture_id)
    }

    fn from_expression_with_stack(
        expression: &Expression,
        lambda_captures: &mut Vec<(char, u32)>,
        capture_id: &mut u32,
    ) -> Self {
        let program = match expression {
            Expression::Lambda(x, def) => {
                *capture_id += 1;

                let our_capture = *capture_id;
                lambda_captures.push((*x, our_capture));

                let inner = Box::new(Self::from_expression_with_stack(
                    def,
                    lambda_captures,
                    capture_id,
                ));

                // outside of the capture scope, we don't want other variables to think
                // they belong to this function
                lambda_captures.retain(|v| *v != (*x, our_capture));

                Program::Lambda(*x, our_capture, inner)
            }
            Expression::Variable(x) => {
                // the capture with the same variable name and the highest id is the one we want to pick, right?
                // whichever capture variable with the same name has the (highest?) id is the one we want?
                let nearest_capture = lambda_captures
                    .iter()
                    .filter(|(v, _)| *v == *x)
                    .map(|(_, cid)| *cid)
                    .max();

                Program::Variable(*x, nearest_capture)
            }
            Expression::Application(lhs, rhs) => {
                // with capture_id as a mutable reference, this ordering will number the lhs of the ast first
                let lhs_inner = Box::new(Self::from_expression_with_stack(
                    lhs,
                    lambda_captures,
                    capture_id,
                ));
                let rhs_inner = Box::new(Self::from_expression_with_stack(
                    rhs,
                    lambda_captures,
                    capture_id,
                ));

                Program::Application(lhs_inner, rhs_inner)
            }
        };

        program
    }
}

/// Evaluates if a given expression has been fully simplified (recursively)
pub fn is_simplified(term: &Program) -> bool {
    match term {
        Program::Variable(_, _) => true,
        Program::Application(lhs, rhs) => {
            if let Program::Lambda(_, _, _) = lhs.as_ref() {
                false
            } else {
                is_simplified(lhs) && is_simplified(rhs)
            }
        }
        Program::Lambda(_, _, def) => is_simplified(def),
    }
}

/// Simplifies an expression into an atomic form. This function will halt when given programs that halt.

// for now, will not apply recursively
// TODO: thinking this program is breaking one of the tests. fix it.
pub fn simplify_expression(program: &Program) -> Program {
    let mut term = program.clone();

    while !is_simplified(&term) {
        term = match &term {
            Program::Variable(_, _) => term,
            Program::Lambda(x, c_id, expression) => {
                Program::Lambda(*x, *c_id, Box::new(simplify_expression(expression)))
            }
            Program::Application(lhs, rhs) => match lhs as &Program {
                Program::Lambda(_, capture_id, inner) => {
                    let applied = replace_captures_with(&inner, *capture_id, &rhs);

                    simplify_expression(&applied)
                }
                _ => Program::Application(
                    Box::new(simplify_expression(lhs)),
                    Box::new(simplify_expression(rhs)),
                ),
            },
        };
    }

    term
}

fn replace_captures_with(program: &Program, capture_id: u32, expression: &Program) -> Program {
    match program {
        Program::Variable(_, c_id) => match c_id {
            Some(x) if *x == capture_id => expression.clone(),
            _ => program.clone(),
        },
        Program::Lambda(l_x, l_capture_id, inner) => Program::Lambda(
            *l_x,
            *l_capture_id,
            Box::new(replace_captures_with(inner, capture_id, expression)),
        ),
        Program::Application(lhs, rhs) => Program::Application(
            Box::new(replace_captures_with(lhs, capture_id, expression)),
            Box::new(replace_captures_with(rhs, capture_id, expression)),
        ),
    }
}

#[cfg(test)]
pub mod tests {
    use crate::parser::parse_string;

    use super::{simplify_expression, Program};

    #[test]
    fn does_not_simplify_atomic_expression() {
        let expression = parse_string("\\x.xy")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| simplify_expression(&program))
            .unwrap();

        assert_eq!(
            Program::Lambda(
                'x',
                1,
                Box::new(Program::Application(
                    Box::new(Program::Variable('x', Some(1))),
                    Box::new(Program::Variable('y', None))
                ))
            ),
            expression
        );
    }

    #[test]
    fn simplifies_applicable_expression() {
        let expression = parse_string("(\\x.xy)z")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| simplify_expression(&program))
            .unwrap();

        assert_eq!(
            Program::Application(
                Box::new(Program::Variable('z', None)),
                Box::new(Program::Variable('y', None))
            ),
            expression
        );
    }

    #[test]
    fn correctly_identifies_capture_variables() {
        let expression = parse_string("(\\f.\\g.f)(\\f.\\g.\\x.g)x").unwrap();

        let program = Program::from_expression(&expression);

        assert_eq!(
            Program::Application(
                Box::new(Program::Application(
                    Box::new(Program::Lambda(
                        'f',
                        1,
                        Box::new(Program::Lambda(
                            'g',
                            2,
                            Box::new(Program::Variable('f', Some(1)))
                        ))
                    )),
                    Box::new(Program::Lambda(
                        'f',
                        3,
                        Box::new(Program::Lambda(
                            'g',
                            4,
                            Box::new(Program::Lambda(
                                'x',
                                5,
                                Box::new(Program::Variable('g', Some(4)))
                            ))
                        ))
                    )),
                )),
                Box::new(Program::Variable('x', None))
            ),
            program
        );
    }

    #[test]
    fn does_not_capture_global_variables() {
        let program =
            Program::from_expression(&parse_string("(\\x.\\y. (\\f. x f) (\\g. y)) y z").unwrap());

        assert_eq!(
            program,
            Program::Application(
                Box::new(Program::Application(
                    Box::new(Program::Lambda(
                        'x',
                        1,
                        Box::new(Program::Lambda(
                            'y',
                            2,
                            Box::new(Program::Application(
                                Box::new(Program::Lambda(
                                    'f',
                                    3,
                                    Box::new(Program::Application(
                                        Box::new(Program::Variable('x', Some(1))),
                                        Box::new(Program::Variable('f', Some(3)))
                                    ))
                                )),
                                Box::new(Program::Lambda(
                                    'g',
                                    4,
                                    Box::new(Program::Variable('y', Some(2)))
                                ))
                            ))
                        ))
                    )),
                    Box::new(Program::Variable('y', None))
                )),
                Box::new(Program::Variable('z', None))
            )
        )
    }

    #[test]
    fn closures_are_not_same_as_var_names() {
        let simplified = parse_string("(\\x.\\y. (\\f. x f) (\\g. y)) y z")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| simplify_expression(&program))
            .unwrap();

        // this would be nicer: must implement equality first, that recognises isomorphic capture ids
        // let expected = Program::from_expression(&parse_string("y (\\g. z)").unwrap());

        let expected = Program::Application(
            Box::new(Program::Variable('y', None)),
            Box::new(Program::Lambda(
                'g',
                4,
                Box::new(Program::Variable('z', None)),
            )),
        );

        // a naive implementation would do the following;
        // (\x.\y.(\f.x f)(\g. y)) y z
        // (\y. (\f. y f) (\g. y)) z
        // (\f. z f)(\g.z)
        // z (\g. z)

        // whereas the correct reduction is:
        // (\x.\y.(\f.x f)(\g. y)) y z
        // (\y. (\f. y f) (\g. y)) z
        // (\f. y f)(\g.z)
        // y (\g. z)

        assert_eq!(expected, simplified);
    }
}
