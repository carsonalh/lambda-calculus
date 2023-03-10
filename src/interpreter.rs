use crate::program::Program;

pub fn is_reducable(program: &Program) -> bool {
    match program {
        Program::Application(lhs, _) => {
            match lhs as &Program {
                Program::Lambda(_, _, _) => true,
                Program::Application(_, _) => is_reducable(lhs),
                Program::Variable(_, _) => false,
            }
        },
        _ => false,
    }
}

pub fn reduce(program: &Program) -> Program {
    match program {
        Program::Application(lhs, rhs) => {
            match lhs as &Program {
                Program::Application(_, _) => {
                    Program::Application(Box::new(reduce(lhs)), rhs.clone())
                },
                Program::Lambda(_, capture_id, inner) => {
                    replace_captures_with(inner, *capture_id, rhs)
                },
                _ => program.clone(),
            }
        },
        _ => program.clone(),
    }
}

pub fn run(program: &Program) -> Program {
    let mut state = program.clone();

    while is_reducable(&state) {
        state = reduce(&state)
    }

    state
}

fn replace_captures_with(program: &Program, capture_id: u64, expression: &Program) -> Program {
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
    use crate::{parser::parse_string, interpreter::run};

    use super::Program;

    #[test]
    fn does_not_simplify_atomic_expression() {
        let expression = parse_string("\\x.xy")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| run(&program))
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
            .map(|program| run(&program))
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
            ),
            program
        )
    }

    #[test]
    fn closures_are_not_same_as_var_names() {
        let simplified = parse_string("(\\x.\\y. (\\f. x f) (\\g. y)) y z")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| run(&program))
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

    #[test]
    fn trivial_y_combinator_evaluates_without_halting() {
        let _ = parse_string("(\\f.(\\x.f(x x))(\\x.f(x x))) (\\f.\\x. x) y")
            .map(|expr| Program::from_expression(&expr))
            .map(|program| run(&program))
            .unwrap();
    }
}
