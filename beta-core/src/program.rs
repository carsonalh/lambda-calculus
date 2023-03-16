use std::collections::HashMap;
use rand::SeedableRng;

use crate::parser::Expression;

/// Represents a program while it's running. Different to Expression as programs
/// can also have closures.
pub enum Program {
    /// A lambda expression that stores the id of the captured variable. Variables that are captured
    /// by this definition should have an id that matches this expression.
    Lambda(char, u64, Box<Program>),
    /// A variable expression; stores the name of the variable along with an optional capture id.
    ///
    /// The capture id is optional because some variables are not captured by a surrounding lambda.
    Variable(char, Option<u64>),
    /// An application expression
    Application(Box<Program>, Box<Program>),
}

struct ProgramState {
    current_id: u64,
}

impl Program {
    pub fn from_expression(expression: &Expression) -> Self {
        let mut captures = HashMap::new();
        let mut rng = rand::rngs::StdRng::seed_from_u64(0);

        Self::from_expression_with_stack(expression, &mut captures, &mut rng).0
    }

    fn from_expression_with_stack<R: rand::Rng>(
        expression: &Expression,
        lambda_captures: &mut HashMap<char, u64>,
        mut rng: R,
    ) -> (Self, R) {
        let program = match expression {
            Expression::Lambda(x, def) => {
                let our_capture: u64 = rng.gen();
                let outer_capture = lambda_captures.get(x).map(|y| *y);

                lambda_captures.insert(*x, our_capture);

                let inner_rng = rng;
                let (program, inner_rng) = Self::from_expression_with_stack(
                    def,
                    lambda_captures,
                    inner_rng,
                );
                rng = inner_rng;

                let inner = Box::new(program);

                outer_capture
                    .and_then(|outer_id| lambda_captures.insert(*x, outer_id))
                    .or_else(|| lambda_captures.remove(x));

                Program::Lambda(*x, our_capture, inner)
            }
            Expression::Variable(x) => Program::Variable(*x, lambda_captures.get(x).map(|y| *y)),
            Expression::Application(lhs, rhs) => {
                // with capture_id as a mutable reference, this ordering will number the lhs of the ast first
                let inner_rng = rng;
                let (lhs_program, inner_rng) = Self::from_expression_with_stack(lhs, lambda_captures, inner_rng);
                let (rhs_program, inner_rng) = Self::from_expression_with_stack(rhs, lambda_captures, inner_rng);
                rng = inner_rng;

                let lhs_inner = Box::new(lhs_program);
                let rhs_inner = Box::new(rhs_program);

                Program::Application(lhs_inner, rhs_inner)
            }
        };

        (program, rng)
    }

    fn substitute_capture_id(&self, current_id: u64, new_id: u64) -> Self {
        match self {
            Self::Application(lhs, rhs) => {
                Self::Application(
                    Box::new(lhs.substitute_capture_id(current_id, new_id)),
                    Box::new(rhs.substitute_capture_id(current_id, new_id))
                )
            },
            Self::Lambda(x, inner_id, inner_def) => {
                Self::Lambda(
                    *x, *inner_id,
                    Box::new(inner_def.substitute_capture_id(current_id, new_id))
                )
            },
            Self::Variable(x, optional_var_id) => {
                if let Some(var_id) = optional_var_id {

                    if *var_id == current_id {
                        Self::Variable(*x, Some(new_id))
                    } else {
                        Self::Variable(*x, Some(*var_id))
                    }

                } else {
                    Self::Variable(*x, None)
                }
            }
        }
    }
}

impl Clone for Program {
    fn clone(&self) -> Self {
        match self {
            Self::Lambda(x, capture, inner) => {
                let mut rng = rand::thread_rng();
                let new_capture: u64 = rng.gen();
                Self::Lambda(*x, new_capture, Box::new(inner.substitute_capture_id(*capture, new_capture).clone()))
            },
            Self::Application(lhs, rhs) => Self::Application(lhs.clone(), rhs.clone()),
            Self::Variable(x, capture) => Self::Variable(*x, *capture),
        }
    }
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

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = self as &dyn std::fmt::Display;

        display.fmt(f)
    }
}

impl PartialEq for Program {
    fn eq(&self, other: &Program) -> bool {
        // a mapping from our captures to the other program's captures
        let mut capture_equivalences: Vec<(u64, u64)> = Vec::new();

        is_equal_helper(self, other, &mut capture_equivalences)
    }

    fn ne(&self, other: &Program) -> bool {
        !self.eq(other)
    }

}

fn is_equal_helper(this: &Program, other: &Program, capture_equivalences: &mut Vec<(u64, u64)>) -> bool {
    match (this, other) {
        (Program::Variable(a, a_capture), Program::Variable(b, b_capture)) => {
            match (a_capture, b_capture) {
                (Some(a_id), Some(b_id)) => {
                    capture_equivalences.iter().any(|e| *e == (*a_id, *b_id))
                },
                (None, None) => *a == *b,
                _ => false,
            }
        },
        (Program::Lambda(_, a_id, a_definition), Program::Lambda(_, b_id, b_definition)) => {
            capture_equivalences.push((*a_id, *b_id));
            let inner_equals = is_equal_helper(a_definition, b_definition, capture_equivalences);
            capture_equivalences.pop();

            inner_equals
        },
        (Program::Application(this_a, this_b), Program::Application(other_a, other_b)) => {
            let a_equal = is_equal_helper(this_a, other_a, capture_equivalences);
            let b_equal = is_equal_helper(this_b, other_b, capture_equivalences);

            a_equal && b_equal
        }
        _ => false,
    }
}
