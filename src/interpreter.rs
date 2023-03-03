use crate::parser::Expression;

/// Evaluates if a given expression has been fully simplified
pub fn is_simplified(term: &Expression) -> bool {
    match term {
        Expression::Variable(_) => true,
        Expression::Application(lhs, rhs) => {
            if let Expression::Lambda(_, _) = lhs.as_ref() {
                false
            } else {
                is_simplified(lhs) && is_simplified(rhs)
            }
        },
        Expression::Lambda(_, def) => is_simplified(def),
    }
}

/// Simplifies an expression into an atomic form. This function will halt programs that halt.
pub fn simplify_term(term: &Expression) -> Expression {
    // for the simplify (compute) rules, it's quite simple in theory,
    // all that needs to happen is if the left of an application is lambda definition
    // then the right term is substituted as the left lambda's variable.
    // that's it

    let mut term = term.clone();

    while !is_simplified(&term) {
        term = match &term {
            Expression::Variable(_) => term.clone(),
            Expression::Application(lhs, rhs)
                if matches!(lhs.as_ref(), Expression::Lambda(_, _)) => {
                    if let Expression::Lambda(x, def) = lhs.as_ref() {
                        substitute_variable(*x, def, rhs)
                    } else {
                        unreachable!()
                    }
                },
            Expression::Application(lhs, rhs) => Expression::Application(Box::new(simplify_term(lhs)), Box::new(simplify_term(rhs))),
            Expression::Lambda(x, def) => Expression::Lambda(*x, Box::new(simplify_term(def))),
        };
    }

    term.clone()
}

fn substitute_variable(var_name: char, expression: &Expression, substitute: &Expression) -> Expression {
    match expression {
        Expression::Variable(n) if n.to_owned() == var_name => substitute.clone(),
        Expression::Variable(_) => expression.clone(),
        Expression::Lambda(x, inner) =>
            if var_name == *x {
                expression.clone()
            } else {
                Expression::Lambda(
                    x.to_owned(),
                    Box::new(substitute_variable(var_name, inner, substitute))
                )
            },
        Expression::Application(a, b) => Expression::Application(
            Box::new(substitute_variable(var_name, &a, substitute)),
            Box::new(substitute_variable(var_name, &b, substitute))
        )
    }
}
