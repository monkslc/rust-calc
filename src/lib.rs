use std::cmp::Ordering;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OperatorPrecedence {
    Lowest,
    Sum,
    Product,
    Exp,
}

impl PartialOrd for OperatorPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OperatorPrecedence {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as usize).cmp(&(*other as usize))
    }
}

pub fn eval_expression(expression: &str) -> f64 {
    let tokens = tokenize(&expression);
    let (tree, _) = parse(&tokens, OperatorPrecedence::Lowest);
    evaluate(tree)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Mult,
    Div,
    Exp,
    LeftParen,
    RightParen,
}

pub fn tokenize(expression: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = expression.char_indices().peekable();
    while let Some((i, ch)) = chars.next() {
        match ch {
            '0'..='9' => {
                let mut end_digit = i;
                while let Some((new_end, '0'..='9')) = chars.peek() {
                    end_digit = *new_end;
                    chars.next();
                }
                let digit = &expression[i..end_digit + 1];
                let digit = f64::from_str(digit).expect("Should always be valid f64");
                tokens.push(Token::Number(digit));
            }
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Mult),
            '/' => tokens.push(Token::Div),
            '^' => tokens.push(Token::Exp),
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            ch if ch.is_whitespace() => (),
            ch => panic!("Not recognized: {}", ch),
        }
    }

    tokens
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Number(f64),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Exp(Box<Expression>, Box<Expression>),
}

pub fn parse(tokens: &[Token], operator_precedence: OperatorPrecedence) -> (Expression, &[Token]) {
    let (left, tokens) = match tokens.get(0) {
        Some(Token::Number(val)) => (Expression::Number(*val), &tokens[1..]),
        Some(Token::LeftParen) => {
            let (expr, tokens) = parse(&tokens[1..], OperatorPrecedence::Lowest);
            (expr, &tokens[1..])
        }
        _ => todo!("This Token does not have a prefix operator for it yet"),
    };

    recursive_parse(tokens, left, operator_precedence)
}

pub fn recursive_parse(
    tokens: &[Token],
    left: Expression,
    precedence: OperatorPrecedence,
) -> (Expression, &[Token]) {
    match tokens.get(0) {
        None => (left, tokens),
        Some(Token::Plus) if OperatorPrecedence::Sum > precedence => {
            let (right, tokens) = parse(&tokens[1..], OperatorPrecedence::Sum);
            let expr = Expression::Plus(Box::new(left), Box::new(right));
            recursive_parse(tokens, expr, precedence)
        }
        Some(Token::Minus) if OperatorPrecedence::Sum > precedence => {
            let (right, tokens) = parse(&tokens[1..], OperatorPrecedence::Sum);
            let expr = Expression::Minus(Box::new(left), Box::new(right));
            recursive_parse(tokens, expr, precedence)
        }
        Some(Token::Mult) if OperatorPrecedence::Product > precedence => {
            let (right, tokens) = parse(&tokens[1..], OperatorPrecedence::Product);
            let expr = Expression::Mult(Box::new(left), Box::new(right));
            recursive_parse(tokens, expr, precedence)
        }
        Some(Token::Div) if OperatorPrecedence::Product > precedence => {
            let (right, tokens) = parse(&tokens[1..], OperatorPrecedence::Product);
            let expr = Expression::Div(Box::new(left), Box::new(right));
            recursive_parse(tokens, expr, precedence)
        }
        Some(Token::Exp) if OperatorPrecedence::Exp > precedence => {
            let (right, tokens) = parse(&tokens[1..], OperatorPrecedence::Exp);
            let expr = Expression::Exp(Box::new(left), Box::new(right));
            recursive_parse(tokens, expr, precedence)
        }
        _ => (left, tokens),
    }
}

pub fn evaluate(expr: Expression) -> f64 {
    match expr {
        Expression::Number(val) => val,
        Expression::Plus(left, right) => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            left + right
        }
        Expression::Minus(left, right) => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            left - right
        }
        Expression::Mult(left, right) => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            left * right
        }
        Expression::Div(left, right) => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            left / right
        }
        Expression::Exp(left, right) => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            left.powf(right)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_addition() {
        let result = eval_expression("123 + 123");
        assert_eq!(result, 246.0);
    }

    #[test]
    fn order_of_operations_left_to_right() {
        let result = eval_expression("5 - 1 + 6");
        assert_eq!(result, 10.0);
    }

    #[test]
    fn order_of_operations() {
        let result = eval_expression("5 ^ 2 + 1 * 10 / 2");
        assert_eq!(result, 30.0);
    }

    #[test]
    fn parens() {
        let result = eval_expression("6 ^ ((3 - 2) * 2)");
        assert_eq!(result, 36.0);
    }
}
