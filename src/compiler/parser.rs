use crate::compiler::tokenizer::{Token, TokenType};

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Mul,
    Sub,
    Div,
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Literal(i64),
    Identifier(String),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

fn op_type_for_binary_operator(operator: &Token) -> BinaryOp {
    assert!(operator.type_ == TokenType::Operator);
    match operator.text.as_str() {
        "+" => BinaryOp::Add,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        _ => todo!(),
    }
}

type ParseResult = Result<Expression, String>;

impl Parser {
    fn has_remaining_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self, expected_text: Option<&[&str]>) -> Result<Token, String> {
        let token = self.peek().unwrap().clone();

        if expected_text.is_some_and(|expected| !expected.contains(&token.text.as_str())) {
            Err(format!(
                "Unexpected token found: {}. Expected one of {:?}",
                token.text,
                expected_text.unwrap()
            )
            .to_string())
        } else {
            self.pos += 1;
            Ok(token)
        }
    }

    fn parse_int_literal(&mut self) -> ParseResult {
        let token = self.consume(None)?;
        if token.type_ == TokenType::Integer {
            Ok(Expression::Literal(token.text.parse().unwrap()))
        } else {
            Err("Unexpected token: expected a literal integer".to_string())
        }
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let token = self.consume(None)?;
        if token.type_ == TokenType::Identifier {
            Ok(Expression::Identifier(token.text.clone()))
        } else {
            Err("Unexpected token: expected a literal integer".to_string())
        }
    }

    fn parse_if(&mut self) -> ParseResult {
        self.consume(Some(&["if"]))?;
        let if_expr = self.parse_expression()?;
        self.consume(Some(&["then"]))?;
        let true_expr = self.parse_expression()?;
        if self.peek().is_some_and(|v| v.text == "else") {
            self.consume(Some(&["else"]))?;
            let otherwise_expr = self.parse_expression()?;
            Ok(Expression::If(
                Box::new(if_expr),
                Box::new(true_expr),
                Some(Box::new(otherwise_expr)),
            ))
        } else {
            Ok(Expression::If(Box::new(if_expr), Box::new(true_expr), None))
        }
    }

    fn parse_factor(&mut self) -> ParseResult {
        let peeked = self.peek().unwrap();
        match (peeked.type_, peeked.text.as_str()) {
            (TokenType::Identifier, "if") => self.parse_if(),
            (TokenType::Identifier, text) => {
                if text == "then" || text == "else" {
                    Err("Expected a variable name, not a then or else".to_string())
                } else {
                    self.parse_identifier()
                }
            }
            (TokenType::Integer, _) => self.parse_int_literal(),

            (TokenType::Punctuation, "(") => self.parse_parenthesized(),
            _ => Err(
                "Unexpected token found: expected an identifier or a literal integer".to_string(),
            ),
        }
    }

    fn parse_parenthesized(&mut self) -> ParseResult {
        self.consume(Some(&["("]))?;
        let expr = self.parse_expression()?;
        self.consume(Some(&[")"]))?;
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult {
        let mut left = self.parse_factor()?;
        while self
            .peek()
            .is_some_and(|v| ["*", "/"].contains(&v.text.as_str()))
        {
            let operator = self.consume(None)?;
            let right = self.parse_factor()?;
            let op_type = op_type_for_binary_operator(&operator);
            left = Expression::Binary(op_type, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_expression(&mut self) -> ParseResult {
        let mut left = self.parse_term()?;

        while self
            .peek()
            .is_some_and(|v| ["+", "-"].contains(&v.text.as_str()))
        {
            let operator = self.consume(None)?;
            let op_type = op_type_for_binary_operator(&operator);
            let right = self.parse_term()?;
            left = Expression::Binary(op_type, Box::new(left), Box::new(right));
        }

        Ok(left)
    }
}

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    if tokens.is_empty() {
        return Err("Input should not be empty".to_string());
    }
    let mut parser = Parser { tokens, pos: 0 };

    let res = parser.parse_expression();

    if parser.has_remaining_tokens() {
        dbg!(&res);
        Err("There should not be any remaining tokens".to_string())
    } else {
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::tokenizer::tokenize;

    use super::*;
    use pretty_assertions::assert_eq;

    fn ident(text: &str) -> Box<Expression> {
        Box::new(Expression::Identifier(text.to_string()))
    }

    #[test]
    fn simple_addition_works() {
        let tokens = tokenize("1 + a");
        dbg!(&tokens);
        let result = parse(tokens);
        assert!(result.is_ok(), "{result:?} should not have been an error");
        let tree = result.unwrap();
        assert_eq!(
            tree,
            Expression::Binary(
                BinaryOp::Add,
                Box::new(Expression::Literal(1)),
                Box::new(Expression::Identifier("a".to_string()))
            )
        );
    }

    #[test]
    fn simple_if_then_works() {
        let tokens = tokenize("if a then b + c");
        dbg!(&tokens);
        let result = parse(tokens);
        assert!(result.is_ok(), "{result:?} should not have been an error");
        let tree = result.unwrap();
        assert_eq!(
            tree,
            Expression::If(
                ident("a"),
                Box::new(Expression::Binary(BinaryOp::Add, ident("b"), ident("c"),)),
                None
            )
        );
    }

    #[test]
    fn simple_if_then_else_works() {
        let tokens = tokenize("if a then b + c else d*x");
        dbg!(&tokens);
        let result = parse(tokens);
        assert!(result.is_ok(), "{result:?} should not have been an error");
        let tree = result.unwrap();
        assert_eq!(
            tree,
            Expression::If(
                ident("a"),
                Box::new(Expression::Binary(BinaryOp::Add, ident("b"), ident("c"),)),
                Some(Box::new(Expression::Binary(
                    BinaryOp::Mul,
                    ident("d"),
                    ident("x")
                ))),
            )
        );
    }

    // #[test]
    // fn two_expressions_works() {
    //     let tokens = tokenize("a + b;\n c + d");
    //     let result = parse(tokens);
    //     assert!(result.is_err(), "{result:?} should not have been an error");
    // }
    #[test]
    fn empty_input_is_an_error() {
        let tokens = tokenize("");
        let result = parse(tokens);
        assert!(result.is_err(), "{result:?} should have been an error");
    }
    #[test]
    fn trailing_garbage_is_an_error() {
        let tokens = tokenize("a + b c");
        let result = parse(tokens);
        assert!(result.is_err(), "{result:?} should have been an error");
    }
    #[test]
    fn garbage_at_the_front_is_an_error() {
        let tokens = tokenize("c a + b");
        let result = parse(tokens);
        assert!(result.is_err(), "{result:?} should have been an error");
    }
}
