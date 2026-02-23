use crate::compiler::tokenizer::{Token, TokenType};

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Mul,
    Sub,
    Div,
    Eq,
    Neq,
    Rem,
    Or,
    And,
    Lt,
    Gt,
    Leq,
    Geq,
    Assign,
}
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Literal(i64),
    Identifier(String),
    // condition, then, else
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    // func identifier, arguments
    Function(String, Vec<Expression>),
    // many expressions, and possibly one last result expression
    Block(Vec<Expression>, Box<Option<Expression>>),
    // variable initialisation
    Local(String, Box<Expression>),
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
        "==" => BinaryOp::Eq,
        "!=" => BinaryOp::Neq,
        "%" => BinaryOp::Rem,
        "or" => BinaryOp::Or,
        "and" => BinaryOp::And,
        "<" => BinaryOp::Lt,
        ">" => BinaryOp::Gt,
        "<=" => BinaryOp::Leq,
        ">=" => BinaryOp::Geq,
        "=" => BinaryOp::Assign,
        _ => todo!(),
    }
}

fn operator_precedence(op: &str) -> usize {
    // higher number means higher precedence
    match op {
        "=" => 1,
        "or" => 2,
        "and" => 3,
        "==" | "!=" => 4,
        "<" | "<=" | ">" | ">=" => 5,
        "+" | "-" => 6,
        "*" | "/" | "%" => 7,
        _ => panic!("this is not a supported operator"),
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

    fn parse_argument_list(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = vec![];
        loop {
            args.push(self.parse_expression()?);
            if self.peek().is_some_and(|v| v.text == ",") {
                self.consume(None)?;
            } else {
                break;
            }
        }
        Ok(args)
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let token = self.consume(None)?;
        if token.type_ != TokenType::Identifier {
            return Err("Unexpected token: expected a literal integer".to_string());
        }

        if self.peek().is_some_and(|v| v.text == "(") {
            // consume (
            self.consume(None)?;
            let args = self.parse_argument_list()?;
            self.consume(Some(&[")"]))?;
            return Ok(Expression::Function(token.text, args));
        }

        Ok(Expression::Identifier(token.text.clone()))
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
            (TokenType::Identifier, "var") => self.parse_local(),
            (TokenType::Identifier, text) => {
                if text == "then" || text == "else" {
                    Err("Expected a variable name, not a then or else".to_string())
                } else {
                    self.parse_identifier()
                }
            }
            (TokenType::Integer, _) => self.parse_int_literal(),

            (TokenType::Punctuation, "(") => self.parse_parenthesized(),
            (TokenType::Punctuation, "{") => self.parse_block(),
            (_, s) => Err(format!(
                "Unexpected token found: expected an identifier or a literal integer, but received: {s}"
            )),
        }
    }

    fn parse_parenthesized(&mut self) -> ParseResult {
        self.consume(Some(&["("]))?;
        let expr = self.parse_expression()?;
        self.consume(Some(&[")"]))?;
        Ok(expr)
    }

    fn parse_block(&mut self) -> ParseResult {
        self.consume(Some(&["{"]))?;
        let mut expressions = vec![];
        let mut return_expr = None;
        while self.peek().is_some_and(|v| v.text.as_str() != "}") {
            let expr = self.parse_expression()?;
            // the last ; is optional as it controls whether the last expression
            // is returned. This means we expect either a } or ; after each expression
            if self.peek().is_some_and(|v| v.text == ";") {
                self.consume(None)?;
                expressions.push(expr);
            } else {
                return_expr.replace(expr);
                break;
            }
        }
        self.consume(Some(&["}"]))?;
        Ok(Expression::Block(expressions, Box::new(return_expr)))
    }

    // this can essentially parse everything. generally that means a single
    // expression, but in the case of a block, parse_factor will call this again
    // to parse all of the block's expressions
    fn parse_expression(&mut self) -> ParseResult {
        let lhs = self.parse_factor()?;
        self.parse_expression_(lhs, 0)
    }

    fn parse_local(&mut self) -> ParseResult {
        self.consume(Some(&["var"]))?;
        let Expression::Identifier(lhs_text) = self.parse_identifier()? else {
            unreachable!()
        };

        self.consume(Some(&["="]))?;
        let rhs = self.parse_expression()?;
        Ok(Expression::Local(lhs_text, Box::new(rhs)))
    }

    // https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_expression_(&mut self, mut lhs: Expression, min_precedence: usize) -> ParseResult {
        // while lookahead is a binary operator whose precedence is >= min_precedence
        while self.peek().is_some_and(|v| v.type_ == TokenType::Operator) {
            let current_prec = operator_precedence(&self.peek().unwrap().text);
            if current_prec < min_precedence {
                break;
            }
            let op = self.consume(None)?;
            let op_type = op_type_for_binary_operator(&op);

            // assignment is handled as a special case as it is the only right-associative operator
            if op_type == BinaryOp::Assign {
                // equals only allows identifiers on the left, or assign operations
                // that have an identifier on the rhs, because a = b = c is allowed.
                if !matches!(lhs, Expression::Identifier(_)) {
                    return Err(format!(
                        "Assignment can only assign to identifiers. Instead found: {lhs:?}"
                    ));
                }
                let rhs = self.parse_expression()?;
                return Ok(Expression::Binary(
                    BinaryOp::Assign,
                    Box::new(lhs),
                    Box::new(rhs),
                ));
            }

            let mut rhs = self.parse_factor()?;
            // while lookahead is a binary operator whose precedence is greater
            // than op's, or a right-associative operator whose precedence is
            // equal to op's
            while let Some(ahead) = self.peek()
                && ahead.type_ == TokenType::Operator
            {
                let greater_prec = operator_precedence(&ahead.text) > current_prec;
                if !greater_prec
                // = is our only right-associative operator
                    || (ahead.text == "="
                    && operator_precedence(&ahead.text) != current_prec)
                {
                    break;
                }
                let next_prec = current_prec + usize::from(greater_prec);
                rhs = self.parse_expression_(rhs, next_prec)?;
            }

            lhs = Expression::Binary(op_type, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }
}

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    if tokens.is_empty() {
        return Err("Input should not be empty".to_string());
    }
    let mut parser = Parser { tokens, pos: 0 };

    let res = parser.parse_expression()?;

    if parser.has_remaining_tokens() {
        dbg!(&res);
        Err("There should not be any remaining tokens".to_string())
    } else {
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::tokenizer::tokenize;

    use super::*;

    #[allow(clippy::unnecessary_box_returns)]
    fn ident(text: &str) -> Box<Expression> {
        Box::new(Expression::Identifier(text.to_string()))
    }

    #[allow(clippy::unnecessary_box_returns)]
    fn literal(n: i64) -> Box<Expression> {
        Box::new(Expression::Literal(n))
    }

    #[allow(clippy::needless_pass_by_value)]
    fn assert_parsing_is_successful_and_equal_to(source_code: &str, goal: Expression) {
        let tokens = tokenize(source_code);
        let result = parse(tokens);
        assert!(result.is_ok(), "{result:?} should not have been an error");
        let tree = result.unwrap();
        assert_eq!(tree, goal);
    }

    fn assert_parsing_fails(source_code: &str) {
        let tokens = tokenize(source_code);
        let result = parse(tokens);
        assert!(result.is_err(), "{result:?} should have been an error");
    }

    #[test]
    fn simple_addition_works() {
        assert_parsing_is_successful_and_equal_to(
            "1 + a",
            Expression::Binary(
                BinaryOp::Add,
                Box::new(Expression::Literal(1)),
                Box::new(Expression::Identifier("a".to_string())),
            ),
        );
    }

    #[test]
    fn simple_if_then_works() {
        assert_parsing_is_successful_and_equal_to(
            "if a then b + c",
            Expression::If(
                ident("a"),
                Box::new(Expression::Binary(BinaryOp::Add, ident("b"), ident("c"))),
                None,
            ),
        );
    }

    #[test]
    fn simple_if_then_else_works() {
        assert_parsing_is_successful_and_equal_to(
            "if a then b + c else d*x",
            Expression::If(
                ident("a"),
                Box::new(Expression::Binary(BinaryOp::Add, ident("b"), ident("c"))),
                Some(Box::new(Expression::Binary(
                    BinaryOp::Mul,
                    ident("d"),
                    ident("x"),
                ))),
            ),
        );
    }

    #[test]
    fn simple_func_works() {
        assert_parsing_is_successful_and_equal_to(
            "f(x, y + z)",
            Expression::Function(
                "f".to_string(),
                vec![
                    *ident("x"),
                    Expression::Binary(BinaryOp::Add, ident("y"), ident("z")),
                ],
            ),
        );
    }

    #[test]
    fn simple_precedence_works() {
        assert_parsing_is_successful_and_equal_to(
            "2*1 + 1",
            Expression::Binary(
                BinaryOp::Add,
                Box::new(Expression::Binary(BinaryOp::Mul, literal(2), literal(1))),
                literal(1),
            ),
        );

        assert_parsing_is_successful_and_equal_to(
            "1+1 and 2 or 1 == 0",
            Expression::Binary(
                BinaryOp::Or,
                Box::new(Expression::Binary(
                    BinaryOp::And,
                    Box::new(Expression::Binary(BinaryOp::Add, literal(1), literal(1))),
                    literal(2),
                )),
                Box::new(Expression::Binary(BinaryOp::Eq, literal(1), literal(0))),
            ),
        );
    }

    #[test]
    fn complex_precedence_works() {
        assert_parsing_is_successful_and_equal_to(
            "2*1 + n*2 % 2 and 2-3 < 4/3 or 1 == 0",
            Expression::Binary(
                BinaryOp::Or,
                Box::new(Expression::Binary(
                    BinaryOp::And,
                    Box::new(Expression::Binary(
                        BinaryOp::Add,
                        Box::new(Expression::Binary(BinaryOp::Mul, literal(2), literal(1))),
                        Box::new(Expression::Binary(
                            BinaryOp::Rem,
                            Box::new(Expression::Binary(BinaryOp::Mul, ident("n"), literal(2))),
                            literal(2),
                        )),
                    )),
                    Box::new(Expression::Binary(
                        BinaryOp::Lt,
                        Box::new(Expression::Binary(BinaryOp::Sub, literal(2), literal(3))),
                        Box::new(Expression::Binary(BinaryOp::Div, literal(4), literal(3))),
                    )),
                )),
                Box::new(Expression::Binary(BinaryOp::Eq, literal(1), literal(0))),
            ),
        );

        // with parantheses majorly changing the tree
        assert_parsing_is_successful_and_equal_to(
            "(2*1 + n*2 % 2 and 2-3 < 4/3 or 1) == 0",
            Expression::Binary(
                BinaryOp::Eq,
                Box::new(Expression::Binary(
                    BinaryOp::Or,
                    Box::new(Expression::Binary(
                        BinaryOp::And,
                        Box::new(Expression::Binary(
                            BinaryOp::Add,
                            Box::new(Expression::Binary(BinaryOp::Mul, literal(2), literal(1))),
                            Box::new(Expression::Binary(
                                BinaryOp::Rem,
                                Box::new(Expression::Binary(BinaryOp::Mul, ident("n"), literal(2))),
                                literal(2),
                            )),
                        )),
                        Box::new(Expression::Binary(
                            BinaryOp::Lt,
                            Box::new(Expression::Binary(BinaryOp::Sub, literal(2), literal(3))),
                            Box::new(Expression::Binary(BinaryOp::Div, literal(4), literal(3))),
                        )),
                    )),
                    literal(1),
                )),
                literal(0),
            ),
        );
    }

    #[test]
    fn nested_function_calls_work() {
        assert_parsing_is_successful_and_equal_to(
            "f(x, g(x), y + 3)",
            Expression::Function(
                "f".to_string(),
                vec![
                    *ident("x"),
                    Expression::Function("g".to_string(), vec![*ident("x")]),
                    Expression::Binary(BinaryOp::Add, ident("y"), literal(3)),
                ],
            ),
        );
    }

    #[test]
    fn simple_block_works() {
        assert_parsing_is_successful_and_equal_to(
            "{
        f(a);
        test(b);
        2+2;
        f(x)
    }
    ",
            Expression::Block(
                vec![
                    Expression::Function("f".to_string(), vec![*ident("a")]),
                    Expression::Function("test".to_string(), vec![*ident("b")]),
                    Expression::Binary(BinaryOp::Add, literal(2), literal(2)),
                ],
                Box::new(Some(Expression::Function(
                    "f".to_string(),
                    vec![*ident("x")],
                ))),
            ),
        );
    }

    #[test]
    fn empty_block_works() {
        assert_parsing_is_successful_and_equal_to("{}", Expression::Block(vec![], Box::new(None)));
    }

    #[test]
    fn assignment_works() {
        assert_parsing_is_successful_and_equal_to(
            "x = lol+20",
            Expression::Binary(
                BinaryOp::Assign,
                ident("x"),
                Box::new(Expression::Binary(BinaryOp::Add, ident("lol"), literal(20))),
            ),
        );

        assert_parsing_fails("test + x = lol+20");

        assert_parsing_is_successful_and_equal_to(
            "a = b = c*123",
            Expression::Binary(
                BinaryOp::Assign,
                ident("a"),
                Box::new(Expression::Binary(
                    BinaryOp::Assign,
                    ident("b"),
                    Box::new(Expression::Binary(BinaryOp::Mul, ident("c"), literal(123))),
                )),
            ),
        );
    }

    #[test]
    fn var_initialisation_works() {
        assert_parsing_is_successful_and_equal_to(
            "var x = 123 + 5434",
            Expression::Local(
                "x".to_string(),
                Box::new(Expression::Binary(
                    BinaryOp::Add,
                    literal(123),
                    literal(5434),
                )),
            ),
        );
    }

    #[test]
    fn empty_input_is_an_error() {
        assert_parsing_fails("");
    }
    #[test]
    fn trailing_garbage_is_an_error() {
        assert_parsing_fails("a + b c");
    }
    #[test]
    fn garbage_at_the_front_is_an_error() {
        assert_parsing_fails("c a + b");
    }
}
