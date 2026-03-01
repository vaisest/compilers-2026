use crate::compiler::tokenizer::{CodeLoc, Token, TokenType};

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
pub enum ExprKind {
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Literal(i64),
    Identifier(String),
    // condition, then, else
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    // condition, then
    While(Box<Expr>, Box<Expr>),
    // func identifier, arguments
    Function(String, Vec<Expr>),
    // many expressions, and mark for if the last expression should be returned
    // as a value
    Block(Vec<Expr>, bool),
    // variable initialisation
    Local(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    kind: ExprKind,
    loc: CodeLoc,
}

impl Expr {
    fn new(kind: ExprKind) -> Self {
        Expr {
            kind,
            loc: CodeLoc::default(),
        }
    }
    fn with_codeloc(kind: ExprKind, loc: CodeLoc) -> Self {
        Expr { kind, loc }
    }

    fn ident_or_literal_from_token(token: Token) -> Self {
        match token.type_ {
            TokenType::Identifier => {
                Self::with_codeloc(ExprKind::Identifier(token.text), token.loc)
            }
            // it is assumed that these tokens are actually numbers
            TokenType::Integer => {
                Self::with_codeloc(ExprKind::Literal(token.text.parse().unwrap()), token.loc)
            }
            _ => todo!(
                "only identifier and integer expressions can be inferred directly from tokens"
            ),
        }
    }
    fn func_from_token(token: Token, args: Vec<Expr>) -> Self {
        Self::with_codeloc(ExprKind::Function(token.text, args), token.loc)
    }
    fn if_from_token(token: &Token, cond: Expr, then: Expr, else_: Option<Expr>) -> Self {
        Self::with_codeloc(
            ExprKind::If(Box::new(cond), Box::new(then), else_.map(Box::new)),
            token.loc,
        )
    }
    fn block_from_token(token: &Token, exprs: Vec<Expr>, returns_value: bool) -> Self {
        Self::with_codeloc(ExprKind::Block(exprs, returns_value), token.loc)
    }
    fn local_from_token(token: &Token, name: String, expr: Expr) -> Self {
        Self::with_codeloc(ExprKind::Local(name, Box::new(expr)), token.loc)
    }
    fn binary_op_from_token(token: &Token, type_: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::with_codeloc(
            ExprKind::Binary(type_, Box::new(lhs), Box::new(rhs)),
            token.loc,
        )
    }
    fn while_from_token(token: &Token, cond: Expr, then: Expr) -> Self {
        Self::with_codeloc(ExprKind::While(Box::new(cond), Box::new(then)), token.loc)
    }
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

type ParseResult = Result<Expr, String>;

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
            Ok(Expr::new(ExprKind::Literal(token.text.parse().unwrap())))
        } else {
            Err("Unexpected token: expected a literal integer".to_string())
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, String> {
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
            return Ok(Expr::func_from_token(token, args));
        }

        Ok(Expr::ident_or_literal_from_token(token))
    }

    fn parse_if(&mut self) -> ParseResult {
        let if_token = self.consume(Some(&["if"]))?;
        let if_expr = self.parse_expression()?;
        self.consume(Some(&["then"]))?;
        let true_expr = self.parse_expression()?;
        if self.peek().is_some_and(|v| v.text == "else") {
            self.consume(Some(&["else"]))?;
            let otherwise_expr = self.parse_expression()?;
            Ok(Expr::if_from_token(
                &if_token,
                if_expr,
                true_expr,
                Some(otherwise_expr),
            ))
        } else {
            Ok(Expr::if_from_token(&if_token, if_expr, true_expr, None))
        }
    }

    fn parse_while(&mut self) -> ParseResult {
        let while_token = self.consume(Some(&["while"]))?;
        let if_expr = self.parse_expression()?;
        self.consume(Some(&["do"]))?;
        self.consume(Some(&["{"]))?;
        let true_block = self.parse_expression()?;
        self.consume(Some(&["}"]))?;
        Ok(Expr::while_from_token(&while_token, if_expr, true_block))
    }

    fn parse_factor(&mut self) -> ParseResult {
        let peeked = self.peek().unwrap();
        match (peeked.type_, peeked.text.as_str()) {
            (TokenType::Identifier, "if") => self.parse_if(),
            (TokenType::Identifier, "while") => self.parse_while(),
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
        let block_token = self.consume(Some(&["{"]))?;
        let mut expressions = vec![];
        // indicates if the block returns its last statement's value or not
        let mut had_semicol = true;
        while self.peek().is_some_and(|v| v.text.as_str() != "}") {
            // make { a b } illegal. only blocks are allowed to omit semicolons on non-last expressions
            if !had_semicol
                && expressions.last().is_some_and(|v: &Expr| {
                    !matches!(v.kind, ExprKind::Block(..) | ExprKind::If(..))
                })
            {
                return Err(format!(
                    "Expected ; following expression in block. Only the blocks or the last expression are allowed to omit semicolons. Instead the last expression was:\n{:?}",
                    expressions.last().unwrap()
                ));
            }

            let expr = self.parse_expression()?;
            // the last ; is optional as it controls whether the last expression
            // is returned. This means we expect either a } or ; after each expression
            had_semicol = self.peek().is_some_and(|v| v.text == ";");
            if had_semicol {
                self.consume(Some(&[";"]))?;
            }
            expressions.push(expr);
        }
        self.consume(Some(&["}"]))?;
        Ok(Expr::block_from_token(
            &block_token,
            expressions,
            !had_semicol,
        ))
    }

    // this can essentially parse everything. generally that means a single
    // expression, but in the case of a block, parse_factor will call this again
    // to parse all of the block's expressions
    fn parse_expression(&mut self) -> ParseResult {
        let lhs = self.parse_factor()?;
        self.parse_expression_(lhs, 0)
    }

    fn parse_local(&mut self) -> ParseResult {
        let token = self.consume(Some(&["var"]))?;
        let ExprKind::Identifier(lhs_text) = self.parse_identifier()?.kind else {
            unreachable!()
        };

        self.consume(Some(&["="]))?;
        let rhs = self.parse_expression()?;
        Ok(Expr::local_from_token(&token, lhs_text, rhs))
    }

    // https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_expression_(&mut self, mut lhs: Expr, min_precedence: usize) -> ParseResult {
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
                if !matches!(lhs.kind, ExprKind::Identifier(_)) {
                    return Err(format!(
                        "Assignment can only assign to identifiers. Instead found: {lhs:?}"
                    ));
                }
                let rhs = self.parse_expression()?;
                return Ok(Expr::binary_op_from_token(&op, op_type, lhs, rhs));
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

            lhs = Expr::binary_op_from_token(&op, op_type, lhs, rhs);
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
    fn ident(text: &str) -> Expr {
        Expr::ident_or_literal_from_token(Token {
            type_: TokenType::Identifier,
            loc: CodeLoc::default(),
            text: text.to_string(),
        })
    }

    #[allow(clippy::unnecessary_box_returns)]
    fn literal(n: i64) -> Expr {
        Expr::ident_or_literal_from_token(Token {
            type_: TokenType::Integer,
            loc: CodeLoc::default(),
            text: n.to_string(),
        })
    }

    fn func(text: &str, args: Vec<Expr>) -> Expr {
        Expr::func_from_token(
            Token {
                type_: TokenType::Dummy,
                loc: CodeLoc::default(),
                text: text.to_string(),
            },
            args,
        )
    }

    #[allow(clippy::needless_pass_by_value)]
    fn assert_parsing_is_successful_and_equal_to(source_code: &str, goal: Expr) {
        let tokens = tokenize(source_code);
        let result = parse(tokens);
        assert!(
            result.is_ok(),
            "{result:?} should not have been an error when parsing:\n{source_code}"
        );
        let tree = result.unwrap();
        assert_eq!(tree, goal);
    }

    fn assert_parsing_fails(source_code: &str) {
        let tokens = tokenize(source_code);
        let result = parse(tokens);
        assert!(
            result.is_err(),
            "{result:?} should have been an error when parsing:\n{source_code}"
        );
    }

    fn assert_parsing_is_successful(source_code: &str) {
        let tokens = tokenize(source_code);
        let result = parse(tokens);
        assert!(
            result.is_ok(),
            "{result:?} should not have been an error when parsing:\n{source_code}"
        );
    }

    // in hindsight: I really should have had something to build the comparison
    // AST from a string representation, because writing theses tests is awful

    #[test]
    fn simple_addition_works() {
        assert_parsing_is_successful_and_equal_to(
            "1 + a",
            Expr::binary_op_from_token(&Token::default(), BinaryOp::Add, literal(1), ident("a")),
        );
    }

    #[test]
    fn simple_if_then_works() {
        assert_parsing_is_successful_and_equal_to(
            "if a then b + c",
            Expr::if_from_token(
                &Token::default(),
                ident("a"),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Add,
                    ident("b"),
                    ident("c"),
                ),
                None,
            ),
        );
    }

    #[test]
    fn simple_if_then_else_works() {
        assert_parsing_is_successful_and_equal_to(
            "if a then b + c else d*x",
            Expr::if_from_token(
                &Token::default(),
                ident("a"),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Add,
                    ident("b"),
                    ident("c"),
                ),
                Some(Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Mul,
                    ident("d"),
                    ident("x"),
                )),
            ),
        );
    }

    #[test]
    fn simple_func_works() {
        assert_parsing_is_successful_and_equal_to(
            "f(x, y + z)",
            Expr::func_from_token(
                Token {
                    type_: TokenType::Dummy,
                    loc: CodeLoc::default(),
                    text: "f".to_string(),
                },
                vec![
                    ident("x"),
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Add,
                        ident("y"),
                        ident("z"),
                    ),
                ],
            ),
        );
    }

    #[test]
    fn simple_precedence_works() {
        assert_parsing_is_successful_and_equal_to(
            "2*1 + 1",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Add,
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Mul,
                    literal(2),
                    literal(1),
                ),
                literal(1),
            ),
        );

        assert_parsing_is_successful_and_equal_to(
            "1+1 and 2 or 1 == 0",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Or,
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::And,
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Add,
                        literal(1),
                        literal(1),
                    ),
                    literal(2),
                ),
                Expr::binary_op_from_token(&Token::default(), BinaryOp::Eq, literal(1), literal(0)),
            ),
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn complex_precedence_works() {
        let and = Expr::binary_op_from_token(
            &Token::default(),
            BinaryOp::And,
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Add,
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Mul,
                    literal(2),
                    literal(1),
                ),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Rem,
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Mul,
                        ident("n"),
                        literal(2),
                    ),
                    literal(2),
                ),
            ),
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Lt,
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Sub,
                    literal(2),
                    literal(3),
                ),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Div,
                    literal(4),
                    literal(3),
                ),
            ),
        );
        assert_parsing_is_successful_and_equal_to(
            "2*1 + n*2 % 2 and 2-3 < 4/3 or 1 == 0",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Or,
                and,
                Expr::binary_op_from_token(&Token::default(), BinaryOp::Eq, literal(1), literal(0)),
            ),
        );

        // with parantheses majorly changing the tree
        assert_parsing_is_successful_and_equal_to(
            "(2*1 + n*2 % 2 and 2-3 < 4/3 or 1) == 0",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Eq,
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Or,
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::And,
                        Expr::binary_op_from_token(
                            &Token::default(),
                            BinaryOp::Add,
                            Expr::binary_op_from_token(
                                &Token::default(),
                                BinaryOp::Mul,
                                literal(2),
                                literal(1),
                            ),
                            Expr::binary_op_from_token(
                                &Token::default(),
                                BinaryOp::Rem,
                                Expr::binary_op_from_token(
                                    &Token::default(),
                                    BinaryOp::Mul,
                                    ident("n"),
                                    literal(2),
                                ),
                                literal(2),
                            ),
                        ),
                        Expr::binary_op_from_token(
                            &Token::default(),
                            BinaryOp::Lt,
                            Expr::binary_op_from_token(
                                &Token::default(),
                                BinaryOp::Sub,
                                literal(2),
                                literal(3),
                            ),
                            Expr::binary_op_from_token(
                                &Token::default(),
                                BinaryOp::Div,
                                literal(4),
                                literal(3),
                            ),
                        ),
                    ),
                    literal(1),
                ),
                literal(0),
            ),
        );
    }

    #[test]
    fn nested_function_calls_work() {
        assert_parsing_is_successful_and_equal_to(
            "f(x, g(x), y + 3)",
            Expr::func_from_token(
                Token {
                    type_: TokenType::Dummy,
                    loc: CodeLoc::default(),
                    text: "f".to_string(),
                },
                vec![
                    ident("x"),
                    Expr::func_from_token(
                        Token {
                            type_: TokenType::Dummy,
                            loc: CodeLoc::default(),
                            text: "g".to_string(),
                        },
                        vec![ident("x")],
                    ),
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Add,
                        ident("y"),
                        literal(3),
                    ),
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
            Expr::block_from_token(
                &Token::default(),
                vec![
                    func("f", vec![ident("a")]),
                    func("test", vec![ident("b")]),
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Add,
                        literal(2),
                        literal(2),
                    ),
                    func("f", vec![ident("x")]),
                ],
                true,
            ),
        );
    }

    #[test]
    fn empty_block_works() {
        assert_parsing_is_successful_and_equal_to(
            "{}",
            Expr::block_from_token(&Token::default(), vec![], false),
        );
    }

    #[test]
    fn assignment_works() {
        assert_parsing_is_successful_and_equal_to(
            "x = lol+20",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Assign,
                ident("x"),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Add,
                    ident("lol"),
                    literal(20),
                ),
            ),
        );

        assert_parsing_fails("test + x = lol+20");

        assert_parsing_is_successful_and_equal_to(
            "a = b = c*123",
            Expr::binary_op_from_token(
                &Token::default(),
                BinaryOp::Assign,
                ident("a"),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Assign,
                    ident("b"),
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Mul,
                        ident("c"),
                        literal(123),
                    ),
                ),
            ),
        );
    }

    #[test]
    fn var_initialisation_works() {
        assert_parsing_is_successful_and_equal_to(
            "var x = 123 + 5434",
            Expr::local_from_token(
                &Token::default(),
                "x".to_string(),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Add,
                    literal(123),
                    literal(5434),
                ),
            ),
        );
    }

    #[test]
    fn while_works() {
        assert_parsing_is_successful_and_equal_to(
            "while n > 1 do {
    n = n - 2
}",
            Expr::while_from_token(
                &Token::default(),
                Expr::binary_op_from_token(&Token::default(), BinaryOp::Gt, ident("n"), literal(1)),
                Expr::binary_op_from_token(
                    &Token::default(),
                    BinaryOp::Assign,
                    ident("n"),
                    Expr::binary_op_from_token(
                        &Token::default(),
                        BinaryOp::Sub,
                        ident("n"),
                        literal(2),
                    ),
                ),
            ),
        );
    }

    #[test]
    fn blocks_behave_properly() {
        assert_parsing_is_successful("{ { a } }");
        assert_parsing_is_successful("{ { a } { b } }");
        assert_parsing_fails("{ a b }");
        assert_parsing_is_successful("{ if true then { a } b }");
        assert_parsing_is_successful("{ if true then { a }; b }");
        assert_parsing_fails("{ if true then { a } b c }");
        assert_parsing_is_successful("{ if true then { a } b; c }");
        assert_parsing_is_successful("{ if true then { a } else { b } c }");
        assert_parsing_is_successful("x = { { f(a) } { b } }");
    }

    // TODO: unary operators, typed variable (not sure these are needed?), while-loop

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
