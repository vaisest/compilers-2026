// use regex::Regex;

pub fn compile(source_code: String, _file_name: Option<String>) -> Vec<u8> {
    let out = vec![];
    let _tokens = tokenize(&source_code);
    out
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum TokenType {
    Identifier,
    Integer,
    Operator,
    Punctuation,
}

#[derive(Debug, PartialEq, Eq)]
struct CodeLoc {
    line: usize,
    col: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct Token {
    type_: TokenType,
    loc: CodeLoc,
    text: String,
}

fn is_valid_for(token: TokenType, c: char) -> bool {
    match token {
        TokenType::Identifier => c == '_' || c.is_alphanumeric(),
        TokenType::Integer => c.is_numeric(),
        TokenType::Operator => ['*', '+', '-', '/', '<', '>', '=', '!'].contains(&c),
        TokenType::Punctuation => ['(', ')', '{', '}', ',', ';'].contains(&c),
    }
}

fn tokenize(source_code: &str) -> Vec<Token> {
    let mut output: Vec<Token> = vec![];

    let mut idx = 0;
    let chars = source_code.chars().collect::<Vec<_>>();
    let mut line = 0usize;
    let mut col = 0usize;
    let mut token = None;

    while idx < chars.len() {
        // ignore whitespace
        if chars[idx].is_whitespace() {
            // windows "\r\n" will for sure cause problems here
            // when we encounter whitespace, flush the token as it ends here
            if token.is_some() {
                output.push(token.unwrap());
                token = None;
            }
            if chars[idx] == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
            idx += 1;
            continue;
        }

        // comment handler: skip until newline
        if chars[idx] == '#' || (chars[idx] == '/' && chars[idx + 1] == '/') {
            while idx != chars.len() && chars[idx] != '\n' {
                idx += 1;
            }
            continue;
        }

        if token.is_some() && !is_valid_for(token.as_ref().unwrap().type_, chars[idx]) {
            output.push(token.unwrap());
            token = None;
        }

        // the first character of a token defines its type
        if token.is_none() {
            let type_ = match chars[idx] {
                '0'..='9' => TokenType::Integer,
                '_' | 'a'..='z' | 'A'..='Z' => TokenType::Identifier,
                '*' | '+' | '-' | '/' | '<' | '>' | '=' | '!' => TokenType::Operator,
                '(' | ')' | '{' | '}' | ',' | ';' => TokenType::Punctuation,
                a => todo!("'{a}'"),
            };
            token.replace(Token {
                type_,
                loc: CodeLoc { line, col },
                text: String::new(),
            });
        }

        token.as_mut().unwrap().text.push(chars[idx]);

        idx += 1;
        col += 1;
    }

    if token.is_some() {
        output.push(token.unwrap());
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn it_works() {
        let input = "if  3\nwhile".to_string();
        assert_eq!(
            tokenize(&input),
            vec![
                Token {
                    loc: CodeLoc { line: 0, col: 0 },
                    type_: TokenType::Identifier,
                    text: "if".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 4 },
                    type_: TokenType::Integer,
                    text: "3".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 0 },
                    type_: TokenType::Identifier,
                    text: "while".to_string()
                }
            ]
        );
    }

    #[test]
    fn comments_are_ignored() {
        let input = "// lol\nprint_int(123)// stupid function\n
# third comment"
            .to_string();
        assert_eq!(
            tokenize(&input),
            vec![
                Token {
                    loc: CodeLoc { line: 1, col: 0 },
                    type_: TokenType::Identifier,
                    text: "print_int".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 9 },
                    type_: TokenType::Punctuation,
                    text: "(".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 10 },
                    type_: TokenType::Integer,
                    text: "123".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 13 },
                    type_: TokenType::Punctuation,
                    text: ")".to_string()
                }
            ]
        );
    }

    #[test]
    fn punctuation_makes_sense() {
        let input = "var n = read_int();\nwhile n > 1 do {\n} ".to_string();
        assert_eq!(
            tokenize(&input),
            vec![
                Token {
                    loc: CodeLoc { line: 0, col: 0 },
                    type_: TokenType::Identifier,
                    text: "var".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 4 },
                    type_: TokenType::Identifier,
                    text: "n".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 6 },
                    type_: TokenType::Operator,
                    text: "=".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 8 },
                    type_: TokenType::Identifier,
                    text: "read_int".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 16 },
                    type_: TokenType::Punctuation,
                    text: "();".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 0 },
                    type_: TokenType::Identifier,
                    text: "while".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 6 },
                    type_: TokenType::Identifier,
                    text: "n".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 8 },
                    type_: TokenType::Operator,
                    text: ">".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 10 },
                    type_: TokenType::Integer,
                    text: "1".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 12 },
                    type_: TokenType::Identifier,
                    text: "do".to_string()
                },
                Token {
                    loc: CodeLoc { line: 1, col: 15 },
                    type_: TokenType::Punctuation,
                    text: "{".to_string()
                },
                Token {
                    loc: CodeLoc { line: 2, col: 0 },
                    type_: TokenType::Punctuation,
                    text: "}".to_string()
                }
            ]
        );
    }
}
