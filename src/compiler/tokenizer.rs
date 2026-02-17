#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum TokenType {
    Identifier,
    Integer,
    Operator,
    Punctuation,
}

#[derive(Debug, PartialEq, Eq)]
// TODO: the course material suggests making a wildcard codeloc that would be
// equal to any location
pub struct CodeLoc {
    line: usize,
    col: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    type_: TokenType,
    loc: CodeLoc,
    text: String,
}

fn is_valid_for(token: TokenType, c: char) -> bool {
    match token {
        TokenType::Identifier => c == '_' || c.is_alphanumeric(),
        TokenType::Integer => c.is_numeric(),
        TokenType::Operator => ['*', '+', '-', '/', '<', '>', '=', '!', '%'].contains(&c),
        TokenType::Punctuation => ['(', ')', '{', '}', ',', ';'].contains(&c),
    }
}

pub fn tokenize(source_code: &str) -> Vec<Token> {
    let mut output: Vec<Token> = vec![];

    let mut idx = 0;
    let chars = source_code.chars().collect::<Vec<_>>();
    let mut line = 0usize;
    let mut col = 0usize;
    let mut current_token = None;

    // we scan through the input by character, while keeping track of the column and line separately
    while idx < chars.len() {
        // ignore whitespace
        if chars[idx].is_whitespace() {
            // windows "\r\n" will for sure cause problems here
            // when we encounter whitespace, flush the token as it ends here
            if current_token.is_some() {
                output.push(current_token.unwrap());
                current_token = None;
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

        if current_token.is_some()
            && !is_valid_for(current_token.as_ref().unwrap().type_, chars[idx])
        {
            output.push(current_token.unwrap());
            current_token = None;
        }

        if let Some(current_token) = current_token.as_mut() {
            current_token.text.push(chars[idx]);
            idx += 1;
            col += 1;
        // the first character of a token defines its type
        } else {
            let loc = CodeLoc { line, col };
            let mut text = chars[idx].to_string();

            let type_ = match chars[idx] {
                '0'..='9' => TokenType::Integer,
                '_' | 'a'..='z' | 'A'..='Z' => TokenType::Identifier,
                first @ ('*' | '+' | '-' | '/' | '<' | '>' | '=' | '!' | '%') => {
                    let next = chars.get(idx + 1);
                    if let ('=' | '!' | '<' | '>', Some('=')) = (first, next) {
                        text.push('=');
                    }
                    TokenType::Operator
                }
                '(' | ')' | '{' | '}' | ',' | ';' => TokenType::Punctuation,
                a => todo!("character '{a}' is not recognized as part of any token"),
            };

            idx += text.len();
            col += text.len();
            let new_token = Token { type_, loc, text };

            match type_ {
                // operators and punctuation are not arbitrarily long and can be flushed right away
                TokenType::Operator | TokenType::Punctuation => {
                    output.push(new_token);
                }
                TokenType::Identifier | TokenType::Integer => {
                    current_token.replace(new_token);
                }
            }
        }
    }

    if let Some(current_token) = current_token {
        output.push(current_token);
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    fn tokens_as_text_vec(tokens: &[Token]) -> Vec<&str> {
        tokens.iter().map(|token| token.text.as_str()).collect_vec()
    }

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
                    text: "(".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 17 },
                    type_: TokenType::Punctuation,
                    text: ")".to_string()
                },
                Token {
                    loc: CodeLoc { line: 0, col: 18 },
                    type_: TokenType::Punctuation,
                    text: ";".to_string()
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

    // compare token output to the course material sandbox. the matching string
    // is acquired directly from the web page with:
    // ´JSON.stringify(Array.from(document.querySelectorAll("span.token")).map(it => it.textContent))`
    // in the js console
    #[test]
    fn language_spec_example_works_and_matches_sandbox() {
        let input = "var not = read_int();
print_int(n);
while n > 1 do {
    if n % 2 == 0 then {
        n = n / 2;
    } else {
        n = 3*n + 1;
    }
    print_int(n);
}
";

        assert_eq!(
            tokens_as_text_vec(&tokenize(input)),
            vec![
                "var",
                "not",
                "=",
                "read_int",
                "(",
                ")",
                ";",
                "print_int",
                "(",
                "n",
                ")",
                ";",
                "while",
                "n",
                ">",
                "1",
                "do",
                "{",
                "if",
                "n",
                "%",
                "2",
                "==",
                "0",
                "then",
                "{",
                "n",
                "=",
                "n",
                "/",
                "2",
                ";",
                "}",
                "else",
                "{",
                "n",
                "=",
                "3",
                "*",
                "n",
                "+",
                "1",
                ";",
                "}",
                "print_int",
                "(",
                "n",
                ")",
                ";",
                "}",
            ],
        );
    }
}
