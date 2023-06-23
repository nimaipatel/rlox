#[cfg(test)]
mod tests {
    use crate::{scanner::scan, token::Token, token_type::TokenType};

    #[test]
    fn leftparen_rightparen_bang() {
        let source = "()!";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::LeftParen, "(", 1),
            Token::new(TokenType::RightParen, ")", 1),
            Token::new(TokenType::Bang, "!", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn leftparen_rightparen_bangequal() {
        let source = "()!=";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::LeftParen, "(", 1),
            Token::new(TokenType::RightParen, ")", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn number_operator_number() {
        let source = "456!=123";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Number(456.), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123.), "123", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn something_then_comment_then_something() {
        let source = "456!=123// this is a comment\n789!=789";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Number(456.), "456", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(123.), "123", 1),
            Token::new(TokenType::Number(789.), "789", 2),
            Token::new(TokenType::BangEqual, "!=", 2),
            Token::new(TokenType::Number(789.), "789", 2),
            Token::new(TokenType::Eof, "", 2),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn string_literal_nl_string_literal() {
        let source = "\"nice\"\n\"lol\"";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::String("nice"), "\"nice\"", 1),
            Token::new(TokenType::String("lol"), "\"lol\"", 2),
            Token::new(TokenType::Eof, "", 2),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn identifier() {
        let source = "nice != 69";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::Identifier, "nice", 1),
            Token::new(TokenType::BangEqual, "!=", 1),
            Token::new(TokenType::Number(69.), "69", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn test() {
        let source = "if nice == 69 {}";
        let act_out = scan(source).unwrap();
        let exp_out = vec![
            Token::new(TokenType::If, "if", 1),
            Token::new(TokenType::Identifier, "nice", 1),
            Token::new(TokenType::EqualEqual, "==", 1),
            Token::new(TokenType::Number(69.), "69", 1),
            Token::new(TokenType::LeftBrace, "{", 1),
            Token::new(TokenType::RightBrace, "}", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(act_out, exp_out);
    }

    #[test]
    fn test_decimal_token() {
        let source = "1.23 >= 3.45";
        let actual = scan(source).unwrap();
        let expected = vec![
            Token::new(TokenType::Number(1.23), "1.23", 1),
            Token::new(TokenType::GreaterEqual, ">=", 1),
            Token::new(TokenType::Number(3.45), "3.45", 1),
            Token::new(TokenType::Eof, "", 1),
        ];
        assert_eq!(actual, expected);
    }
}
