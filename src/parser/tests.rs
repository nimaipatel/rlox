#[cfg(test)]
mod tests {
    use crate::{expr::*, parser::*, scanner};

    #[test]
    fn test_parse_literal() {
        let source = "123\n\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_primary(&tokens, 0).unwrap();
        let expected = Expr::NumericLiteral(123.);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_unary() {
        let source = "\n!123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_unary(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Unary {
                op: Token {
                    token_type: TokenType::Bang,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_factor() {
        let source = "\n123 * 123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_factor(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Star,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_factor2() {
        let source = "\n123 / 123\n";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_factor(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Slash,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_term() {
        let source = "1 * 2 + 2";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_term(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Plus,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_parse_term2() {
        let source = "1 - 2 * 2";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_term(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::Minus,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_comp() {
        let source = "1 * 2 >=  1 + 1";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_comp(&tokens, 0).unwrap();
        assert!(matches!(
            actual,
            Expr::Binary {
                op: Token {
                    token_type: TokenType::GreaterEqual,
                    ..
                },
                ..
            }
        ))
    }

    #[test]
    fn test_grouping() {
        let source = "(1 + 3)";
        let tokens = scanner::scan(source).unwrap();
        let (actual, _) = parse_primary(&tokens, 0).unwrap();
        assert!(matches!(actual, Expr::Grouping(..)))
    }
}
