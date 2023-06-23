use self::ast::{NodeMap, AbstractSyntaxTree, NodeIndex, BinaryOperator, Node, NodeType, Block, Expression, Declaration, Statement};

use super::{lexer::{Token, TokenKind, Keyword}, SymbolMap, errors::{Error, CompilerError, ErrorBuilder, CombineIntoError}, SourceRange, SymbolIndex, Literal};


mod ast;


struct Parser<'a> {
    index: usize,
    tokens: &'a [Token],

    symbol_map: &'a mut SymbolMap,
    node_map: NodeMap,

    file: SymbolIndex,
}


pub fn parse(file: SymbolIndex, tokens: &[Token], symbol_map: &mut SymbolMap) -> Result<AbstractSyntaxTree, Error> {
    let mut parser = Parser {
        index: 0,
        tokens,
        symbol_map,

        // `tokens.len()` is the maximum 
        // amount of instructions that can be created
        node_map: NodeMap::with_capacity(tokens.len()),

        file,
    };

    let result = parser.parse_till(TokenKind::EndOfFile)?;


    // Assert that the map does not grow. 
    // Just a fail safe in case we miss something.
    assert_eq!(parser.node_map.capacity(), tokens.len());
    Ok(AbstractSyntaxTree::new(result, parser.node_map))
}


impl Parser<'_> {
    fn parse_till(&mut self, token: TokenKind) -> Result<Block, Error> {
        let mut nodes = vec![];
        let mut errors = vec![];
        let start = self.current_range();
        
        let mut last_semicolon = None;

        
        loop {
            if self.current_kind() == token || self.current_kind() == TokenKind::EndOfFile {
                break
            } else if let Some(v) = last_semicolon {
                errors.push(v);
                last_semicolon = None;
            }

            let statement = self.statement();
            self.advance();

            match statement {
                Ok(v)  => nodes.push(v),
                Err(v) => errors.push(v),
            }

            if let Err(e) = self.expect(TokenKind::Semicolon) {
                last_semicolon = Some(e);
                continue
            }

            self.advance();

        }


        if let Err(e) = self.expect(token) {
            errors.push(e);
        }

        
        if errors.is_empty() {
            if last_semicolon.is_none() || nodes.is_empty() {
                nodes.push(self.new_expression(start, Expression::Literal(Literal::Empty)))
            }

            Ok(Block::new(nodes))
        } else {
            Err(errors.combine_into_error())
        }
    }


    fn parse_block(&mut self) -> Result<Block, Error> {
        self.expect(TokenKind::LeftBracket)?;
        self.advance();

        self.parse_till(TokenKind::RightBracket)
    }


    fn advance(&mut self) -> &Token {
        self.index += 1;
        self.current()
    }


    fn current(&self) -> &Token {
        &self.tokens[self.index]
    }

    
    fn current_kind(&self) -> TokenKind {
        self.current().token_kind
    }


    fn current_range(&self) -> SourceRange {
        self.current().source_range
    }


    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index + 1)
    }


    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|x| x.token_kind)
    }


    fn peek_range(self) -> Option<SourceRange> {
        self.peek().map(|x| x.source_range)
    }


    fn new_node(&mut self, start: SourceRange, node_type: NodeType) -> NodeIndex {
        let node = Node {
            node_type,
            source_range: start.with(self.current_range()),
        };

        self.node_map.push(node)
    }


    fn new_declaration(&mut self, start: SourceRange, node_type: Declaration) -> NodeIndex {
        self.new_node(start, NodeType::Declaration(node_type))
    }


    fn new_statement(&mut self, start: SourceRange, node_type: Statement) -> NodeIndex {
        self.new_node(start, NodeType::Statement(node_type))
    }


    fn new_expression(&mut self, start: SourceRange, node_type: Expression) -> NodeIndex {
        self.new_node(start, NodeType::Expression(node_type))
    }


    fn expect(&self, token: TokenKind) -> Result<(), Error> {
        if self.current_kind() != token {
            return Err(CompilerError::new(self.file, "unexpected token")
                .highlight(self.current_range())
                    .note(format!("expected '{}' found '{}'", token.to_str(self.symbol_map), self.current_kind().to_str(self.symbol_map)))
                .build())
        }

        Ok(())
    }
}


impl Parser<'_> {
    fn statement(&mut self) -> Result<NodeIndex, Error> {
        match self {
            _ => self.expression()
        }
    }
}


impl Parser<'_> {
    fn expression(&mut self) -> Result<NodeIndex, Error> {
        self.comparisson_expression()
    }


    fn comparisson_expression(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::bitwise_or,
            Parser::bitwise_or,
            &[
                TokenKind::RightAngle,
                TokenKind::LeftAngle,
                TokenKind::GreaterEquals,
                TokenKind::LesserEquals,
                TokenKind::EqualsTo,
                TokenKind::NotEqualsTo,
            ]
        )
    }


    fn bitwise_or(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::bitwise_xor,
            Parser::bitwise_xor,
            &[TokenKind::BitwiseOR]
        )
    }


    fn bitwise_xor(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::bitwise_and,
            Parser::bitwise_and,
            &[TokenKind::BitwiseXOR]
        )
    }


    fn bitwise_and(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::bitwise_shifts,
            Parser::bitwise_shifts,
            &[TokenKind::BitwiseAND]
        )
    }


    fn bitwise_shifts(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::arithmetic_expression,
            Parser::arithmetic_expression,
            &[TokenKind::LeftShift, TokenKind::RightShift, TokenKind::RightShiftZero]
        )
    }


    fn arithmetic_expression(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::product_expression,
            Parser::product_expression,
            &[TokenKind::Plus, TokenKind::Minus]
        )
    }


    fn product_expression(&mut self) -> Result<NodeIndex, Error> {
        self.binary_operation(
            Parser::atom, 
            Parser::atom,
            &[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]
        )
    }
    

    fn atom(&mut self) -> Result<NodeIndex, Error> {
        match self.current_kind() {
            TokenKind::Literal(v) => {
                let node = self.new_expression(self.current_range(), Expression::Literal(v));
                Ok(node)
            },


            TokenKind::LeftParenthesis => {
                let start = self.current_range();
                self.advance();
                if self.current_kind() == TokenKind::RightParenthesis {
                    let node = self.new_expression(start, Expression::Literal(Literal::Empty));
                    return Ok(node)
                }

                let expression = self.expression()?;

                self.advance();
                self.expect(TokenKind::RightParenthesis)?;

                Ok(expression)
            }


            TokenKind::Keyword(Keyword::If) => self.if_expression(),

            _ => return Err(CompilerError::new(self.file, "unexpected token")
                    .highlight(self.current_range())
                    .build())
        }
    }
    

    fn if_expression(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::If))?;
        self.advance();

        let condition = self.expression()?;
        self.advance();

        let block = self.parse_block()?;

        let else_block = if self.peek_kind() == Some(TokenKind::Keyword(Keyword::Else)) {
            self.advance();

            Some(self.if_expression()?)
        } else { None };


        Ok(self.new_expression(
            start, 
            Expression::IfExpression { 
                condition, 
                block, 
                else_block 
            })
        )
    }
}


impl Parser<'_> {
    fn binary_operation(
        &mut self,
        left_func: fn(&mut Self) -> Result<NodeIndex, Error>,
        right_func: fn(&mut Self) -> Result<NodeIndex, Error>,

        operators: &[TokenKind],
    ) -> Result<NodeIndex, Error> {
        let mut base = left_func(self)?;

        loop {
            if !self.peek_kind().map(|x| operators.contains(&x)).unwrap_or(false) {
                break
            }

            self.advance();
            let operator = BinaryOperator::from_token(self.current());

            self.advance();
            let right = right_func(self)?;

            let start = self.node_map[base].source_range;
            base = self.new_expression(
                start, 
                Expression::BinaryOperation {
                    operator,
                    left: base,
                    right,
                }
            );
        }

        Ok(base)
    }
}
