use self::ast::{NodeMap, AbstractSyntaxTree, NodeIndex, BinaryOperator, Node, NodeType, Block, Expression, Declaration, Statement, DataTypeNode, PathSymbol, DataType, DataTypeConditionNode};

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
        node_map: NodeMap::with_capacity(tokens.len() * 2),

        file,
    };


    let result = parser.parse_till(TokenKind::EndOfFile)?;
    // let result = parser.parse_type();


    // Assert that the map does not grow. 
    // Just a fail safe in case we miss something.
    assert_eq!(parser.node_map.capacity(), tokens.len() * 2);

    parser.node_map.trim();

    Ok(AbstractSyntaxTree::new(result, parser.node_map))
}


impl Parser<'_> {
    fn parse_till(&mut self, token: TokenKind) -> Result<Block, Error> {
        let mut nodes = vec![];
        let mut errors = vec![];
        let start = self.current_range();
        
        let mut last_semicolon = None;
        let mut is_in_panic = false;

        
        loop {
            if self.current_kind() == token || self.current_kind() == TokenKind::EndOfFile {
                break
            } else if let Some(v) = last_semicolon {
                errors.push(v);
                last_semicolon = None;
            }

            if matches!(self.current_kind(), TokenKind::Keyword(_)) {
                is_in_panic = false;
            }
            
            let statement = self.statement();

            match statement {
                Ok(v)  => nodes.push(v),
                Err(v) if !is_in_panic => {
                    errors.push(v);
                    is_in_panic = true;
                },
                _ => (),
            }


            if self.expect(TokenKind::RightBracket).is_ok() {
                self.advance();
                continue
            }


            if self.current_kind() != TokenKind::Semicolon {
                self.advance();
            }


            if !is_in_panic {
                if let Err(e) = self.expect(TokenKind::Semicolon) {
                    last_semicolon = Some(e);
                    continue
                }

            }


            
            self.advance();


        }

        assert!(!(is_in_panic && errors.is_empty()));

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


    fn parse_type(&mut self) -> Result<DataTypeNode, Error> {
        let start = self.current_range();
        if self.expect(TokenKind::Star).is_ok() {
            self.advance();

            let is_mut = self.expect(TokenKind::Keyword(Keyword::Mut)).is_ok();

            if !is_mut && self.expect(TokenKind::Keyword(Keyword::Const)).is_err() {                
                return Err(CompilerError::new(self.file, "invalid raw pointer type")
                    .highlight(SourceRange::new(start.start, self.current_range().end))
                        .note(format!(
                            "a pointer can be either 'const' or 'mut', {} is not a valid pointer type", 
                            self.current_kind().to_str(self.symbol_map)))
                    .build())
            }


            self.advance();
            let datatype = self.parse_type()?;

            if let Some(cond) = datatype.condition {
                return Err(CompilerError::new(self.file, "pointer types can't have condition")
                    .highlight(cond.source_range)
                    .build())
            }


            return Ok(DataTypeNode {
                source_range: SourceRange::new(start.start, self.current_range().end), 
                data_type: if is_mut { DataType::MutPointer(Box::new(datatype)) }
                            else { DataType::ConstPointer(Box::new(datatype)) },
                condition: None,
            })
            
        }

        
        let type_path = self.parse_path()?;
        
        let condition = if self.peek_kind() == Some(TokenKind::SquigglyDash) {
            self.advance();
            self.advance();

            let start = self.current_range();
            self.expect(TokenKind::LeftSquare)?;
            self.advance();

            let binding_name = self.expect_identifier()?;
            self.advance();

            self.expect(TokenKind::Colon)?;
            self.advance();
            
            let condition = self.expression()?;
            self.advance();

            self.expect(TokenKind::RightSquare)?;

            Some(DataTypeConditionNode {
                binding_name,
                condition,
                source_range: SourceRange::new(start.start, self.current_range().end),
            })
        } else {
            None
        };


        let data_type = if type_path.value.len() == 1 {
            match self.symbol_map.get(type_path.value[0]).map(|x| x.as_str()) {
                Some("u8") => DataType::U8,
                Some("u16") => DataType::U16,
                Some("u32") => DataType::U32,
                Some("u64") => DataType::U64,
                Some("i8") => DataType::I8,
                Some("i16") => DataType::I16,
                Some("i32") => DataType::I32,
                Some("i64") => DataType::I64,
                Some("bool") => DataType::Bool,
                _ => DataType::Custom(type_path),
            }
        } else { DataType::Custom(type_path) };
        

        Ok(DataTypeNode {
            source_range: start.with(self.current_range()),
            data_type,
            condition,
        })
    }


    fn parse_path(&mut self) -> Result<PathSymbol, Error> {
        let start = self.current_range();
        let mut path_list = Vec::with_capacity(8);
        path_list.push(self.expect_identifier()?);

        while self.peek_kind() == Some(TokenKind::DoubleColon) {
            self.advance();
            self.advance();
            
            let ident = self.expect_identifier()?;
            path_list.push(ident);
        }
        
        Ok(PathSymbol::new(start.with(self.current_range()), path_list))
    }


    fn parse_block(&mut self) -> Result<Block, Error> {
        self.expect(TokenKind::LeftBracket)?;
        self.advance();

        self.parse_till(TokenKind::RightBracket)
    }


    fn left_value(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        let expression = self.expression()?;

        match self.node_map.get(expression).unwrap().node_type {
            NodeType::Expression(Expression::Identifier { .. }) => Ok(expression),

            _ => Err(CompilerError::new(self.file, "value is not an left-side value")
                    .highlight(SourceRange::new(start.start, self.current_range().end))
                        .note("is not one of the following: identifier".to_string())
                    .build()
                )
        }
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


    fn expect_identifier(&self) -> Result<SymbolIndex, Error> {
        if let TokenKind::Identifier(i) = self.current_kind() {
            Ok(i)
        } else {
            Err(CompilerError::new(self.file, "unexpected token")
                .highlight(self.current_range())
                    .note(format!("expected 'an identifier' found '{}'", self.current_kind().to_str(self.symbol_map)))
                .build())
        }
    }


    fn expect_str_literal(&self) -> Result<SymbolIndex, Error> {
        if let TokenKind::Literal(Literal::String(i)) = self.current_kind() {
            Ok(i)
        } else {
            Err(CompilerError::new(self.file, "unexpected token")
                .highlight(self.current_range())
                    .note(format!("expected 'a literal string' found '{}'", self.current_kind().to_str(self.symbol_map)))
                .build())
        }
    }
}


impl Parser<'_> {
    fn statement(&mut self) -> Result<NodeIndex, Error> {
        match self.current_kind() {
            TokenKind::Keyword(Keyword::While) => self.while_statement(),
            TokenKind::Keyword(Keyword::Break) => self.break_statement(),
            TokenKind::Keyword(Keyword::Continue) => self.continue_statement(),
            TokenKind::Keyword(Keyword::Return) => self.return_statement(),
            TokenKind::Keyword(Keyword::Let) => self.variable_declaration(),
            TokenKind::Keyword(Keyword::Struct) => self.structure_declaration(),
            TokenKind::Keyword(Keyword::Fn) => self.function_declaration(),
            TokenKind::Keyword(Keyword::Extern) => self.extern_declaration(),
            TokenKind::Keyword(Keyword::Const) => self.const_item_declaration(),
            TokenKind::Keyword(Keyword::Static) => self.static_item_declaration(),
            TokenKind::Keyword(Keyword::Using) => self.using_declaration(),
            TokenKind::Keyword(Keyword::Namespace) => self.namespace_declaration(),
            _ => self.expression()
        }
    }
    

    fn while_statement(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::While))?;
        self.advance();

        let condition = self.expression()?;
        self.advance();

        let block = self.parse_block()?;


        {
            let continue_stmt = Vec::from([self.new_statement(start, Statement::Continue)]);

            
            let else_block = Some(self.new_expression(
                start,
                Expression::Block {
                    block: Block::new(continue_stmt),
                }
            ));

        
            let break_if_condition_is_false = self.new_expression(
                start, 
                Expression::IfExpression {
                    condition,
                    block,
                    else_block
                }
            );


            Ok(self.new_expression(
                start,
                Expression::Loop {
                    block: Block::new(Vec::from([break_if_condition_is_false]))
                }
            ))
        }

    }


    fn break_statement(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Break))?;
        self.advance();

        let expr = self.expression()?;

        Ok(self.new_statement(start, Statement::Break(expr)))
    }


    fn return_statement(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Return))?;
        self.advance();

        let expr = self.expression()?;

        Ok(self.new_statement(start, Statement::Return(expr)))
    }


    fn continue_statement(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Continue))?;

        Ok(self.new_statement(start, Statement::Continue))
    }

    
    fn variable_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Let))?;
        self.advance();

        let mutability = if self.expect(TokenKind::Keyword(Keyword::Mut)).is_ok() {
            self.advance();
            true
        } else { false };

        let name = self.expect_identifier()?;
        self.advance();

        let type_hint = if self.expect(TokenKind::Colon).is_ok() {
            self.advance();
            let type_hint = self.parse_type()?;
            self.advance();
            
            Some(type_hint)
            
        } else { None };
        
        self.expect(TokenKind::Equals)?;
        self.advance();

        let value = self.expression()?;

        Ok(self.new_statement(start, Statement::VarDeclaration {
            name,
            type_hint,
            value,
            mutability
        }))
    }


    fn function_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Fn))?;
        self.advance();

        let ident = self.expect_identifier()?;
        self.advance();

        self.expect(TokenKind::LeftParenthesis)?;
        self.advance();

        let mut arguments: Vec<(SymbolIndex, DataTypeNode)> = vec![];
        loop {
            if self.expect(TokenKind::RightParenthesis).is_ok() {
                break;
            }


            if !arguments.is_empty() {
                self.expect(TokenKind::Comma)?;
                self.advance();
            }

            
            if self.expect(TokenKind::RightParenthesis).is_ok() {
                break;
            }

            
            let ident = self.expect_identifier()?;
            self.advance();

            self.expect(TokenKind::Colon)?;
            self.advance();

            let data_type = self.parse_type()?;
            self.advance();

            arguments.push((ident, data_type));
        }

        self.expect(TokenKind::RightParenthesis)?;
        self.advance();

        let return_type = if self.current_kind() == TokenKind::Colon {
            self.advance();
            let t = self.parse_type()?;
            self.advance();

            t
        } else { DataTypeNode { source_range: self.current_range(), data_type: DataType::Empty, condition: None }};


        let block = self.parse_block()?;


        Ok(self.new_declaration(start, Declaration::FunctionDeclaration { 
            ident,
            arguments,
            return_type,
            block }
        ))
    }


    fn structure_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Struct))?;
        self.advance();

        let ident = self.expect_identifier()?;
        self.advance();

        self.expect(TokenKind::LeftBracket)?;
        self.advance();

        let mut fields = vec![];

        loop {
            if self.expect(TokenKind::RightBracket).is_ok() {
                break
            }


            if !fields.is_empty() {
                self.expect(TokenKind::Comma)?;
                self.advance();
            }


            if self.expect(TokenKind::RightBracket).is_ok() {
                break
            }


            let ident = self.expect_identifier()?;
            self.advance();

            self.expect(TokenKind::Colon)?;
            self.advance();

            let field_type = self.parse_type()?;
            self.advance();

            
            fields.push((ident, field_type));
        }

        self.expect(TokenKind::RightBracket)?;

        Ok(self.new_declaration(start, Declaration::StructureDeclaration { ident, fields }))
    }


    fn namespace_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Namespace))?;
        self.advance();

        let ident = self.expect_identifier()?;
        self.advance();

        let block = self.parse_block()?;

        Ok(self.new_declaration(start, Declaration::NamespaceDeclaration { ident, block }))
    }


    fn extern_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Extern))?;
        self.advance();

        let extern_type = self.expect_str_literal()?;
        self.advance();

        self.expect(TokenKind::Keyword(Keyword::Fn))?;
        self.advance();

        let custom_name = self.expect_str_literal().ok();
        if custom_name.is_some() {
            self.advance();
        }
        
        let ident = self.expect_identifier()?;
        
        let custom_name = if let Some(v) = custom_name { v }
                          else { ident };
        self.advance();

        
        self.expect(TokenKind::LeftParenthesis)?;
        self.advance();

        let mut arguments: Vec<(SymbolIndex, DataTypeNode)> = vec![];
        loop {
            if self.expect(TokenKind::RightParenthesis).is_ok() {
                break;
            }


            if !arguments.is_empty() {
                self.expect(TokenKind::Comma)?;
                self.advance();
            }

            
            if self.expect(TokenKind::RightParenthesis).is_ok() {
                break;
            }

            
            let ident = self.expect_identifier()?;
            self.advance();

            self.expect(TokenKind::Colon)?;
            self.advance();

            let data_type = self.parse_type()?;
            self.advance();

            arguments.push((ident, data_type));
        }

        self.expect(TokenKind::RightParenthesis)?;


        let return_type = if self.peek_kind() == Some(TokenKind::Colon) {
            self.advance();
            self.advance();
            let t = self.parse_type()?;
            self.advance();

            t
        } else { DataTypeNode { source_range: self.current_range(), data_type: DataType::Empty, condition: None }};


        Ok(self.new_declaration(start, Declaration::ExternFunctionDeclaration { ident, extern_type, arguments, return_type, custom_name }))
    }


    fn const_item_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Const))?;
        self.advance();

        let ident = self.expect_identifier()?;
        self.advance();

        let type_hint = if self.expect(TokenKind::Colon).is_ok() {
            self.advance();
            let type_hint = self.parse_type()?;
            self.advance();

            Some(type_hint)
        } else { None };

        self.expect(TokenKind::Equals)?;
        self.advance();

        let value = self.expression()?;


        Ok(self.new_declaration(start, Declaration::ConstItemDeclaration { ident, type_hint, value }))
    }


    fn static_item_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Static))?;
        self.advance();

        let mutability = if self.expect(TokenKind::Keyword(Keyword::Mut)).is_ok() {
            self.advance();
            true
        } else { false };


        let ident = self.expect_identifier()?;
        self.advance();

        let type_hint = if self.expect(TokenKind::Colon).is_ok() {
            self.advance();
            let type_hint = self.parse_type()?;
            self.advance();

            Some(type_hint)
        } else { None };

        self.expect(TokenKind::Equals)?;
        self.advance();

        let value = self.expression()?;


        Ok(self.new_declaration(start, Declaration::StaticItemDeclaration { ident, type_hint, value, mutability }))
    }


    fn using_declaration(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Using))?;
        self.advance();

        let path = self.parse_path()?;

        Ok(self.new_declaration(start, Declaration::UsingDeclaration { path }))
    }


}


impl Parser<'_> {
    fn expression(&mut self) -> Result<NodeIndex, Error> {
        macro_rules! def_binary_assign {
            ($start: expr, $left: expr, $token: expr, $operator: expr) => {
                if self.current_kind() == $token {
                    let token_pos = self.current_range();
                    self.advance();

                    let right = self.expression()?;

                    return Ok(self.new_expression($start, Expression::BinaryOperation { operator: BinaryOperator::from_token(&Token { source_range: token_pos, token_kind: $operator }), left: $left, right }))
                }
            };
        }

        
        let start = self.current_range();
        let left_val = self.logical_or()?;

        const ASSIGN_TOKENS : [TokenKind; 10] = [
            TokenKind::AddEquals,
            TokenKind::SubEquals,
            TokenKind::MulEquals,
            TokenKind::DivEquals,
            TokenKind::ModEquals,
            TokenKind::BitOrAssign,
            TokenKind::BitAndAssign,
            TokenKind::BitXORAssign,
            TokenKind::LeftShiftAssign,
            TokenKind::RightShiftAssign,
        ];

        if self.peek_kind().map(|x| ASSIGN_TOKENS.contains(&x)).unwrap_or(false) {
            self.advance();
            def_binary_assign!(start, left_val, TokenKind::AddEquals, TokenKind::Plus);
            def_binary_assign!(start, left_val, TokenKind::SubEquals, TokenKind::Minus);
            def_binary_assign!(start, left_val, TokenKind::MulEquals, TokenKind::Star);
            def_binary_assign!(start, left_val, TokenKind::DivEquals, TokenKind::Slash);
            def_binary_assign!(start, left_val, TokenKind::ModEquals, TokenKind::Percent);
            def_binary_assign!(start, left_val, TokenKind::BitOrAssign, TokenKind::BitwiseOR);
            def_binary_assign!(start, left_val, TokenKind::BitAndAssign, TokenKind::BitwiseAND);
            def_binary_assign!(start, left_val, TokenKind::BitXORAssign, TokenKind::BitwiseXOR);
            def_binary_assign!(start, left_val, TokenKind::LeftShiftAssign, TokenKind::LeftShift);
            def_binary_assign!(start, left_val, TokenKind::RightShiftAssign, TokenKind::RightShift);

            unreachable!()

        } else {
            Ok(left_val)
        }
    }


    fn logical_or(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        let first = self.logical_and()?;

        if self.peek_kind() != Some(TokenKind::LogicalOr) {
            return Ok(first)
        }

        self.advance();
        self.advance();

        let other = self.expression()?;


        let true_val = self.new_expression(start, Expression::Literal(Literal::Bool(true)));
        Ok(self.new_expression(start, Expression::IfExpression { condition: first, block: Block::new(vec![true_val]), else_block: Some(other) }))
    }


    fn logical_and(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        let first = self.comparisson_expression()?;

        if self.peek_kind() != Some(TokenKind::LogicalAnd) {
            return Ok(first)
        }

        self.advance();
        self.advance();

        let other = self.expression()?;


        let false_val = self.new_expression(start, Expression::Literal(Literal::Bool(false)));
        Ok(self.new_expression(start, Expression::IfExpression { condition: first, block: Block::new(vec![other]), else_block: Some(false_val) }))
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
            Parser::function_call, 
            Parser::function_call,
            &[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]
        )
    }


    fn function_call(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        let mut atom = self.atom()?;
        

        while self.peek_kind() == Some(TokenKind::LeftParenthesis) {
            self.advance();
            self.advance();

            let mut arguments: Vec<NodeIndex> = vec![];
            loop {
                if self.expect(TokenKind::RightParenthesis).is_ok() {
                    break;
                }


                if !arguments.is_empty() {
                    self.expect(TokenKind::Comma)?;
                    self.advance();
                }

        
                if self.expect(TokenKind::RightParenthesis).is_ok() {
                    break;
                }

        
                let expr = self.expression()?;
                self.advance();

                arguments.push(expr);
            }

            self.expect(TokenKind::RightParenthesis)?;

            atom = self.new_expression(start, Expression::FunctionCall { path: atom, arguments })
            
        }

        Ok(atom)
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
            TokenKind::Keyword(Keyword::Loop) => self.loop_expression(),
            TokenKind::Keyword(Keyword::Unsafe) => self.unsafe_expression(),


            TokenKind::Identifier(_) if self.peek_kind() == Some(TokenKind::LeftBracket) => self.structure_creation(),
            TokenKind::Identifier(_) => self.identifier_access(),


            TokenKind::Semicolon => {
                Ok(self.new_expression(self.current_range(), Expression::Literal(Literal::Empty)))
            },

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
            if self.peek_kind() == Some(TokenKind::Keyword(Keyword::If)) {
                self.advance();
                Some(self.if_expression()?)
            } else {
                self.advance();
                let block = self.parse_block()?;
                Some(self.new_expression(block.source_range(&self.node_map), Expression::Block { block }))
            }

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


    fn loop_expression(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Loop))?;
        self.advance();

        let block = self.parse_block()?;

        Ok(self.new_expression(
            start,
            Expression::Loop { block }
        ))
    }


    fn unsafe_expression(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        self.expect(TokenKind::Keyword(Keyword::Unsafe))?;
        self.advance();

        let block = self.parse_block()?;

        Ok(self.new_expression(
            start, 
            Expression::Unsafe { block }
        ))
    }


    fn identifier_access(&mut self) -> Result<NodeIndex, Error> {
        let ident = self.parse_path()?;

        Ok(self.new_expression(self.current_range(), Expression::Identifier { ident }))
    }


    fn structure_creation(&mut self) -> Result<NodeIndex, Error> {
        let start = self.current_range();
        let path = self.parse_path()?;
        self.advance();

        self.expect(TokenKind::LeftBracket)?;
        self.advance();

        
        let mut fields: Vec<(SymbolIndex, NodeIndex)> = vec![];
        loop {
            if self.expect(TokenKind::RightBracket).is_ok() {
                break;
            }


            if !fields.is_empty() {
                self.expect(TokenKind::Comma)?;
                self.advance();
            }

    
            if self.expect(TokenKind::RightBracket).is_ok() {
                break;
            }

    
            let name = self.expect_identifier()?;
            self.advance();

            self.expect(TokenKind::Colon)?;
            self.advance();
            
            let expr = self.expression()?;
            self.advance();

            fields.push((name, expr));
        }

        self.expect(TokenKind::RightBracket)?;

        Ok(self.new_expression(start, Expression::StructureCreation { path, fields }))
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
