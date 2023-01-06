use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, PartialEq, Clone)]
enum Tok {
    String(String),
    Id(String),
    I64(i64),
    F64(f64),

    StringType,
    I64Type,
    F64Type,

    Return,
    Fn,
    If,

    Semicolon,
    LParen,
    RParen,
    LCurly,
    RCurly,
    Arrow,
    Colon,

    Minus,
    Plus,
    EqEq,
    OrOr,
    Eq,
    Or,
}

#[derive(Debug)]
struct Token {
    tok: Tok,
    underlying: std::ops::Range<usize>,
}

impl Token {
    fn new(tok: Tok, underlying: std::ops::Range<usize>) -> Self {
        Self { tok, underlying }
    }

    fn copy_underlying(&self, code: &str) -> String {
        match &self.tok {
            Tok::String(s) => {
                format!("\"{}\"", s)
            }
            _ => code[self.underlying.clone()].to_string(),
        }
    }

    fn keyword_to_tok(keyword: &str) -> Tok {
        match keyword {
            "fn" => Tok::Fn,
            "if" => Tok::If,
            "i64" => Tok::I64Type,
            "f64" => Tok::F64Type,
            "String" => Tok::StringType,
            "return" => Tok::Return,
            _ => unimplemented!(),
        }
    }
}

struct Lexer {
    code: String,
    tokens: Vec<Token>,

    keywords: HashSet<String>,
}

impl Lexer {
    fn new(code: String) -> Self {
        let mut keywords = HashSet::new();
        keywords.insert("fn".to_string());
        keywords.insert("if".to_string());
        keywords.insert("i64".to_string());
        keywords.insert("f64".to_string());
        keywords.insert("String".to_string());
        keywords.insert("return".to_string());

        let mut lexer = Self {
            code,
            tokens: vec![],
            keywords,
        };

        lexer.tokenize();

        lexer
    }

    fn matches(a: Option<&char>, b: char) -> bool {
        a.is_some() && a.unwrap() == &b
    }

    fn tokenize(&mut self) {
        let mut chars = self.code.chars().peekable();
        let mut idx = 0;

        while let Some(curr) = chars.next() {
            match curr {
                '\n' | '\r' | '\t' | ' ' => {}
                '0'..='9' => {
                    let start = idx;
                    let mut decimals = 0;

                    while let Some(curr) = chars.peek() {
                        match curr {
                            '0'..='9' | '.' => {
                                if curr == &'.' {
                                    decimals += 1;
                                }
                                idx += 1;
                                chars.next().unwrap();
                            }
                            _ => {
                                break;
                            }
                        }
                    }

                    if decimals == 0 {
                        let int = self.code[start..idx + 1]
                            .to_string()
                            .parse::<i64>()
                            .unwrap();
                        self.tokens.push(Token::new(Tok::I64(int), start..idx + 1));
                    } else if decimals == 1 {
                        let float = self.code[start..idx + 1]
                            .to_string()
                            .parse::<f64>()
                            .unwrap();
                        self.tokens
                            .push(Token::new(Tok::F64(float), start..idx + 1));
                    } else {
                        panic!("Error, too many decimals in number");
                    }
                }
                'A'..='Z' | 'a'..='z' => {
                    let start = idx;

                    while let Some(curr) = chars.peek() {
                        match curr {
                            'A'..='Z' | 'a'..='z' | '0'..='9' => {
                                idx += 1;
                                chars.next().unwrap();
                            }
                            _ => {
                                break;
                            }
                        }
                    }

                    let string = &self.code[start..idx + 1];

                    let tok = if self.keywords.contains(string) {
                        Token::keyword_to_tok(string)
                    } else {
                        Tok::Id(string.to_string())
                    };

                    self.tokens.push(Token::new(tok, start..idx + 1));
                }
                '"' => {
                    idx += 1;
                    let start = idx;

                    while let Some(curr) = chars.peek() {
                        match curr {
                            '"' => {
                                self.tokens.push(Token::new(
                                    Tok::String((self.code[start..idx]).to_string()),
                                    start..idx,
                                ));
                                chars.next().unwrap(); // second quote
                                break;
                            }
                            _ => {
                                idx += 1;
                                chars.next().unwrap();
                            }
                        }
                    }
                }
                '-' => {
                    if Self::matches(chars.peek(), '>') {
                        self.tokens.push(Token::new(Tok::Arrow, idx..idx + 2));
                        chars.next().unwrap();
                        idx += 1;
                    } else {
                        self.tokens.push(Token::new(Tok::Minus, idx..idx + 1));
                    }
                }
                '=' => {
                    if Self::matches(chars.peek(), '=') {
                        self.tokens.push(Token::new(Tok::EqEq, idx..idx + 2));
                        chars.next().unwrap();
                        idx += 1;
                    } else {
                        self.tokens.push(Token::new(Tok::Eq, idx..idx + 1));
                    }
                }
                '|' => {
                    if Self::matches(chars.peek(), '|') {
                        self.tokens.push(Token::new(Tok::OrOr, idx..idx + 2));
                        chars.next().unwrap();
                        idx += 1;
                    } else {
                        self.tokens.push(Token::new(Tok::Or, idx..idx + 1));
                    }
                }
                ';' => self.tokens.push(Token::new(Tok::Semicolon, idx..idx + 1)),
                '(' => self.tokens.push(Token::new(Tok::LParen, idx..idx + 1)),
                ')' => self.tokens.push(Token::new(Tok::RParen, idx..idx + 1)),
                '{' => self.tokens.push(Token::new(Tok::LCurly, idx..idx + 1)),
                '}' => self.tokens.push(Token::new(Tok::RCurly, idx..idx + 1)),
                ':' => self.tokens.push(Token::new(Tok::Colon, idx..idx + 1)),
                '+' => self.tokens.push(Token::new(Tok::Plus, idx..idx + 1)),
                _ => {
                    println!("Unimplemented token: '{}'", curr);
                    unimplemented!();
                }
            }

            idx += 1;
        }
    }
}

struct TokenIterator {
    tokens: Vec<Token>,
    idx: usize,
}

impl TokenIterator {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    fn curr(&self) -> Option<&Token> {
        if self.idx >= self.tokens.len() {
            None
        } else {
            Some(&self.tokens[self.idx])
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        // println!("adv {:?}", self.curr());
        if self.idx >= self.tokens.len() {
            None
        } else {
            self.idx += 1;
            Some(&self.tokens[self.idx - 1])
        }
    }

    fn eat(&mut self, tok: Tok) -> Option<&Token> {
        // println!("eat {:?}", self.curr());
        if self.idx >= self.tokens.len() {
            None
        } else {
            self.idx += 1;
            if self.tokens[self.idx - 1].tok != tok {
                panic!(
                    "{:?} != {:?}, ate unexpected value!",
                    self.tokens[self.idx - 1].tok,
                    tok
                );
            }
            Some(&self.tokens[self.idx - 1])
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.idx + 1 >= self.tokens.len() {
            None
        } else {
            Some(&self.tokens[self.idx + 1])
        }
    }
}

#[derive(Debug)]
enum Type {
    I64,
    F64,
    String,
}

impl Type {
    fn tok_to_type(tok: &Tok) -> Type {
        match tok {
            Tok::StringType => Type::String,
            Tok::I64Type => Type::I64,
            Tok::F64Type => Type::F64,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
struct FunctionParameter {
    param_type: Type,
    param_name: String,
}

#[derive(Debug)]
enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    EqEq,
    OrOr,
}

impl Operation {
    fn from(tok: &Tok) -> Operation {
        match tok {
            Tok::Plus => Operation::Add,
            Tok::Minus => Operation::Sub,
            Tok::EqEq => Operation::EqEq,
            Tok::OrOr => Operation::OrOr,
            _ => {
                unimplemented!()
            }
        }
    }
}

#[derive(Debug)]
struct FunctionCall {
    func_args: Vec<Expr>,
    func_name: String,
}

#[derive(Debug)]
enum ExprValue {
    FunctionCall(FunctionCall),
    Operation(Operation),
    Var(String),
    I64(i64),
}

#[derive(Debug)]
struct Expr {
    value: ExprValue,
    left: Option<Box<Expr>>,
    right: Option<Box<Expr>>,
}

impl Expr {
    fn new_leaf(value: ExprValue) -> Self {
        Self {
            value,
            left: None,
            right: None,
        }
    }

    fn new_parent(op: Operation, expr1: Expr, expr2: Expr) -> Self {
        Self {
            value: ExprValue::Operation(op),
            left: Some(Box::new(expr1)),
            right: Some(Box::new(expr2)),
        }
    }

    fn infix_binding_power(op: &Tok) -> Option<(usize, usize)> {
        let res = match op {
            Tok::Plus | Tok::Minus => (10, 20),
            Tok::EqEq => (5, 6),
            Tok::OrOr => (3, 4),
            _ => return None,
        };

        Some(res)
    }
}

#[derive(Debug)]
enum Statement {
    Return(Expr),
    If {
        condition: Expr,
        body: Vec<Statement>,
    },
    Print(Expr),
}

#[derive(Debug)]
struct FunctionDefinition {
    name: String,
    parameters: Vec<FunctionParameter>,
    return_type: Option<Type>,
    body: Vec<Statement>,
}

struct Parser {
    code: String,
    tokens: TokenIterator,
    functions: Vec<FunctionDefinition>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            code: lexer.code,
            tokens: TokenIterator::new(lexer.tokens),
            functions: vec![],
        };

        parser.parse();
        parser
    }

    fn parse(&mut self) {
        let mut functions = vec![];
        while self.tokens.peek().is_some() {
            functions.push(self.parse_function());
        }

        self.functions = functions;
    }

    fn parse_function(&mut self) -> FunctionDefinition {
        self.tokens.eat(Tok::Fn);
        let name = self.tokens.advance().unwrap().copy_underlying(&self.code);

        self.tokens.eat(Tok::LParen);

        // Parameters
        let mut parameters = vec![];
        while let Some(curr) = self.tokens.curr() {
            match curr.tok {
                Tok::RParen => break,
                _ => parameters.push(self.parse_function_parameter()),
            }
        }

        self.tokens.eat(Tok::RParen);

        // Return type
        let return_type = if let Some(curr) = self.tokens.curr() {
            match curr.tok {
                Tok::Arrow => {
                    self.tokens.eat(Tok::Arrow);
                    Some(Type::tok_to_type(&self.tokens.advance().unwrap().tok))
                }
                _ => None,
            }
        } else {
            None
        };

        self.tokens.eat(Tok::LCurly);

        // Body
        let mut body = vec![];
        while let Some(curr) = self.tokens.curr() {
            match curr.tok {
                Tok::RCurly => break,
                _ => body.push(self.parse_statement()),
            }
        }

        self.tokens.eat(Tok::RCurly);

        FunctionDefinition {
            name,
            parameters,
            return_type,
            body,
        }
    }

    fn parse_function_parameter(&mut self) -> FunctionParameter {
        let param_name = self.tokens.advance().unwrap().copy_underlying(&self.code);
        self.tokens.eat(Tok::Colon);
        let param_type = Type::tok_to_type(&self.tokens.advance().unwrap().tok);

        FunctionParameter {
            param_type,
            param_name,
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.tokens.advance().unwrap().tok {
            Tok::Id(i) => {
                if i == "print" {
                    let ret = Statement::Print(self.parse_expression());
                    self.tokens.eat(Tok::Semicolon);
                    ret
                } else {
                    unimplemented!();
                }
            }
            Tok::If => {
                let condition = self.parse_expression();

                self.tokens.eat(Tok::LCurly);

                let mut body = vec![];
                while let Some(curr) = self.tokens.curr() {
                    match curr.tok {
                        Tok::RCurly => break,
                        _ => body.push(self.parse_statement()),
                    }
                }

                self.tokens.eat(Tok::RCurly);

                Statement::If { condition, body }
            }
            Tok::Return => {
                let ret = Statement::Return(self.parse_expression());
                self.tokens.eat(Tok::Semicolon);
                ret
            }
            _ => unimplemented!(),
        }
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_expression_bp(0)
    }

    fn parse_expression_bp(&mut self, min_bp: usize) -> Expr {
        let lhs_token = self.tokens.curr().unwrap();
        let mut lhs = match lhs_token.tok {
            Tok::I64(i) => {
                self.tokens.advance();
                Expr::new_leaf(ExprValue::I64(i))
            }
            Tok::LParen => {
                self.tokens.advance();
                let lhs = self.parse_expression_bp(0);
                self.tokens.eat(Tok::RParen);
                lhs
            }
            Tok::Id(_) => {
                if self.tokens.peek().is_some() && self.tokens.peek().unwrap().tok == Tok::LParen {
                    Expr::new_leaf(ExprValue::FunctionCall(self.parse_function_call()))
                } else {
                    Expr::new_leaf(ExprValue::Var(
                        self.tokens.advance().unwrap().copy_underlying(&self.code),
                    ))
                }
            }
            _ => {
                unimplemented!();
            }
        };

        loop {
            let op_token = self.tokens.curr().unwrap();
            let op = match op_token.tok {
                Tok::Plus | Tok::Minus | Tok::OrOr | Tok::EqEq => op_token.tok.clone(),
                _ => break,
            };

            if let Some((lbp, rbp)) = Expr::infix_binding_power(&op) {
                if lbp < min_bp {
                    break;
                }

                self.tokens.advance();
                let rhs = self.parse_expression_bp(rbp);

                lhs = Expr::new_parent(Operation::from(&op), lhs, rhs);
                continue;
            }
        }

        lhs
    }

    fn parse_function_call(&mut self) -> FunctionCall {
        let func_name = self.tokens.advance().unwrap().copy_underlying(&self.code);
        self.tokens.eat(Tok::LParen);

        let func_args = vec![self.parse_expression()];

        self.tokens.eat(Tok::RParen);

        FunctionCall {
            func_args,
            func_name,
        }
    }
}

struct Output {
    output: String,
}

impl Output {
    fn line(&mut self, line: &str) {
        self.output.push_str(&format!("{}\n", line));
    }

    fn indented_line(&mut self, line: &str) {
        self.output.push_str(&format!("\t{}\n", line));
    }
}

struct Codegen {
    functions: Vec<FunctionDefinition>,

    output: Output,
}

impl Codegen {
    fn new(parser: Parser) -> Self {
        let mut codegen = Self {
            functions: parser.functions,
            output: Output {
                output: String::new(),
            },
        };
        codegen.gen();
        codegen
    }

    fn gen(&mut self) {
        self.output.line("format: .asciz \"%i\\n\"");
        self.output.indented_line(".text");
        self.output.indented_line(".globl main");

        for function in &self.functions {
            Self::gen_function(function, &mut self.output);
        }
    }

    fn gen_function(function: &FunctionDefinition, output: &mut Output) {
        output.line(&format!("{}:", function.name));

        if (function.name == "main") {
            output.indented_line("push %rbx");
        }

        for statement in &function.body {
            Self::gen_statement(statement, output);
        }

        if (function.name == "main") {
            output.indented_line("pop %rbx");
            output.indented_line("xor %rax, %rax");
        }

        output.indented_line("ret");
    }

    fn gen_statement(statement: &Statement, output: &mut Output) {
        match statement {
            Statement::Print(expr) => {
                Self::gen_expression(expr, output);
                output.indented_line("pop %rax");
                output.indented_line("lea format(%rip), %rdi");
                output.indented_line("mov %rax, %rsi");
                output.indented_line("call printf");
            }
	    Statement::Return(expr) => {

	    }
	    Statement::If { condition, body } => {

	    }
        }
    }

    fn gen_expression(expression: &Expr, output: &mut Output) {
        match expression.value {
            ExprValue::I64(i) => output.indented_line(&format!("push ${}", i)),
            ExprValue::Operation(ref o) => {
                Self::gen_expression(&expression.left.as_ref().unwrap(), output);
                Self::gen_expression(&expression.right.as_ref().unwrap(), output);
                output.indented_line("pop %rax");
                output.indented_line("pop %rbx");
                match o {
                    Operation::Add => {
                        output.indented_line("add %rbx, %rax");
                        output.indented_line("push %rax");
                    }
                    Operation::Sub => {
                        output.indented_line("sub %rax, %rbx");
                        output.indented_line("push %rbx");
                    }
                    _ => todo!(),
                }
            }
            ExprValue::FunctionCall(ref f) => {
		for arg in &f.func_args {
		    Self::gen_expression(arg, output);
		}
		output.indented_line(&format!("call {}", f.func_name));
	    }
            ExprValue::Var(ref v) => {}
        }
    }
}

fn main() {
    let mut f = File::open("examples/hello.l").unwrap();
    let mut code = String::new();
    f.read_to_string(&mut code).unwrap();

    let lexer = Lexer::new(code);
    let parser = Parser::new(lexer);
    // let codegen = Codegen::new(parser);

    // let mut out = File::create("out.bc").unwrap();
    // out.write_all(codegen.output.output.as_bytes()).unwrap();
}
