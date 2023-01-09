pub struct Label {
    name: String,
    body: Vec<Instruction>,
}

enum Value {
    Int(i64),
    Argument(String),
}

enum Instruction {
    Push(Value),
    Pop,

    Add,
    Sub,
    Mul,
    Div,

    Cmpj(Label),
    Cmpgj(Label),

    Ret,
    Call(Label), // Account for _print
}

struct Function {
    name: Label,
    arguments: Vec<String>,
    body: Vec<Instruction>,
}

pub fn run_fib() {
    let main = Function {
	name: "_main".to_string(),
	arguments: vec![],
	body: vec![
	    Instruction::Push(Value::Int(5)),
	    Instruction::Call("@fib".to_string()),
	    Instruction::Call("_print".to_string()),
	],
    };

    let fib = Function {
	name: "@fib".to_string(),
	arguments: vec!["n".to_string()],
	body: vec![
	    Instruction::Push(Value::Argument("n".to_string())),
	    Instruction::Push(Value::Int(0)),
	    Instruction::Cmpj("return".to_string()),

	    Instruction::Pop,
	    Instruction::Push(Value::Int(1)),
	    Instruction::Cmpj("return".to_string()),
	    Instruction::Pop,

	    Instruction::Call("@fib".to_string()),
	    Instruction::Call("@fib".to_string()),
	    Instruction::Add,
	    Instruction::Ret,
	],
    };
}
