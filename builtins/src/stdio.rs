use object::object::Object;
use std::io::Write;
use std::rc::Rc;

pub fn builtin_func_print(params: Vec<Rc<Object>>) -> Rc<Object> {
    params.iter().for_each(|obj| {
        print!("{}", obj);
        std::io::stdout().flush().unwrap();
    });

    Rc::from(Object::Null)
}

pub fn builtin_func_println(params: Vec<Rc<Object>>) -> Rc<Object> {
    params.iter().for_each(|obj| {
        println!("{}", obj);
    });

    Rc::from(Object::Null)
}

pub fn builtin_func_input(_params: Vec<Rc<Object>>) -> Rc<Object> {
    let mut s = String::new();

    match std::io::stdin().read_line(&mut s) {
        Ok(_) => Rc::from(Object::String(s.trim_end().to_string())),
        Err(e) => Rc::from(Object::Error(e.to_string())),
    }
}

pub fn builtin_func_clear_screen(_params: Vec<Rc<Object>>) -> Rc<Object> {
    print!("\x1B[2J\x1B[1;1H");

    Rc::from(Object::Null)
}
