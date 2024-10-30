use std::io::Write;

use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;

use crate::{
    eval::{Env, Evaluator},
    parser::Parser,
};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

struct ConsoleStdErr {
    console: HtmlElement,
}

impl Write for ConsoleStdErr {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(new_value) = String::from_utf8(buf.to_vec()) {
            let trimmed_value = new_value.trim();
            let prev_values = self.console.inner_html();

            let new_html = if prev_values.is_empty() {
                format!("<span>{trimmed_value}</span>")
            } else {
                format!("{prev_values}<span>{trimmed_value}</span>")
            };

            self.console.set_inner_html(&new_html);
            return Ok(buf.len());
        }

        return Ok(0);
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

struct ConsoleStdOut {
    console: HtmlElement,
    buffer: String,
}

impl ConsoleStdOut {
    pub fn new(console: HtmlElement) -> Self {
        return ConsoleStdOut {
            console,
            buffer: String::new(),
        };
    }

    fn flush_to_dom(&mut self) {
        if self.buffer.is_empty() {
            return;
        }

        let prev_values = self.console.inner_html();
        let new_html = if prev_values.is_empty() {
            format!("<span>{}</span>", self.buffer)
        } else {
            format!("{prev_values}<span>{}</span>", self.buffer)
        };

        self.console.set_inner_html(&new_html);
        self.buffer.clear();
    }
}

impl Write for ConsoleStdOut {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(new_value) = String::from_utf8(buf.to_vec()) {
            let trimmed_value = new_value.trim();
            if !trimmed_value.is_empty() {
                self.buffer.push_str(trimmed_value);
            }

            return Ok(buf.len());
        }

        return Ok(0);
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.flush_to_dom();
        Ok(())
    }
}

#[wasm_bindgen]
pub fn eval_code(code: String, console: HtmlElement) -> String {
    let stdout = ConsoleStdOut::new(console.clone());
    let stderr = ConsoleStdErr { console };

    let ast = Parser::new(code).parse();
    let mut evaluator = Evaluator::new(Env::new(), stdout, stderr);
    let output = evaluator.eval(ast);

    return format!("{output}");
}
