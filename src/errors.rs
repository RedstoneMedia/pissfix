use crate::Span;
use colored::Colorize;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    LexingError,
    ParsingError,
    TypeCheckError,
    RuntimeError
}


#[derive(Clone, Debug)]
pub struct Error {
    pub text : String,
    pub start_char : usize,
    pub end_char : usize,
    pub kind : ErrorKind
}

impl Error {

    pub fn from_span(span: Span, text: String, kind: ErrorKind) -> Self {
        Self {
            text,
            start_char: span.start_char,
            end_char: span.end_char,
            kind,
        }
    }

    pub fn get_error_text(&self, original_code : &str) -> String {
        if self.end_char > original_code.len()-1 {
            return String::from(format!("[{:?}] Error: {}", self.kind, self.text));
        }
        let start_line = original_code[..self.start_char].split("\n").collect::<Vec<&str>>().len();
        let end_line = original_code[self.start_char..self.start_char+1].split("\n").collect::<Vec<&str>>().len() + start_line;
        let line_start_char_index = &original_code.split("\n").collect::<Vec<&str>>()[..start_line-1].join("").len();

        let relevant_lines : &[&str] = &original_code.split("\n").collect::<Vec<&str>>()[start_line-1..end_line-1];

        let start_char_index_in_line = self.start_char - line_start_char_index - (start_line-1);
        let mut pointer_string = (0..start_char_index_in_line).map(|_| " ").collect::<String>();
        pointer_string += &(self.start_char - line_start_char_index..self.end_char - line_start_char_index + 1).map(|_| "^").collect::<String>();

        let error_message = format!(
            "[{:?}] {} {}",
            self.kind,
            format!("{}:{}", start_line, start_char_index_in_line).blue(),
            self.text.white().bold()
        );

        String::from(format!(
            "{}\n{}\n{}",
            error_message.bright_red().bold(),
            relevant_lines.join("\n"),
            pointer_string.red()
        ))
    }

}

#[derive(Debug, Clone)]
pub struct ErrorTracker {
    errors : Vec::<Error>
}

impl ErrorTracker {

    pub fn new() -> Self {
        Self {errors: vec![]}
    }

    pub fn add_error(&mut self, error : Error) {
        self.errors.push(error);
    }

    pub fn get_errors_text(&self, original_code : &str) -> String {
        self.errors.iter().map(|e| e.get_error_text(original_code))
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

}