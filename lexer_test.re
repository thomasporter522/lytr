open Tylr_core.LytrToken;
open Tylr_core.LytrLexer;

let print_token = (t: token) => print_endline(string_of_token(t));

let test_basic_tokens = () => {
  print_endline("=== Testing Basic Tokens ===");
  let input = "( ) * - 0";
  let tokens = lex(input);
  print_endline("Input: " ++ input);
  print_endline("Tokens:");
  List.iter(print_token, tokens);
  print_newline();
};

let test_expression = () => {
  print_endline("=== Testing Expression ==="); 
  let input = "(* 0 - hello)";
  let tokens = lex(input);
  print_endline("Input: " ++ input);
  print_endline("Tokens:");
  List.iter(print_token, tokens);
  print_newline();
};

let test_numbers_and_identifiers = () => {
  print_endline("=== Testing Numbers and Identifiers ===");
  let input = "123 abc def 0 xyz";
  let tokens = lex(input);
  print_endline("Input: " ++ input);
  print_endline("Tokens:");
  List.iter(print_token, tokens);
  print_newline();
};

let () = {
  test_basic_tokens();
  test_expression();
  test_numbers_and_identifiers();
};
