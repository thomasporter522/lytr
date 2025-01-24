open Stds;
open Tylr_core;

let test_sample = () =>
  Alcotest.(check(string))("same string", "hello", "hello");

let test_lists_rotate = () => {
  let test_cases = [
    ([], []), /* empty list */
    ([1], [1]), /* single element */
    ([1, 2], [2, 1]), /* two elements */
    ([1, 2, 3], [2, 3, 1]) /* three elements */
  ];

  List.iter(
    ((input, expected)) =>
      Alcotest.(check(list(int)))("rotate", expected, Lists.rotate(input)),
    test_cases,
  );
};

// Convert a string into a cell by applying edits
let string_to_cell = (input: string) => {
  // Use Labeler to get tokens
  let tokens = Labeler.label(input);

  // Start with empty zipper
  let initial_zipper = Zipper.empty;

  // Convert tokens to edits and apply them sequentially
  let final_zipper =
    tokens
    |> List.fold_left(
         (zipper, token: Token.Unmolded.t) => {
           // For each token, create an insert edit and apply it
           let edit = Edit.Insert(token.text);
           Edit.perform(edit, zipper) |> Option.get;
         },
         initial_zipper,
       );

  // Zip up the zipper without saving cursor position
  Zipper.zip(~save_cursor=false, final_zipper);
};

let clear_ids = Zipper.map_toks((tok: Token.t) => {...tok, id: 0});

let test_cell_edits = () => {
  let test_cases = [
    // (input_string, input_path, actions, expected_string, expected_path)
    (
      "", // empty input string
      [0], // input path
      [Edit.Insert("ABC")], // actions
      "ABC", // expected output string
      [2] // expected output path
    ),
  ];

  List.iter(
    ((input, input_path, actions, expected, expected_path)) => {
      // Convert input string to cell and add cursor
      let input_cell = string_to_cell(input);
      let input_cell_with_cursor =
        Cell.put_cursor(Point(Caret.focus(input_path)), input_cell);
      let input_zipper = Zipper.unzip_exn(input_cell_with_cursor);

      // Apply actions to get final zipper
      let final_zipper =
        List.fold_left(
          (zipper, action) => Edit.perform(action, zipper) |> Option.get,
          input_zipper,
          actions,
        );

      // Generate expected zipper
      let expected_cell = string_to_cell(expected);
      let expected_cell_with_cursor =
        Cell.put_cursor(Point(Caret.focus(expected_path)), expected_cell);
      let expected_zipper = Zipper.unzip_exn(expected_cell_with_cursor);

      // Compare the zippers
      Alcotest.(check(bool))(
        "zipper equality",
        true,
        clear_ids(final_zipper) == clear_ids(expected_zipper),
      );
    },
    test_cases,
  );
};

let () = {
  Alcotest.run(
    "Tylr Tests",
    [
      ("sample", [Alcotest.test_case("Sample test", `Quick, test_sample)]),
      ("Lists", [Alcotest.test_case("rotate", `Quick, test_lists_rotate)]),
      (
        "Cell Edits",
        [Alcotest.test_case("basic edits", `Quick, test_cell_edits)],
      ),
    ],
  );
};
