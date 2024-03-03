(** A program to solve sudoku **)
(**
let puzzle = [[1; 1; 1]; [1; 1; 1]; [1; 1; 1]] ;;

let rec traverseList (f: 'a -> 'b -> 'c) (l: 'a list) : 'a = match l with
        | [] -> 0
        | h :: t -> f h (traverseList f t) ;;

let rec traverseListList f l =
        match l with
        | [] -> 0
        | h :: t -> f (traverseList f h) (traverseListList f t) ;;

let sumList l = traverseListList (+) l ;;

let rec viableNums (l : int list list) : int list =
        let possibleNums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
        traverseListList (fun n k ->  ;;

print_int (sumList puzzle) ;;
print_endline "" ;;
**)

(* Helper function to check if a value is valid in a given position *)
let is_valid grid row col num =
  let rec check_row row num =
    match row with
    | [] -> true
    | hd::tl -> if hd = num then false else check_row tl num
  in
  let rec check_col grid col num =
    match grid with
    | [] -> true
    | hd::tl -> if List.nth hd col = num then false else check_col tl col num
  in
  let rec check_box grid row col num =
    let row_start = (row / 3) * 3 in
    let col_start = (col / 3) * 3 in
    let rec check_box_row row col num =
      if row >= row_start + 3 then true
      else if col >= col_start + 3 then check_box_row (row + 1) (col_start) num
      else if List.nth (List.nth grid row) col = num then false
      else check_box_row row (col + 1) num
    in
    check_box_row row col num
  in
  check_row (List.nth grid row) num && check_col grid col num && check_box grid row col num

(* Helper function to find an empty position in the puzzle *)
let find_empty_position grid =
  let rec find_row grid row col =
    if row = 9 then (-1, -1)
    else if col = 9 then find_row grid (row + 1) 0
    else if List.nth (List.nth grid row) col = 0 then (row, col)
    else find_row grid row (col + 1)
  in
  find_row grid 0 0

(* Main function to solve Sudoku using backtracking *)
let rec solve_sudoku grid =
  match find_empty_position grid with
  | (-1, -1) -> grid (* Puzzle is solved *)
  | (row, col) ->
    let rec try_values grid row col num =
      if num > 9 then None
      else if is_valid grid row col num then
        match solve_sudoku (List.mapi (fun r x -> if r = row then List.mapi (fun c y -> if c = col then num else y) x else x) grid) with
        | solution -> Some solution
        | exception _ -> try_values grid row col (num + 1)
      else try_values grid row col (num + 1)
    in
    (match try_values grid row col 1 with
    | None -> failwith "No solution found"
    | Some solution -> solution)

(* Example Sudoku puzzle *)
let sudoku_puzzle = [
  [0;0;0;0;0;6;0;0;9];
  [0;0;2;0;0;0;6;0;0];
  [3;0;0;4;2;0;0;0;0];
  [1;0;0;0;5;8;0;0;7];
  [7;6;0;0;0;0;0;5;2];
  [5;0;0;2;9;0;0;0;6];
  [0;0;0;0;7;4;0;0;3];
  [0;0;1;0;0;0;9;0;0];
  [4;0;0;5;0;0;0;0;0]
]

(* Solve the puzzle *)
let solution = solve_sudoku sudoku_puzzle

(* Print the solution *)
let () =
  List.iter (fun row -> 
    List.iter (fun cell -> print_int cell; print_string " ") row; 
    print_newline ()
  ) solution

