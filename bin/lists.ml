let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs
;;

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs
;;

let rec at i lst =
  match i, lst with
  | n, _ when n <= 0 -> None
  | 1, [] -> None
  | 1, x :: _ -> Some x
  | _, [] -> None
  | n, _ :: xs -> at (n - 1) xs
;;

let length lst =
  let rec length' lst acc =
    match lst with
    | [] -> acc
    | _ :: xs -> length' xs (acc + 1)
  in
  length' lst 0
;;

let rev lst =
  let rec rev' lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> rev' xs (x :: acc)
  in
  rev' lst []
;;

let is_palindrome lst =
  let rev_lst = rev lst in
  let rec cmp l_a l_b =
    match l_a, l_b with
    | [], [] -> true
    | x :: xs, y :: ys when x = y -> cmp xs ys
    | _, _ -> false
  in
  cmp lst rev_lst
;;

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec flatten' lst acc =
    match lst with
    | [] -> acc
    | x :: xs ->
      let matched =
        match x with
        | One elt -> elt :: acc
        | Many node_lst -> flatten' node_lst acc
      in
      flatten' xs matched
  in
  rev @@ flatten' lst []
;;

let compress lst =
  let rec compress' lst acc =
    match lst, acc with
    | [], y_acc -> y_acc
    | x :: xs, (y :: _ as y_acc) when x = y -> compress' xs y_acc
    | x :: xs, y_acc -> compress' xs (x :: y_acc)
  in
  rev @@ compress' lst []
;;

let pack lst =
  let rec pack' lst acc =
    match lst, acc with
    | [], y_acc -> y_acc
    | x :: xs, (y :: _ as h) :: y_acc when x = y -> pack' xs ((x :: h) :: y_acc)
    | x :: xs, y_acc -> pack' xs ([ x ] :: y_acc)
  in
  rev @@ pack' lst []
;;

let encode lst =
  let rec encode' lst acc =
    match lst with
    | [] -> acc
    | [] :: xs -> encode' xs acc
    | (h_x :: _ as x) :: xs -> encode' xs ((length x, h_x) :: acc)
  in
  rev @@ encode' (pack lst) []
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_rle lst =
  let rec encode' lst acc =
    match lst with
    | [] -> acc
    | [] :: xs -> encode' xs acc
    | (h_x :: _ as x) :: xs ->
      let len_x = length x in
      let head = if len_x = 1 then One h_x else Many (len_x, h_x) in
      encode' xs (head :: acc)
  in
  rev @@ encode' (pack lst) []
;;

let decode_rle (lst : 'a rle list) =
  let rec decode_rle' lst acc =
    match lst with
    | [] -> acc
    | One a :: xs -> decode_rle' xs (a :: acc)
    | Many (ia, a) :: xs when ia > 0 -> decode_rle' (Many (ia - 1, a) :: xs) (a :: acc)
    | Many (_, _) :: xs -> decode_rle' xs acc
  in
  rev @@ decode_rle' lst []
;;

let encode_direct lst =
  let rec encode_direct' lst acc =
    match lst, acc with
    | [], acc -> acc
    | x :: xs, One y :: acc when x = y -> encode_direct' xs (Many (2, x) :: acc)
    | x :: xs, Many (iy, y) :: acc when x = y ->
      encode_direct' xs (Many (iy + 1, x) :: acc)
    | x :: xs, acc -> encode_direct' xs (One x :: acc)
  in
  rev @@ encode_direct' lst []
;;

let duplicate lst =
  let rec duplicate' lst acc =
    match lst with
    | [] -> acc
    | x :: xs -> duplicate' xs (x :: x :: acc)
  in
  rev @@ duplicate' lst []
;;

let replicate lst times =
  let rec replicate' lst count acc =
    match lst, count with
    | [], _ -> acc
    | x :: xs, n when n > 0 -> replicate' (x :: xs) (n - 1) (x :: acc)
    | _ :: xs, _ -> replicate' xs times acc
  in
  rev @@ replicate' lst times []
;;

let drop lst nth =
  let rec drop' lst count acc =
    match lst, count with
    | [], _ -> acc
    | x :: xs, n when n > 1 -> drop' xs (n - 1) (x :: acc)
    | _ :: xs, _ -> drop' xs nth acc
  in
  rev @@ drop' lst nth []
;;

let split lst len =
  let rec split' lst i acc =
    match lst, acc, i with
    | [], (acc_1, acc_2), i -> if i >= 0 then rev acc_1, rev acc_2 else acc_1, rev acc_2
    | x :: xs, (acc_1, _), i when i > 0 -> split' xs (i - 1) (x :: acc_1, [])
    | xs, (acc_1, _), i when i = 0 -> split' xs (i - 1) (rev acc_1, [])
    | x :: xs, (acc_1, acc_2), _ -> split' xs (-1) (acc_1, x :: acc_2)
  in
  split' lst len ([], [])
;;

let slice lst i_start i_end =
  let rec slice' lst start i rem acc =
    match lst with
    | [] -> rev acc
    | _ :: xs when i < start -> slice' xs start (i + 1) rem acc
    | _ when rem <= 0 -> rev acc
    | x :: xs -> slice' xs start (i + 1) (rem - 1) (x :: acc)
  in
  let size = i_end - i_start + 1 in
  slice' lst i_start 0 size []
;;

let rotate lst amt =
  let rec rotate' lst acc rem =
    match lst, acc with
    | [], [] -> []
    | [], acc -> if rem > 0 then rotate' (rev acc) [] (rem - 1) else rev acc
    | xs, acc when rem <= 0 -> xs @ rev acc
    | x :: xs, acc -> rotate' xs (x :: acc) (rem - 1)
  in
  let rem = if amt < 0 then length lst + amt else amt in
  rotate' lst [] rem
;;

let remove_at i lst =
  let rec remove_at' cnt lst acc =
    match lst with
    | [] -> []
    | _ :: xs when cnt <= 0 -> rev acc @ xs
    | x :: xs -> remove_at' (cnt - 1) xs (x :: acc)
  in
  remove_at' i lst []
;;
