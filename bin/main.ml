let test_lists () =
  let p12_inp = [ "a"; "b"; "c"; "d" ] in
  let p1_1 = Lists.last p12_inp in
  let p1_2 = Lists.last [] in
  assert (Option.equal String.equal p1_1 (Some "d"));
  assert (Option.equal String.equal p1_2 None);
  let p3_inp = [ "a"; "b"; "c"; "d"; "e" ] in
  let p3_1 = Lists.at 3 p3_inp in
  let p3_2 = Lists.at 3 [ "a" ] in
  let p3_3 = Lists.at 0 p3_inp in
  assert (Option.equal String.equal p3_1 (Some "c"));
  assert (Option.equal String.equal p3_2 None);
  assert (Option.is_none p3_3);
  let p4_1 = Lists.length [ "a"; "b"; "c" ] in
  assert (p4_1 = 3);
  assert (Lists.length [] = 0);
  let p5_1 = Lists.rev [ "a"; "b"; "c" ] in
  assert (List.equal String.equal p5_1 [ "c"; "b"; "a" ]);
  assert (Lists.is_palindrome [ "h"; "a"; "n"; "n"; "a"; "h" ]);
  assert (not @@ Lists.is_palindrome [ "h"; "a" ]);
  let p7 =
    Lists.flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  assert (List.equal String.equal p7 [ "a"; "b"; "c"; "d"; "e" ]);
  let p8_10_inp =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  let p8 = Lists.compress p8_10_inp in
  assert (List.equal String.equal p8 [ "a"; "b"; "c"; "a"; "d"; "e" ]);
  let p9 = Lists.pack p8_10_inp in
  assert (
    List.equal
      (List.equal String.equal)
      p9
      [ [ "a"; "a"; "a"; "a" ]
      ; [ "b" ]
      ; [ "c"; "c" ]
      ; [ "a"; "a" ]
      ; [ "d" ]
      ; [ "e"; "e"; "e"; "e" ]
      ]);
  let p10 = Lists.encode p8_10_inp in
  assert (
    List.equal
      (fun (i1, x1) (i2, x2) -> i1 = i2 && x1 = x2)
      p10
      [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]);
  let p11 = Lists.encode_rle p8_10_inp in
  assert (
    p11 = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]);
  let p12 = Lists.decode_rle p11 in
  assert (p12 = p8_10_inp);
  let p13 = Lists.encode_direct p8_10_inp in
  assert (p13 = p11);
  let p14 = Lists.duplicate [ "a"; "b"; "c"; "c"; "d" ] in
  assert (p14 = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
  let p15 = Lists.replicate [ "a"; "b"; "c" ] 3 in
  assert (p15 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);
  let p16 = Lists.drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3 in
  assert (p16 = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]);
  let (p17_1a, p17_1b) = Lists.split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3 in
  let p17_1 = (p17_1a, p17_1b) in
  assert (p17_1 = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
  let (p17_2a, p17_2b) = Lists.split [ "a"; "b"; "c"; "d" ] 5 in
  let p17_2 = (p17_2a, p17_2b) in
  assert (p17_2 = ([ "a"; "b"; "c"; "d" ], []));
  let p18_1 = Lists.slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 in
  assert (p18_1 = ["c"; "d"; "e"; "f"; "g"]);
  let p19_1 = Lists.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 in
  assert (p19_1 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);
  let p19_2 = Lists.rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) in
  assert (p19_2 = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);
  let p20_1 = Lists.remove_at 1 ["a"; "b"; "c"; "d"] in
  assert (p20_1 = ["a"; "c"; "d"])
;;

let () = test_lists ()
