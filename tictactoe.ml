(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tictactoe.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/20 17:32:25 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/21 18:13:11 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type player = POne | PTwo
type cell = player Option.t

let string_of_player : player -> string = function
    | POne -> "X"
    | PTwo -> "O"

let bigstring_of_player : player -> string list = function
    | POne -> ["\\   /"; "  X  "; "/   \\"]
    | PTwo -> ["/ - \\"; "|   |"; "\\ - /"]

let string_of_cell : cell -> string = function
    | Option.Some(x)    -> string_of_player x
    | Option.None       -> "-"

type smallboard = (cell * cell * cell *
                   cell * cell * cell *
                   cell * cell * cell)

let smallboard_winner : smallboard -> cell = function
    | (a, _, _,
       b, _, _,
       c, _, _) when a = b && b = c && a != Option.None -> a
    | (_, a, _,
       _, b, _,
       _, c, _) when a = b && b = c && a != Option.None -> a
    | (_, _, a,
       _, _, b,
       _, _, c) when a = b && b = c && a != Option.None -> a
    | (a, b, c,
       _, _, _,
       _, _, _) when a = b && b = c && a != Option.None -> a
    | (_, _, _,
       a, b, c,
       _, _, _) when a = b && b = c && a != Option.None -> a
    | (_, _, _,
       _, _, _,
       a, b, c) when a = b && b = c && a != Option.None -> a
    | (a, _, _,
       _, b, _,
       _, _, c) when a = b && b = c && a != Option.None -> a
    | (_, _, a,
       _, b, _,
       c, _, _) when a = b && b = c && a != Option.None -> a
    | _                                                 -> Option.None

let stringlist_of_smallboard ((a, b, c, d, e, f, g, h, i) : smallboard) =
    ((string_of_cell a) ^ " " ^ (string_of_cell b) ^ " " ^ (string_of_cell c)) ::
    ((string_of_cell d) ^ " " ^ (string_of_cell e) ^ " " ^ (string_of_cell f)) ::
    ((string_of_cell g) ^ " " ^ (string_of_cell h) ^ " " ^ (string_of_cell i)) :: []

let newsmallboard : smallboard =
    (Option.None, Option.None, Option.None,
     Option.None, Option.None, Option.None,
     Option.None, Option.None, Option.None)

let update_9_tuple tuple cmp fn pos = match (pos, tuple) with
    | (1, (k, b, c, d, e, f, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(k, b, c, d, e, f, g, h, i)
        | Option.None    -> Option.None)
    | (2, (a, k, c, d, e, f, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, k, c, d, e, f, g, h, i)
        | Option.None    -> Option.None)
    | (3, (a, b, k, d, e, f, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, k, d, e, f, g, h, i)
        | Option.None    -> Option.None)
    | (4, (a, b, c, k, e, f, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, k, e, f, g, h, i)
        | Option.None    -> Option.None)
    | (5, (a, b, c, d, k, f, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, d, k, f, g, h, i)
        | Option.None    -> Option.None)
    | (6, (a, b, c, d, e, k, g, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, d, e, k, g, h, i)
        | Option.None    -> Option.None)
    | (7, (a, b, c, d, e, f, k, h, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, d, e, f, k, h, i)
        | Option.None    -> Option.None)
    | (8, (a, b, c, d, e, f, g, k, i)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, d, e, f, g, k, i)
        | Option.None    -> Option.None)
    | (9, (a, b, c, d, e, f, g, h, k)) when cmp k -> (match fn k with
        | Option.Some(k) -> Option.Some(a, b, c, d, e, f, g, h, k)
        | Option.None    -> Option.None)
    | _ ->                                           Option.None

let insert_smallboard (board : smallboard) n pos : smallboard Option.t =
    if smallboard_winner board != Option.None then Option.None
    else                                           update_9_tuple board (( = ) Option.None) (fun _ -> Option.Some(Option.Some(n))) pos

let draw_smallboard board =
    List.iter print_endline (stringlist_of_smallboard board)

let otherplayer = function
    | POne -> PTwo
    | PTwo -> POne

type bigboard = (smallboard * smallboard * smallboard *
                 smallboard * smallboard * smallboard *
                 smallboard * smallboard * smallboard)

let newbigboard : bigboard =
    (newsmallboard, newsmallboard, newsmallboard,
     newsmallboard, newsmallboard, newsmallboard,
     newsmallboard, newsmallboard, newsmallboard)

let smallboard_of_bigboard ((a, b, c, d, e, f, g, h, i) : bigboard) : smallboard =
    (smallboard_winner a, smallboard_winner b, smallboard_winner c,
     smallboard_winner d, smallboard_winner e, smallboard_winner f,
     smallboard_winner g, smallboard_winner h, smallboard_winner i)

let draw_bigboard (a, b, c, d, e, f, g, h, i) =
    let rec print_triple_list = function
        | (h1 :: t1, h2 :: t2, h3 :: t3) -> (print_endline (h1 ^ " | " ^ h2 ^ " | " ^ h3); print_triple_list (t1, t2, t3))
        | _ -> ()
    in
    let rec bigstr board = match smallboard_winner board with
        | Option.None -> stringlist_of_smallboard board
        | Option.Some(p) -> bigstring_of_player p
    in
    print_triple_list (bigstr a, bigstr b, bigstr c);
    print_endline "---------------------";
    print_triple_list (bigstr d, bigstr e, bigstr f);
    print_endline "---------------------";
    print_triple_list (bigstr g, bigstr h, bigstr i)

let insert_bigboard (board : bigboard) n (pos1, pos2) =
    update_9_tuple board (fun a -> smallboard_winner a = Option.None) (fun k -> insert_smallboard k n pos2) pos1

let bigboard_full (a, b, c, d, e, f, g, h, i) =
    let smallboard_full x = smallboard_winner x != Option.None || (match x with
        | (Option.Some(_), Option.Some(_), Option.Some(_),
           Option.Some(_), Option.Some(_), Option.Some(_),
           Option.Some(_), Option.Some(_), Option.Some(_)) -> true
        | _ -> false)
    in
    smallboard_full a && smallboard_full b && smallboard_full c &&
    smallboard_full d && smallboard_full e && smallboard_full f &&
    smallboard_full g && smallboard_full h && smallboard_full i

let rec main board (player : player) =
    let my_double_int () =
        let is_num c = 48 <= (int_of_char c) && (int_of_char c) <= 57 in
        let str = read_line () in
        if String.length str = 3 && is_num (String.get str 0) &&
            String.get str 1 = ' ' && is_num (String.get str 2) then
            Option.Some(int_of_char (String.get str 0) - 48, int_of_char (String.get str 2) - 48)
        else
            Option.None
    in
    draw_bigboard board;
    if smallboard_winner (smallboard_of_bigboard board) != Option.None then begin
        print_string "Winner is ";
        print_endline (string_of_cell (smallboard_winner (smallboard_of_bigboard board)))
    end else if bigboard_full board then
        print_endline "It's a tie"
    else begin
        print_string (string_of_player player);
        print_endline "'s turn to play.";
        match Option.and_then (insert_bigboard board player) (my_double_int ()) with
        | Option.Some(board) -> (main board (otherplayer player))
        | Option.None        -> (print_endline "Incorrect format"; main board player)
    end

let () =
    main newbigboard POne
