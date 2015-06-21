(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Option.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/21 09:16:24 by roblabla          #+#    #+#             *)
(*   Updated: 2015/06/21 12:58:38 by roblabla         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a t = Some of 'a | None

let map f = function
    | Some(x) -> Some(f x)
    | None    -> None

let default x = function
    | Some(y) -> y
    | None    -> x

let map_or f x = f (default x)

let and_then fn = function
    | Some(x) -> fn x
    | None    -> None
