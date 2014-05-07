(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Xenstore
open OUnit
open Lwt
open Sexplib.Std

let ( |> ) a b = b a
let ( ++ ) a b x = a (b x)
let id x = x

let _ = Database.initialise S.NoPersistence

let enable_debug = ref false

let debug fmt =
  Printf.kprintf (fun s -> if !enable_debug then (print_string s; print_string "\n")) fmt

(* Sets *)

module PSet = PSet.Make(struct
  type t = int with sexp
end)

let set_simple_t () =
  PSet.create ["hello"] >>= fun t ->
  PSet.cardinal t >>= fun n ->
  assert_equal ~printer:string_of_int 0 n;
  return ()

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true; enable_debug := true), "Run in verbose mode with lots of debugging";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test persistence code";

  let suite = "persistence" >:::
    [
      "set_simple" >:: (fun () -> Lwt_main.run (set_simple_t ()));
	] in
  run_test_tt ~verbose:!verbose suite
