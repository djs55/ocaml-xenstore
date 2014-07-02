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

module type S = sig
  type v
  (** The type of values within the set *)

  type t
  (** A persistent set of values of type v *)

  val create: string list -> (t * 'view Transaction.side_effects) Lwt.t
  (** [create name]: loads the set at [name] *)

  val name: t -> string list
  (** [name t]: returns the [name] associated with the set *)

  val cardinal: t -> int Lwt.t
  (** [cardinal t]: the number of elements in the set *)

  val add: v -> t -> 'view Transaction.side_effects Lwt.t
  (** [add v t]: adds [v] to set [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val remove: v -> t -> 'view Transaction.side_effects Lwt.t
  (** [remove v t]: removes [v] from the set [t].
      When the thread completes the update will be in the persistent
      store and will survive a crash. *)

  val mem: v-> t -> bool Lwt.t
  (** [mem v t]: true if [v] is in [t], false otherwise *)

  val clear: t -> 'view Transaction.side_effects Lwt.t
  (** [clear t]: deletes all bindings from map [t] *)

  val fold: ('b -> v -> 'b) -> 'b -> t -> 'b Lwt.t
  (** [fold f initial t]: folds [f] across all the elements in
      [t] starting with [initial] *)
end
(** A persistent set which contains values of type v *)

open Sexplib
open Xenstore

let debug fmt = Logging.debug "pset" fmt
let info  fmt = Logging.info  "pset" fmt
let error fmt = Logging.debug "pset" fmt

open Lwt

module Make(T: S.SEXPABLE) = struct

  type t = {
    name: string list;
    m: Lwt_mutex.t;
  }

  let recreate t =
    Database.store >>= fun db ->
    Transaction.make Transaction.none db >>= fun tr ->
    let path = Protocol.Path.of_string_list t.name in
    if not(Transaction.exists tr (Perms.of_domain 0) path) then Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
    return (Transaction.get_side_effects tr)

  (* fold over keys and values: for internal use only. *)
  let fold f initial t =
    recreate t >>= fun effects ->
    Database.persist effects >>= fun () ->
    Database.store >>= fun db ->
    Transaction.make Transaction.none db >>= fun tr ->
    let path = Protocol.Path.of_string_list t.name in
    let ls = Transaction.ls tr (Perms.of_domain 0) path in
    return (List.fold_left (fun acc k ->
      let v = Transaction.read tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [k])) in
      f acc k (T.t_of_sexp (Sexp.of_string v))
    ) initial ls)

  let mem v t = fold (fun acc _ v' -> acc || v = v') false t

  (* Everything below here is in the public API and should consider
   * holding the mutex over the operation. *)

  let create name =
    let m = Lwt_mutex.create () in
    let t = { name; m } in
    Lwt_mutex.with_lock t.m
      (fun () ->
        recreate t >>= fun effects ->
        return (t, effects)
      )

  let name t = t.name

  let cardinal t = Lwt_mutex.with_lock t.m (fun () -> fold (fun acc _ _ -> acc + 1) 0 t)

  let add v t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        mem v t >>= function
        | true ->
          Transaction.no_side_effects ()
        | false ->
          fold (fun acc k _ -> try max acc (int_of_string k) with _ -> acc) (-1) t >>= fun max_id ->
          Database.store >>= fun db ->
          Transaction.make Transaction.none db >>= fun tr ->
          Transaction.write tr None 0 (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ string_of_int (max_id + 1) ])) (Sexp.to_string (T.sexp_of_t v));
          return (Transaction.get_side_effects tr)
      )

  let remove v t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        fold (fun acc k v' -> if v' = v then Some k else None) None t >>= function
        | None ->
          Transaction.no_side_effects ()
        | Some key ->
          Database.store >>= fun db ->
          Transaction.make Transaction.none db >>= fun tr ->
          Transaction.rm tr (Perms.of_domain 0) (Protocol.Path.of_string_list (t.name @ [ key ]));
          return (Transaction.get_side_effects tr)
      )

  let clear t =
    Lwt_mutex.with_lock t.m
      (fun () ->
        Database.store >>= fun db ->
        Transaction.make Transaction.none db >>= fun tr ->
        let path = Protocol.Path.of_string_list t.name in
        if Transaction.exists tr (Perms.of_domain 0) path then Transaction.rm tr (Perms.of_domain 0) path;
        Transaction.mkdir tr None 0 (Perms.of_domain 0) path;
        return (Transaction.get_side_effects tr)
      )

  let fold f i t = Lwt_mutex.with_lock t.m (fun () -> fold (fun acc _ v -> f acc v) i t)

  let mem v t = Lwt_mutex.with_lock t.m (fun () -> mem v t)
end
