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

open Lwt
open Xs_protocol

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

let debug fmt = Logging.debug "xs_server" fmt
let error fmt = Logging.error "xs_server" fmt

let store =
	let store = Store.create () in
	List.iter
		(fun path ->
			let p = Store.Path.create path (Store.Path.getdomainpath 0) in
			if not (Store.exists store p)
			then Store.mkdir store 0 (Perms.of_domain 0) p
		) [ "/local"; "/local/domain"; "/tool"; "/tool/xenstored"; "/tool/xenstored/quota"; "/tool/xenstored/connection"; "/tool/xenstored/log"; "/tool/xenstored/memory" ];
	store

module type TRANSPORT = sig
  type 'a t = 'a Lwt.t
  val return: 'a -> 'a Lwt.t
  val ( >>= ): 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  type server
  val listen: unit -> server Lwt.t

  type channel
  val read: channel -> string -> int -> int -> int Lwt.t
  val write: channel -> string -> int -> int -> unit Lwt.t
  val destroy: channel -> unit Lwt.t
  val address_of: channel -> Xs_protocol.address Lwt.t

  val namespace_of: channel -> (module Namespace.IO) option

  val accept_forever: server -> (channel -> unit Lwt.t) -> 'a Lwt.t
end

module Server = functor(T: TRANSPORT) -> struct
	module PS = PacketStream(T)

	let handle_connection t =
		lwt address = T.address_of t in
		let interface = T.namespace_of t in
		let c = Connection.create address interface in
		let channel = PS.make t in
		let m = Lwt_mutex.create () in
		let take_watch_events () =
			let q = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
			Queue.clear c.Connection.watch_events;
			q in
		let flush_watch_events q =
			Lwt_list.iter_s
				(fun (path, token) ->
					debug "%s flush path=%s token=%s" (Xs_protocol.string_of_address address) path token;
					PS.send channel (Xs_protocol.(Response.(print (Watchevent(path, token)) 0l 0l)))
				) q in
		let (background_watch_event_flusher: unit Lwt.t) =
			while_lwt true do
				Lwt_mutex.with_lock m
					(fun () ->
						lwt () = while_lwt Queue.length c.Connection.watch_events = 0 do
							Lwt_condition.wait ~mutex:m c.Connection.cvar
						done in
						let events = take_watch_events () in
						debug "%s start background watch flush" (Xs_protocol.string_of_address address);
						lwt () = flush_watch_events events in
						debug "%s stop background watch flush" (Xs_protocol.string_of_address address);
						return ()
					)
			done in

		try_lwt
			lwt () =
			while_lwt true do
				lwt request = match_lwt (PS.recv channel) with
					| Ok x -> return x
					| Exception e -> raise_lwt e in
				debug "%s received %s" (Xs_protocol.string_of_address address) (Xs_protocol.to_debug_string request);
				Lwt_mutex.with_lock m
					(fun () ->
						(* A side-effect of processing a Watch request is that other parallel
						   modifications of the tree will start to queue watch events on this
						   'new' path. We need to prevent the background watch flusher thread
						   flushing these before the Watch reply packet is sent. Note we still
						   want to interleave RPCs with sending watch events here to avoid a
						   large pending queue of requests building up. *)

						(* These cannot include any new watch events *)
						let events = take_watch_events () in

						let reply = Call.reply store c request in
						(* New watch events can be generated but not flushed because we hold m *)
						debug "%s reply generated" (Xs_protocol.string_of_address address);

						lwt () = flush_watch_events events in	
						lwt () = PS.send channel reply in
						debug "%s sent %s" (Xs_protocol.string_of_address address) (Xs_protocol.to_debug_string reply);
						return ()
					)
			done in
			T.destroy t
		with e ->
			Lwt.cancel background_watch_event_flusher;
			Connection.destroy address;
			T.destroy t

	let serve_forever () =
		lwt server = T.listen () in
		T.accept_forever server handle_connection
end
