(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module Server(IO: Make.IO) = struct

  module Request = Request.Make(IO)
  module Response = Response.Make(IO)
  module Stream = Lazy_stream.Make(IO)

  let (>>=) = IO.(>>=)
  let return = IO.return

  type conn_id = int
  let string_of_conn_id = string_of_int

  module Body = struct
    type t =
    | String of string
    | Stream of string Stream.t

    let get_transfer_encoding = function
    | String x -> Transfer.Fixed (String.length x)
    | Stream _ -> Transfer.Chunked

    let of_request req ic =
      let elements = match Request.has_body req with
      | false -> fun () -> return Stream.End
      | true -> 
        let rec loop () =
          Request.read_body req ic >>= function
          | Transfer.Done ->
            return Stream.End
          | Transfer.Final_chunk c ->
            return (Stream.Chunk (c, fun () -> return Stream.End))
          | Transfer.Chunk c ->
            return (Stream.Chunk (c, loop)) in
		loop in
      Stream (ref elements)

    let iter_s f = function
    | String x -> f x
    | Stream s -> Stream.iter_s f s

    let to_string x =
      let b = Buffer.create 128 in
      iter_s (fun x -> Buffer.add_string b x; return ()) x >>= fun () ->
      return (Buffer.contents b)

    let drain = iter_s (fun _ -> return ())
  end

  type config = {
    callback: conn_id -> Request.t -> Body.t -> (Response.t * Body.t) IO.t;
    conn_closed : conn_id -> unit -> unit;
  }

  let respond ?headers ~status ~body () =
    let encoding = Body.get_transfer_encoding body in
    let res = Response.make ~status ~encoding ?headers () in
    return (res, body)

  let respond_string ?headers ~status ~body () =
    respond ?headers ~status ~body:(Body.String body) ()

  let respond_error ~status ~body () =
    respond_string ~status ~body:("Error: "^body) ()

  let respond_not_found ?uri () =
    let body = match uri with
     |None -> "Not found"
     |Some uri -> "Not found: " ^ (Uri.to_string uri) in
    respond_string ~status:`Not_found ~body ()

  let callback spec =
    let conn_id = ref 0 in
    let rec daemon_callback ic oc =
      let conn_id = incr conn_id; !conn_id in
      Request.read ic >>= function
      | None ->
          spec.conn_closed conn_id ();
          return None
      | Some req ->
          let body = Body.of_request req ic in
          (try spec.callback conn_id req body
           with exn -> respond_error ~status:`Internal_server_error ~body:(Printexc.to_string exn) ())
          >>= fun (res, body) ->
          Response.write (fun res oc ->
            Body.iter_s (Response.write_body res oc) body
          ) res oc >>= fun () -> daemon_callback ic oc in
    daemon_callback
end
