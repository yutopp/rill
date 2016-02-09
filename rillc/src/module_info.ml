open Batteries

module type INFO_TYPE =
  sig
    val full_filepath   : string
    val package_names   : string list
    val module_name     : string
  end

type 'env dep_t =
    Pkg of (string, 'env dep_t) Hashtbl.t
  | Mod of 'env

module Bag =
  struct
    type 'env t = {
      mutable fresh_id  : int;
      mutable modules   : 'env dep_t;
    }

    let empty () =
      {
        fresh_id = 0;
        modules = Pkg (Hashtbl.create 4);
      }

    let search_package (bag:'env t) pkg_names =
      let dig opt_mods pkg_name =
        let f mods =
          match mods with
          | Pkg (inner_tbl) ->
             Hashtbl.find_option inner_tbl pkg_name
          | _ -> None
        in
        Option.bind opt_mods f
      in
      List.fold_left dig (Some bag.modules) pkg_names

    let search_module (bag:'env t) pkg_names mod_name =
      let opt_pkg = search_package bag pkg_names in
      let find_mod pkg =
        match pkg with
        | Pkg tbl ->
           begin
             let regard_as_mod m : 'env option =
               match m with
               | Mod env -> Some env
               | _ -> None
             in
             Option.bind (Hashtbl.find_option tbl mod_name) regard_as_mod
           end
        | Mod _ -> None
      in
      Option.bind opt_pkg find_mod

    let register bag pkg_names mod_name env =
      let dig mods pkg_name =
        match mods with
        | Pkg (inner_tbl) ->
           begin
             match Hashtbl.find_option inner_tbl pkg_name with
             | Some r -> r
             | None ->
                begin
                  let npkg = Pkg (Hashtbl.create 4) in
                  Hashtbl.add inner_tbl pkg_name npkg;
                  npkg
                end
           end
        | _ -> failwith "[ERR] package name is already registerd as module"
      in
      let holder = List.fold_left dig bag.modules pkg_names in
      match holder with
      | Pkg tbl ->
         begin
           if Hashtbl.mem tbl mod_name then
             failwith "[ERR] already defined";

           let mod_ = Mod env in
           Hashtbl.add tbl mod_name mod_;
           0
         end
      | Mod _ ->
         failwith "[ERR] package name is already registered as package"
  end
