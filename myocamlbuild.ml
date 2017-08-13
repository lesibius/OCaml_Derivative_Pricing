open Ocamlbuild_plugin;;
open Command;;

let () = dispatch begin function
    | Before_options ->
      Options.use_ocamlfind := true
    | After_options -> pdep  ["link"] "linkdep" (fun param -> [param])
    | _           -> ()
  end

