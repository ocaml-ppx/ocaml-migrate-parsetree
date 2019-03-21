let error_of_exn = Location.error_of_exn

let get_load_path () =
  !Config.load_path

let get_unboxed_types () =
  false

let set_unboxed_types _b =
  ()
