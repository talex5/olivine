module Vkt = Vk.Types

let ( !@ ) = Ctypes.( !@ )

let test_struct_string () =
  let name = "test string" in
  let module M = Vkt.Pipeline_shader_stage_create_info in
  let x = M.make ()
      ~stage:Vkt.Shader_stage_flags.empty
      ~module':Vkt.Shader_module.null
      ~name
  in
  Alcotest.(check string) "Name before GC" name @@ M.name x;
  Gc.full_major ();
  Alcotest.(check string) "Name after GC" name @@ M.name x

let test_struct_ptr () =
  let application_name = "test string" in
  let module A = Vkt.Application_info in
  let module I = Vkt.Instance_create_info in
  let application_info =
    A.make ()
      ~application_name
      ~application_version:1
      ~api_version:1
      ~engine_version:1
  in
  let x = I.make ~application_info () in
  let info x = I.application_info x |> Option.get in
  let name x = !@ (info x) |> A.application_name |> Option.get in
  Alcotest.(check string) "Name before GC" application_name @@ name x;
  Gc.full_major ();
  Alcotest.(check string) "Name after GC" application_name @@ name x

let test_union_string () =
  let s = "test string" in
  let module M = Vkt.Performance_value_data_intel in
  let x = M.value_string s in
  let get x = Ctypes.(coerce (ptr char) string) @@ Ctypes.getf x M.Fields.value_string in
  Alcotest.(check string) "String before GC" s @@ get x;
  Gc.full_major ();
  Alcotest.(check string) "String after GC" s @@ get x

let test_union_ptr () =
  let s = "test string" in
  let module M = Vkt.Device_or_host_address_khr in
  let x = M.host_address (Ctypes.(coerce string (ptr void)) s) in
  let get x = Ctypes.(coerce (ptr void) string) @@ Ctypes.getf x M.Fields.host_address in
  Alcotest.(check string) "String before GC" s @@ get x;
  Gc.full_major ();
  Alcotest.(check string) "String after GC" s @@ get x

let () =
  let open Alcotest in
  run "Olivine" [
      "gc", [
          test_case "struct-string"  `Quick test_struct_string;
          test_case "struct-ptr"     `Quick test_struct_ptr;
          test_case "union-string"   `Quick test_union_string;
          test_case "union-ptr"      `Quick test_union_ptr;
        ];
    ]
