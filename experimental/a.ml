let () =
  let s =
    {|
    module test
    type T {
      f: float
      f: int
    }
    pipeline P() {}
    pipeline P() {}
    |}
  in
  let p = Pcre.quote "pipeline P() {}" in
  let rex = Pcre.regexp ~flags:[`DOTALL; `MULTILINE] p in
  let subs = Pcre.exec_all ~rex s in
  let start, finish = Pcre.get_substring_ofs subs.(2) 0 in
  Printf.printf "OK: start=%d finish=%d\n" start finish
