#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "src/shared.dats"
#include "$PATSHOMELOCS/specats-0.2.3/mylibies.hats"

fn test_file(s : string, expected : pl_type) : bool =
  let
    var t0_actual = prune_extension("test/data/" + s, s)
    val b = expected = t0_actual
    val _ = free_pl(expected)
    val _ = free_pl(t0_actual)
  in
    b
  end

implement main0 () =
  {
    var e = empty_file
    var t0_expected = happy(e)
    var b0 = test_file("Python.y", t0_expected)
    var n0 = @{ fst = "happy", snd = b0 }
    var t1_expected = yacc(e)
    var b1 = test_file("rust.y", t1_expected)
    var n1 = @{ fst = "yacc", snd = b1 }
    var t2_expected = coq(e)
    var b2 = test_file("Coq.v", t2_expected)
    var n2 = @{ fst = "coq", snd = b2 }
    var t3_expected = verilog(e)
    var b3 = test_file("verilog.v", t3_expected)
    var n3 = @{ fst = "verilog", snd = b3 }
    var xs = n0 :: n1 :: n2 :: n3 :: nil
    var total = list_vt_length(xs)
    val g = @{ group = "Overlaps", leaves = xs } : test_tree
    val _ = iterate_list(g, 0, total)
  }
