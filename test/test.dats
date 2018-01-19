#include "src/shared.dats"

fn test_file(s : string, expected : pl_type) : void =
  {
    var t0_actual = prune_extension("test/data/" + s, s)
    val () = assertloc(expected = t0_actual)
    val _ = free_pl(expected)
    val _ = free_pl(t0_actual)
  }

implement main0 () =
  {
    var f0 = empty_file()
    var t0_expected = happy(f0)
    val _ = test_file("Python.y", t0_expected)
    var t1_expected = yacc(f0)
    val _ = test_file("rust.y", t1_expected)
    val _ = println!("Test suite passed!")
  }