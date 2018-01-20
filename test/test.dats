#include "src/shared.dats"

fn test_file(s : string, expected : pl_type) : bool =
  let
    var t0_actual = prune_extension("test/data/" + s, s)
    val b = expected = t0_actual
    val _ = free_pl(expected)
    val _ = free_pl(t0_actual)
  in
    b
  end

vtypedef named = @{ fst = string, snd = bool }
vtypedef test_tree = List_vt(named)

fn fail_incomplete(i : int, n : int) : void =
  {
    val () = prerr!("Test suite complete (" + tostring_int(i) + "/" + tostring_int(n) + ")\n")
    val () = if n != i then
      (exit(1) ; ())
    else
      ()
  }

// TODO multithreaded test suite?
fnx iterate_list(t : test_tree, i : int, n : int) : void =
  let
    fun handle_loop(s : string, b : bool, xs : test_tree) : void =
      if b then
        (prerr!("\33[32msucceeded:\33[0m " + s + "\n") ; iterate_list(xs, i + 1, n))
      else
        (prerr!("\33[31mfailed:\33[0m " + s + "\n") ; iterate_list(xs, i, n))
  in
    case+ t of
      | ~list_vt_nil() => (prerr!("Test suite complete (" + tostring_int(i) + "/" + tostring_int(n) + ")\n"))
      | ~list_vt_cons (x, xs) => handle_loop(x.fst, x.snd, xs)
  end

implement main0 () =
  {
    var e = empty_file()
    var t0_expected = happy(e)
    var t1_expected = yacc(e)
    var b0 = test_file("Python.y", t0_expected)
    var b1 = test_file("rust.y", t1_expected)
    var n0 = @{ fst = "happy", snd = b0 }
    var n1 = @{ fst = "yacc", snd = b1 }
    var xs = list_vt_cons(n0, list_vt_cons(n1, list_vt_nil))
    val _ = iterate_list(xs, 0, 2)
  }