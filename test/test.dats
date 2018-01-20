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
vtypedef test_tree = @{ group = string, leaves = List_vt(named) }

fn fail_incomplete(i : int, n : int) : void =
  {
    val _ = prerr!("\nTest suite complete (" + tostring_int(i) + "/" + tostring_int(n) + ")\n")
    val _ = if n != i then
      (exit(1) ; ())
    else
      ()
  }

// TODO multithreaded test suite?
fnx iterate_list(t : test_tree, i : int, n : int) : void =
  let
    val _ = if i = 0 then
      println!(t.group + ":")
    else
      ()
    
    fun handle_loop(s : string, b : bool, xs : test_tree) : void =
      if b then
        (println!("  \33[32msucceeded:\33[0m " + s) ; iterate_list(xs, i + 1, n))
      else
        (println!("  \33[31mfailed:\33[0m " + s) ; iterate_list(xs, i, n))
  in
    case+ t.leaves of
      | ~list_vt_nil() => fail_incomplete(i, n)
      | ~list_vt_cons (x, xs) => handle_loop(x.fst, x.snd, @{ group = t.group, leaves = xs })
  end

implement main0 () =
  {
    var e = empty_file()
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
    var xs = list_vt_cons(n0, list_vt_cons(n1, list_vt_cons(n2, list_vt_cons(n3, list_vt_nil))))
    var total = list_vt_length(xs)
    val g = @{ group = "Overlaps", leaves = xs } : test_tree
    val _ = iterate_list(g, 0, total)
  }