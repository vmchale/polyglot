#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "src/shared.dats"
#include "$PATSHOMELOCS/ats-bench-0.3.3/bench.dats"

fun linecount_bench() : void =
  { val _ = line_count("src/cli.dats", Some_vt("//")) }

fun keyword_bench() : void =
  {
    var empty_file = @{ lines = 0, blanks = 0, comments = 0, files = 0 } : file
    val pl_t = check_keywords("test/data/Coq.v", empty_file, "v")
    val () = free(pl_t)
  }

val linecount_delay = lam () => linecount_bench()
val keyword_delay = lam () => keyword_bench()

implement main0 () =
  {
    val _ = print_slope("linecount (src/cli.dats)", 10, linecount_delay)
    val _ = print_slope("keyword check (test/data/Coq.v)", 8, keyword_delay)
  }
