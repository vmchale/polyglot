#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "DATS/shared.dats"
#include "$PATSHOMELOCS/ats-bench-0.3.3/bench.dats"
#include "DATS/utils.dats"
#include "DATS/error.dats"

fun linecount_bench() : void =
  { val _ = line_count("DATS/cli.dats", Some_vt("//")) }

fun flow_stream_bench() : void =
  {
    var x = empty_contents()
    val _ = step_stream(x, ".", ".", list_nil, false)
  }

fun shebang_bench() : void =
  {
    val pl = check_shebang("bash/install.sh")
    val () = free(pl)
  }

fun keyword_bench() : void =
  {
    var empty_file = @{ lines = 0, blanks = 0, comments = 0, files = 0 } : file
    val pl_t = check_keywords("test/data/Coq.v", empty_file, "v")
    val () = free(pl_t)
  }

val linecount_delay = lam () => linecount_bench()
val keyword_delay = lam () => keyword_bench()
val stream_delay = lam () => flow_stream_bench()
val shebang_delay = lam () => shebang_bench()

implement main0 () =
  {
    val _ = print_slope("linecount (DATS/cli.dats)", 10, linecount_delay)
    val _ = print_slope("keyword check (test/data/Coq.v)", 8, keyword_delay)
    val _ = print_slope("step_stream", 5, stream_delay)
    val _ = print_slope("shebang check", 10, shebang_delay)
  }
