%{^
#include <pthread.h>
#ifdef ATS_MEMALLOC_GCBDW
#undef GC_H
#define GC_THREADS
#include <gc/gc.h>
#endif
%}

#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "src/shared.dats"
#include "$PATSHOMELOCS/ats-concurrency-0.4.8/mylibies.hats"

staload "libats/SATS/deqarray.sats"
staload "libats/SATS/athread.sats"
staload _ = "libats/DATS/deqarray.dats"
staload _ = "libats/DATS/athread.dats"

#define NCPU 4

fn step_list(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes) && test_file_isdir(s + "/" + x) > 0))
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fn step_list_files(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)) && test_file_isdir(s + "/" + x) = 0)
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) when s = "." => list_cons(x, stream2list(xs))
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fn map_depth(xs : List0(string), excludes : List0(string)) : List0(string) =
  let
    fun loop {i:nat} .<i>. (i : int(i), xs : List0(string), excludes : List0(string)) : List0(string) =
      let
        var xs0 = list0_filter(g0ofg1(xs), lam x => test_file_isdir(x) > 0)
      in
        case+ i of
          | 0 => g1ofg0(list0_mapjoin(xs0, lam x => if not(bad_dir(x, excludes)) then
                                       g0ofg1(step_list(x, excludes))
                                     else
                                       list0_nil))
          | _ =>> g1ofg0(list0_mapjoin(xs0, lam x => let
                                        var ys = step_list(x, excludes)
                                        var zs = step_list_files(x, excludes)
                                      in
                                        if not(bad_dir(x, excludes)) then
                                          g0ofg1(loop(i - 1, ys, excludes)) + g0ofg1(zs)
                                        else
                                          if x = "." && i = 3 then
                                            g0ofg1(loop(i - 1, ys, excludes)) + g0ofg1(zs)
                                          else
                                            list0_nil
                                      end))
      end
  in
    loop(3, xs, excludes)
  end

fn get_or_nothing(n : intGte(0), xs : List0(List0(string))) : List0(string) =
  case+ list_get_at_opt(xs, n) of
    | ~Some_vt (x) => x
    | ~None_vt() => list_nil

// FIXME this is slow because of the List0
fn apportion(includes : List0(string), excludes : List0(string)) :
  '(List0(string), List0(string), List0(string), List0(string)) =
  let
    var ys = list0_filter(g0ofg1(includes), lam x => test_file_isdir(x) != 1)
    var deep = map_depth(includes, excludes) + g1ofg0(ys)
    var n = length(deep) / 4
    val (p, pre_q) = list_split_at(deep, n)
    val (q, pre_r) = list_split_at(pre_q, n)
    val (r, s) = list_split_at(pre_r, n)
  in
    '(list_vt2t(p), list_vt2t(q), list_vt2t(r), s)
  end

fn handle_unref(x : channel(string)) : void =
  case+ channel_unref(x) of
    | ~None_vt() => ()
    | ~Some_vt (q) => queue_free<List0(string)>(q)

fn work(excludes : List0(string), send : channel(List0(string)), chan : channel(source_contents)) : void =
  {
    var n = channel_remove(send)
    var x = map_stream(empty_contents(), n, excludes)
    val () = channel_insert(chan, x)
    val () = handle_unref(chan)
    val () = handle_unref(send)
  }

// ideally we want one "large" channel that will handle back-and-forth communication between threads.
fn threads(includes : List0(string), excludes : List0(string)) : source_contents =
  let
    // this will hold the results sent back
    val chan = channel_make<source_contents>(NCPU)
    var new_includes = if length(includes) > 0 then
      includes
    else
      list_cons(".", list_nil())
    val '(fst, snd, thd, fth) = apportion(new_includes, excludes)
    
    fun loop { i : nat | i > 0 && i <= NCPU } .<i>. (i : int(i), chan : !channel(source_contents)) : void =
      {
        val chan_ = channel_ref(chan)
        
        // this will simply communicate the work to be 
        val send = channel_make<List0(string)>(1)
        val send_r = channel_ref(send)
        val _ = case+ i of
          | 1 => channel_insert(send, fst)
          | 2 => channel_insert(send, snd)
          | 3 => channel_insert(send, thd)
          | 4 => channel_insert(send, fth)
        val _ = athread_create_cloptr_exn(llam () =>
            work(excludes, send_r, chan_))
        val () = handle_unref(send)
        val () = if i >= 2 then
          loop(i - 1, chan)
      }
    
    val _ = loop(NCPU, chan)
    
    fun loop_return { i : nat | i >= 0 && i <= NCPU } .<i>. (i : int(i), chan : !channel(source_contents)) :
      source_contents =
      case+ i of
        | 0 => empty_contents()
        | _ =>> let
          var n = channel_remove(chan)
          var m = loop_return(i - 1, chan)
        in
          m + n
        end
    
    var r = loop_return(NCPU, chan)
    
    // FIXME this
    val () = ignoret(usleep(80u))
    val () = while(channel_refcount(chan) > 1)()
    val () = handle_unref(chan)
  in
    r
  end

implement main0 (argc, argv) =
  let
    val cli = @{ version = false
               , help = false
               , no_table = false
               , no_parallel = false
               , no_colorize = false
               , excludes = list_nil()
               , includes = list_nil()
               } : command_line
    val parsed = get_cli(argc, argv, 0, false, cli)
  in
    if parsed.help then
      (help() ; exit(0))
    else
      if parsed.version then
        (version() ; exit(0))
      else
        let
          var result = if not(parsed.no_parallel) then
            threads(parsed.includes, parsed.excludes)
          else
            if length(parsed.includes) > 0 then
              map_stream(empty_contents(), parsed.includes, parsed.excludes)
            else
              map_stream(empty_contents(), list_cons(".", list_nil()), parsed.excludes)
        in
          if parsed.no_table then
            print(make_output(result))
          else
            print(make_table(result, not(parsed.no_colorize)))
        end
  end
