#include "src/shared.dats"
#include "src/concurrency.dats"

staload "libats/SATS/athread.sats"
staload "libats/DATS/athread.dats"

fun step_list(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)
                                        && test_file_isdir(s + "/" + x) != 0))
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fun step_list_files(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes))
                                        && test_file_isdir(s + "/" + x) = 0)
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) when s = "." => list_cons(x, stream2list(xs))
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fun map_depth(xs : List0(string), excludes : List0(string)) : List0(string) =
  let
    fun loop(i : int, xs : List0(string), excludes : List0(string)) : List0(string) =
      let
        var xs0 = list0_filter(g0ofg1(xs), lam x => test_file_isdir(x) != 0)
      in
        case+ i of
          | 0 => g1ofg0(list0_mapjoin(xs0, lam x => if not(bad_dir(x, excludes)) then
                                       g0ofg1(step_list(x, excludes))
                                     else
                                       list0_nil))
          | _ => g1ofg0(list0_mapjoin(xs0, lam x => let
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

fun apportion(includes : List0(string), excludes : List0(string)) :
  (List0(string), List0(string), List0(string), List0(string)) =
  let
    var deep = map_depth(includes, excludes)
    var n = length(deep) / 4
    val (p, pre_q) = list_split_at(deep, n)
    val (q, pre_r) = list_split_at(pre_q, n)
    val (r, s) = list_split_at(pre_r, n)
  in
    (list_vt2t(p), list_vt2t(q), list_vt2t(r), s)
  end

fun work( excludes : List0(string)
        , send : channel(List0(string))
        , chan : channel(source_contents)
        ) : void =
  {
    val- (n) = channel_remove(send)
    var x = map_stream(empty_contents(), n, excludes)
    val () = channel_insert(chan, x)
    val- ~None_vt() = channel_unref(chan)
    val- () = case channel_unref<List0(string)>(send) of
      | ~None_vt() => ()
      | ~Some_vt (snd) => queue_free<List0(string)>(snd)
  }

fn handle_unref(x : channel(string)) : void =
  case+ channel_unref(x) of
    | ~None_vt() => ()
    | ~Some_vt (q) => queue_free<List0(string)>(q)

fun threads(includes : List0(string), excludes : List0(string)) : source_contents =
  let
    val chan = channel_make<source_contents>(4)
    var new_includes = if length(includes) > 0 then
      includes
    else
      list_cons(".", list_nil())
    val (fst, snd, thd, fth) = apportion(new_includes, excludes)
    
    fun loop {i : nat} .<i>. (i : int(i), chan : !channel(source_contents)) : void =
      {
        val chan_ = channel_ref(chan)
        val send = channel_make<List0(string)>(1)
        val send_r = channel_ref(send)
        val _ = case- i of
          | 1 => channel_insert(send, fst)
          | 2 => channel_insert(send, snd)
          | 3 => channel_insert(send, thd)
          | 4 => channel_insert(send, fth)
        val _ = athread_create_cloptr_exn(llam () =>
            work(excludes, send_r, chan_))
        val _ = handle_unref(send)
        val _ = if i < 2 then
          ()
        else
          loop(i - 1, chan)
      }
    
    val _ = loop(4, chan)
    
    fun loop_return(i : int, chan : !channel(source_contents)) : source_contents =
      case+ i of
        | 0 => empty_contents()
        | _ => let
          val- (n) = channel_remove(chan)
          var m = loop_return(i - 1, chan)
        in
          add_contents(m, n)
        end
    
    var r = loop_return(4, chan)
    val () = ignoret(usleep(1u))
    val () = while(channel_refcount(chan) >= 2)()
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
            print(make_table(result))
        end
  end