// adapted from the book. You can find the original here:
// https://github.com/githwxi/ATS-Postiats/blob/master/doc/EXAMPLE/MISC/wclines.dats
staload "src/filetype.sats"
staload "libats/libc/SATS/stdio.sats"
staload UN = "prelude/SATS/unsafe.sats"

#define BUFSZ (32*1024)

// monoidal addition for 'file' type
fn add_results(x : file, y : file) : file =
  let
    var next = @{ lines = x.lines + y.lines
                , blanks = x.blanks + y.blanks
                , comments = x.comments + y.comments
                , files = x.files + y.files
                }
  in
    next
  end

overload + with add_results

extern
fun memchr {l:addr}{m:nat}(pf : bytes_v(l, m) | p : ptr(l), c : char, size_t) :
  [ l2 : addr | l+m > l2 ] (bytes_v(l, l2-l), bytes_v(l2, l+m-l2)| ptr(l2)) =
  "mac#"

fn freadc {l:addr}(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l), c : char) : size_t =
  let
    extern
    castfn as_fileref(x : !FILEptr1) : FILEref
    
    var n = $extfcall(size_t, "fread", p, sizeof<char>, BUFSZ - 1, as_fileref(inp))
    val () = $UN.ptr0_set<char>(ptr_add<char>(p, n), c)
  in
    n
  end

vtypedef pair = @{ f = char, s = Option_vt(char) }

fn get_chars {m:nat}(s : string(m)) : Option_vt(pair) =
  if length(s) >= 2 then
    let
      val p = @{ f = string_head(s), s = Some_vt(s[1]) }
    in
      Some_vt(p)
    end
  else
    if length(s) >= 1 then
      let
        val p = @{ f = string_head(s), s = None_vt }
      in
        Some_vt(p)
      end
    else
      None_vt

// safely read bytes at a pointer
fn read_bytes {l:addr}{ m : nat | m > 0 }(pf : !bytes_v(l, m) | p : ptr(l)) : char =
  $UN.ptr0_get<char>(p)

fn read_bytes_succ {l:addr}{ m : nat | m > 1 }(pf : !bytes_v(l, m) | p : ptr(l)) : char =
  $UN.ptr0_get<char>(ptr_succ<char>(p))

fn compare_bytes {l:addr}{m:nat}(pf : !bytes_v(l, m) | p : ptr(l), compare : char, comment : !Option_vt(pair)) :
  '(bool, bool) =
  let
    var match = lam@ (x : char, y : !Option_vt(char)) : bool =>
      case+ y of
        | Some_vt (z) => x = z
        | None_vt() => true
    
    // FIXME: this is unsafe and in fact immoral
    var s2 = $UN.ptr0_get<char>(p)
    var s3 = $UN.ptr0_get<char>(ptr_succ<char>(p))
    var b = s2 = compare
    var b2 = case+ comment of
      | None_vt() => false
      | Some_vt (p) => s2 = p.f && match(s3, p.s)
  in
    '(b, b2)
  end

fun wclbuf {l:addr}{n:nat}(pf : !bytes_v(l, n)
                          | p : ptr(l), pz : ptr, c : char, res : file, comment : !Option_vt(pair)) : file =
  let
    val (pf1, pf2 | p2) = memchr(pf | p, c, i2sz(BUFSZ))
    var match_acc_file = lam@ (b : bool, b2 : bool) : file =>
      case+ (b, b2) of
        | (true, true) => @{ files = 0, blanks = 1, comments = 1, lines = 1 }
        | (true, false) => @{ files = 0, blanks = 1, comments = 0, lines = 1 }
        | (false, true) => @{ files = 0, blanks = 0, comments = 1, lines = 1 }
        | (false, false) => @{ files = 0, blanks = 0, comments = 0, lines = 1 }
  in
    if p2 < pz then
      let
        prval (pf21, pf22) = array_v_uncons(pf2)
        val '(cmp1, cmp2) = compare_bytes(pf22 | ptr_succ<byte>(p2), '\n', comment)
        var acc_file = match_acc_file(cmp1, cmp2)
        var res = wclbuf(pf22 | ptr_succ<byte>(p2), pz, c, res + acc_file, comment)
        prval () = pf2 := array_v_cons(pf21, pf22)
        prval () = pf := bytes_v_unsplit(pf1, pf2)
      in
        res
      end
    else
      let
        prval () = pf := bytes_v_unsplit(pf1, pf2)
      in
        res
      end
  end

fn wclfil {l:addr}(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l), c : char, comment : !Option_vt(pair)) : file =
  let
    var acc_file = @{ files = 1, blanks = ~1, comments = 0, lines = 0 } : file
    
    fun loop(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l), c : char, res : file, comment : !Option_vt(pair)) :
      file =
      let
        var n = freadc(pf | inp, p, c)
      in
        if n > 0 then
          let
            // TODO: make ptr_add take appropriate type??
            var pz = ptr_add<char>(p, n)
            var res = wclbuf(pf | p, pz, c, res, comment)
          in
            loop(pf | inp, p, c, res, comment)
          end
        else
          let
            // FIXME time-consuming?
            fn postproc(acc : file) : file =
              let
                var acc_r = ref<file>(acc)
                val () = if acc.blanks = ~1 then
                  acc_r -> blanks := 0
                else
                  ()
              in
                !acc_r
              end
          in
            postproc(res)
          end
      end
  in
    loop(pf | inp, p, c, acc_file, comment)
  end

fn clear_opt(x : Option_vt(char)) : void =
  case+ x of
    | ~None_vt() => ()
    | ~Some_vt (_) => ()

overload free with clear_opt

fn clear_function(x : Option_vt(pair)) : void =
  case+ x of
    | ~None_vt() => ()
    | ~Some_vt (x) => free(x.s)

overload free with clear_function

fn count_char(s : string, c : char, comment : Option_vt(pair)) : file =
  let
    // TODO: use a dataview to make this safe??
    var inp: FILEptr1 = fopen_exn(s, file_mode_r)
    val (pfat, pfgc | p) = malloc_gc(g1i2u(BUFSZ))
    prval () = pfat := b0ytes2bytes_v(pfat)
    var res = wclfil(pfat | inp, p, c, comment)
    val () = mfree_gc(pfat, pfgc | p)
    val () = fclose1_exn(inp)
    val () = free(comment)
  in
    res
  end

// This ensures safety.
typedef small_string = [ m : nat | m <= 2 && m > 0 ] string(m)

fn line_count(s : string, pre : Option_vt(small_string)) : file =
  case+ pre of
    | ~Some_vt (x) => count_char(s, '\n', get_chars(x))
    | ~None_vt() => count_char(s, '\n', None_vt)
