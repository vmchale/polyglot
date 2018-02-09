// adapted from the book. You can find the original here:
// https://github.com/githwxi/ATS-Postiats/blob/master/doc/EXAMPLE/MISC/wclines.dats
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "libats/libc/SATS/stdio.sats"
staload "src/filetype.sats"
staload "prelude/SATS/string.sats"

%{^
extern void *rawmemchr(const void *s, int c);
#define atslib_rawmemchr rawmemchr
%}

// monoidal addition for 'file' type
fun add_results(x : file, y : file) : file =
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
fun rawmemchr {l:addr}{m:int}(pf : bytes_v(l, m) | p : ptr(l), c : int) :
  [ l2 : addr | l+m > l2 ] (bytes_v(l, l2-l), bytes_v(l2, l+m-l2) | ptr(l2)) =
  "mac#atslib_rawmemchr"

#define BUFSZ (32*1024)

extern
fun freadc {l:addr} (pf : !bytes_v(l, BUFSZ) | inp : FILEref, p : ptr(l), c : char) : size_t

implement freadc (pf | inp, p, c) =
  let
    var n = $extfcall(size_t, "fread", p, sizeof<char>, BUFSZ - 1, inp)
    val () = $UN.ptr0_set<char>(ptr_add<char>(p, n), c)
  in
    n
  end

vtypedef pair = @{ f = char, s = char }

// TODO refactor the '?' stuff out
fun get_chars(s : string) : Option(pair) =
  if length(s) >= 2 then
    let
      var p = @{ f = $UN.ptr1_get<char>(string2ptr(s))
               , s = $UN.ptr1_get<char>(add_ptr_bsz(string2ptr(s), i2sz(1)))
               }
    in
      Some(p)
    end
  else
    if length(s) >= 1 then
      let
        var p = @{ f = $UN.ptr1_get<char>(string2ptr(s)), s = '?' }
      in
        Some(p)
      end
    else
      None

// TODO - this should account for comments as well; should be pretty easy once we pass a string?
fun compare_bytes {l:addr}{m:int}( pf : !bytes_v(l, m) | p : ptr(l)
                                 , compare : char
                                 , comment : Option(pair)
                                 ) : (bool, bool) =
  let
    var s2 = $UN.ptr0_get<char>(p)
    var s3 = $UN.ptr0_get<char>(ptr_succ<byte>(p))
    var b = s2 = compare
    var b2 = case+ comment of
      | None() => false
      | Some (p) => s2 = p.f && (s3 = p.s || p.s = '?')
  in
    (b, b2)
  end

extern
fun wclbuf {l:addr}{n:int} ( pf : !bytes_v(l, n) | p : ptr(l)
                           , pz : ptr
                           , c : int
                           , res : file
                           , comment : Option(pair)
                           ) : file

fun match_acc_file(b : bool, b2 : bool) : file =
  case+ (b, b2) of
    | (true, true) => @{ files = 0, blanks = 1, comments = 1, lines = 1 }
    | (true, false) => @{ files = 0, blanks = 1, comments = 0, lines = 1 }
    | (false, true) => @{ files = 0, blanks = 0, comments = 1, lines = 1 }
    | (false, false) => @{ files = 0, blanks = 0, comments = 0, lines = 1 }

// TODO get previous?
// we should be able to use the bytes view?
implement wclbuf (pf | p, pz, c, res, comment) =
  let
    val (pf1, pf2 | p2) = rawmemchr(pf | p, c)
  in
    if p2 < pz then
      let
        prval (pf21, pf22) = array_v_uncons(pf2)
        val (cmp1, cmp2) = compare_bytes(pf22 | ptr_succ<byte>(p2), '\n', comment)
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

extern
fun wclfil {l:addr} ( pf : !bytes_v(l, BUFSZ) | inp : FILEref
                    , p : ptr(l)
                    , c : int
                    , comment : Option(pair)
                    ) : file

implement wclfil {l} (pf | inp, p, c, comment) =
  let
    var acc_file = @{ files = 1, blanks = ~1, comments = 0, lines = 0 } : file
    
    fun loop(pf : !bytes_v(l, BUFSZ) | inp : FILEref, p : ptr(l), c : int, res : file) : file =
      let
        val n = freadc(pf | inp, p, $UN.cast{char}(c))
      in
        if n > 0 then
          let
            var pz = ptr_add<char>(p, n)
            var res = wclbuf(pf | p, pz, c, res, comment)
          in
            loop(pf | inp, p, c, res)
          end
        else
          res
      end
  in
    loop(pf | inp, p, c, acc_file)
  end

// TODO don't use _exn here 
fn count_char(s : string, c : char, comment : Option(pair)) : file =
  let
    var inp: FILEref = fopen_ref_exn(s, file_mode_r)
    val (pfat, pfgc | p) = malloc_gc(g1i2u(BUFSZ))
    prval () = pfat := b0ytes2bytes_v(pfat)
    var res = wclfil(pfat | inp, p, $UN.cast2int(c), comment)
    val () = mfree_gc(pfat, pfgc | p)
    val _ = fclose_exn(inp)
  in
    res
  end

typedef small_string = [ m : nat | m <= 2 ] string(m)

fun line_count(s : string, pre : Option_vt(small_string)) : file =
  case+ pre of
    | ~Some_vt (x) => count_char(s, '\n', get_chars(x))
    | ~None_vt() => count_char(s, '\n', None)