// adapted from the book. You can find the original here:
// https://github.com/githwxi/ATS-Postiats/blob/master/doc/EXAMPLE/MISC/wclines.dats
#include "src/utils.dats"

staload "src/filetype.sats"
staload "libats/libc/SATS/stdio.sats"
staload UN = "prelude/SATS/unsafe.sats"

#define BUFSZ (32*1024)

%{^
#include <stdbool.h>

bool is_link(char *path) {
  struct stat buf;
  int x;
  x = lstat(path, &buf);
  return S_ISLNK(buf.st_mode);
}
%}

extern
fn is_link(string) : bool =
  "mac#"

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
  [ l0 : addr | l+m > l0 ] (bytes_v(l, l0-l), bytes_v(l0, l+m-l0)| ptr(l0)) =
  "mac#"

fn freadc {l:addr}(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l), c : char) : size_t =
  let
    extern
    castfn as_fileref(x : !FILEptr1) :<> FILEref
    
    var n = $extfcall(size_t, "fread", p, sizeof<char>, BUFSZ - 1, as_fileref(inp))
    val () = $UN.ptr0_set<char>(ptr_add<char>(p, n), c)
  in
    n
  end

vtypedef pair = @{ f = char, s = Option_vt(char) }

fn get_chars {m:nat}(s : string(m)) : Option_vt(pair) =
  let
    var l = length(s)
  in
    ifcase
      | l >= 2 => let
        val p = @{ f = string_head(s), s = Some_vt(s[1]) }
      in
        Some_vt(p)
      end
      | l >= 1 => let
        val p = @{ f = string_head(s), s = None_vt }
      in
        Some_vt(p)
      end
      | _ => None_vt
  end

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

// TODO: try to do call-by-reference but also stack allocate?
// this uses call-by-reference to go extra fast
fun wclbuf {l:addr}{n:nat}{l1:addr}( pf : !bytes_v(l, n) | p : ptr(l)
                                   , pz : ptr(l1)
                                   , c : char
                                   , res : file
                                   , comment : !Option_vt(pair)
                                   , ret : &file? >> file
                                   ) : void =
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
        val () = wclbuf(pf22 | ptr_succ<byte>(p2), pz, c, res + acc_file, comment, ret)
        prval () = pf2 := array_v_cons(pf21, pf22)
        prval () = pf := bytes_v_unsplit(pf1, pf2)
      in end
    else
      let
        prval () = pf := bytes_v_unsplit(pf1, pf2)
      in
        ret := res
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
            fn {a:vt@ype} ptr_add_pf {l:addr}{m:nat}(p : ptr(l), ofs : size_t(m)) : ptr(l+m) =
              $UN.cast(ptr_add<a>(p, ofs))
            
            extern
            castfn witness(x : size_t) :<> [m:nat] size_t(m)
            
            var pz = ptr_add_pf<char>(p, witness(n))
            var ret: file
            val () = wclbuf(pf | p, pz, c, res, comment, ret)
          in
            loop(pf | inp, p, c, ret, comment)
          end
        else
          let
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

val empty_file: file = let
  var f = @{ files = 0, blanks = 0, comments = 0, lines = 0 } : file
in
  f
end

fn count_char(s : string, c : char, comment : Option_vt(pair)) : file =
  let
    // TODO: use a dataview to make this safe??
    // fails on a bad symlink, which is bad
    var inp = fopen(s, file_mode_r)
  in
    if FILEptr_is_null(inp) then
      let
        extern
        castfn fp_is_null { l : addr | l == null }{m:fm} (FILEptr(l,m)) :<> void
        
        val () = fp_is_null(inp)
        val () = bad_file(s)
      in
        (free(comment) ; empty_file)
      end
    else
      let
        val (pfat, pfgc | p) = malloc_gc(g1i2u(BUFSZ))
        prval () = pfat := b0ytes2bytes_v(pfat)
        var res = wclfil(pfat | inp, p, c, comment)
        val () = mfree_gc(pfat, pfgc | p)
        val () = fclose1_exn(inp)
        val () = free(comment)
      in
        res
      end
  end

// This ensures safety.
typedef small_string = [ m : nat | m <= 2 && m > 0 ] string(m)

fn line_count_skip_links(s : string, pre : Option_vt(small_string)) : file =
  if is_link(s) then
    case+ pre of
      | ~Some_vt (_) => empty_file
      | ~None_vt() => empty_file
  else
    case+ pre of
      | ~Some_vt (x) => count_char(s, '\n', get_chars(x))
      | ~None_vt() => count_char(s, '\n', None_vt)

fn line_count(s : string, pre : Option_vt(small_string)) : file =
  case+ pre of
    | ~Some_vt (x) => count_char(s, '\n', get_chars(x))
    | ~None_vt() => count_char(s, '\n', None_vt)
