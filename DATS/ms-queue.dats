staload "SATS/ms-queue.sats"

extern
fn malloc {a:vt@ype}(size_t) : [ l : addr | l > null ] (a? @ l | ptr(l)) =
  "mac#"

extern
praxi prfree {l:addr}{a:vt@ype} (a? @ l) : void

// FIXME
implement {a} new_node () =
  let
    val (pf | pre_x) = malloc(sizeof<a>)
    val ret = allocated_t(@{ value = !pre_x, next = none_t })
  in
    (pf | ret)
  end
