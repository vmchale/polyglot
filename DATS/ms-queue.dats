staload "SATS/ms-queue.sats"

extern
fn malloc {a:vt@ype}(size_t) : [ l : addr | l > null ] (a? @ l | ptr(l)) =
  "mac#"

extern
praxi prfree {a:vt@ype}{l:addr} (a? @ l) : void
