staload "SATS/ms-queue.sats"

extern
fn malloc {a:vt@ype}(size_t) : a? =
  "mac#"

implement {a} new_node () =
  let
    val pre_x = malloc(sizeof<a>)
  in
    allocated_t(@{ value = pre_x, next = none_t })
  end
