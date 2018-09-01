datavtype pointer_t(a: vtype) =
  pointer_t of @{ pointer = [l:addr] (node_t(a) @ l | ptr(l)), count = uint }
and node_t(a: vtype) =
  node_t of @{ value = a, next = pointer_t(a) }

vtypedef queue_t(a: vtype) = @{ head = pointer_t(a), tail = pointer_t(a) }

fun {a:vtype} new_node () : node_t(a)

fun {a:vtype} initialize (&queue_t(a)? >> queue_t(a)) : void
