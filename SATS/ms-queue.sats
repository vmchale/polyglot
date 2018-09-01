// explanation given here: http://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf
datavtype pointer_t(a: vt@ype) =
  | pointer_t of @{ pointer = [ l : addr | l > null ] (node_t(a) @ l | ptr(l))
                  , count = uint
                  }
  | none_t
and node_t(a: vt@ype) =
  | node_t of @{ value = a, next = pointer_t(a) }
  | allocated_t of @{ value = a?, next = pointer_t(a) }

vtypedef node_ptr(a: vt@ype) = [l:addr] (node_t(a) @ l | ptr(l))
vtypedef queue_t(a: vt@ype) = @{ queue_head = pointer_t(a), queue_tail = pointer_t(a) }

fun {a:vt@ype} new_node () : node_ptr(a)

fun {a:vt@ype} initialize (&queue_t(a)? >> queue_t(a)) : void

fun {a:vt@ype} enqueue (&queue_t(a) >> queue_t(a), a) : void

fun {a:vt@ype} dequeue (&queue_t(a) >> _, &a) : bool
