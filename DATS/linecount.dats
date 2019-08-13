// Contains line counter state machine
extern
fun strpbrk { l1, l2 : addr }{ m, n : nat } (pf_s : strbuf_v(l1, 1, n), pf_a : !strbuf_v(l2, 1, m) | ptr(l1), ptr(l2)) :
  [ l0 : addr | l0 >= l1 && l1+n > l0 ] (bytes_v(l1, 1, m), strbuf_v(l0, 1, n)| ptr(l0))
