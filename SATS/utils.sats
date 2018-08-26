staload "SATS/filetype.sats"

fn eq_pl_type(x : !pl_type, y : !pl_type) : bool

overload = with eq_pl_type

fn free_pl(pl : pl_type) : void

overload free with free_pl
