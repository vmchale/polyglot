staload "SATS/filetype.sats"

fn eq_pl_type(x : !pl_type, y : !pl_type) : bool

overload = with eq_pl_type

fn free_pl(pl_type) : void

overload free with free_pl

// monoidal addition for 'file' type
fn add_results(file, file) : file

overload + with add_results
