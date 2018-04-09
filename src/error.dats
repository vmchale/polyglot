// This file contains various error functions.
staload "libats/ML/SATS/string.sats"

fn bad_file(s : string) : void =
  if s != "" then
    prerr!("\33[33mWarning:\33[0m could not open file at " + s + "\n")
  else
    ()

fn maybe_err(next : string) : void =
  (prerr("\33[31mError:\33[0m directory '" + next + "' does not exist\n") ; exit(1))
