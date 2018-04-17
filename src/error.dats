// This file contains various error functions.
staload "libats/ML/SATS/string.sats"

fn bad_file(s : string) : void =
  if s != "" then
    prerr!("\33[33mWarning:\33[0m could not open file at " + s + "\n")
  else
    ()

fn bad_directory(s : string) : void =
  (prerr!("\33[31mError:\33[0m flag " + s + " found where a directory name was expected\n") ; exit(0))

fn bad_flag(s : string) : void =
  (prerr!("\33[31mError:\33[0m flag " + s + " must appear can occur at most once\n") ; exit(0))

fn bad_exclude(s : string) : void =
  (prerr!("\33[31mError:\33[0m flag "
  + s
  + " must be followed by an argument and must occur alone\n") ; exit(0))

fn maybe_err(next : string) : void =
  (prerr("\33[31mError:\33[0m directory '" + next + "' does not exist\n") ; exit(1))
