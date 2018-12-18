staload "libats/ML/SATS/string.sats"
staload "SATS/error.sats"

implement redundant_cli_flag () =
  prerr!("\33[33mWarning:\33[0m Flag --no-style has no effect when --html is not present\n")

implement bad_file (s) =
  if s != "" then
    prerr!("\33[33mWarning:\33[0m could not open file at " + s + "\n")
  else
    ()

implement bad_directory (s) =
  (prerr!("\33[31mError:\33[0m flag " + s + " found where a directory name was expected\n") ; exit(1))

implement bad_flag (s) =
  (prerr!("\33[31mError:\33[0m flag " + s + " must appear occur at most once\n") ; exit(1))

implement error_flag (s) =
  (prerr!("\33[31mError:\33[0m flag '" + s + "' not recognized. Try 'poly --help'\n") ; exit(1))

implement internal_error () =
  (prerr!("\33[31mError:\33[0m internal error\n") ; exit(1))

implement bad_exclude (s) =
  (prerr!("\33[31mError:\33[0m flag "
  + s
  + " must be followed by an argument and must occur alone\n") ; exit(1))

implement maybe_err (next) =
  (prerr("\33[31mError:\33[0m directory '" + next + "' does not exist\n") ; exit(1))
