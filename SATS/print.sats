// This file contains various functions for printing generated output
staload "SATS/filetype.sats"
staload "libats/ML/SATS/string.sats"

fun print_file(pt : !pl_type, string) : string

// function to print tabular output at the end
fn make_table(source_contents, bool) : string

// Function to print output sorted by type of language.
fn make_output(source_contents, bool) : string
