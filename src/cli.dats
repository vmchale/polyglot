#include "share/atspre_staload.hats"

staload "libats/ML/DATS/string.dats"

// Type for the parsed command-line arguments. 
vtypedef command_line = @{ version = bool
                         , help = bool
                         , no_table = bool
                         , no_parallel = bool
                         , excludes = [ m : nat ] list(string, m)
                         , includes = [ m : nat ] list(string, m)
                         }

fun version() : void =
  println!("polygot version 0.3.20\nCopyright (c) 2018 Vanessa McHale")

fun help() : void =
  print("polyglot - Count lines of code quickly.
\33[36mUSAGE:\33[0m poly [DIRECTORY] ... [OPTION] ...
\33[36mFLAGS:\33[0m
    -V, --version            show version information
    -h, --help               display this help and exit
    -e, --exclude            exclude a directory
    -p, --no-parallel        do not execute in parallel
    -t, --no-table           display results in alternate format
                                                                                                                                                                  
    When no directory is provided poly will execute in the
    current directory.
                                                                                                                                                                  
    Bug reports and updates: github.com/vmchale/polyglot\n")

fun is_flag(s : string) : bool =
  string_is_prefix("-", s)

fun process(s : string, acc : command_line, is_first : bool) : command_line =
  let
    var acc_r = ref<command_line>(acc)
    val () = if is_flag(s) then
      case+ s of
        | "--help" => acc_r->help := true
        | "-h" => acc_r->help := true
        | "--no-table" => if not(acc.no_table) then
          acc_r->no_table := true
        else
          (println!("\33[31mError:\33[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
        | "-t" => if not(acc.no_table) then
          acc_r->no_table := true
        else
          (println!("\33[31mError:\33[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
        | "--no-parallel" => acc_r->no_parallel := true
        | "-p" => acc_r->no_parallel := true
        | "--version" => acc_r->version := true
        | "-V" => acc_r->version := true
        | "-e" => ( println!("\33[31mError:\33[0m flag " + s + " must be followed by an argument")
                  ; exit(0)
                  ; ()
                  )
        | "--exclude" => ( println!("\33[31mError:\33[0m flag " + s + " must be followed by an argument")
                         ; exit(0)
                         ; ()
                         )
        | _ => (println!("\33[31mError:\33[0m flag '" + s + "' not recognized") ; exit(0) ; ())
    else
      if not(is_first) then
        acc_r->includes := list_cons(s, acc.includes)
      else
        ()
  in
    !acc_r
  end

fun process_excludes(s : string, acc : command_line) : command_line =
  let
    var acc_r = ref<command_line>(acc)
    val () = if is_flag(s) then
      (println!("Error: flag " + s + " found where a directory name was expected") ; exit(0) ; ())
    else
      acc_r->excludes := list_cons(s, acc.excludes)
  in
    !acc_r
  end

fnx get_cli { n : int | n >= 1 }{ m : nat | m < n } .<n-m>. ( argc : int(n)
                                                            , argv : !argv(n)
                                                            , current : int(m)
                                                            , prev_is_exclude : bool
                                                            , acc : command_line
                                                            ) : command_line =
  let
    var arg = argv[current]
  in
    if current < argc - 1 then
      if arg != "--exclude" && arg != "-e" then
        let
          val c = get_cli(argc, argv, current + 1, false, acc)
        in
          if prev_is_exclude && current != 0 then
            process_excludes(arg, c)
          else
            if current != 0 then
              process(arg, c, current = 0)
            else
              c
        end
      else
        let
          val c = get_cli(argc, argv, current + 1, true, acc)
        in
          c
        end
    else
      if prev_is_exclude then
        process_excludes(arg, acc)
      else
        process(arg, acc, current = 0)
  end