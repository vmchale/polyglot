staload "libats/ML/SATS/string.sats"
staload _ = "libats/ML/DATS/string.dats"
staload UN = "prelude/SATS/unsafe.sats"

// Type for the parsed command-line arguments.
vtypedef command_line = @{ version = bool
                         , help = bool
                         , no_table = bool
                         , no_parallel = bool
                         , excludes = [m:nat] list(string, m)
                         , includes = [m:nat] list(string, m)
                         }

fun version() : void =
  println!("polygot version 0.4.20\nCopyright (c) 2018 Vanessa McHale")

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

fn bad_exclude(s : string) : void =
  (println!("\33[31mError:\33[0m flag "
  + s
  + " must be followed by an argument and must occur alone") ; exit(0) ; ())

fun process_short { s : int | s > 0 }(s : string(s), acc : command_line) : command_line =
  let
    var str = string_make_substring(s, i2sz(0), i2sz(1))
    var acc_r = ref<command_line>(acc)
    val () = case+ str of
      | "h" => acc_r -> help := true
      | "p" => acc_r -> no_parallel := true
      | "t" => acc_r -> no_table := true
      | "e" => bad_exclude(s)
      | "V" => acc_r -> version := true
      | "-" => ()
      | _ => (println!("\33[31mError:\33[0m flag '" + s + "' not recognized") ; exit(0) ; ())
    
    fn witness(s : string) : [ n : nat | n > 0 ] string(n) =
      $UN.cast(s)
    
    val inter = !acc_r
  in
    if length(s) > 1 then
      process_short(witness(string_make_substring(s, i2sz(1), length(s))), inter)
    else
      inter
  end

fun process(s : string, acc : command_line, is_first : bool) : command_line =
  let
    fn witness(s : string) : [ s : nat | s > 0 ] string(s) =
      $UN.cast(s)
    
    fn process(s : string) : string =
      case+ s of
        | ".." => "../"
        | _ => s
    
    var acc_r = ref<command_line>(acc)
    val () = if is_flag(s) then
      case+ s of
        | "--help" => acc_r -> help := true
        | "-h" => acc_r -> help := true
        | "--no-table" => if not(acc.no_table) then
          acc_r -> no_table := true
        else
          (println!("\33[31mError:\33[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
        | "-t" => if not(acc.no_table) then
          acc_r -> no_table := true
        else
          (println!("\33[31mError:\33[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
        | "--no-parallel" => acc_r -> no_parallel := true
        | "-p" => acc_r -> no_parallel := true
        | "--version" => acc_r -> version := true
        | "-V" => acc_r -> version := true
        | "-e" => bad_exclude(s)
        | "--exclude" => bad_exclude(s)
        | _ => let
          val new_acc = process_short(witness(s), acc)
          val _ = acc_r -> help := new_acc.help
          val _ = acc_r -> no_parallel := new_acc.no_parallel
          val _ = acc_r -> version := new_acc.version
          val _ = acc_r -> no_table := new_acc.no_table
        in
          ()
        end
    else
      if not(is_first) then
        acc_r -> includes := list_cons(process(s), acc.includes)
      else
        ()
  in
    !acc_r
  end

fun process_excludes(s : string, acc : command_line) : command_line =
  let
    var acc_r = ref<command_line>(acc)
    val () = if is_flag(s) then
      ( println!("\33[31mError:\33[0m flag " + s + " found where a directory name was expected")
      ; exit(0)
      ; ()
      )
    else
      acc_r -> excludes := list_cons(s, acc.excludes)
  in
    !acc_r
  end

// TODO minor problem in how exclusions are handled with short flags
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
