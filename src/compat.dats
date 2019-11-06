#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "DATS/shared.dats"
#include "$PATSHOMELOCS/edit-distance-0.5.0/DATS/edit-distance.dats"
#include "DATS/utils.dats"
#include "DATS/error.dats"
#include "DATS/html.dats"

implement main0 (argc, argv) =
  let
    val cli = @{ version = false
               , help = false
               , no_table = false
               , html = false
               , no_parallel = false
               , no_colorize = false
               , skip_links = false
               , verbose = false
               , excludes = list_nil()
               , includes = list_nil()
               } : command_line
    val parsed = get_cli(argc, argv, 0, false, cli)
    val () = check_cli(parsed)
  in
    if parsed.help then
      (help() ; exit(0))
    else
      if parsed.version then
        (version() ; exit(0))
      else
        let
          var result = if length(parsed.includes) > 0 then
            let
              var x = empty_contents()
              val () = map_stream(x, parsed.includes, parsed.excludes, parsed.verbose)
            in
              x
            end
          else
            let
              var x = empty_contents()
              val () = map_stream(x, list_cons(".", list_nil()), parsed.excludes, parsed.verbose)
            in
              x
            end
        in
          if parsed.no_table then
            print(make_output(result, not(cli.no_colorize)))
          else
            if parsed.html then
              print(make_html(result))
            else
              print(make_table(result, not(cli.no_colorize)))
        end
  end
