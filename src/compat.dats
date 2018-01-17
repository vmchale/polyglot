#include "src/shared.dats"

implement main0 (argc, argv) =
  let
    val cli = @{ version = false
               , help = false
               , no_table = false
               , no_parallel = false
               , excludes = list_nil()
               , includes = list_nil()
               } : command_line
    val parsed = get_cli(argc, argv, 0, false, cli)
  in
    if parsed.help then
      (help() ; exit(0))
    else
      if parsed.version then
        (version() ; exit(0))
      else
        let
          var result = if length(parsed.includes) > 0 then
            map_stream(empty_contents(), parsed.includes, parsed.excludes)
          else
            map_stream(empty_contents(), list_cons(".", list_nil()), parsed.excludes)
        in
          if parsed.no_table then
            print(make_output(result))
          else
            print(make_table(result))
        end
  end