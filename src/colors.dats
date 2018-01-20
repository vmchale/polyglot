#include "share/atspre_staload.hats"

staload "libats/ML/DATS/string.dats"

fun cyan(s : string) : string =
  "\33[36m" + s + "\33[0m"

fun magenta(s : string) : string =
  "\33[35m" + s + "\33[0m"

fun yellow(s : string) : string =
  "\33[33m" + s + "\33[0m"

fun red(s : string) : string =
  "\33[31m" + s + "\33[0m"