parse (Chapter_07.return 1) "abc"
parse failure "abc"
parse item ""
parse item "abc"

parse (item +++ return' 'd') "abc"
parse (failure +++ return' 'd') "abc"
parse (failure +++ return' 'd') "abc"