TINY_AUTH <- `if`(
	file.exists( "~/tinycards_auth.json" ),
	jsonlite::read_json( "~/tinycards_auth.json"),
	list( username = NA_character_, password = NA_character_ )
)

check_auth <- function() {
	if ( is.na( TINY_AUTH$username ) ) {
		testthat::skip( "In order to properly test this package, you must provide your tinycards authentication. Please save them to ~/tinycards_auth.json with two slots, `username` and `password`." )
	}
}