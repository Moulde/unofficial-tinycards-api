source( "pretest.R" )
library( tidyverse )
library( httr )
library( jsonlite )
library( readxl )
library( xlsx )

if ( !is.na( TINY_AUTH$username ) ) {
	test_conn <- tinycards::create_connection( )
	test_conn <- tinycards::login( test_conn, TINY_AUTH$username, TINY_AUTH$password )
}



testthat::test_that( "We can write a deck to an excel file", {
	check_auth()
	deck <- EXAMPLE_DECK
	new_deck <- tinycards::create_deck( test_conn, deck )
	new_deck_obj <- tinycards::load_deck( test_conn, new_deck$id )
	tinycards::delete_deck( test_conn, new_deck_obj$id )
	fname <- tempfile( fileext = ".xlsx" )
	tinycards::deck2excel( new_deck_obj, fname )
	meta <- readxl::read_excel( fname )[ 1:9, 1:2 ]
	cards <- readxl::read_excel( fname, skip = 10 )
	testthat::expect_equal( meta[ meta$PROPERTY == "name", 2 ][[ 1 ]], new_deck_obj$name )
	testthat::expect_equal( meta[ meta$PROPERTY == "description", 2 ][[ 1 ]], new_deck_obj$description )
	testthat::expect_equal( cards[ 2, 1 ][[ 1 ]], "FRONT OF CARD 2" )
})