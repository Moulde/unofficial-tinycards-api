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

testthat::test_that( "We can write read a deck from an excel file", {
	deck <- tinycards::excel2deck( "example_deck.xlsx" )

	testthat::expect_equal( deck$name, "Example saved excel deck" )
	testthat::expect_equal( deck$id, "lol" )
	testthat::expect_equal( deck$description, "This is for the unit test" )
	testthat::expect_equal( deck$imageUrl, "a" )
	testthat::expect_equal( deck$private, T )
	testthat::expect_equal( deck$ttsLanguages, list() )
	testthat::expect_equal( deck$blacklistedSideIndices, list() )
	testthat::expect_equal( deck$blacklistedQuestionTypes, list() )
	testthat::expect_equal( deck$gradingModes, list() )
	testthat::expect_equal( length( deck$cards ), 3 )
	
	
	# card 1
	testthat::expect_equal( deck$cards[[ 1 ]]$id, "a" )
	testthat::expect_equal( deck$cards[[ 1 ]]$userId, "AAA" )
	testthat::expect_equal( length( deck$cards[[ 1 ]]$sides ), 2 )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$id, "d" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$userId, "DDD" )
	testthat::expect_equal( length( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts ), 3 )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$id, "j" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$userId, "JJJ" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text, "one" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$id, "B" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$id, "m" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$userId, "MMM" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$text, "two" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$id, "E" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$id, "p" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$userId, "PPP" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$text, "three" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$id, "H" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$id, "g" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$userId, "GGG" )
	testthat::expect_equal( length( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts ), 3 )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$id, "s" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$userId, "SSS" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$text, "four" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$id, "K" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$id, "v" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$userId, "VVV" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$text, "five" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$id, "N" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$id, "y" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$userId, "YYY" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$text, "six" )
	testthat::expect_equal( deck$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$id, "Q" )
	
	
	# card 2
	testthat::expect_equal( deck$cards[[ 2 ]]$id, "b" )
	testthat::expect_equal( deck$cards[[ 2 ]]$userId, "BBB" )
	testthat::expect_equal( length( deck$cards[[ 2 ]]$sides ), 2 )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$id, "e" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$userId, "EEE" )
	testthat::expect_equal( length( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts ), 3 )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$id, "k" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$userId, "KK" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text, "seven" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$id, "C" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$id, "n" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$userId, "NNN" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$text, "eight" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$id, "F" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$id, "q" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$userId, "QQQ" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$text, "nine" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 1 ]]$concepts[[ 3 ]]$fact$id, "I" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$id, "h" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$userId, "HHH" )
	testthat::expect_equal( length( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts ), 3 )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$id, "t" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$userId, "TTT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$text, "ten" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$id, "L" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$id, "w" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$userId, "WWW" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$text, "eleven" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$id, "O" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$id, "z" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$userId, "ZZZ" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$type, "TEXT" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$text, "twelve" )
	testthat::expect_equal( deck$cards[[ 2 ]]$sides[[ 2 ]]$concepts[[ 3 ]]$fact$id, "R" )
	
	
	# card 3
	testthat::expect_equal( deck$cards[[ 3 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$id, "o" )
	testthat::expect_equal( deck$cards[[ 3 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$userId, "OOO" )
	testthat::expect_equal( deck$cards[[ 3 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$type, "IMAGE" )
	testthat::expect_equal( deck$cards[[ 3 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$imageUrl, "http://image" )
	testthat::expect_equal( deck$cards[[ 3 ]]$sides[[ 1 ]]$concepts[[ 2 ]]$fact$id, "G" )
	
})



testthat::test_that( "We can create a new deck from a spreadsheet", {
	check_auth()
	deck <- tinycards::excel2deck( "example_deck2.xlsx" )
	new_deck <- tinycards::create_deck( test_conn, deck )
	new_deck_obj <- tinycards::load_deck( test_conn, new_deck$id )
	tinycards::delete_deck( test_conn, new_deck_obj$id )
	expect_equal( new_deck_obj$cards[[ 1 ]]$sides[[ 2 ]]$concepts[[ 2 ]]$fact$text, "C" )
	expect_equal( new_deck_obj$cards[[ 3 ]]$sides[[ 2 ]]$concepts[[ 1 ]]$fact$type, "IMAGE" )
})





testthat::test_that( "We can update an existing deck from a spreadsheet", {
	check_auth()
	deck <- EXAMPLE_DECK
	new_deck <- tinycards::create_deck( test_conn, deck )
	new_deck_obj <- tinycards::load_deck( test_conn, new_deck$id )
	fname <- tempfile( fileext = ".xlsx" )
	tinycards::deck2excel( new_deck_obj, fname )
	wb <- xlsx::loadWorkbook( fname )
	sheet <- xlsx::getSheets( wb )[[ 1 ]]
	row <- xlsx::getRows( sheet, 13 )
	cell <- xlsx::getCells( row, 1 )[[ 1 ]]
	xlsx::setCellValue( cell, "UPDATED CARD 1 FRONT" )
	fname2 <- tempfile( fileext = ".xlsx" )
	xlsx::saveWorkbook( wb, fname2 )
	updated_xl_deck <- tinycards::excel2deck( fname2 )
	tinycards::update_deck( test_conn, updated_xl_deck )
	updated_deck <- tinycards::load_deck( test_conn, updated_xl_deck$id )
	tinycards::delete_deck( test_conn, updated_deck$id )
	expect_equal( updated_xl_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$id, updated_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$id )
	#expect_equal( updated_xl_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$id, updated_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$id )
	expect_equal( updated_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text, "UPDATED CARD 1 FRONT" )
})