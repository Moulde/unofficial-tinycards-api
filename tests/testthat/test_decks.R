source( "pretest.R" )
library( httr )
library( jsonlite )

if ( !is.na( TINY_AUTH$username ) ) {
	test_conn <- tinycards::create_connection( )
	test_conn <- tinycards::login( test_conn, TINY_AUTH$username, TINY_AUTH$password )
}




testthat::test_that( "We can list decks", {
	check_auth()
	decks <- tinycards::list_decks( test_conn )
	testthat::expect_equal( is.list( decks ), T )
	if ( length( decks ) == 0 ) {
		testthat::skip( "You have no decks :(." )
	}
	else {
		for ( i in seq_along( decks ) ) {
			n <- names( decks[[ i ]] )
			testthat::expect_equal( "id" %in% n, T )
			testthat::expect_equal( "name" %in% n, T )
			testthat::expect_equal( "image_url" %in% n, T )
		}
	}
})

testthat::test_that( "We can retrieve a deck", {
	check_auth()
	decks <- tinycards::list_decks( test_conn )
	if ( length( decks ) == 0 ) {
		testthat::skip( "You have no decks :(." )
	}
	else {
		id <- decks[[ 1 ]]$id
		deck <- tinycards::load_deck( test_conn, id )
		testthat::expect_equal( "id" %in% names( deck ), T )
	}
})



testthat::test_that( "We can create and delete a deck", {
	check_auth()
	deck <- EXAMPLE_DECK
	new_deck <- tinycards::create_deck( test_conn, deck )
	new_deck_obj <- tinycards::load_deck( test_conn, new_deck$id )
	testthat::expect_equal( "id" %in% names( new_deck_obj ), T )
	tinycards::delete_deck( test_conn, new_deck$id )
})



testthat::test_that( "We can update a deck", {
	check_auth()
	deck <- EXAMPLE_DECK
	new_deck <- tinycards::create_deck( test_conn, deck )
	new_deck_obj <- tinycards::load_deck( test_conn, new_deck$id )
	new_deck_obj$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text <- "BUTT"
	tinycards::update_deck( test_conn, new_deck_obj )
	updated_deck_obj <- tinycards::load_deck( test_conn, new_deck_obj$id )
	testthat::expect_equal( updated_deck_obj$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text, "BUTT" )
	tinycards::delete_deck( test_conn, updated_deck_obj$id )
})



