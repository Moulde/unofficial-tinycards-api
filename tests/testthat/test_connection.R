source( "pretest.R" )
library( httr )
library( jsonlite )


testthat::test_that( "We can connect to Tidycards", {
	connection <- tinycards::create_connection( )
	testthat::expect_equal( "connected" %in% names( connection ), T )
	testthat::expect_equal( "authenticated" %in% names( connection ), T )
	testthat::expect_equal( "userid" %in% names( connection ), T )
	testthat::expect_equal( connection$connected, T )
	testthat::expect_equal( connection$authenticated, F )
	testthat::expect_equal( is.na( connection$userid ), T )
})


testthat::test_that( "An error is thrown when bad username/password are provided", {
	connection <- tinycards::create_connection( )
	testthat::expect_error( tinycards::login( connection, "im a dumb butthead", "69" ) )
})


testthat::test_that( "Logging in works", {
	check_auth()
	connection <- tinycards::create_connection( )
	connection <- tinycards::login( connection, TINY_AUTH$username, TINY_AUTH$password )
	testthat::expect_equal( connection$authenticated, T )
	testthat::expect_equal( "userid" %in% names( connection ), T )
	testthat::expect_equal( is.na( connection$userid ), F )
})
