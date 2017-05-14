#' Create a connection to tinycards
#'
#' @return list connection with slots useragent, connected, and authenticated
#' @export
#'
create_connection <- function( ) {
	
	connection <- list()
	connection$useragent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"
	connection$connected <- F
	connection$authenticated <- F
	connection$userid <- NA
	
	# get a session cookie
	resp <- httr::GET(
		url = "https://tinycards.duolingo.com/",
		httr::add_headers( "User-Agent" = connection$useragent )
	)
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards initial connection failed [%s]\n%s\n", 
				httr::status_code( resp ),
				httr::content( resp, "text", encoding = "UTF-8" )
			),
			call. = FALSE
		)
	}
	connection$connected <- T
	connection
}

#' Login to tidycards
#'
#' @param connection connection
#' @param username username
#' @param password password
#' @return list connection
#' @export
#'
login <- function( connection, username, password ) {
	
	# login
	resp <- httr::POST(
		url = paste0( api_base_url(), "/login" ),
		body = list(
			error = F,
			facebookError = F,
			hasResetPassword = F,
			identifier = username,
			isResettingPassword = F,
			password = password
		),
		encode = "json"
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API authentication failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	
	if( "id" %in% names( parsed ) ) {
		connection$authenticated = T
	}
	else {
		stop(
			sprintf(
				"Tinycards API authentication failed:\n %s\n",
				parsed
			)
		)
	}
	connection$userid <- parsed$id
	connection
}

#' The base API url for tinycards
#' @return character
api_base_url <- function() {
	"https://tinycards.duolingo.com/api/1/"
}

