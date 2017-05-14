
#' List all your current decks
#' @param connection connection
#' @return decklist each item with slots id, name, and image_url
#' @export
list_decks <- function( connection ) {
	if ( connection$authenticated != T ) {
		stop( "u cant list ur decks without bein authenticated" )
	}
	resp <- httr::GET(
		url = paste0( api_base_url(), "decks" ),
		httr::add_headers( 
			"User-Agent" = connection$useragent,
			"Referer" = "https://tinycards.duolingo.com/profile",
			"Accept" = "application/json, text/plain, */*"
		),
		query = list(
			userId = connection$userid
		)
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API request failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	
	decks <- list()
	if ( "decks" %in% names( parsed ) ) {
		for ( i in 1:length( parsed$decks ) ) {
			decks[[ i ]] <- list(
				id = parsed$decks[[ i ]]$id,
				name = parsed$decks[[ i ]]$name,
				image_url = parsed$decks[[ i ]]$imageUrl
			)
		}
	}
	decks
}






#' Load a deck
#' @param connection connection
#' @param deck_id deck id
#' @return list, all about the deck including its cards
#' @export
load_deck <- function( connection, deck_id ) {
	if ( connection$authenticated != T ) {
		stop( "u cant load a decks without bein authenticated" )
	}
	resp <- httr::GET(
		url = paste0( api_base_url(), "decks/", deck_id ),
		httr::add_headers( 
			"User-Agent" = connection$useragent,
			"Referer" = "https://tinycards.duolingo.com/profile",
			"Accept" = "application/json, text/plain, */*"
		),
		query = list(
			expand = "true"
		)
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API request failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	parsed
}





#' Update a deck
#' @param connection connection
#' @param deck deck object
#' @return boolean success
#' @export
update_deck <- function( connection, deck ) {
	if ( connection$authenticated != T ) {
		stop( "u cant update a deck without bein authenticated" )
	}
	resp <- httr::PATCH(
		url = paste0( api_base_url(), "decks/", deck$id ),
		httr::add_headers( 
			"User-Agent" = connection$useragent,
			"Referer" = "https://tinycards.duolingo.com/profile",
			"Accept" = "application/json, text/plain, */*"
		),
		body = list(
			name = deck$name,
			description = deck$description,
			private = deck$private,
			ttsLanguages = deck$ttsLanguages,
			blacklistedSideIndices = deck$blacklistedSideIndices,
			blacklistedQuestionTypes = deck$blacklistedQuestionTypes,
			gradingModes = deck$gradingModes,
			cards = deck$cards
		),
		encode = "json"
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API request failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	TRUE
}




#' Create a deck
#' @param connection connection
#' @param deck deck object
#' @return boolean success
#' @export
create_deck <- function( connection, deck ) {
	if ( connection$authenticated != T ) {
		stop( "u cant update a deck without bein authenticated" )
	}
	tfile <- tempfile()
	httr::GET( deck$imageUrl, httr::write_disk( tfile, overwrite=TRUE ) )
	resp <- httr::POST(
		url = paste0( api_base_url(), "decks" ),
		httr::add_headers( 
			"User-Agent" = connection$useragent,
			"Referer" = "https://tinycards.duolingo.com/profile",
			"Accept" = "application/json, text/plain, */*"
		),
		body = list(
			name = deck$name,
			description = deck$description,
			private = jsonlite::toJSON( deck$private, auto_unbox = T ),
			ttsLanguages = jsonlite::toJSON( deck$ttsLanguages ),
			blacklistedSideIndices = jsonlite::toJSON( deck$blacklistedSideIndices ),
			blacklistedQuestionTypes = jsonlite::toJSON( deck$blacklistedQuestionTypes ),
			gradingModes = jsonlite::toJSON( deck$gradingModes ),
			imageAttribution = deck$imageUrl,
			imageFile = httr::upload_file( tfile ),
			cards = jsonlite::toJSON( deck$cards, auto_unbox = T )
		),
		encode = "multipart"
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API request failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	list(
		id = parsed$id,
		name = parsed$name,
		image_url = parsed$image_url
	)
}

#' Delete a deck
#' @param connection connection
#' @param deck_id deck id
#' @return boolean success
#' @export
delete_deck <- function( connection, deck_id ) {
	if ( connection$authenticated != T ) {
		stop( "u cant delete a deck without bein authenticated" )
	}
	resp <- httr::DELETE(
		url = paste0( api_base_url(), "decks/", deck_id ),
		httr::add_headers(
			"User-Agent" = connection$useragent,
			"Referer" = "https://tinycards.duolingo.com/profile",
			"Accept" = "application/json, text/plain, */*"
		)
	)
	parsed <- jsonlite::fromJSON( httr::content( resp, "text", encoding = "UTF-8" ), simplifyVector = FALSE )
	if ( httr::http_error( resp ) ) {
		stop(
			sprintf(
				"Tinycards API request failed [%s]\n%s\n", 
				httr::status_code(resp),
				parsed$message
			),
			call. = F
		)
	}
	is.null( parsed )
}



#' Get the Nth card from a deck
#' @param deck deck
#' @param N N
#' @return card
#' @export
get_card <- function( deck, N ) {
	deck$cards[[ N ]]
}

#' Get the Nth side from a card
#' @param card card
#' @param N N
#' @return side
#' @export
get_side <- function( card, N ) {
	card$sides[[ N ]]
}

#' Does this card side have an Nth concept?
#' @param card_side card side
#' @param N M
#' @return TRUE/FALSE
#' @export
has_concept <- function( card_side, N ) {
	N %in% seq_along( card_side$concepts )
}

#' Get the Nth concept from a card side
#' @param card_side card side
#' @param N N
#' @return concept
#' @export
get_concept <- function( card_side, N ) {
	if ( !has_concept( card_side, N ) ) {
		return( list() )
	}
	card_side$concepts[[ N ]]
}


#' Get the contents of a concept
#' @param concept concept
#' @return string
#' @export
get_concept_contents <- function( concept ) {
	if ( is.null( names( concept ) ) ) {
		return( "" )
	}
	else if ( concept$fact$type == "image" ) {
		return( concept$fact$imageUrl )
	}
	else {
		return( concept$fact$text )
	}
}


#' Get the contents of a concept
#' @param concept concept
#' @return string
#' @export
get_concept_contents <- function( concept ) {
	if ( is.null( names( concept ) ) ) {
		return( "" )
	}
	else if ( concept$fact$type == "image" ) {
		return( concept$fact$imageUrl )
	}
	else {
		return( concept$fact$text )
	}
}


#' Get the ID of a concept
#' @param concept concept
#' @return string
#' @export
get_concept_id <- function( concept ) {
	if ( is.null( names( concept ) ) ) {
		return( "" )
	}
	else {
		return( concept$id )
	}
}

#' Get the user ID of a concept
#' @param concept concept
#' @return string
#' @export
get_concept_user_id <- function( concept ) {
	if ( is.null( names( concept ) ) ) {
		return( "" )
	}
	else {
		return( concept$userId )
	}
}


#' Get the fact ID of a concept
#' @param concept concept
#' @return string
#' @export
get_concept_fact_id <- function( concept ) {
	if ( is.null( names( concept ) ) ) {
		return( "" )
	}
	else {
		return( concept$fact$id )
	}
}
