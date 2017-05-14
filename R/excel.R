`%>%` <- purrr::`%>%`


#' Convert a deck to an excel file
#' @param deck deck
#' @param filename where to put the file
#' @param overwrite whether to overwrite an existing file
#' @return boolean success
#' @export
deck2excel <- function( deck, filename, overwrite = F ) {
	
	if ( file.exists( filename ) && overwrite == F ) {
		stop( sprintf( "File %s exists and overwrite set to FALSE", filename ) )
	}
	
	wb <- xlsx::createWorkbook()
	sheet <- xlsx::createSheet( wb, sheetName = "deck" )
	row <- xlsx::createRow( sheet, rowIndex = 1 )
	
	make_cell <- function( val, row, col_i ) {
		cell <- xlsx::createCell( row, colIndex = col_i )[[ 1, 1 ]]
		xlsx::setCellValue( cell, val )
	}
	make_cell( "PROPERTY", row, 1 )
	make_cell( "VALUE", row, 2 )
	
	
	lead_in_slots <- c(
		"name",
		"id",
		"description",
		"imageUrl",
		"private",
		"ttsLanguages",
		"blacklistedSideIndices",
		"blacklistedQuestionTypes",
		"gradingModes"
	)
	
	for ( i in seq_along( lead_in_slots ) ) {
		slot <- lead_in_slots[ i ]
		val <- deck[[ slot ]]
		if ( slot == "private" ) {
			val <- jsonlite::toJSON( val, auto_unbox = T )
		}
		else if ( is.list( val ) ) {
			val <- jsonlite::toJSON( val )
		}
		row <- xlsx::createRow( sheet, rowIndex = i + 1 )
		make_cell( slot, row, 1 )
		make_cell( val, row, 2 )
	}
	

	row_headers <- c(
		"side1_concept1",
		"side1_concept2",
		"side1_concept3",
		"side2_concept1",
		"side2_concept2",
		"side2_concept3",
		"c_id",
		"c_uid",
		"s1_id",
		"s2_id",
		"s1_uid",
		"s2_uid",
		"s1_c1_id",
		"s1_c2_id",
		"s1_c3_id",
		"s2_c1_id",
		"s2_c2_id",
		"s2_c3_id",
		"s1_c1_uid",
		"s1_c2_uid",
		"s1_c3_uid",
		"s2_c1_uid",
		"s2_c2_uid",
		"s2_c3_uid",
		"s1_c1_fid",
		"s1_c2_fid",
		"s1_c3_fid",
		"s2_c1_fid",
		"s2_c2_fid",
		"s2_c3_fid"
	)
	row <- xlsx::createRow( sheet, rowIndex = length( lead_in_slots ) + 3 )
	for ( ci in seq_along( row_headers ) ) {
		make_cell( row_headers[ ci ], row, ci )
	}

	

	for ( i in seq_along( deck$cards ) ) {
		row <- xlsx::createRow( sheet, rowIndex = length( lead_in_slots ) + 3 + i )
		card <- get_card( deck, i )
		card %>% get_side( 1 ) %>% get_concept( 1 ) %>% get_concept_contents( ) %>% make_cell( row, 1 )
		card %>% get_side( 1 ) %>% get_concept( 2 ) %>% get_concept_contents( ) %>% make_cell( row, 2 )
		card %>% get_side( 1 ) %>% get_concept( 3 ) %>% get_concept_contents( ) %>% make_cell( row, 3 )
		card %>% get_side( 2 ) %>% get_concept( 1 ) %>% get_concept_contents( ) %>% make_cell( row, 4 )
		card %>% get_side( 2 ) %>% get_concept( 2 ) %>% get_concept_contents( ) %>% make_cell( row, 5 )
		card %>% get_side( 2 ) %>% get_concept( 3 ) %>% get_concept_contents( ) %>% make_cell( row, 6 )
		card$id %>% make_cell( row, 7 )
		card$userId %>% make_cell( row, 8 )
		card %>% get_side( 1 ) %>% ( function( l ) l$id ) %>% make_cell( row, 9 )
		card %>% get_side( 2 ) %>% ( function( l ) l$id ) %>% make_cell( row, 10 )
		card %>% get_side( 1 ) %>% ( function( l ) l$userId ) %>% make_cell( row, 11 )
		card %>% get_side( 2 ) %>% ( function( l ) l$userId ) %>% make_cell( row, 12 )
		card %>% get_side( 1 ) %>% get_concept( 1 ) %>% get_concept_id( ) %>% make_cell( row, 13 )
		card %>% get_side( 1 ) %>% get_concept( 2 ) %>% get_concept_id( ) %>% make_cell( row, 14 )
		card %>% get_side( 1 ) %>% get_concept( 3 ) %>% get_concept_id( ) %>% make_cell( row, 15 )
		card %>% get_side( 2 ) %>% get_concept( 1 ) %>% get_concept_id( ) %>% make_cell( row, 16 )
		card %>% get_side( 2 ) %>% get_concept( 2 ) %>% get_concept_id( ) %>% make_cell( row, 17 )
		card %>% get_side( 2 ) %>% get_concept( 3 ) %>% get_concept_id( ) %>% make_cell( row, 18 )
		card %>% get_side( 1 ) %>% get_concept( 1 ) %>% get_concept_user_id( ) %>% make_cell( row, 19 )
		card %>% get_side( 1 ) %>% get_concept( 2 ) %>% get_concept_user_id( ) %>% make_cell( row, 20 )
		card %>% get_side( 1 ) %>% get_concept( 3 ) %>% get_concept_user_id( ) %>% make_cell( row, 21 )
		card %>% get_side( 2 ) %>% get_concept( 1 ) %>% get_concept_user_id( ) %>% make_cell( row, 22 )
		card %>% get_side( 2 ) %>% get_concept( 2 ) %>% get_concept_user_id( ) %>% make_cell( row, 23 )
		card %>% get_side( 2 ) %>% get_concept( 3 ) %>% get_concept_user_id( ) %>% make_cell( row, 24 )
		card %>% get_side( 1 ) %>% get_concept( 1 ) %>% get_concept_fact_id( ) %>% make_cell( row, 25 )
		card %>% get_side( 1 ) %>% get_concept( 2 ) %>% get_concept_fact_id( ) %>% make_cell( row, 26 )
		card %>% get_side( 1 ) %>% get_concept( 3 ) %>% get_concept_fact_id( ) %>% make_cell( row, 27 )
		card %>% get_side( 2 ) %>% get_concept( 1 ) %>% get_concept_fact_id( ) %>% make_cell( row, 28 )
		card %>% get_side( 2 ) %>% get_concept( 2 ) %>% get_concept_fact_id( ) %>% make_cell( row, 29 )
		card %>% get_side( 2 ) %>% get_concept( 3 ) %>% get_concept_fact_id( ) %>% make_cell( row, 30 )
	}
	
	xlsx::saveWorkbook( wb, filename )
}





#' Convert an excel file to a deck
#' @param filename where the file is
#' @return deck
#' @export
excel2deck <- function( filename ) {
	xlmeta <- readxl::read_excel( filename )[ 1:9, 1:2 ]
	xlcards <- readxl::read_excel( filename, skip = 10 )
	deck <- list(
		name = xlmeta[ xlmeta$PROPERTY == "name", 2 ][[ 1 ]],
		description = xlmeta[ xlmeta$PROPERTY == "description", 2 ][[ 1 ]],
		imageUrl = xlmeta[ xlmeta$PROPERTY == "imageUrl", 2 ][[ 1 ]],
		private = xlmeta[ xlmeta$PROPERTY == "private", 2 ][[ 1 ]] %>% jsonlite::fromJSON(),
		ttsLanguages = xlmeta[ xlmeta$PROPERTY == "ttsLanguages", 2 ][[ 1 ]] %>% jsonlite::fromJSON(),
		blacklistedSideIndices = xlmeta[ xlmeta$PROPERTY == "blacklistedSideIndices", 2 ][[ 1 ]] %>% jsonlite::fromJSON(),
		blacklistedQuestionTypes = xlmeta[ xlmeta$PROPERTY == "blacklistedQuestionTypes", 2 ][[ 1 ]] %>% jsonlite::fromJSON(),
		gradingModes = xlmeta[ xlmeta$PROPERTY == "gradingModes", 2 ][[ 1 ]] %>% jsonlite::fromJSON(),
		cards = list()
	)
	if ( !is.na( xlmeta[ xlmeta$PROPERTY == "id", 2 ][[ 1 ]] ) ) deck$id <- xlmeta[ xlmeta$PROPERTY == "id", 2 ][[ 1 ]]
	for ( i in 1:nrow( xlcards ) ) {
		xlcard <- xlcards[ i, ]
		if ( all( is.na( unname( unlist( xlcard ) ) ) ) ) {
			next
		}
		deck$cards[[ i ]] <- list(
			sides = list(
				list(
					concepts = list( )
				),
				list(
					concepts = list( )
				)
			)
		)
		if ( !is.na( xlcard$c_id ) ) deck$cards[[ i ]]$id = xlcard$c_id
		if ( !is.na( xlcard$c_uid ) ) deck$cards[[ i ]]$userId = xlcard$c_uid
		
		for ( side_number in 1:2 ) {
			items <- c( "id" = "s%d_id", "userId" = "s%d_uid" )
			for ( k in names( items ) ) {
				xlk <- sprintf( items[ k ], side_number )
				if ( !is.na( xlcard[[ xlk ]] ) ) {
					deck$cards[[ i ]]$sides[[ side_number ]][ k ] = xlcard[[ xlk ]]
				}
			}
			for ( concept_number in 1:3 ) {
				this_concept <- list( fact = list() )
				base_items <- c( "id" = "s%d_c%d_id", "userId" = "s%d_c%d_uid" )
				fact_items <- c( "id" = "s%d_c%d_fid", "text" = "side%d_concept%d" )
				for ( k in names( base_items ) ) {
					xlk <- sprintf( base_items[ k ], side_number, concept_number )
					if ( !is.na( xlcard[[ xlk ]] ) ) {
						this_concept[ k ] = xlcard[[ xlk ]]
					}
				}
				for ( k in names( fact_items ) ) {
					xlk <- sprintf( fact_items[ k ], side_number, concept_number )
					if ( !is.na( xlcard[[ xlk ]] ) ) {
						this_concept$fact[ k ] = xlcard[[ xlk ]]
					}
					if ( "text" %in% names( this_concept$fact ) ) {
						if ( !is.na( stringr::str_match( this_concept$fact$text, "^https?[^ ]+$" )[ 1 ] ) ) {
							this_concept$fact$imageUrl <- this_concept$fact$text
							this_concept$fact$text <- NULL
							this_concept$fact$type <- "IMAGE"
						}
						else {
							this_concept$fact$type <- "TEXT"
						}
					}
				}
				if ( names( this_concept ) != "fact" || !is.null( names( this_concept$fact ) ) ) {
					deck$cards[[ i ]]$sides[[ side_number ]]$concepts[[ concept_number ]] <- this_concept
				}
			}
		}
	}
	deck
}
