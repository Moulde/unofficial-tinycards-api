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
		"s1_c1f_id",
		"s1_c2f_id",
		"s1_c3f_id",
		"s2_c1f_id",
		"s2_c2f_id",
		"s2_c3f_id"
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
		card %>% get( "id", . ) %>% make_cell( row, 7 )
		card %>% get( "userId", . ) %>% make_cell( row, 8 )
		card %>% get_side( 1 ) %>% get( "id", . ) %>% make_cell( row, 9 )
		card %>% get_side( 2 ) %>% get( "id", . ) %>% make_cell( row, 10 )
		card %>% get_side( 1 ) %>% get( "userId", . ) %>% make_cell( row, 11 )
		card %>% get_side( 2 ) %>% get( "userId", . ) %>% make_cell( row, 12 )
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
	
	# cards
	xlsx::saveWorkbook( wb, filename )
}