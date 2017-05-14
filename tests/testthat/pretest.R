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

EXAMPLE_DECK <- list(
	name = "This is an automated test",
	description = "This is an automated test",
	imageUrl = "https://www.wikipedia.org/portal/wikipedia.org/assets/img/Wikipedia_wordmark@2x.png",
	private = T,
	ttsLanguages = list(),
	blacklistedSideIndices = list(),
	blacklistedQuestionTypes = list(),
	gradingModes = list(),
	cards = list(
		list(
			sides = list(
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "FRONT OF CARD 1"
							)
						)
					)
				),
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "BACK OF CARD 1"
							)
						)
					)
				)
			)
		),
		list(
			sides = list(
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "FRONT OF CARD 2"
							)
						)
					)
				),
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "BACK OF CARD 2"
							)
						)
					)
				)
			)
		),
		list(
			sides = list(
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "FRONT OF CARD 3"
							)
						)
					)
				),
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "BACK OF CARD 3"
							)
						)
					)
				)
			)
		),
		list(
			sides = list(
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "FRONT OF CARD 4"
							)
						)
					)
				),
				list(
					concepts = list(
						list(
							fact = list(
								type = "TEXT",
								text = "BACK OF CARD 4"
							)
						)
					)
				)
			)
		)
	)
)
