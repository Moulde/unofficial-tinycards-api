# unofficial-tinycards-api
This is an unofficial R package to access the Tinycards API. It was created because their interface for creating and maintaining decks is trash.

## installation

```{r}
devtools::install_github( "dirtytrashgirl/unofficial-tinycards-api" )
```

## usage

```{r}

# connect to tinycards
conn <- tinycards::create_connection( )
conn <- tinycards::login( conn, USERNAME, PASSWORD )

# list all decks
decks <- tinycards::list_decks( conn )

# load the first deck
my_deck <- tinycards::load_deck( conn, decks[[ 1 ]]$id )

# make a change to a deck and update
my_deck$cards[[ 1 ]]$sides[[ 1 ]]$concepts[[ 1 ]]$fact$text <- "UPDATED TEXT FOR CARD 1 SIDE 1 FACT 1"
tinycards::update_deck( conn, my_deck )

# coming soon: create and update decks from excel files
deck2excel( my_deck, "~/desktop/my_deck.xlsx" )
# make changes in excel file...
my_deck <- excel2deck( "~/desktop/my_deck.xlsx" )
tinycards::update_deck( conn, my_deck )

```


## testing prerequisites

Create `~/tinycards_auth.json` like so:

```{json}
{
	"username":"<YOUR USERNAME>",
	"password":"<YOUR PASSWORD>"
}
```