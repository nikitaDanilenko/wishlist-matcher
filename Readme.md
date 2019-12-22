# Synopsis

This is a simple script to create a one-to-one association of spare games (on Steam) to friends (on Steam). 
To do that, we use the [gwaf](https://github.com/nikitaDanilenko/gwaf) library and compute a maximum matching in the
graph theoretic sense. "Maximum" means "as many as possible", and "matching" describes the one-to-one assignment.

In its current form, the `json` files containing the Steam wishlists need to be supplied externally (somehow).
This may change in the future, if the Steam API changes. In its current form, the script does not use the Steam API key at all.

# Usage

1. Put the wishlists in separate files in the folder `wishlists`, where each file is called `<SteamID>.json`.
2. Put your spare games one per line in a file called `games.txt` in the main folder.
3. Call `main` in the file `Matcher.hs` (or compile and call the executable).

The script will then print an assignment. If you decide that you want to keep a game, simply remove it from the game list
and run the script again.
