# Synopsis

This is a script to create a one-to-one association of spare games (on Steam) to friends (on Steam). 
To do that, we use the [gwaf](https://github.com/nikitaDanilenko/gwaf) library and compute a maximum matching in the
graph theoretic sense. "Maximum" means "as many as possible", and "matching" describes the one-to-one assignment.

# Usage

1. Put your spare games one per line in a file called `games.txt` in the main folder.
   The expected format is `<game name> -> <appId>`, and there is no error correction,
   i.e. remove missing app ids beforehand.
2. Put friends you wish to exclude from the matching one per line in a file called `exclude.txt` in the main folder.
   You can use `#` for line comments for, say, a short explanation of why you exclude a friend.
3. Run `stack ghci` from the main folder.
4. Run `:l src/Matcher.hs` in the GHCi prompt.
5. Gather the following values:
    1. Your Steam id
    2. Your Steam API key
6. Run `withArgs [ <yourSteamId>, <apiKey> ] startWorkflow`
7. If everything works, the script will fetch your friends, their wishlists, and compute a matching.
   It will then ask you to fill out the links in a file `assignments.txt`.
   Once you finished, the script will continue, and create two additional files:
   1. `messages.txt` contains the messages that you can send to your friends.
      The messages contain the links to the games, and differentiate between two languages.
   2. `GiftsThisYear.txt` contains a list of game assignments with an obfuscated link to the gift.
      The link contains the last five letters of the actual link, and may help you find a game that was not redeemed
      at a later stage.
