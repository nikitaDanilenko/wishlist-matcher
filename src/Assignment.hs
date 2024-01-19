module Assignment where

data Assignment = Assignment {
    friendName        :: String,
    friendCountryCode :: Maybe String,
    gameName          :: String,
    link              :: String
} deriving (Show, Read)

initial :: String -> Maybe String -> String -> Assignment
initial friendName countryCode gameName = Assignment friendName countryCode gameName "<link>"

takeLastFive :: String -> String
takeLastFive = reverse . take 5 . reverse

obfuscate :: Assignment -> Assignment
obfuscate assignment = assignment { link = concat ["...", takeLastFive (link assignment)] }

createMessage :: Assignment -> String
createMessage assignment = case friendCountryCode assignment of
  Just "DE" -> germanMessage assignment
  _         -> englishMessage assignment

germanMessage :: Assignment -> String
germanMessage assignment = unlines [
    "Frohe Festtage, " ++ friendName assignment ++ "!",
    "",
    "Hier ist ein Spielgeschenk von Deiner Wunschliste! Viel Spaß damit!",
    "",
    link assignment,
    "",
    "Viele Grüße",
    "",
    "Denoevyn"
  ]

englishMessage :: Assignment -> String
englishMessage assignment = unlines [
    "Happy holidays, " ++ friendName assignment ++ "!",
    "",
    "Here is a game gift from your wishlist for you to enjoy!",
    "",
    link assignment,
    "",
    "Best regards and season's greetings",
    "",
    "Denoevyn"
  ]
