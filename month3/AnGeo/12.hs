import Text.Regex.PCRE

f1::String -> String
f1 str = str =~ "^[1-2]+" :: String