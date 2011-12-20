module MakeToken where

import Happstack.Crypto.SHA1

main :: IO ()
main = do
    putStrLn "username: "
    user <- getLine
    putStrLn "password: "
    pass <- getLine
    putStrLn "salt: "
    salt <- getLine
    let h1 = sha1 pass
    let h2 = sha1 $ h1 ++ salt
    putStrLn $ "?username=" ++ user ++ "&timestamp=" ++ salt ++ "&token=" ++ h2
