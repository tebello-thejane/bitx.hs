module Network.Bitcoin.BitX.Internal
    (
    chopUpToPrime
    )
where

chopUpToPrime :: String -> String
chopUpToPrime [] = []
chopUpToPrime ('\'' : cs) = cs
chopUpToPrime (_ : cs) = chopUpToPrime cs

