module CodeError(CodeError,
                 findErrors) where

data CodeError = CodeError
                 deriving (Eq, Ord, Show)

findErrors :: String -> [CodeError]
findErrors code = [CodeError]
