module ScmTypes where

import Text.ParserCombinators.Parsec (ParseError)

import ScmEval (unwordsList)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotAFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " show form
showError (NotAFunction message func)   = message ++ ": " show func
showError(NumArgs expected found)       = "Expected " ++ show expected
                                        ++ " args; found values " ++ unwordsList found
