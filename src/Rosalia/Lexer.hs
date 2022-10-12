module Rosalia.Lexer where
import Text.Megaparsec

data Token = TokInt Int
	| TokFloat Float
	| TokString String
	| TokChar Char
	| TokIdent String
	| TokKeyword String
	| TokOperator String
	| TokPunctuation String
	| TokComment String
	| TokWhitespace String
	| TokNewline String
	| TokEOF
	deriving (Show, Eq)

type Lexer = Parsec Void String
