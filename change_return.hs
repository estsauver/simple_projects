import Options.Applicative
import Data.Monoid (mconcat)
import Data.List.Split
import Text.Regex.Posix


-- All money is represented as cents
type Cents = Int
type Dollars = Int
data ChangeOpts = ChangeOpts
  { cost :: Cents
  , paid :: Cents} deriving (Show)


costParser :: Parser Cents
costParser = option auto
           (long "cost"
           <> short 'c'
           <> metavar "CENTS")

paidParser :: Parser Cents
paidParser = option auto
           (long "paid"
           <> short 'p'
           <> metavar "CENTS"
           <> help "The amount paid") 

optsParser :: Parser ChangeOpts
optsParser = ChangeOpts
  <$> costParser
  <*> paidParser

opts = info (helper <*> optsParser)
          (fullDesc
          <> progDesc "Calculates change due"
          <> header "A change calculator")

calcChange :: ChangeOpts -> IO ()
calcChange ch@ChangeOpts {cost= c, paid =p} = print $ p - c

main = execParser opts >>= calcChange 
