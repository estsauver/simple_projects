import Options.Applicative


type Percent = Int
data MortgageOpts = MortgageOpts
  { principal :: Double
  , rate :: Double 
  , term :: Int } deriving (Show)


optsParser :: Parser MortgageOpts
optsParser = MortgageOpts
  <$> option auto
      (long "principal"
      <> short 'p'
      <> metavar "DOLLARS"
      <> help "The initial balance of the loan")
  <*> option auto
      (long "rate"
      <> short 'r'
      <> metavar "%, i.e. 10% = 0.10"
      <> help "The principal rate in % pts.")
  <*> option auto
      (long "term"
      <> short 't'
      <> metavar "INT"
      <> help "The number of months for repayment")

opts :: ParserInfo MortgageOpts
opts = info (helper <*> optsParser)
          (fullDesc
          <> progDesc "Calculates mortgage term parents"
          <> header "Calculates the mortgage payments")

monthlyRate :: Double -> Double
monthlyRate r = r / 12.0

calcMortgage :: MortgageOpts -> Double
calcMortgage m = p * (r / (1 - ((1 + r) ^^ (- t))))
  where 
    p = principal m
    r = monthlyRate (rate m)
    t = term m

main = execParser opts >>= fmap print calcMortgage 
