import Options.Applicative hiding (helper)
import Data.Monoid (mconcat)

data FloorOpts = FloorOpts
  { width :: Double
  , height :: Double
  , cost :: Double} deriving (Show)

widthParser :: Parser Double
widthParser = option auto
            (long "width"
             <> short 'w'
             <> metavar "DOUBLE"
             <> help "Takes in a wdith of W feet")

heightParser :: Parser Double
heightParser = option auto
             (long "height"
              <> short 'h'
              <> metavar "DOUBLE"
              <> help "Takes in a height of H feet")

costParser :: Parser Double
costParser = option auto 
            (long "cost"
            <> short 'c'
            <> metavar "DOUBLE"
            <> help "Takes in a cost in $/sq foot.")

optsParser :: Parser FloorOpts 
optsParser = FloorOpts 
  <$> widthParser
  <*> heightParser
  <*> costParser

opts :: ParserInfo FloorOpts
opts = info (helper <*> optsParser)
              (fullDesc
               <> progDesc "Calculates square tiling cost"
               <> header "A square footage tiling cost calculator")

-- We redefine the helper here so we don't lost the -h option
helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , help "Show this help text"
  , hidden ]

greet :: FloorOpts -> IO () 
greet FloorOpts{width = w,  height = h,  cost = c } = print $ w * h * c

main :: IO ()
main =  execParser opts >>= greet
