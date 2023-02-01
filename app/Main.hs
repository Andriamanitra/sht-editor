module Main where
import System.IO
import System.Environment ( getArgs )
import qualified System.Console.ANSI as Terminal
import Data.Maybe (fromMaybe)
import System.Exit ( exitFailure, exitSuccess )



--- MODEL

data CursorPos = CursorPos Int Int
type TerminalSize = (Int, Int)

data Model = Model
    { title :: String
    , terminalSize :: TerminalSize
    , currentFileName :: Maybe String
    , textBuffer :: [String]
    , cursorPos :: CursorPos
    }



--- UPDATE

data Msg =
    AddStr Int Int String
    | TypeChar Char
    | Enter
    | Backspace
    | Resize TerminalSize

update :: Msg -> Model -> Model
update (TypeChar c) model =
    update (AddStr curR curC [c]) model { cursorPos = nextPos }
    where
        CursorPos curR curC = cursorPos model
        nextPos = CursorPos curR (curC + 1)

update (AddStr r c txt) model =
    model { textBuffer = updatedTextBuffer }
    where
        (pre, after) = splitAt r (textBuffer model)
        updatedTextBuffer = case after of
            [] -> pre ++ [txt]
            (x:xs) -> pre ++ [take c x ++ txt ++ drop c x] ++ xs

update Enter model =
    model { cursorPos = CursorPos (curR + 1) 0, textBuffer = updatedTextBuffer }
    where 
        CursorPos curR _curC = cursorPos model
        (pre, after) = splitAt (curR + 1) (textBuffer model)
        updatedTextBuffer = pre ++ [""] ++ after

update Backspace model =
    if curR == 0 && curC == 0
    then model
    else model { cursorPos = nextPos, textBuffer = updatedTextBuffer}
    where
        tbuf = textBuffer model
        prevRow = tbuf !! (curR - 1)
        curRow = tbuf !! curR
        updatedTextBuffer =
            if curC == 0
            then take (curR - 1) tbuf
                    ++ [prevRow ++ curRow]
                    ++ drop (curR + 1) tbuf
            else take curR tbuf
                    ++ [take (curC - 1) curRow ++ drop curC curRow]
                    ++ drop (curR + 1) tbuf
        CursorPos curR curC = cursorPos model
        nextPos =
            if curC == 0
            then CursorPos (curR - 1) (length prevRow)
            else CursorPos curR (curC - 1)

update (Resize newSize) model =
    model { terminalSize = newSize }



--- VIEW

view :: Model -> IO ()
view model = do
    let Model title _tsize _ textBuffer (CursorPos curR curC) = model
    let titleStyle = [ Terminal.SetColor Terminal.Background Terminal.Dull Terminal.Blue ]
    Terminal.clearScreen

    Terminal.setSGR titleStyle
    Terminal.setTitle title
    Terminal.setCursorPosition 0 0
    Terminal.clearFromCursorToLineEnd
    putStr (' ':title)

    Terminal.setCursorPosition 1 0
    Terminal.setSGR [Terminal.Reset]
    putStr $ unlines textBuffer

    Terminal.setCursorPosition (curR + 1) curC
    hFlush stdout



--- MAIN

data Args = Args
    { fileName :: Maybe String
    , textBuf :: [String] 
    }

parseArgs :: [String] -> IO Args
parseArgs ("-h":_) = help >> exitSuccess
parseArgs [] = pure $ Args Nothing [""]
parseArgs [fname] = do
    handle <- openFile fname ReadMode
    contents <- hGetContents handle
    pure $ Args (Just fname) (lines contents)
parseArgs _ = help >> exitFailure

help :: IO ()
help = putStrLn "Usage: sht [-h] [FILE]"

getMsg :: IO Msg
getMsg = do
    key <- getChar
    case key of
        '\ESC' -> error "key not implemented"
        '\DEL' -> pure Backspace
        '\n' -> pure Enter
        ch -> pure $ TypeChar ch

resetScreen :: IO ()
resetScreen = do
    Terminal.setSGR [Terminal.Reset]
    Terminal.clearScreen
    Terminal.setCursorPosition 0 0

mainLoop :: Model -> IO ()
mainLoop model = do
    view model
    msg <- getMsg
    let updatedModel = update msg model
    mainLoop updatedModel

main :: IO ()
main = do
    args <- fmap parseArgs getArgs
    Args fname textBuf <- args
    let title = "sht: scuffed haskell text editor"
    tsize <- fromMaybe (error "failed to read terminal size") <$> Terminal.getTerminalSize
    let initialModel = Model title tsize fname textBuf (CursorPos 0 0)
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    mainLoop initialModel
    resetScreen
