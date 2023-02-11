module Main where
import System.IO
    ( BufferMode(NoBuffering),
      hFlush,
      stdout,
      hSetBuffering,
      stdin,
      hSetEcho,
      hIsReadable,
      hGetContents,
      openFile,
      hReady,
      IOMode(ReadWriteMode) )
import System.Environment ( getArgs )
import qualified System.Console.ANSI as Terminal
import Data.Maybe ( fromMaybe )
import System.Exit ( exitFailure, exitSuccess )
import Data.Char ( isControl )


slice :: Int -> Int -> [a] -> [a]
slice startingFrom width = take width . drop startingFrom

preCurrAfter :: Int -> [a] -> ([a], a, [a])
preCurrAfter idx arr = (take idx arr, arr !! idx, drop (idx + 1) arr)

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
    | MoveCursorRelative CursorPos
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
        CursorPos curR curC = cursorPos model
        tbuf = textBuffer model
        (pre, currentLine, after) = preCurrAfter curR tbuf
        (lineBeforeCursor, lineAfterCursor) = splitAt curC currentLine
        updatedTextBuffer
          | curC < length currentLine = pre ++ [lineBeforeCursor, lineAfterCursor] ++ after
          | otherwise = pre ++ [currentLine, ""] ++ after


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

update (MoveCursorRelative (CursorPos dr dc)) model =
    model { cursorPos = nextPos }
    where
        CursorPos currR currC = cursorPos model
        tbuf = textBuffer model
        numRows = length tbuf
        nextR = min (numRows - 1) (currR + dr)
        nextC = max 0 (currC + dc)
        currRow = tbuf !! nextR
        nextPos
            | nextR < 0 = CursorPos 0 0
            | otherwise = CursorPos nextR (min nextC $ length currRow)




--- VIEW

topBarHeight :: Int
topBarHeight = 1

getViewPort :: Model -> (Int, Int)
getViewPort model = (viewR, viewC)
    where
        (tsizeR, tsizeC) = terminalSize model
        CursorPos currR currC = cursorPos model
        viewR = max 0 $ currR - (tsizeR - topBarHeight) + 2
        viewC = max 0 $ currC - tsizeC + 1

view :: Model -> IO ()
view model = do
    let Model title (tsizeR, tsizeC) _ textBuffer (CursorPos curR curC) = model
    let titleStyle = [ Terminal.SetColor Terminal.Background Terminal.Dull Terminal.Blue ]
    Terminal.clearScreen

    Terminal.setSGR titleStyle
    Terminal.setTitle title
    Terminal.setCursorPosition 0 0
    Terminal.clearFromCursorToLineEnd
    putStr (' ':title)

    Terminal.setCursorPosition topBarHeight 0
    Terminal.setSGR [Terminal.Reset]

    let (viewR, viewC) = getViewPort model
    putStr $ unlines $ map (slice viewC tsizeC) $ slice viewR (tsizeR - topBarHeight - 1) textBuffer

    Terminal.setCursorPosition (topBarHeight + curR - viewR) (curC - viewC)
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
    handle <- openFile fname ReadWriteMode
    readable <- hIsReadable handle
    if readable
    then do
        contents <- hGetContents handle
        pure $ Args (Just fname) (lines contents)
    else pure $ Args (Just fname) [""]

parseArgs _ = help >> exitFailure

help :: IO ()
help = putStrLn "Usage: sht [-h] [FILE]"

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
    where
        getKey' chars = do
            char <- getChar
            more <- hReady stdin
            (if more then getKey' else return) (char:chars)

data Op = OpMsg Msg | SaveFile

getOp :: IO Op
getOp = do
    let sendMsg = pure . OpMsg
    key <- getKey    
    case key of
        -- Arrows
        "\ESC[A" -> sendMsg $ MoveCursorRelative (CursorPos (-1) 0)
        "\ESC[B" -> sendMsg $ MoveCursorRelative (CursorPos 1 0)
        "\ESC[C" -> sendMsg $ MoveCursorRelative (CursorPos 0 1)
        "\ESC[D" -> sendMsg $ MoveCursorRelative (CursorPos 0 (-1))
        -- Home
        "\ESC[H" -> sendMsg $ MoveCursorRelative (CursorPos 0 (-1337))
        -- End
        "\ESC[F" -> sendMsg $ MoveCursorRelative (CursorPos 0 1337)
        -- PageUp
        "\ESC[5~" -> sendMsg $ MoveCursorRelative (CursorPos (-40) 0)
        -- PageDown
        "\ESC[6~" -> sendMsg $ MoveCursorRelative (CursorPos 40 0)
        "\ESC" -> resetScreen >> exitSuccess
        "\DEL" -> sendMsg Backspace
        "\n" -> sendMsg Enter
        -- regular typing
        [ch] | not $ isControl ch -> sendMsg $ TypeChar ch
        -- Ctrl + q
        "\DC1" -> resetScreen >> exitSuccess
        -- Ctrl + s
        "\DC3" -> pure SaveFile
        x -> error $ "key " ++ show x ++ " is  not implemented yet"

resetScreen :: IO ()
resetScreen = do
    Terminal.setSGR [Terminal.Reset]
    Terminal.clearScreen
    Terminal.setCursorPosition 0 0

saveFile :: Model -> IO ()
saveFile model =
    case currentFileName model of
        Just fname -> writeFile fname $ unlines $ textBuffer model
        Nothing -> error "can't save: no file is open"

mainLoop :: Model -> IO ()
mainLoop model = do
    view model
    msgOrOp <- getOp
    case msgOrOp of
        OpMsg msg -> do
            let updatedModel = update msg model
            mainLoop updatedModel
        SaveFile -> do
            saveFile model
            mainLoop model

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
