import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import System.Environment
import System.FilePath
import System.IO
import Text.Regex
import Text.Regex.Posix

data Element = EInt Integer | EDouble Double | EStr String | EStack [Element] deriving Show

type EZipper = (Element, [Element])

parse :: String -> Element
parse s = reverseEStack $ fst $ evalState (parseHelper (EStack [], [])) s
          where reverseEStack (EStack xs) = EStack $ reverse $ map reverseEStack xs
                reverseEStack (EStr s) = EStr $ reverse s
                reverseEStack x = x

showElement :: Element -> String
showElement (EStack xs) = execWriter $ mapM_ traverse xs
  where traverse (EInt x) = tell $ show x ++ "\n"
        traverse (EDouble x) = tell $ show x ++ "\n"
        traverse (EStack xs) = tell (show (length xs) ++ "\n") >> mapM_ traverse xs
        traverse x = tell $ show x ++ "\n"

parseHelper :: EZipper -> State String EZipper
parseHelper z = do
  s <- get
  if null s
    then return z
    else
    do
      let (c:s') = s; (EStack xs, bs) = z
      modify (const s')
      case c of
        '"' -> let (s1, _:s2) = span (/='"') s'
               in modify (const s2) >> parseHelper (EStack $ EStr s1:xs, bs)
        ch | elem ch "-.0123456789" -> let (s1, s2) = span (`elem` "-.0123456789") s
                                       in modify (const s2) >> parseHelper (EStack $ (if elem '.' s1 then EDouble (read s1) else EInt (read s1)):xs, bs)
        '{' -> parseHelper (EStack [], EStack xs:bs)
        '}' -> let (EStack b:bs') = bs in parseHelper (EStack $ EStack xs:b, bs')
        _ -> parseHelper z
  where

parseWeb :: String -> FilePath -> StateT (String, String) IO ()
parseWeb ls path =
  forM_ (map (dropWhile isSpace) (lines ls))
  (\l -> do
      (idx, contents) <- get
      let s = l =~ "^[0-9]+\\)" :: String
      if not $ null s
        then do
        put (init $ l =~ "[0-9]+\\)", contents)
        else
          let match = l =~ "Returns: .*" :: String
          in if not $ null match
             then do
               put ("", "")
               lift $ withFile (path <.> idx <.> "in") WriteMode $ flip hPutStr (showElement . parse $ contents)
               lift $ withFile (path <.> idx <.> "out") WriteMode $ flip hPutStr (showElement . parse . drop 9 $ match)
             else
               when (not $ null idx) $ modify (second $ (++(l++",")))
      )

parseStatistics :: String -> FilePath -> IO ()
parseStatistics contents path =
  forM_ (zip [0..] (lines contents))
  (\(idx, l) -> do
      let parts = filter (not . null) $ splitRegex (mkRegex "\t") l
      when (length parts == 3) $ do
        withFile (path <.> show idx <.> "in") WriteMode $ flip hPutStr (showElement . parse $ parts!!0)
        withFile (path <.> show idx <.> "out") WriteMode $ flip hPutStr (showElement . parse $ parts!!1)
  )

main = do
  args <- getArgs
  if null args
    then hPutStr stderr "usage: "
    else do
      classname <- fmap head getArgs
      contents <- getContents
      if contents =~ "^[0-9]+\\)"
        then evalStateT (parseWeb contents $ "/tmp" </> classname) ("", "")
        else parseStatistics contents $ "/tmp" </> classname
