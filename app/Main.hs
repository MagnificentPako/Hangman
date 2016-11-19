module Main where
import Data.Char
import Brick
import Control.Monad
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Types
import Graphics.Vty
import Brick.Util
import Network.Download
import Data.Either

-- TODO: Implement a real "random word"

randomWord = do
    r <- openURIString "http://www.setgetgo.com/randomword/get.php"
    case r of { Left err -> error err; Right res -> return res}

maskString :: String -> String -> String
maskString raw hid = blankOrChar <$> raw
  where
    blankOrChar c
      | (toLower c) `elem` hid = c
      | otherwise = '_'

drawUI :: (String,String) -> [Widget ()]
drawUI l = [ui]
  where
    (input,word) = l
    ui = if (maskString word input) /= word
        then vCenter $ vBox [ hCenter $ str "Press keys until this makes sense."
                            , hCenter $ str $ maskString word input
                            ]
        else vCenter $ vBox [ hCenter $ str $ "Well done, the word was " ++ word
                            , hCenter $ str "Press Esc to quit!"
                            ]

appEvent :: (String, String) -> BrickEvent () e -> EventM () (Next (String,String))
appEvent l (VtyEvent e) = let (input,word) = l in case e of
    EvKey KEsc [] -> halt $ l
    {-EvKey KBS [] -> case l of
      "" -> continue ""
      _ ->  continue $ init l-} -- No backspace for you!
    EvKey (KChar ch) [] -> continue (input++[ch],word)
    _ -> continue l

theApp :: App (String,String) e ()
theApp =
  App { appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return
      , appAttrMap = const $ attrMap defAttr []
      }

main :: IO ()
main = do
  r <- randomWord
  void $ defaultMain theApp ("", r)
