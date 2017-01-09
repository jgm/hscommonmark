module CommonMark.Render.Html5 where
import CommonMark.Types
import Lucid
import Lucid.Html5
import Control.Monad
import qualified Data.Text.Lazy as LazyText
import Debug.Trace

renderHtml5 :: Tree Block -> LazyText.Text
renderHtml5 = renderText . blockToHtml5

blocksToHtml5 :: [Tree Block] -> Html ()
blocksToHtml5 [] = return ()
blocksToHtml5 (x:xs) = do
  blockToHtml5 x
  mapM_ (\(Node elt ns) -> do
             case eltType elt of
                Document -> return ()
                BlankLines -> return ()
                _ -> nl
             blockToHtml5 (Node elt ns)) xs

nl :: Html ()
nl = toHtml ("\n" :: String)

blockToHtml5 :: Tree Block -> Html ()
blockToHtml5 (Node elt ns) = do
  case eltType elt of
       Document -> blocksToHtml5 ns
       BlockQuote -> blockquote_ (do nl
                                     blocksToHtml5 ns
                                     unless (null ns) nl)
       List -> ul_ (blocksToHtml5 ns)
       Item -> li_ (blocksToHtml5 ns)
       Paragraph{ paragraphContents = ils } -> p_ (inlineToHtml5 ils)
       Heading{ headingLevel = lev
              , headingContents = ils } ->
                 (case lev of
                        1 -> h1_
                        2 -> h2_
                        3 -> h3_
                        4 -> h4_
                        5 -> h5_
                        6 -> h6_
                        _ -> p_) (inlineToHtml5 ils)
       CodeBlock{ codeIndented = indented
                , codeInfoString = info } ->
                pre_ (code_ (toHtml (tokensToText (contentToks elt))))
       HtmlBlock -> toHtmlRaw (tokensToText (contentToks elt))
       ThematicBreak -> hr_ []
       BlankLines -> return ()

inlineToHtml5 :: Tree Inline -> Html ()
inlineToHtml5 (Node elt ns) =
  case eltType elt of
       Inlines -> mapM_ inlineToHtml5 ns
       Txt -> toHtml (tokensToText (contentToks elt))
       Space -> toHtml (" " :: String)
       Softbreak -> nl
       Linebreak -> br_ []
       Code -> code_ (toHtml (tokensToText (contentToks elt)))
       HtmlInline -> toHtmlRaw (tokensToText (contentToks elt))
       Emph -> em_ (mapM_ inlineToHtml5 ns)
       Strong -> strong_ (mapM_ inlineToHtml5 ns)
       Link{ linkDestination = dest
           , linkTitle = title } ->
              a_ [href_ dest, title_ title] (mapM_ inlineToHtml5 ns)
       Image{ imageSource = src
             , imageTitle = title } ->
                img_ [src_ src, title_ title,
                      alt_ (tokensToText (contentToks elt))]


