module CommonMark.Render.Html5 where
import CommonMark.Types
import Lucid
import Lucid.Html5
import Control.Monad
import qualified Data.Text.Lazy as LazyText

renderHtml5 :: Tree Block -> LazyText.Text
renderHtml5 = renderText . blockToHtml5

blockToHtml5 :: Tree Block -> Html ()
blockToHtml5 (Node elt ns) =
  case eltType elt of
       Paragraph{ paragraphContents = ils } -> p_ (inlineToHtml5 ils)
       _ -> mapM_ blockToHtml5 ns

inlineToHtml5 :: Tree Inline -> Html ()
inlineToHtml5 (Node elt ns) =
  case eltType elt of
       Txt -> toHtml (mconcat $ map tokenToText $ contentToks elt)
       Emph -> em_ (mapM_ inlineToHtml5 ns)
       Strong -> strong_ (mapM_ inlineToHtml5 ns)
       _ -> mapM_ inlineToHtml5 ns


