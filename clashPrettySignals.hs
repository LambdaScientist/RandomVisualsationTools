{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import qualified Prelude as P
import CLaSH.Prelude 
import GHC.Generics (Generic)
import Control.DeepSeq
import Text.PrettyPrint

type Name = String
type SampleLength = Int
type FormatID = Int

shTxt = text.show
newline = text "\n"
data WaveSignal = forall a . (Show a, NFData a) =>
                  WS { name    :: Name
                     , sig     :: Signal a
                     , wFormat :: [FormatID]
                     , len     :: SampleLength
                     }
instance NFData WaveSignal where
  rnf a = seq a ()                     
                     

printWaveSignal :: WaveSignal -> Doc
printWaveSignal ws@WS{..} = document
  where
    formats = (P.concat.P.map show) $ P.take len wFormat
    sigValues = sampleN len sig
    sigDoc = (P.map (quotes.shTxt)) sigValues 
    document = lbrace
            <> text "name" <> colon <> space <> quotes (text name) 
            <> comma <> space
            <> text "wave" <> colon <> space <> quotes (text formats)
            <> comma <> space
            <> text "data" <> colon <> space <> lbrack
            <> hcat (punctuate comma sigDoc)
            <> rbrack
            <> rbrace

mkDiagram :: [WaveSignal] -> Doc
mkDiagram inputs = lbrace <> text "signal" <> colon <> lbrack
                $$ nest 4 (vcat (punctuate comma sigs))
                $$ rbrack $$ rbrace
  where
    sigs = P.map printWaveSignal inputs


foo :: Signal Int 
foo = signal 5
fooAcc :: Signal Int 
fooAcc = register 0 (fooAcc + 1)


bar = WS "foo" foo [2 | x <- [1..]] 10
bar2 = WS "fooAcc" fooAcc [3 | x <- [1..]] 10


test = mkDiagram [bar,bar2]



{signal:[
  {name: "60 Hz Frame Sync", wave: "l...hl...........|...........hl....",period:.5},
  {name: "pldCnt"          , wave: "2....2....2....xx|xx2....2....2....", data: ["192","0","1","191","192","0"],period:.5},
  {name: "clkCnt"          , wave: "=x|x==x|x==x|x==x|x==x|x==x|x==x|x="   , data: ["0","N","0","N","0","N","0","N","0","N","0","N","0","N"],period:.5},
  {name: "50 MHz Sys Clk"  , wave: "p.|....|....|....|",period:.25}
]


}

