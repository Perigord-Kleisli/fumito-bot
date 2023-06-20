module Fumito.Utils where

import Data.Aeson.Types
import Data.Scientific
import Data.String.Interpolate
import Language.Haskell.TH (Body (NormalB), Clause (Clause), Dec (FunD, InstanceD), Exp (..), Lit (..), Name, Pat (..), Type (AppT, ConT), nameBase)
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax (Q)

assertInteger :: Scientific -> Parser ()
assertInteger n = do
    if isInteger n
        then pass
        else fail [i|passed non-integral number #{n}|]

{- | For enums that contain skip in their integer representation
 i.e

 > data SampleEnum
 >  = Foo
 >  | Bar
 >  | Baz
 >  | Lorem

>>> deriveEnumWithGaps [('Baz,5)] 'SampleEnum
instance Enum SampleEnum
fromEnum Foo = 1
fromEnum Bar = 2
fromEnum Baz = 5
fromEnum Lorem = 6
toEnum 1 = Foo
toEnum 2 = Bar
toEnum 5 = Baz
toEnum 6 = Lorem
 works just like enums in C-Like languages
-}
deriveEnumWithGaps :: [(Name, Integer)] -> Name -> Q [Dec]
deriveEnumWithGaps gaps name = do
    DatatypeInfo {datatypeCons} <- reifyDatatype name
    let fromClauses = goFromEnum (0 :: Integer) (map constructorName datatypeCons) gaps
        toClauses = goToEnum (0 :: Integer) (map constructorName datatypeCons) gaps
    return
        . one
        $ InstanceD
            Nothing
            []
            (AppT (ConT ''Enum) (ConT name))
            [ FunD 'fromEnum fromClauses
            , FunD 'toEnum toClauses
            ]
    where
        goFromEnum _ [] _ = []
        goFromEnum n (consName : xs) [] =
            Clause [ConP consName [] []] (NormalB (LitE (IntegerL n))) []
                : goFromEnum (n + 1) xs []
        goFromEnum n (consName : xs) ys'@((gapName, n') : ys)
            | consName == gapName =
                Clause [ConP consName [] []] (NormalB (LitE (IntegerL n'))) []
                    : goFromEnum (n' + 1) xs ys
            | otherwise =
                Clause [ConP consName [] []] (NormalB (LitE (IntegerL n))) []
                    : goFromEnum (n + 1) xs ys'
        goToEnum _ [] _ = []
        goToEnum n (consName : xs) [] =
            Clause [LitP (IntegerL n)] (NormalB (ConE consName)) []
                : goToEnum (n + 1) xs []
        goToEnum n (consName : xs) ys'@((gapName, n') : ys)
            | consName == gapName =
                Clause [LitP (IntegerL n')] (NormalB (ConE consName)) []
                    : goToEnum (n' + 1) xs ys
            | otherwise =
                Clause [LitP (IntegerL n)] (NormalB (ConE consName)) []
                    : goToEnum (n + 1) xs ys'

-- | Create a FromJSON and ToJSON instance by treating enum constructors as integers
deriveJSONFromEnum :: Name -> Q [Dec]
deriveJSONFromEnum name = do
    let nameT = pure (ConT name)
        jsonObjectName = pure (LitE (StringL (nameBase name)))
    [d|
        instance FromJSON $nameT where
            parseJSON = withScientific $jsonObjectName \n -> do
                assertInteger n
                maybe (fail $ "Unknown " <> $jsonObjectName <> " " <> show n) pure $ safeToEnum $ floor n

        instance ToJSON $nameT where
            toJSON = Number . fromIntegral . fromEnum
        |]

-- | combination of @deriveEnumWithGaps@ and @deriveJSONFromEnum@
deriveGappedJSONEnum :: [(Name, Integer)] -> Name -> Q [Dec]
deriveGappedJSONEnum gaps name =
    (<>)
        <$> deriveEnumWithGaps gaps name
        <*> deriveJSONFromEnum name
