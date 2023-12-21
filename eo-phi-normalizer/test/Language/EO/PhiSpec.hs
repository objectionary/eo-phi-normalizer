module Language.EO.PhiSpec where

import           Test.Hspec
import qualified Language.EO.Phi as Phi

unsafeParseProgramFromFile :: FilePath -> IO Phi.Program
unsafeParseProgramFromFile path =
  Phi.unsafeParseProgram <$> readFile path

normalizesTo :: FilePath -> FilePath -> Expectation
normalizesTo beforePath afterPath = do
  beforeProgram <- unsafeParseProgramFromFile beforePath
  afterProgram <- unsafeParseProgramFromFile afterPath
  Phi.normalize beforeProgram `shouldBe` afterProgram

normalizesCorrectly :: FilePath -> Expectation
normalizesCorrectly baseName = beforePath `normalizesTo` afterPath
  where
    beforePath = "test/eo/phi/" <> baseName
    afterPath  = "test/eo/phi/" <> baseName <> ".normalized"

spec :: Spec
spec = do
  describe "Normalizer" $ do
    it "Normalizes static attribute references" $ do
      normalizesCorrectly "test.phi"
