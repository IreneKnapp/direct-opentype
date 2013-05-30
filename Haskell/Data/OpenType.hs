{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveDataTypeable,
             FlexibleInstances #-}
module Data.OpenType
  (Font,
   loadFontFromFile,
   debugFont)
  where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import BinaryFiles
import Control.Monad
import Data.Typeable
import Data.Word
import System.Exit

import LowLevelStructure


data Font =
  FontData {
      fontCharacterMap :: Map.Map FontEncoding (Map.Map Word32 Word32)
    }


data FontMagic
  = PostScriptFontMagic
  | TrueTypeFontMagic
  deriving (Eq, Ord)


data FontEncoding = FontEncoding Word16 Word16
  deriving (Eq, Ord, Show)


class HasFontMagic context where
  contextFontMagic :: context -> FontMagic
class HasTableMap context where
  contextTableMap :: context -> Map.Map Tag (Word32, Word32)


data FontHeaderContext
  = FontHeaderContext {
        fontHeaderContextFontMagic :: FontMagic,
        fontHeaderContextTableMap :: Map.Map Tag (Word32, Word32)
      }
instance HasFontMagic FontHeaderContext where
  contextFontMagic = fontHeaderContextFontMagic
instance HasTableMap FontHeaderContext where
  contextTableMap = fontHeaderContextTableMap


data Failure
  = Failure
  deriving (Show, Typeable)
instance SerializationFailure Failure


lowLevelStructure "OffsetTable"
  [("Magic", TagFieldType),
   ("NTables", UShortFieldType),
   ("SearchRange", UShortFieldType),
   ("EntrySelector", UShortFieldType),
   ("RangeShift", UShortFieldType)]


lowLevelStructure "OffsetTableRecord"
  [("Tag", TagFieldType),
   ("Checksum", ULongFieldType),
   ("Offset", ULongFieldType),
   ("Length", ULongFieldType)]


lowLevelStructure "CharacterMapTable"
  [("Version", UShortFieldType),
   ("NTables", UShortFieldType)]


lowLevelStructure "CharacterMapEncodingRecord"
  [("PlatformID", UShortFieldType),
   ("EncodingID", UShortFieldType),
   ("Offset", ULongFieldType)]


lowLevelStructure "CharacterMapSubtableHeader"
  [("Format", UShortFieldType),
   ("Length", UShortFieldType),
   ("Language", UShortFieldType)]


loadFontFromFile :: FilePath -> IO Font
loadFontFromFile filePath = do
  result <- runDeserializationFromFile deserializeFont filePath
  case result of
    Left _ -> do
      putStrLn $ "Unable to load font: " ++ filePath
      exitFailure
    Right font -> return font


debugFont :: Font -> IO ()
debugFont font = do
  putStrLn $ show $ fontCharacterMap font


deserializeFont :: Deserialization Font
deserializeFont = do
  fontHeaderContext <- deserializeHeader
  withContext fontHeaderContext $ do
    characterMap <- deserializeTable (stringTag "cmap") deserializeCharacterMap
    return $ FontData {
                 fontCharacterMap = characterMap
               }


deserializeHeader :: Deserialization FontHeaderContext
deserializeHeader = withTag "font header" $ do
  seek OffsetFromStart 0
  offsetTable <- deserialize
  fontMagic <-
    case offsetTableMagic offsetTable of
      magic | magic == integerTag 0x00010000 -> return TrueTypeFontMagic
            | magic == stringTag "OTTO" -> return PostScriptFontMagic
            | otherwise -> throw Failure
  tables <- mapM (\_ -> do
                    record <- deserialize
                    return (offsetTableRecordTag record,
                            (offsetTableRecordOffset record,
                             offsetTableRecordLength record)))
                 [0 .. offsetTableNTables offsetTable - 1]
            >>= return . Map.fromList
  return $ FontHeaderContext {
               fontHeaderContextFontMagic = fontMagic,
               fontHeaderContextTableMap = tables
             }


deserializeTable
  :: (HasTableMap context)
  => Tag
  -> ContextualDeserialization context a
  -> ContextualDeserialization context a
deserializeTable tag action = do
  context <- getContext
  case Map.lookup tag (contextTableMap context) of
    Nothing -> throw Failure
    Just (offset, length) -> do
      seek OffsetFromStart (fromIntegral offset)
      withWindow OffsetFromCurrent 0 (fromIntegral length) action


deserializeCharacterMap
  :: Deserialization (Map.Map FontEncoding (Map.Map Word32 Word32))
deserializeCharacterMap = withTag "character map" $ do
  seek OffsetFromStart 0
  characterMapTable <- deserialize
  let loopEncodings i soFar = do
        if i == characterMapTableNTables characterMapTable
          then return soFar
          else do
            encodingRecord <- deserialize
            offset <- tell
            seek OffsetFromStart
                 $ fromIntegral
                     $ characterMapEncodingRecordOffset encodingRecord
            encodingMap <- return $ Map.empty
            seek OffsetFromStart offset
            let platformID =
                  characterMapEncodingRecordPlatformID encodingRecord
                encodingID =
                  characterMapEncodingRecordEncodingID encodingRecord
                soFar' = Map.insert (FontEncoding platformID encodingID)
                                    encodingMap soFar
            loopEncodings (i + 1) soFar'
  loopEncodings 0 Map.empty

