{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, DeriveDataTypeable,
             FlexibleInstances #-}
module Data.OpenType
  (Font,
   loadFontFromFile,
   debugFont)
  where

import qualified Data.ByteString as BS

import BinaryFiles
import Data.Typeable
import Data.Word

import LowLevelStructure


data Font = FontData BS.ByteString


lowLevelStructure "OffsetTable"
  [("Magic", TagFieldType),
   ("NTables", UShortFieldType),
   ("SearchRange", UShortFieldType),
   ("EntrySelector", UShortFieldType),
   ("RangeShift", UShortFieldType)]


lowLevelStructure "TableRecord"
  [("Tag", TagFieldType),
   ("Checksum", ULongFieldType),
   ("Offset", ULongFieldType),
   ("Length", ULongFieldType)]


loadFontFromFile :: FilePath -> IO Font
loadFontFromFile filePath = do
  fontData <- BS.readFile filePath
  return $ FontData fontData


debugFont :: Font -> IO ()
debugFont font = do
  let FontData fontData = font
  case runDeserializationFromByteString deserializeHeader fontData of
    Left _ -> putStrLn $ "Can't load."
    Right (isCFF, tables) -> do
      putStrLn $ "Is CFF: " ++ (show isCFF)
      mapM_ (\(tag, offset, length) -> do
               putStrLn $ (show tag) ++ " "
                          ++ (show offset) ++ " "
                          ++ (show length))
            tables


data Failure
  = Failure
  deriving (Show, Typeable)
instance SerializationFailure Failure


deserializeHeader :: Deserialization (Bool, [(Tag, Word32, Word32)])
deserializeHeader = withTag "Font header" $ do
  offsetTable <- deserialize
  isCFF <- case offsetTableMagic offsetTable of
             magic | magic == integerTag 0x00010000 -> return False
                   | magic == stringTag "OTTO" -> return True
                   | otherwise -> throw Failure
  tables <- mapM (\_ -> do
                     tableRecord <- deserialize
                     return (tableRecordTag tableRecord,
                             tableRecordOffset tableRecord,
                             tableRecordLength tableRecord))
                 [0 .. offsetTableNTables offsetTable - 1]
  return (isCFF, tables)

