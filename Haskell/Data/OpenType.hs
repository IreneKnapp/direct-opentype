{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module Data.OpenType
  (Font,
   loadFontFromFile,
   debugFont)
  where

import qualified Data.ByteString as BS

import BinaryFiles

import LowLevelStructure


data Font = FontData BS.ByteString


lowLevelStructure "OffsetTable"
  [("Magic", TagFieldType),
   ("NTables", UShortFieldType),
   ("SearchRange", UShortFieldType),
   ("EntrySelector", UShortFieldType),
   ("RangeShift", UShortFieldType)]


loadFontFromFile :: FilePath -> IO Font
loadFontFromFile filePath = do
  fontData <- BS.readFile filePath
  return $ FontData fontData


debugFont :: Font -> IO ()
debugFont font = do
  let FontData fontData = font
  case fromByteString fontData of
    Left _ -> putStrLn $ "Can't load."
    Right offsetTable -> do
      putStrLn $ "Magic " ++ (show $ offsetTableMagic offsetTable)
      putStrLn $ "NTables " ++ (show $ offsetTableNTables offsetTable)
      putStrLn $ "SearchRange " ++ (show $ offsetTableSearchRange offsetTable)
      putStrLn $ "EntrySelector "
                 ++ (show $ offsetTableEntrySelector offsetTable)
      putStrLn $ "RangeShift " ++ (show $ offsetTableRangeShift offsetTable)

