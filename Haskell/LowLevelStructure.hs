{-# LANGUAGE TemplateHaskell #-}
module LowLevelStructure
  (FieldType(..),
   F16Dot16,
   F2Dot14,
   LongDateTime,
   Tag,
   stringTag,
   integerTag,
   lowLevelStructure)
  where

import BinaryFiles
import Data.Bits
import Data.Char
import Data.Fixed
import Data.Int
import Data.Word
import Language.Haskell.TH


data FieldType
  = ByteFieldType
  | CharFieldType
  | UShortFieldType
  | ShortFieldType
  | UInt24FieldType
  | ULongFieldType
  | LongFieldType
  | FixedFieldType
  | FWordFieldType
  | UFWordFieldType
  | F2Dot14FieldType
  | LongDateTimeFieldType
  | TagFieldType
  | GlyphIDFieldType
  | OffsetFieldType


data F16Dot16 = F16Dot16Data Word32
  deriving (Eq, Ord)
instance Show F16Dot16 where
  show (F16Dot16Data word) = "{16.16} " ++ show word


data F2Dot14 = F2Dot14Data Word16
  deriving (Eq, Ord)
instance Show F2Dot14 where
  show (F2Dot14Data word) = "{2.14} " ++ show word


data LongDateTime = LongDateTimeData Word64
  deriving (Eq, Ord)
instance Show LongDateTime where
  show (LongDateTimeData word) = "{LongDateTime} " ++ show word


data Tag = TagData Word8 Word8 Word8 Word8
  deriving (Eq, Ord)
instance Show Tag where
  show (TagData byte1 byte2 byte3 byte4) =
    if all isShowable bytes
      then "{Tag} '" ++ map (chr . fromIntegral) bytes ++ "'"
      else "{Tag} 0x" ++ concatMap showByte [byte1, byte2, byte3, byte4]
    where bytes = [byte1, byte2, byte3, byte4]
          isShowable byte = (byte >= 32) && (byte < 127)
          showByte byte =
            (showNibble $ shiftR byte 4 .&. 0x0F)
            ++ (showNibble $ shiftR byte 0 .&. 0x0F)
          showNibble nibble =
            if nibble < 10
              then [chr (ord '0' + (fromIntegral nibble - 0))]
              else [chr (ord 'a' + (fromIntegral nibble - 10))]


stringTag :: String -> Tag
stringTag (a : b : c : d : []) =
  TagData (fromIntegral $ ord a)
          (fromIntegral $ ord b)
          (fromIntegral $ ord c)
          (fromIntegral $ ord d)
stringTag _ = TagData 0x00 0x00 0x00 0x00


integerTag :: Word32 -> Tag
integerTag word =
  TagData (fromIntegral $ shiftR word 24 .&. 0xFF)
          (fromIntegral $ shiftR word 16 .&. 0xFF)
          (fromIntegral $ shiftR word 8 .&. 0xFF)
          (fromIntegral $ shiftR word 0 .&. 0xFF)


lowLevelStructure :: String -> [(String, FieldType)] -> Q [Dec]
lowLevelStructure structureName fieldSpecifications = do
  dataName <- newName structureName
  constructorName <- newName structureName
  let parameterName = mkName "structure"
  fieldDefinitions <-
    mapM (\(fieldName, fieldType) -> do
           accessorName <-
             newName $ (computeDowncase structureName) ++ fieldName
           temporaryName <- newName $ "the" ++ fieldName
           let fieldDataType =
                 case fieldType of
                   ByteFieldType -> ConT ''Word8
                   CharFieldType -> ConT ''Char
                   UShortFieldType -> ConT ''Word16
                   ShortFieldType -> ConT ''Int16
                   UInt24FieldType -> ConT ''Word32
                   ULongFieldType -> ConT ''Word32
                   LongFieldType -> ConT ''Int32
                   FixedFieldType -> ConT ''F16Dot16
                   FWordFieldType -> ConT ''Int16
                   UFWordFieldType -> ConT ''Word16
                   F2Dot14FieldType -> ConT ''F2Dot14
                   LongDateTimeFieldType -> ConT ''LongDateTime
                   TagFieldType -> ConT ''Tag
                   GlyphIDFieldType -> ConT ''Word16
                   OffsetFieldType -> ConT ''Word16
           return (fieldName, temporaryName, accessorName,
                   fieldType, fieldDataType))
         fieldSpecifications
  let structureDefinition =
        DataD [] dataName []
              [RecC constructorName
                    (map (\(_, _, accessorName, _, fieldDataType) ->
                            (accessorName, NotStrict, fieldDataType))
                         fieldDefinitions)] []
      computeSerializeAction fieldExpression fieldType =
        case fieldType of
          ByteFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          CharFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord)
                            (SigE (AppE (VarE 'fromIntegral)
                                        (AppE (VarE 'ord) fieldExpression))
                                  (ConT ''Word8))]
          UShortFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          ShortFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          UInt24FieldType ->
            let temporaryName = mkName "value"
            in [LetS [ValD (VarP temporaryName) (NormalB fieldExpression) []],
                NoBindS $ AppE (VarE 'serializeWord)
                               (InfixE (Just (AppE (AppE (VarE 'shiftR)
                                                         (VarE temporaryName))
                                                   (LitE (IntegerL 16))))
                                       (VarE (mkName ".&."))
                                       (Just (LitE (IntegerL 0xFF)))),
                NoBindS $ AppE (VarE 'serializeWord)
                               (InfixE (Just (AppE (AppE (VarE 'shiftR)
                                                         (VarE temporaryName))
                                                   (LitE (IntegerL 8))))
                                       (VarE (mkName ".&."))
                                       (Just (LitE (IntegerL 0xFF)))),
                NoBindS $ AppE (VarE 'serializeWord)
                               (InfixE (Just (AppE (AppE (VarE 'shiftR)
                                                         (VarE temporaryName))
                                                   (LitE (IntegerL 0))))
                                       (VarE (mkName ".&."))
                                       (Just (LitE (IntegerL 0xFF))))]
          ULongFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          LongFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          FixedFieldType ->
            let temporaryName = mkName "value"
            in [LetS [ValD (ConP 'F16Dot16Data [VarP temporaryName])
                           (NormalB fieldExpression)
                           []],
                NoBindS $ AppE (VarE 'serializeWord) (VarE temporaryName)]
          FWordFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          UFWordFieldType ->
            [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          F2Dot14FieldType ->
            let temporaryName = mkName "value"
            in [LetS [ValD (ConP 'F2Dot14Data [VarP temporaryName])
                           (NormalB fieldExpression)
                           []],
                NoBindS $ AppE (VarE 'serializeWord) (VarE temporaryName)]
          LongDateTimeFieldType ->
            let temporaryName = mkName "value"
            in [LetS [ValD (ConP 'LongDateTimeData [VarP temporaryName])
                           (NormalB fieldExpression)
                           []],
                NoBindS $ AppE (VarE 'serializeWord) (VarE temporaryName)]
          TagFieldType ->
            let byte1Name = mkName "byte1"
                byte2Name = mkName "byte2"
                byte3Name = mkName "byte3"
                byte4Name = mkName "byte4"
            in [LetS [ValD (ConP 'TagData
                                 [VarP byte1Name,
                                  VarP byte2Name,
                                  VarP byte3Name,
                                  VarP byte4Name])
                           (NormalB fieldExpression)
                           []],
                NoBindS $ AppE (VarE 'serializeWord) (VarE byte1Name),
                NoBindS $ AppE (VarE 'serializeWord) (VarE byte2Name),
                NoBindS $ AppE (VarE 'serializeWord) (VarE byte3Name),
                NoBindS $ AppE (VarE 'serializeWord) (VarE byte4Name)]
          GlyphIDFieldType ->
             [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
          OffsetFieldType ->
             [NoBindS $ AppE (VarE 'serializeWord) fieldExpression]
      computeFieldExpression accessorName =
        AppE (VarE accessorName) (VarE parameterName)
      serializeBody =
        NormalB $ AppE (AppE (VarE 'withContext) (ConE 'BigEndian))
         $ DoE $ concatMap (\(_, _, accessorName, fieldType, _) ->
                               computeSerializeAction
                                 (computeFieldExpression accessorName)
                                 fieldType)
                           fieldDefinitions
      computeDeserializeAction temporaryName fieldType =
        case fieldType of
          ByteFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          CharFieldType ->
            [BindS (VarP temporaryName)
              (InfixE (Just (VarE 'deserializeWord))
                      (VarE $ mkName ">>=")
                      (Just (InfixE
                        (Just (VarE 'return))
                        (VarE $ mkName ".")
                        (Just (InfixE (Just (VarE 'chr))
                                      (VarE $ mkName ".")
                                      (Just (VarE 'fromIntegral)))))))]
          UShortFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          ShortFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          UInt24FieldType ->
            let byte1Name = mkName "byte1"
                byte2Name = mkName "byte2"
                byte3Name = mkName "byte3"
            in [BindS (VarP byte1Name)
                      (SigE (VarE 'deserializeWord) (VarT ''Word8)),
                BindS (VarP byte2Name)
                      (SigE (VarE 'deserializeWord) (VarT ''Word8)),
                BindS (VarP byte3Name)
                      (SigE (VarE 'deserializeWord) (VarT ''Word8)),
                NoBindS $ AppE (VarE 'return)
                 (InfixE
                   (Just (InfixE (Just (AppE (AppE (VarE 'shiftL)
                                                   (AppE (VarE 'fromIntegral)
                                                         (VarE byte1Name)))
                                             (LitE (IntegerL 16))))
                                 (VarE $ mkName ".|.")
                                 (Just (AppE (AppE (VarE 'shiftL)
                                                   (AppE (VarE 'fromIntegral)
                                                         (VarE byte2Name)))
                                             (LitE (IntegerL 8))))))
                   (VarE $ mkName ".|.")
                   (Just (AppE (AppE (VarE 'shiftL)
                                     (AppE (VarE 'fromIntegral)
                                           (VarE byte3Name)))
                               (LitE (IntegerL 0)))))]
          ULongFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          LongFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          FixedFieldType ->
            let temporary2Name = mkName "temporary2"
            in [BindS (VarP temporary2Name) (VarE 'deserializeWord),
                LetS [ValD (VarP temporaryName)
                           (NormalB (AppE (ConE 'F16Dot16Data)
                                          (VarE temporaryName)))
                           []]]
          FWordFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          UFWordFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          F2Dot14FieldType ->
            let temporary2Name = mkName "temporary2"
            in [BindS (VarP temporary2Name) (VarE 'deserializeWord),
                LetS [ValD (VarP temporaryName)
                           (NormalB (AppE (ConE 'F2Dot14Data)
                                          (VarE temporaryName)))
                           []]]
          LongDateTimeFieldType ->
            let temporary2Name = mkName "temporary2"
            in [BindS (VarP temporary2Name) (VarE 'deserializeWord),
                LetS [ValD (VarP temporaryName)
                           (NormalB (AppE (ConE 'LongDateTimeData)
                                          (VarE temporaryName)))
                           []]]
          TagFieldType ->
            let byte1Name = mkName "byte1"
                byte2Name = mkName "byte2"
                byte3Name = mkName "byte3"
                byte4Name = mkName "byte4"
            in [BindS (VarP byte1Name) (VarE 'deserializeWord),
                BindS (VarP byte2Name) (VarE 'deserializeWord),
                BindS (VarP byte3Name) (VarE 'deserializeWord),
                BindS (VarP byte4Name) (VarE 'deserializeWord),
                LetS [ValD (VarP temporaryName)
                           (NormalB (AppE (AppE (AppE (AppE (ConE 'TagData)
                                                            (VarE byte1Name))
                                                      (VarE byte2Name))
                                                (VarE byte3Name))
                                          (VarE byte4Name)))
                           []]]
          GlyphIDFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
          OffsetFieldType ->
            [BindS (VarP temporaryName) (VarE 'deserializeWord)]
      deserializeBody =
        NormalB $ AppE (AppE (VarE 'withContext) (ConE 'BigEndian))
         $ DoE $ concat
           [concatMap (\(_, temporaryName, _, fieldType, _) ->
                          computeDeserializeAction temporaryName fieldType)
                      fieldDefinitions,
            [NoBindS $ AppE (VarE 'return)
                            (RecConE constructorName
                              (map (\(_, temporaryName, accessorName, _, _) ->
                                       (accessorName, VarE temporaryName))
                                   fieldDefinitions))]]
      instanceDefinition =
        InstanceD [] (AppT (AppT (ConT ''Serializable)
                                 (VarT $ mkName "context"))
                           (ConT dataName))
          [FunD 'serialize [Clause [VarP parameterName] serializeBody []],
           FunD 'deserialize [Clause [] deserializeBody []]]
  return [structureDefinition,
          instanceDefinition]


computeDowncase :: String -> String
computeDowncase "" = ""
computeDowncase (c : rest) = toLower c : rest

