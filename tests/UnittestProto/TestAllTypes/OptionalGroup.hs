module UnittestProto.TestAllTypes.OptionalGroup (OptionalGroup(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data OptionalGroup = OptionalGroup{a :: P'.Maybe P'.Int32, unknown'field :: P'.UnknownField}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage OptionalGroup where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable OptionalGroup where
  mergeEmpty = OptionalGroup P'.mergeEmpty P'.mergeEmpty
  mergeAppend (OptionalGroup x'1 x'2) (OptionalGroup y'1 y'2) = OptionalGroup (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OptionalGroup where
  defaultValue = OptionalGroup P'.defaultValue P'.defaultValue
 
instance P'.Wire OptionalGroup where
  wireSize ft' self'@(OptionalGroup x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 2 5 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(OptionalGroup x'1 x'2)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
        _ -> P'.wirePutErr ft' self'
    where
        put'Fields
          = do
              P'.wirePutOpt 136 5 x'1
              P'.wirePutUnknownField x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              17 -> P'.fmap (\ new'Field -> old'Self{a = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> OptionalGroup) OptionalGroup where
  getVal m' f' = f' m'
 
instance P'.GPB OptionalGroup
 
instance P'.ReflectDescriptor OptionalGroup where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"OptionalGroup\"}, descFilePath = [\"UnittestProto\",\"TestAllTypes\",\"OptionalGroup.hs\"], isGroup = True, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes.OptionalGroup\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"