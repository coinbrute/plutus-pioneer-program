{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week03.Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeVestingValidator
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import           Week03.Parameterized

-- this is using the Cardano Data and we need to convert to the Plutus Data type so here we are converting from Data to ScriptData for use on the Cardano Node so we can use Plutus Data types as Cardano Data types with the Cardano.Api
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

-- this gets some instance of ToData from a file path then serializes that thing to a file as json
    -- it converts it to Plutus Data then to Cardano Data with the dataToScriptData then to json using Cardano.Api with scriptDataToJson using that ScriptData we have after converting the Plutus Data we got from the FilePath 
    -- then we encode it and write it to a file
writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

-- this writes the validator to a file using a filepath and a validator
    -- it unwraps an instance of a script and serialized it then takes that and sends it to plutus script and writes to file
writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- this is a special version that writes a serialized version of unit to the file given 
-- this is because the datum and redeemer are always unit and we just need the serialized versions
{-
unit.json 
{"constructor":0,"fields":[]}
-} 
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

-- the beneficiary value below was generated using the following 
{-
cardano-cli address key-hash --payment-verifaction-key-file 02.vkey --out-file 02.pkh

cat 02.pkh
-}
-- then we need a deadline which is just ms time in the future from now
-- then you run this function in the repl to generate the file
-- after the file is generated run the following to build the script
{-
cardano-cli address build-script --script-file vesting.plutus --testnet-magic <networkMagic> --out-file vesting.addr

cat vesting.addr
-}
-- we now can go on to the give script in the notes.
writeVestingValidator :: IO (Either (FileError ()) ())
writeVestingValidator = writeValidator "testnet/vesting.plutus" $ validator $ VestingParam
    { beneficiary = Ledger.PaymentPubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a"
    , deadline    = 1643235300000
    }
