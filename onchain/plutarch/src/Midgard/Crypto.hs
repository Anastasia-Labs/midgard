module Midgard.Crypto (
  pcardanoPubKeyToPubKeyHash,
  pethereumPubKeyToPubKeyHash,
  pcompressPublicKey,
  scriptHashV3
) where 

import Plutarch (
  Term,
  type (:-->),
  plet,
  phoistAcyclic, 
  (#), 
  plam,
 )
import Plutarch.ByteString (PByteString, plengthBS, psliceBS, pindexBS)
import Plutarch.Crypto (pkeccak_256, pblake2b_224)
import Plutarch.Integer (PInteger, pmod)
import Plutarch.Lift (pconstant) 
import Plutarch.Bool (pif, (#==))
import PlutusCore.Crypto.Hash qualified as Hash
import PlutusLedgerApi.Common (serialiseUPLC)
import Data.ByteString.Short (fromShort)
import Plutarch.Script (Script(unScript))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

scriptHashV3 :: Script -> ByteString
scriptHashV3 = hashScriptWithPrefix 0x3

hashScriptWithPrefix :: Word8 -> Script -> ByteString
hashScriptWithPrefix prefix scr = 
  Hash.blake2b_224
    $ BS.singleton prefix <> (fromShort . serialiseUPLC . unScript $ scr)

pcardanoPubKeyToPubKeyHash :: Term s (PByteString :--> PByteString)
pcardanoPubKeyToPubKeyHash = phoistAcyclic $ plam $ \pubKey -> pblake2b_224 # pubKey

pethereumPubKeyToPubKeyHash :: Term s (PByteString :--> PByteString)
pethereumPubKeyToPubKeyHash = phoistAcyclic $ plam $ \pubKey -> 
  plet (pkeccak_256 # pubKey) $ \fullHash ->
    (pdropBS # (plengthBS # fullHash - 20) # fullHash)

pcompressPublicKey :: Term s PByteString -> Term s PByteString
pcompressPublicKey pubKey = 
  plet (ptakeBS # 32 # pubKey) $ \xCoordinate -> 
    pif
      (peven yCoordinate)
      (pconstant "\x02" <> xCoordinate)
      (pconstant "\x03" <> xCoordinate)
  where
    yCoordinate = pdropBS # 32 # pubKey
    peven bs = (pmod # (pindexBS # bs # 31) # 2) #== 0

ptakeBS :: Term s (PInteger :--> PByteString :--> PByteString)
ptakeBS = phoistAcyclic $ plam $ \n bs ->
  psliceBS # 0 # n # bs

pdropBS :: Term s (PInteger :--> PByteString :--> PByteString)
pdropBS = phoistAcyclic $ plam $ \n bs -> 
  psliceBS # n # (plengthBS # bs - n) # bs 