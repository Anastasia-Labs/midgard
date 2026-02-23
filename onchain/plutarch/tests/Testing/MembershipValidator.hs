{-# LANGUAGE OverloadedStrings #-}

module Testing.MembershipValidator (tests) where

import Test.Tasty
import Test.Tasty.HUnit

-- import Midgard.Utils (pand'List, pintToByteString)
import Testing.Eval (psucceeds)

-- import Midgard.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)

import Codec.Serialise (deserialise)
import Data.ByteString.Base16.Lazy qualified as BS16
import Data.ByteString.Lazy.Char8 qualified as BS8
import Plutarch.Internal.Term
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.MerkleTree.PatriciaForestry (MerklePatriciaForestry (..), ProofStep)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 hiding (POSIXTime)
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import PlutusTx.IsData qualified as PlutusTx
import Testing.ScriptContextBuilder (buildScriptContext, withRewardingScript)
import Types.Membership (MerkleMembershipRedeemer (..), MerkleNonMembershipRedeemer (..))
import Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)

tests :: TestTree
tests =
  testGroup
    "(Non)Membership Validator Tests"
    [ testCase "Simple Membership" $ do
        psucceeds $
          membershipStakeValidator #$ pconstant . buildScriptContext $
            withRewardingScript (toBuiltinData simpleMembershipRedeemer1) membershipValidatorCredential 0
        psucceeds $
          membershipStakeValidator #$ pconstant . buildScriptContext $
            withRewardingScript (toBuiltinData simpleMembershipRedeemer2) membershipValidatorCredential 0
    , testCase "Simple Non Membership" $ do
        psucceeds $
          nonMembershipStakeValidator #$ pconstant . buildScriptContext $
            withRewardingScript (toBuiltinData simpleNonMembershipRedeemer) nonMembershipValidatorCredential 0
    ]

-- Note: Should always use the same compile config as 'psucceeds' above.
membershipValidatorCredential :: Credential
membershipValidatorCredential =
  ScriptCredential
    . scriptHash
    . either (error . show) id
    $ compile (Tracing LogInfo DoTracing) membershipStakeValidator
nonMembershipValidatorCredential :: Credential
nonMembershipValidatorCredential =
  ScriptCredential
    . scriptHash
    . either (error . show) id
    $ compile (Tracing LogInfo DoTracing) nonMembershipStakeValidator

-- Example Merkle Tree generated via mpf-cli
-- N.B: Keys and values are in hex.
-- { "deadbeef": "face", "cafe": "decaf" }
-- root_hash: 90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d
-- proofCBOR (0): 9fd87b9f0058204e400278c29c37ee640391dfb9792390a8ac9adb6200ed47c725a86099a8586c582028b77fdfb12e58d34edf655736e1d62414635099c5a9e1ac49e52cbb89ae3100ffff
-- proofCBOR (1): 9fd87b9f005820f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd915820888938a89cccc775894c0a433c2f7d7bfee5a7d64ac5563c22ca54b8a0cc4644ffff
-- TODO (chase): In the future, we should try to use Haskell for the generation of these.
simpleMembershipRedeemer1 :: MerkleMembershipRedeemer
simpleMembershipRedeemer1 =
  MerkleMembershipRedeemer
    { mmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d"
    , mmInputKey = stringToBuiltinByteStringHex "deadbeef"
    , mmInputValue = stringToBuiltinByteStringHex "face"
    , mmInputProof = parseProofCBOR "9fd87b9f0058204e400278c29c37ee640391dfb9792390a8ac9adb6200ed47c725a86099a8586c582028b77fdfb12e58d34edf655736e1d62414635099c5a9e1ac49e52cbb89ae3100ffff"
    }
simpleMembershipRedeemer2 :: MerkleMembershipRedeemer
simpleMembershipRedeemer2 =
  MerkleMembershipRedeemer
    { mmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "90ea40c96e91b5d6c4518a56e468e20ed0ecfee07c4d300503198dfc7173bc9d"
    , mmInputKey = stringToBuiltinByteStringHex "cafe"
    , mmInputValue = stringToBuiltinByteStringHex "decaf"
    , mmInputProof = parseProofCBOR "9fd87b9f005820f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd915820888938a89cccc775894c0a433c2f7d7bfee5a7d64ac5563c22ca54b8a0cc4644ffff"
    }

-- The root hash has changed to the previous tree after deleting "cafe".
simpleNonMembershipRedeemer :: MerkleNonMembershipRedeemer
simpleNonMembershipRedeemer =
  MerkleNonMembershipRedeemer
    { mnmInputRoot = MerklePatriciaForestry $ stringToBuiltinByteStringHex "a821131f1d697b5f9203b2d95ac8c54ecde0135301ec32e31a8c1255b7c65a7f"
    , mnmInputKey = stringToBuiltinByteStringHex "cafe"
    , mnmInputProof = parseProofCBOR "9fd87b9f005820f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd915820888938a89cccc775894c0a433c2f7d7bfee5a7d64ac5563c22ca54b8a0cc4644ffff"
    }

-- | Given a hex string representing a CBOR encoded proof, parse it into the corresponding list of ProofSteps.
parseProofCBOR :: String -> [ProofStep]
parseProofCBOR =
  PlutusTx.unsafeFromData
    . deserialise @PD.Data
    . either error id
    . BS16.decode
    . BS8.pack
