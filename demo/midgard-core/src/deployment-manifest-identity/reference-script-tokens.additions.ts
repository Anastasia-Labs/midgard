import { DEPLOYMENT_MANIFEST_BASE_REFERENCE_TOKEN_NAMES } from "./reference-script-tokens.js";

// Reference-script token names added after the base table.
const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAME_ADDITIONS = {
  "V1 validation-trace CEK core staged bytes": "V1VtCekCoreStaByt",
  "V1 validation-trace CEK core staged integer": "V1VtCekCoreStaInt",
  "V1 validation-trace CEK core staged product": "V1VtCekCoreStaPro",
  "V1 validation-trace CEK core semantic serialise loop":
    "V1VtCekCoreSemSerLoo",
  "V1 validation-trace CEK core staged failure": "V1VtCekCoreStaFai",
  "V1 validation-trace CEK core staged conversion": "V1VtCekCoreStaCon",
  "V1 validation-trace CEK core staged write bits loop":
    "V1VtCekCoreStaWriBitLoo",
  "V1 validation-trace CEK core failure bls uncompress": "V1VtCekCoreFaiBlsUnc",
  "V1 validation-trace CEK core pv11 roots": "V1VtCekCorePv1Roo",
  "V1 validation-trace CEK core pv11 budget": "V1VtCekCorePv1Bud",
  "V1 validation-trace CEK core pv11 semantics": "V1VtCekCorePv1Sem",
  "V1 validation-trace native-scripts prepare": "V1VtNativeScriptsPrepare",
  "V1 validation-trace native-scripts terminal semantic":
    "V1VtNativeScriptsTerminalSem",
  "V1 validation-trace native-scripts native semantic":
    "V1VtNativeScriptsNativeSem",
  "V1 validation-trace native-scripts effectful semantic":
    "V1VtNativeScriptsEffectfulSem",
  "V1 validation-trace script-integrity prepare": "V1VtScriptIntegrityPrepare",
  "V1 validation-trace script-integrity authentication semantic":
    "V1VtIntegrityAuthenticationSem",
  "V1 validation-trace script-integrity compact semantic":
    "V1VtIntegrityCompactSem",
  "V1 validation-trace script-integrity witness set semantic":
    "V1VtIntegrityWitnessSetSem",
  "V1 validation-trace script-integrity finalize semantic":
    "V1VtIntegrityFinalizeSem",
  "V1 validation-trace value-and-mint prepare": "V1VtValueAndMintPrepare",
  "V1 validation-trace value-and-mint begin semantic": "V1VtValueMintBeginSem",
  "V1 validation-trace value-and-mint replay begin semantic":
    "V1VtValueMintReplayBeginSem",
  "V1 validation-trace value-and-mint replay input semantic":
    "V1VtValueMintReplayInputSem",
  "V1 validation-trace value-and-mint replay asset semantic":
    "V1VtValueMintReplayAssetSem",
  "V1 validation-trace value-and-mint replay finish semantic":
    "V1VtValueMintReplayFinishSem",
  "V1 validation-trace value-and-mint output descriptor semantic":
    "V1VtValueMintOutDescriptorSem",
  "V1 validation-trace value-and-mint output asset semantic":
    "V1VtValueMintOutputAssetSem",
  "V1 validation-trace value-and-mint output finish semantic":
    "V1VtValueMintOutputFinishSem",
  "V1 validation-trace value-and-mint mint asset semantic":
    "V1VtValueMintMintAssetSem",
  "V1 validation-trace value-and-mint mint finish semantic":
    "V1VtValueMintMintFinishSem",
  "V1 validation-trace value-and-mint finalize semantic":
    "V1VtValueMintFinalizeSem",
  "V1 field grammar certificate": "V1FieldGrammarCertSpend",
  "V1 field grammar certificate minting": "V1FieldGrammarCertMint",
} as const;

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES = Object.freeze({
  ...DEPLOYMENT_MANIFEST_BASE_REFERENCE_TOKEN_NAMES,
  ...DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAME_ADDITIONS,
} as const);
