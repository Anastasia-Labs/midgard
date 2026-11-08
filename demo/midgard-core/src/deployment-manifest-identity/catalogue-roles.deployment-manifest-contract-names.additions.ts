import { DEPLOYMENT_MANIFEST_BASE_CONTRACT_NAMES } from "./catalogue-roles.deployment-manifest-contract-names.js";

// Contract names appended after the base table, in manifest order.
const DEPLOYMENT_MANIFEST_CONTRACT_NAME_ADDITIONS = [
  "validationTraceDisputeCekCoreStagedBytes",
  "validationTraceDisputeCekCoreStagedInteger",
  "validationTraceDisputeCekCoreStagedProduct",
  "validationTraceDisputeCekCoreSemanticSerialiseLoop",
  "validationTraceDisputeCekCoreStagedFailure",
  "validationTraceDisputeCekCoreStagedConversion",
  "validationTraceDisputeCekCoreStagedWriteBitsLoop",
  "fraudProofDistinctAssetAccumulationLimitInputRank",
  "fraudProofDistinctAssetAccumulationLimitOutputsGrammar",
  "fraudProofDistinctAssetAccumulationLimitOutputs",
  "fraudProofDistinctAssetAccumulationLimitMintGrammar",
  "fraudProofDistinctAssetAccumulationLimitMint",
  "fraudProofDistinctAssetAccumulationLimitTerminal",
  "validationTraceDisputeCekCoreFailureBlsUncompress",
  "validationTraceDisputeCekCorePv11Roots",
  "validationTraceDisputeCekCorePv11Budget",
  "validationTraceDisputeCekCorePv11Semantics",
  "validationTraceDisputeNativeScriptsPrepare",
  "validationTraceDisputeNativeScriptsTerminalSemantic",
  "validationTraceDisputeNativeScriptsNativeSemantic",
  "validationTraceDisputeNativeScriptsEffectfulSemantic",
  "validationTraceDisputeScriptIntegrityPrepare",
  "validationTraceDisputeScriptIntegrityAuthenticationSemantic",
  "validationTraceDisputeScriptIntegrityCompactSemantic",
  "validationTraceDisputeScriptIntegrityWitnessSetSemantic",
  "validationTraceDisputeScriptIntegrityFinalizeSemantic",
  "validationTraceDisputeValueAndMintPrepare",
  "validationTraceDisputeValueAndMintBeginSemantic",
  "validationTraceDisputeValueAndMintReplayBeginSemantic",
  "validationTraceDisputeValueAndMintReplayInputSemantic",
  "validationTraceDisputeValueAndMintReplayAssetSemantic",
  "validationTraceDisputeValueAndMintReplayFinishSemantic",
  "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
  "validationTraceDisputeValueAndMintOutputAssetSemantic",
  "validationTraceDisputeValueAndMintOutputFinishSemantic",
  "validationTraceDisputeValueAndMintMintAssetSemantic",
  "validationTraceDisputeValueAndMintMintFinishSemantic",
  "validationTraceDisputeValueAndMintFinalizeSemantic",
  // The field grammar certificate: one script whose mint opens a certificate
  // and whose spend advances or reclaims it. A step that holds one reads an
  // item of a variable-width field without walking the whole field.
  "fieldGrammarCertificateSpend",
  "fieldGrammarCertificateMint",
] as const;

export const DEPLOYMENT_MANIFEST_CONTRACT_NAMES = Object.freeze([
  ...DEPLOYMENT_MANIFEST_BASE_CONTRACT_NAMES,
  ...DEPLOYMENT_MANIFEST_CONTRACT_NAME_ADDITIONS,
] as const);
