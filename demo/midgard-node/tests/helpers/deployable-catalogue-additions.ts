/**
 * The catalogue entries added after each pinned generation of
 * `deployable-scripts.test.ts`: projecting out exactly these entries must
 * reproduce the older generation's order digests.
 */

import { expect } from "vitest";

export const HISTORY_CATALOGUE_ADDITIONS = [
  {
    contract: "depositHistoryRetentionSpend",
    role: "deposit history retention",
    purpose: "spend",
    commands: ["deposit"],
  },
  {
    contract: "depositHistoryRetirementWithdraw",
    role: "deposit history retirement",
    purpose: "withdraw",
    commands: ["deposit"],
  },
  {
    contract: "withdrawalHistoryRetentionSpend",
    role: "withdrawal history retention",
    purpose: "spend",
    commands: ["withdrawal"],
  },
  {
    contract: "withdrawalHistoryRetirementWithdraw",
    role: "withdrawal history retirement",
    purpose: "withdraw",
    commands: ["withdrawal"],
  },
  {
    contract: "fraudProofTransitionTraceL1EventWithdraw",
    role: "V1 fraud-proof transition-trace final-6 L1 event yield",
    purpose: "withdraw",
    commands: [],
  },
  {
    contract: "fraudProofTransitionTraceForcedTimingWithdraw",
    role: "V1 fraud-proof transition-trace final-6 forced timing yield",
    purpose: "withdraw",
    commands: [],
  },
] as const;

// The pooled DA committee bond: its spend and its one-shot mint, which the
// atomic protocol init runs to create the pool.
export const POOL_CATALOGUE_ADDITIONS = [
  {
    contract: "daBondPoolSpend",
    role: "da-bond-pool spending",
    purpose: "spend",
    commands: ["da"],
  },
  {
    contract: "daBondPoolMint",
    role: "da-bond-pool minting",
    purpose: "mint",
    commands: ["protocol-init", "da"],
  },
] as const;

export const poolContracts = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const poolRoles = new Set<string>(
  POOL_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

export const historyContracts = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const historyRoles = new Set<string>(
  HISTORY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The inline-datum steps of the non-canonical and resolved output proofs, the
// data step of the redeemer canonicity proof, and the scalar half of the
// redeemer item's Data refusal executor.
const DATA_CANONICITY_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeRedeemerItemInvalidScalarDataExecutor",
    role: "V1 validation-trace redeemer item invalid scalar data executor",
  },
  {
    contract: "fraudProofTransactionOutputNonCanonicalStep03Datum",
    role: "V1 fraud-proof transaction-output-non-canonical step-03 datum",
  },
  {
    contract: "fraudProofResolvedOutputNonCanonicalStep04Datum",
    role: "V1 fraud-proof resolved-output-non-canonical step-04 datum",
  },
  {
    contract: "fraudProofRedeemerCanonicityStep02Data",
    role: "V1 fraud-proof redeemer-canonicity step-02 data",
  },
] as const;

export const dataCanonicityContracts = new Set<string>(
  DATA_CANONICITY_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const dataCanonicityRoles = new Set<string>(
  DATA_CANONICITY_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The per-header bond yield sat right after the availability mint in every
// order and in the one command (`da`) that published the mint.
const RETIRED_BOND_ROLE = "availability-challenge bond withdrawal";

export const withRetiredBondRole = <T extends string | null>(
  names: readonly T[],
): readonly (T | string)[] => {
  const mint = names.indexOf("availability-challenge minting" as T);
  if (mint < 0) return names;
  expect(names).not.toContain(RETIRED_BOND_ROLE);
  return [
    ...names.slice(0, mint + 1),
    RETIRED_BOND_ROLE,
    ...names.slice(mint + 1),
  ];
};

// The validation case, its one-shot configuration mint (which protocol
// initialization runs), its thread expiry, the award's mint (the lost-defense
// receipt policy) and the dispute bond spend.
const CASE_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeAwardMint",
    role: "V1 validation-trace award minting",
  },
  { contract: "validationTraceDisputeCase", role: "V1 validation-trace case" },
  {
    contract: "validationTraceDisputeCaseMint",
    role: "V1 validation-trace case minting",
  },
  {
    contract: "validationTraceDisputeCaseConfigMint",
    role: "V1 validation-trace case configuration minting",
  },
  {
    contract: "validationTraceDisputeThreadExpiryWithdraw",
    role: "V1 validation-trace thread expiry",
  },
  {
    contract: "validationTraceDisputeBond",
    role: "V1 validation-trace dispute bond",
  },
] as const;

export const caseContracts = new Set<string>(
  CASE_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const caseRoles = new Set<string>(
  CASE_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The CEK phase's one-step resolver and its four semantic resolvers, which a
// validation-trace thread visits between the boundary and the CEK stages.
const CEK_RESOLVER_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeCekPrepare",
    role: "V1 validation-trace CEK prepare",
  },
  {
    contract: "validationTraceDisputeCekFinishSemantic",
    role: "V1 validation-trace CEK finish semantic",
  },
  {
    contract: "validationTraceDisputeCekExecutionSelectionSemantic",
    role: "V1 validation-trace CEK execution selection semantic",
  },
  {
    contract: "validationTraceDisputeCekContextStepSemantic",
    role: "V1 validation-trace CEK context step semantic",
  },
  {
    contract: "validationTraceDisputeCekCoreStepSemantic",
    role: "V1 validation-trace CEK core step semantic",
  },
] as const;

export const cekResolverContracts = new Set<string>(
  CEK_RESOLVER_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const cekResolverRoles = new Set<string>(
  CEK_RESOLVER_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The validation phases' one-step resolvers and their semantic resolvers,
// added phase by phase: a validation-trace thread visits them between the
// boundary and the award.
const PHASE_RESOLVER_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeCompactBindingPrepare",
    role: "V1 validation-trace compact-binding prepare",
  },
  {
    contract: "validationTraceDisputeCompactBindingSemantic",
    role: "V1 validation-trace compact-binding semantic",
  },
  {
    contract: "validationTraceDisputeStaticLedgerRulesPrepare",
    role: "V1 validation-trace static-ledger-rules prepare",
  },
  {
    contract: "validationTraceDisputeStaticLedgerRulesSemantic",
    role: "V1 validation-trace static-ledger-rules semantic",
  },
  {
    contract: "validationTraceDisputeInputSetsPrepare",
    role: "V1 validation-trace input-sets prepare",
  },
  {
    contract: "validationTraceDisputeInputSetsEmptySemantic",
    role: "V1 validation-trace input-sets empty semantic",
  },
  {
    contract: "validationTraceDisputeInputSetsItemSemantic",
    role: "V1 validation-trace input-sets item semantic",
  },
  {
    contract: "validationTraceDisputeSignaturesPrepare",
    role: "V1 validation-trace signatures prepare",
  },
  {
    contract: "validationTraceDisputeSignaturesAdvanceSemantic",
    role: "V1 validation-trace signatures advance semantic",
  },
  {
    contract: "validationTraceDisputeSignaturesAddressItemSemantic",
    role: "V1 validation-trace signatures address item semantic",
  },
  {
    contract: "validationTraceDisputeSignaturesRequiredItemSemantic",
    role: "V1 validation-trace signatures required item semantic",
  },
  {
    contract: "validationTraceDisputeSignaturesHandoffSemantic",
    role: "V1 validation-trace signatures handoff semantic",
  },
  {
    contract: "validationTraceDisputePhaseANativeScriptsPrepare",
    role: "V1 validation-trace phase-A native scripts prepare",
  },
  {
    contract: "validationTraceDisputePhaseAScriptPreconditionsPrepare",
    role: "V1 validation-trace phase-A script preconditions prepare",
  },
  {
    contract: "validationTraceDisputeResolveInputsPrepare",
    role: "V1 validation-trace resolve-inputs prepare",
  },
  {
    contract: "validationTraceDisputeScriptSourcesPrepare",
    role: "V1 validation-trace script-sources prepare",
  },
] as const;

export const phaseResolverContracts = new Set<string>(
  PHASE_RESOLVER_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const phaseResolverRoles = new Set<string>(
  PHASE_RESOLVER_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The CEK core's semantics steps over staged operands: the byte-string and
// string builtins, the linear integer builtins, multiplyInteger and the
// division builtins, the serialise loop for serialiseData and
// byteStringToInteger; the failure hop of the byte-string, string,
// signature and integer builtins; the BLS12-381 uncompress failure arm; and
// the protocol version 11 builtins' roots, budget and semantics arms.
const CEK_STAGED_CATALOGUE_ADDITIONS = [
  {
    contract: "validationTraceDisputeCekCoreStagedBytes",
    role: "V1 validation-trace CEK core staged bytes",
  },
  {
    contract: "validationTraceDisputeCekCoreStagedInteger",
    role: "V1 validation-trace CEK core staged integer",
  },
  {
    contract: "validationTraceDisputeCekCoreStagedProduct",
    role: "V1 validation-trace CEK core staged product",
  },
  {
    contract: "validationTraceDisputeCekCoreSemanticSerialiseLoop",
    role: "V1 validation-trace CEK core semantic serialise loop",
  },
  {
    contract: "validationTraceDisputeCekCoreStagedFailure",
    role: "V1 validation-trace CEK core staged failure",
  },
  {
    contract: "validationTraceDisputeCekCoreStagedConversion",
    role: "V1 validation-trace CEK core staged conversion",
  },
  {
    contract: "validationTraceDisputeCekCoreStagedWriteBitsLoop",
    role: "V1 validation-trace CEK core staged write bits loop",
  },
  {
    contract: "validationTraceDisputeCekCoreFailureBlsUncompress",
    role: "V1 validation-trace CEK core failure bls uncompress",
  },
  {
    contract: "validationTraceDisputeCekCorePv11Roots",
    role: "V1 validation-trace CEK core pv11 roots",
  },
  {
    contract: "validationTraceDisputeCekCorePv11Budget",
    role: "V1 validation-trace CEK core pv11 budget",
  },
  {
    contract: "validationTraceDisputeCekCorePv11Semantics",
    role: "V1 validation-trace CEK core pv11 semantics",
  },
] as const;

export const cekStagedContracts = new Set<string>(
  CEK_STAGED_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const cekStagedRoles = new Set<string>(
  CEK_STAGED_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The native-scripts, script-integrity and value-and-mint phases' prepare and
// semantic resolvers, the last validation-trace steps to gain members. Each
// is a spend reference script that only the node runtime publishes.
export const REMAINING_PHASE_CATALOGUE_ADDITIONS = (
  [
    {
      contract: "validationTraceDisputeNativeScriptsPrepare",
      role: "V1 validation-trace native-scripts prepare",
    },
    {
      contract: "validationTraceDisputeNativeScriptsTerminalSemantic",
      role: "V1 validation-trace native-scripts terminal semantic",
    },
    {
      contract: "validationTraceDisputeNativeScriptsNativeSemantic",
      role: "V1 validation-trace native-scripts native semantic",
    },
    {
      contract: "validationTraceDisputeNativeScriptsEffectfulSemantic",
      role: "V1 validation-trace native-scripts effectful semantic",
    },
    {
      contract: "validationTraceDisputeScriptIntegrityPrepare",
      role: "V1 validation-trace script-integrity prepare",
    },
    {
      contract: "validationTraceDisputeScriptIntegrityAuthenticationSemantic",
      role: "V1 validation-trace script-integrity authentication semantic",
    },
    {
      contract: "validationTraceDisputeScriptIntegrityCompactSemantic",
      role: "V1 validation-trace script-integrity compact semantic",
    },
    {
      contract: "validationTraceDisputeScriptIntegrityWitnessSetSemantic",
      role: "V1 validation-trace script-integrity witness set semantic",
    },
    {
      contract: "validationTraceDisputeScriptIntegrityFinalizeSemantic",
      role: "V1 validation-trace script-integrity finalize semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintPrepare",
      role: "V1 validation-trace value-and-mint prepare",
    },
    {
      contract: "validationTraceDisputeValueAndMintBeginSemantic",
      role: "V1 validation-trace value-and-mint begin semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintReplayBeginSemantic",
      role: "V1 validation-trace value-and-mint replay begin semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintReplayInputSemantic",
      role: "V1 validation-trace value-and-mint replay input semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintReplayAssetSemantic",
      role: "V1 validation-trace value-and-mint replay asset semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintReplayFinishSemantic",
      role: "V1 validation-trace value-and-mint replay finish semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintOutputDescriptorSemantic",
      role: "V1 validation-trace value-and-mint output descriptor semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintOutputAssetSemantic",
      role: "V1 validation-trace value-and-mint output asset semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintOutputFinishSemantic",
      role: "V1 validation-trace value-and-mint output finish semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintMintAssetSemantic",
      role: "V1 validation-trace value-and-mint mint asset semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintMintFinishSemantic",
      role: "V1 validation-trace value-and-mint mint finish semantic",
    },
    {
      contract: "validationTraceDisputeValueAndMintFinalizeSemantic",
      role: "V1 validation-trace value-and-mint finalize semantic",
    },
  ] as const
).map((addition) => ({
  ...addition,
  purpose: "spend" as const,
  commands: [] as const,
}));

export const remainingPhaseContracts = new Set<string>(
  REMAINING_PHASE_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const remainingPhaseRoles = new Set<string>(
  REMAINING_PHASE_CATALOGUE_ADDITIONS.map(({ role }) => role),
);

// The field grammar certificate's spend and mint, appended after the last
// member. A field grammar certificate lets a step read one item of a
// variable-width field without walking the whole field, so the step's cost no
// longer grows with the field's item count. Both are reference scripts that
// only the node runtime publishes.
export const FIELD_GRAMMAR_CATALOGUE_ADDITIONS = [
  {
    contract: "fieldGrammarCertificateSpend",
    role: "V1 field grammar certificate",
    purpose: "spend",
    commands: [],
  },
  {
    contract: "fieldGrammarCertificateMint",
    role: "V1 field grammar certificate minting",
    purpose: "mint",
    commands: [],
  },
] as const;

export const fieldGrammarContracts = new Set<string>(
  FIELD_GRAMMAR_CATALOGUE_ADDITIONS.map(({ contract }) => contract),
);
export const fieldGrammarRoles = new Set<string>(
  FIELD_GRAMMAR_CATALOGUE_ADDITIONS.map(({ role }) => role),
);
