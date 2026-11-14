/** Each validation phase the runner drives, with its resolver's and its
 * semantic resolvers' holders (one-step-phase.ts). */
import type * as SDK from "@al-ft/midgard-sdk";

import type { ValidationTracePhaseHolder } from "./one-step-phase-contracts.js";
import { SCRIPT_SOURCES_REDEEMER_SEMANTIC } from "./one-step-script-sources-redeemer.js";

/** Each phase the runner drives: its `resolver_index`
 * (validation-resolution-v1), its resolver's holder, and the holder of each
 * of its semantic resolvers, by the group-local index the evidence selects;
 * a step for a semantic resolver with no holder here is not driven, and
 * waits under the one-step-required alarm. */
export const PHASE_RESOLUTIONS = Object.freeze({
  compactBinding: {
    resolverIndex: 1,
    prepare: "compactBindingPrepare",
    semantics: ["compactBindingSemantic"],
  },
  staticLedgerRules: {
    resolverIndex: 2,
    prepare: "staticLedgerRulesPrepare",
    semantics: ["staticLedgerRulesSemantic"],
  },
  inputSets: {
    resolverIndex: 3,
    prepare: "inputSetsPrepare",
    semantics: ["inputSetsEmptySemantic", "inputSetsItemSemantic"],
  },
  signatures: {
    resolverIndex: 4,
    prepare: "signaturesPrepare",
    semantics: [
      "signaturesAdvanceSemantic",
      "signaturesAddressItemSemantic",
      "signaturesRequiredItemSemantic",
      "signaturesHandoffSemantic",
    ],
  },
  phaseANativeScripts: {
    resolverIndex: 5,
    prepare: "phaseANativeScriptsPrepare",
    semantics: [
      "phaseANativeScriptsAdvanceSemantic",
      "phaseANativeScriptsItemSemantic",
      "phaseANativeScriptsTokenHeadSemantic",
      "phaseANativeScriptsAllOrAnyContainerFrameSemantic",
      "phaseANativeScriptsAllOrAnyEmptySemantic",
      "phaseANativeScriptsAtLeastContainerFrameSemantic",
      "phaseANativeScriptsAtLeastEmptySemantic",
      "phaseANativeScriptsTimelockSemantic",
      "phaseANativeScriptsSignatureMembershipSemantic",
      "phaseANativeScriptsSignatureEmptySemantic",
      "phaseANativeScriptsSignatureBelowFirstSemantic",
      "phaseANativeScriptsSignatureAboveLastSemantic",
      "phaseANativeScriptsSignatureBetweenSemantic",
      "phaseANativeScriptsFrameSemantic",
    ],
  },
  phaseAScriptPreconditions: {
    resolverIndex: 6,
    prepare: "phaseAScriptPreconditionsPrepare",
    semantics: [
      "phaseAScriptPreconditionsFinalizeSemantic",
      "phaseAScriptPreconditionsItemSemantic",
    ],
  },
  // All but the non-membership semantic resolver, which a step reaches only
  // for a spent output that does not exist.
  resolveInputs: {
    resolverIndex: 7,
    prepare: "resolveInputsPrepare",
    semantics: [
      "resolveInputsInitialSemantic",
      "resolveInputsFinishSemantic",
      "resolveInputsMembershipBeginSemantic",
      "resolveInputsMembershipStepSemantic",
      "resolveInputsMembershipFinalizeSemantic",
      undefined,
    ],
  },
  // The semantic resolvers that verify the transition beside the auxiliary
  // evidence our trace carries, or none, invoking beside it the yields its
  // ledger-output proof names, or the observer yields, if any; the
  // non-output and redeemer descriptor semantic resolvers, invoking beside
  // it the yield of its role (one-step-script-sources-yields.ts); and the
  // redeemer normalization semantic resolver, whose resolution runs on
  // through the redeemer item chain when the deployment publishes it
  // (one-step-script-sources-redeemer.ts). The two missing-source semantic
  // resolvers reject a step our trace, which ends at its first rejection
  // and rejects only in CEK, never holds.
  scriptSources: {
    resolverIndex: 8,
    prepare: "scriptSourcesPrepare",
    semantics: [
      "scriptSourcesNonOutputSemantic",
      "scriptSourcesOutputProofBeginSemantic",
      "scriptSourcesOutputProofStepSemantic",
      "scriptSourcesOutputProofFinalizeSemantic",
      "scriptSourcesOutputProofFinishSemantic",
      "scriptSourcesStageZeroBeginSemantic",
      "scriptSourcesStageZeroFinishSemantic",
      "scriptSourcesStageZeroHashBlockSemantic",
      "scriptSourcesStageZeroHashAdvanceSemantic",
      "scriptSourcesStageZeroHashTerminalSemantic",
      "scriptSourcesStageNineMismatchSemantic",
      "scriptSourcesStageNineNativeMatchSemantic",
      "scriptSourcesStageNineEffectfulMatchSemantic",
      undefined,
      "scriptSourcesStageOneFinishSemantic",
      "scriptSourcesStageOneRedeemerSemantic",
      "scriptSourcesStageElevenFinishSemantic",
      "scriptSourcesStageElevenSourceSemantic",
      "scriptSourcesStageTwelveFinishSemantic",
      "scriptSourcesStageTwelveRedeemerSemantic",
      undefined,
      "scriptSourcesStageTenMismatchSemantic",
      "scriptSourcesStageTenMatchSemantic",
      "scriptSourcesStageEightFinishSemantic",
      "scriptSourcesStageEightPurposeSemantic",
      "scriptSourcesStageSevenObserverSemantic",
      "scriptSourcesStageSevenReceiveSemantic",
      "scriptSourcesStageSevenFinishSemantic",
      SCRIPT_SOURCES_REDEEMER_SEMANTIC,
    ],
  },
  nativeScripts: {
    resolverIndex: 9,
    prepare: "nativeScriptsPrepare",
    semantics: [
      "nativeScriptsTerminalSemantic",
      "nativeScriptsNativeSemantic",
      "nativeScriptsEffectfulSemantic",
    ],
  },
  scriptIntegrity: {
    resolverIndex: 10,
    prepare: "scriptIntegrityPrepare",
    semantics: [
      "scriptIntegrityAuthenticationSemantic",
      "scriptIntegrityCompactSemantic",
      "scriptIntegrityWitnessSetSemantic",
      "scriptIntegrityFinalizeSemantic",
    ],
  },
} as const satisfies Readonly<
  Partial<
    Record<
      keyof typeof SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS,
      Readonly<{
        resolverIndex: number;
        prepare: ValidationTracePhaseHolder;
        semantics: readonly (ValidationTracePhaseHolder | undefined)[];
      }>
    >
  >
>);

export type ValidationTracePhase = keyof typeof PHASE_RESOLUTIONS;

export const isValidationTracePhase = (
  phase: string,
): phase is ValidationTracePhase => Object.hasOwn(PHASE_RESOLUTIONS, phase);
