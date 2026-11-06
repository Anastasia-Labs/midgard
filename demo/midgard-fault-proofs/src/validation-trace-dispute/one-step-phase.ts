/** The one-step resolution of a step of a validation phase whose resolver
 * is the generic phase resolver and whose semantic resolver awards the step
 * in one hop: past the boundary, the phase resolver commits the transition
 * and its auxiliary evidence and hands the step to the semantic resolver
 * the evidence selects, which verifies the transition beside that evidence
 * (none, or one item of a field whose preimage the evidence carries
 * inline, with a required signer's proof of signing beside it, or with the
 * yield that verifies the item by its kind invoked beside it) and awards
 * the step. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import { NO_AUXILIARY_WITNESS_CBOR } from "./one-step-canonical-decode.js";
import type { ValidationTracePhaseHolder } from "./one-step-phase-contracts.js";
import {
  inlineMaxPreimageBytesBeside,
  LEDGER_OUTPUT_PROOF_SEMANTICS,
  PHASE_SEMANTIC_EVIDENCE,
} from "./one-step-phase-evidence.js";
import {
  itemYieldOf,
  type ValidationTracePhaseItemYield,
  type ValidationTracePhaseYield,
} from "./one-step-phase-yields.js";
import { SCRIPT_SOURCES_REDEEMER_SEMANTIC } from "./one-step-script-sources-redeemer.js";
import {
  SCRIPT_SOURCES_YIELD_SEMANTICS,
  type ScriptSourcesYieldSemantic,
} from "./one-step-script-sources-yields.js";

export {
  isValidationTracePhaseHolder,
  PHASE_CONTRACT_NAMES,
  PHASE_HOLDERS,
  type ValidationTracePhaseHolder,
} from "./one-step-phase-contracts.js";
export { phaseSemanticHop } from "./one-step-phase-hop.js";
export {
  phaseYieldListReads,
  phaseYieldReads,
  type ValidationTracePhaseItemYield,
  type ValidationTracePhaseYield,
  type ValidationTracePhaseYieldListReads,
  type ValidationTracePhaseYieldReads,
} from "./one-step-phase-yields.js";

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

type Chain = Pick<
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  "prepareResolvers" | "semanticResolvers"
>;

/** The validator of each phase holder on `chain`. */
export const phaseValidators = (
  chain: Chain,
): Readonly<Record<ValidationTracePhaseHolder, SDK.SpendingValidator>> => {
  const validators: Partial<
    Record<ValidationTracePhaseHolder, SDK.SpendingValidator>
  > = {};
  for (const [phase, { resolverIndex, prepare, semantics }] of Object.entries(
    PHASE_RESOLUTIONS,
  ) as [
    ValidationTracePhase,
    (typeof PHASE_RESOLUTIONS)[ValidationTracePhase],
  ][]) {
    validators[prepare] = chain.prepareResolvers[resolverIndex];
    semantics.forEach((holder, local) => {
      if (holder === undefined) return;
      const index = SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS[phase][local];
      const resolver =
        index === undefined ? undefined : chain.semanticResolvers[index];
      if (resolver === undefined)
        throw new Error(
          `validation-trace deployment has no ${phase} semantic resolver ${local.toString()}`,
        );
      validators[holder] = resolver;
    });
  }
  return Object.freeze(
    validators as Record<ValidationTracePhaseHolder, SDK.SpendingValidator>,
  );
};

/** The script-sources semantic resolver `holder` whose evidence and yield
 * one-step-script-sources-yields.ts derives, if it is one. */
const yieldSemantic = (
  holder: ValidationTracePhaseHolder,
): ScriptSourcesYieldSemantic | undefined =>
  Object.hasOwn(SCRIPT_SOURCES_YIELD_SEMANTICS, holder)
    ? SCRIPT_SOURCES_YIELD_SEMANTICS[
        holder as keyof typeof SCRIPT_SOURCES_YIELD_SEMANTICS
      ]
    : undefined;

/** The yields the semantic resolvers of `phase` invoke. */
export const phaseYields = (
  phase: ValidationTracePhase,
): readonly ValidationTracePhaseYield[] =>
  PHASE_RESOLUTIONS[phase].semantics.flatMap((holder) =>
    holder === undefined
      ? []
      : (PHASE_SEMANTIC_EVIDENCE[holder]?.itemYields ??
        PHASE_SEMANTIC_EVIDENCE[holder]?.yieldFields ??
        LEDGER_OUTPUT_PROOF_SEMANTICS[holder]?.yields ??
        yieldSemantic(holder)?.yields ??
        []),
  );

/** `Inline`'s constructor among the field carriages. */
const INLINE_CARRIAGE = 0;

/** The longest field preimage the resolution of a step of `phase` for the
 * group-local semantic resolver `semanticResolverIndex` carries inline,
 * when its transition and auxiliary evidence carry `besideBytes` beside the
 * field: past it the step's evidence names our publications of the field
 * instead. None for a semantic resolver that reads no field. */
export const phaseInlineMaxPreimageBytes = (
  phase: ValidationTracePhase,
  semanticResolverIndex: number,
  besideBytes: number,
): number | undefined => {
  const semantic: ValidationTracePhaseHolder | undefined =
    PHASE_RESOLUTIONS[phase].semantics[semanticResolverIndex];
  const limit =
    semantic === undefined
      ? undefined
      : (PHASE_SEMANTIC_EVIDENCE[semantic]?.inlineLimit ??
        yieldSemantic(semantic)?.inlineLimit);
  return limit === undefined
    ? undefined
    : inlineMaxPreimageBytesBeside(limit, besideBytes);
};

/** A phase step as its resolution needs it: the holders the thread visits
 * past the boundary, the fields of the auxiliary evidence the semantic
 * resolver takes beside the transition, as their Data's CBOR (none when it
 * takes `NoAuxiliaryWitness`), with what it claims of the item beside them,
 * and the yield it invokes beside the item, or the yields whose
 * reference-input indices follow the evidence, as one list or each a field
 * of its own, if any. */
export type ValidationTracePhaseStep = Readonly<{
  phase: ValidationTracePhase;
  prepare: ValidationTracePhaseHolder;
  semantic: ValidationTracePhaseHolder;
  /** The semantic resolver's action, when not its first. */
  action?: number;
  evidenceCbors: readonly string[];
  itemYield?: ValidationTracePhaseItemYield;
  yieldList?: readonly ValidationTracePhaseYield[];
  yieldFields?: readonly ValidationTracePhaseYield[];
}>;

/**
 * The resolution of the step of `phase` whose one-step argument selects
 * `resolverIndex` and the group-local `semanticResolverIndex` with the
 * transition `transitionCbor` beside the auxiliary evidence
 * `auxiliaryCbor`, to the successor whose work witness is
 * `successorWitnessCbor` (none when the step rejects), whose evidence reads
 * the field preimage `carried` by reference, if any; or why the runner does
 * not drive it.
 */
export const planValidationTracePhaseStep = ({
  phase,
  resolverIndex,
  semanticResolverIndex,
  transitionCbor,
  auxiliaryCbor,
  successorWitnessCbor,
  redeemerChain = false,
  carried,
}: {
  readonly phase: ValidationTracePhase;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: string;
  readonly auxiliaryCbor: string;
  readonly successorWitnessCbor: Uint8Array | undefined;
  /** Whether the deployment publishes the script-sources redeemer item
   * chain past its entry. */
  readonly redeemerChain?: boolean;
  /** The preimage of the field the evidence reads by reference. */
  readonly carried?: Buffer | undefined;
}): ValidationTracePhaseStep | Readonly<{ reason: string }> => {
  const resolution = PHASE_RESOLUTIONS[phase];
  if (resolverIndex !== resolution.resolverIndex)
    throw new Error(`validation-trace ${phase} step selects another resolver`);
  const semantic: ValidationTracePhaseHolder | undefined =
    resolution.semantics[semanticResolverIndex];
  const notDriven = {
    reason: `a ${phase} step for semantic resolver ${semanticResolverIndex.toString()}`,
  };
  if (semantic === undefined) return notDriven;
  // Only a semantic resolver that opens a field reads one by reference.
  const unread = {
    reason: `a ${phase} step for semantic resolver ${semanticResolverIndex.toString()} reading a field by reference`,
  };
  const step = (
    evidenceCbors: readonly string[],
    itemYield?: ValidationTracePhaseItemYield,
  ): ValidationTracePhaseStep =>
    Object.freeze({
      phase,
      prepare: resolution.prepare,
      semantic,
      evidenceCbors: Object.freeze(evidenceCbors),
      ...(itemYield === undefined ? {} : { itemYield }),
    });
  // The redeemer item chain derives its states from the evidence whole.
  if (semantic === SCRIPT_SOURCES_REDEEMER_SEMANTIC)
    return carried !== undefined
      ? unread
      : redeemerChain
        ? step([])
        : notDriven;
  const yielding = yieldSemantic(semantic);
  if (yielding !== undefined) {
    // The middle yield of its role opens the field it reads.
    if (carried !== undefined && yielding.inlineLimit === undefined)
      return unread;
    const yielded = yielding.evidence({ transitionCbor, auxiliaryCbor });
    if (yielded === undefined) return notDriven;
    const { evidenceCbors, action, itemYield } = yielded;
    return Object.freeze({
      ...step(evidenceCbors, itemYield),
      ...(action === 0 ? {} : { action }),
    });
  }
  const evidence = PHASE_SEMANTIC_EVIDENCE[semantic];
  if (carried !== undefined && evidence?.carriage === undefined) return unread;
  const derived = LEDGER_OUTPUT_PROOF_SEMANTICS[semantic];
  if (derived !== undefined) {
    const { evidenceCbors, yieldList } = derived.evidence({
      transitionCbor,
      auxiliaryCbor,
      successorWitnessCbor,
    });
    return Object.freeze({ ...step(evidenceCbors), yieldList });
  }
  const auxiliary = Data.from(auxiliaryCbor);
  // The semantic resolver verifies the transition beside the auxiliary
  // evidence it takes, which is all the resolver may then have committed.
  if (
    evidence === undefined
      ? auxiliaryCbor !== NO_AUXILIARY_WITNESS_CBOR
      : !(auxiliary instanceof Constr) ||
        Number(auxiliary.index) !== evidence.auxiliary
  )
    throw new Error(
      `validation-trace ${phase} step carries auxiliary evidence its semantic resolver does not take`,
    );
  if (evidence === undefined) return step([]);
  // It takes the auxiliary evidence's fields but those it fixes itself,
  // which our evidence must carry at the values it fixes them to.
  const { fields } = auxiliary as Constr<Data>;
  const fixes = evidence.fixes ?? {};
  const fieldCbors = fields.flatMap((_, index) => {
    const cbor = plutusConstrFieldCbor(auxiliaryCbor, [index]);
    if (!Object.hasOwn(fixes, index)) return [cbor];
    if (cbor !== fixes[index])
      throw new Error(
        `validation-trace ${phase} step carries auxiliary evidence its semantic resolver does not take`,
      );
    return [];
  });
  if (evidence.carriage === undefined) return step(fieldCbors);
  // The field the item is read from: carried inline, or by reference.
  const carriage = fields[evidence.carriage];
  const inline =
    carriage instanceof Constr && Number(carriage.index) === INLINE_CARRIAGE
      ? carriage.fields[0]
      : undefined;
  const preimage = carried?.toString("hex") ?? inline;
  if (
    typeof preimage !== "string" ||
    (carried !== undefined) === (inline !== undefined)
  )
    throw new Error(
      `validation-trace ${phase} step's evidence carries its field neither inline nor by reference`,
    );
  return Object.freeze({
    ...step(
      [
        ...fieldCbors,
        ...(evidence.claims?.(preimage, Number(fields[1])) ?? []),
      ],
      evidence.itemYields === undefined
        ? undefined
        : itemYieldOf(evidence.itemYields, preimage, fields[1]),
    ),
    ...(evidence.yieldFields === undefined
      ? {}
      : { yieldFields: evidence.yieldFields }),
  });
};

/** Where each phase holder's datum keeps our successor's hash: the
 * resolution's challenger successor hash, behind the prepared state at the
 * semantic resolver. */
export const PHASE_CHALLENGER_SUCCESSOR_HASH = Object.freeze(
  Object.fromEntries(
    Object.values(PHASE_RESOLUTIONS).flatMap(({ prepare, semantics }) => [
      [prepare, [1, 0, 3]],
      ...semantics.flatMap((holder) =>
        holder === undefined ? [] : [[holder, [1, 0, 1, 3]] as const],
      ),
    ]),
  ) as Readonly<Record<ValidationTracePhaseHolder, readonly number[]>>,
);
