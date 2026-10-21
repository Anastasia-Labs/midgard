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
import {
  Constr,
  Data,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import { NO_AUXILIARY_WITNESS_CBOR } from "./one-step-canonical-decode.js";
import { constr, continued, integer, stepDatum } from "./one-step-data.js";
import {
  LEDGER_OUTPUT_PROOF_SEMANTICS,
  PHASE_SEMANTIC_EVIDENCE,
} from "./one-step-phase-evidence.js";
import {
  itemYieldOf,
  type ValidationTracePhaseItemYield,
  type ValidationTracePhaseYield,
  type ValidationTracePhaseYieldListReads,
  type ValidationTracePhaseYieldReads,
} from "./one-step-phase-yields.js";
import { SCRIPT_SOURCES_REDEEMER_SEMANTIC } from "./one-step-script-sources-redeemer.js";

export {
  phaseYieldListReads,
  phaseYieldReads,
  type ValidationTracePhaseItemYield,
  type ValidationTracePhaseYield,
  type ValidationTracePhaseYieldListReads,
  type ValidationTracePhaseYieldReads,
} from "./one-step-phase-yields.js";

/** The holders of each phase's resolution, with the deployment manifest's
 * name for each. */
export const PHASE_CONTRACT_NAMES = Object.freeze({
  compactBindingPrepare: "validationTraceDisputeCompactBindingPrepare",
  compactBindingSemantic: "validationTraceDisputeCompactBindingSemantic",
  staticLedgerRulesPrepare: "validationTraceDisputeStaticLedgerRulesPrepare",
  staticLedgerRulesSemantic: "validationTraceDisputeStaticLedgerRulesSemantic",
  inputSetsPrepare: "validationTraceDisputeInputSetsPrepare",
  inputSetsEmptySemantic: "validationTraceDisputeInputSetsEmptySemantic",
  inputSetsItemSemantic: "validationTraceDisputeInputSetsItemSemantic",
  signaturesPrepare: "validationTraceDisputeSignaturesPrepare",
  signaturesAdvanceSemantic: "validationTraceDisputeSignaturesAdvanceSemantic",
  signaturesAddressItemSemantic:
    "validationTraceDisputeSignaturesAddressItemSemantic",
  signaturesRequiredItemSemantic:
    "validationTraceDisputeSignaturesRequiredItemSemantic",
  signaturesHandoffSemantic: "validationTraceDisputeSignaturesHandoffSemantic",
  phaseANativeScriptsPrepare:
    "validationTraceDisputePhaseANativeScriptsPrepare",
  phaseANativeScriptsItemSemantic:
    "validationTraceDisputePhaseANativeScriptsItemSemantic",
  phaseAScriptPreconditionsPrepare:
    "validationTraceDisputePhaseAScriptPreconditionsPrepare",
  phaseAScriptPreconditionsFinalizeSemantic:
    "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
  resolveInputsPrepare: "validationTraceDisputeResolveInputsPrepare",
  resolveInputsInitialSemantic:
    "validationTraceDisputeResolveInputsInitialSemantic",
  resolveInputsFinishSemantic:
    "validationTraceDisputeResolveInputsFinishSemantic",
  resolveInputsMembershipBeginSemantic:
    "validationTraceDisputeResolveInputsMembershipBeginSemantic",
  resolveInputsMembershipStepSemantic:
    "validationTraceDisputeResolveInputsMembershipStepSemantic",
  resolveInputsMembershipFinalizeSemantic:
    "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
  scriptSourcesPrepare: "validationTraceDisputeScriptSourcesPrepare",
  scriptSourcesOutputProofBeginSemantic:
    "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
  scriptSourcesOutputProofStepSemantic:
    "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
  scriptSourcesOutputProofFinalizeSemantic:
    "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
  scriptSourcesOutputProofFinishSemantic:
    "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
  scriptSourcesStageZeroBeginSemantic:
    "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
  scriptSourcesStageZeroFinishSemantic:
    "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
  scriptSourcesStageZeroHashBlockSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
  scriptSourcesStageZeroHashAdvanceSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
  scriptSourcesStageZeroHashTerminalSemantic:
    "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
  scriptSourcesStageNineEffectfulMatchSemantic:
    "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
  scriptSourcesStageOneFinishSemantic:
    "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
  scriptSourcesStageOneRedeemerSemantic:
    "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
  scriptSourcesStageElevenFinishSemantic:
    "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
  scriptSourcesStageElevenSourceSemantic:
    "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
  scriptSourcesStageTwelveFinishSemantic:
    "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
  scriptSourcesStageEightFinishSemantic:
    "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
  scriptSourcesStageEightPurposeSemantic:
    "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
  scriptSourcesStageSevenFinishSemantic:
    "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
  [SCRIPT_SOURCES_REDEEMER_SEMANTIC]:
    "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
} as const);

export type ValidationTracePhaseHolder = keyof typeof PHASE_CONTRACT_NAMES;

export const PHASE_HOLDERS = Object.freeze(
  Object.keys(PHASE_CONTRACT_NAMES),
) as readonly ValidationTracePhaseHolder[];

export const isValidationTracePhaseHolder = (
  holder: string,
): holder is ValidationTracePhaseHolder =>
  Object.hasOwn(PHASE_CONTRACT_NAMES, holder);

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
  // Only the item semantic resolver, which reads a witness script's header.
  phaseANativeScripts: {
    resolverIndex: 5,
    prepare: "phaseANativeScriptsPrepare",
    semantics: [undefined, "phaseANativeScriptsItemSemantic"],
  },
  // Only the finalize semantic resolver, which closes the phase.
  phaseAScriptPreconditions: {
    resolverIndex: 6,
    prepare: "phaseAScriptPreconditionsPrepare",
    semantics: ["phaseAScriptPreconditionsFinalizeSemantic", undefined],
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
  // ledger-output proof names, if any; and the redeemer normalization
  // semantic resolver, whose resolution runs on through the redeemer item
  // chain when the deployment publishes it
  // (one-step-script-sources-redeemer.ts). The rest are not yet driven.
  scriptSources: {
    resolverIndex: 8,
    prepare: "scriptSourcesPrepare",
    semantics: [
      undefined,
      "scriptSourcesOutputProofBeginSemantic",
      "scriptSourcesOutputProofStepSemantic",
      "scriptSourcesOutputProofFinalizeSemantic",
      "scriptSourcesOutputProofFinishSemantic",
      "scriptSourcesStageZeroBeginSemantic",
      "scriptSourcesStageZeroFinishSemantic",
      "scriptSourcesStageZeroHashBlockSemantic",
      "scriptSourcesStageZeroHashAdvanceSemantic",
      "scriptSourcesStageZeroHashTerminalSemantic",
      undefined,
      undefined,
      "scriptSourcesStageNineEffectfulMatchSemantic",
      undefined,
      "scriptSourcesStageOneFinishSemantic",
      "scriptSourcesStageOneRedeemerSemantic",
      "scriptSourcesStageElevenFinishSemantic",
      "scriptSourcesStageElevenSourceSemantic",
      "scriptSourcesStageTwelveFinishSemantic",
      undefined,
      undefined,
      undefined,
      undefined,
      "scriptSourcesStageEightFinishSemantic",
      "scriptSourcesStageEightPurposeSemantic",
      undefined,
      undefined,
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

/** The yields the semantic resolvers of `phase` invoke. */
export const phaseYields = (
  phase: ValidationTracePhase,
): readonly ValidationTracePhaseYield[] =>
  PHASE_RESOLUTIONS[phase].semantics.flatMap((holder) =>
    holder === undefined
      ? []
      : (PHASE_SEMANTIC_EVIDENCE[holder]?.itemYields ??
        LEDGER_OUTPUT_PROOF_SEMANTICS[holder]?.yields ??
        []),
  );

/** `Inline`'s constructor among the field carriages. */
const INLINE_CARRIAGE = 0;

/** A phase step as its resolution needs it: the holders the thread visits
 * past the boundary, the fields of the auxiliary evidence the semantic
 * resolver takes beside the transition, as their Data's CBOR (none when it
 * takes `NoAuxiliaryWitness`), and the yield it invokes beside the item,
 * or the yields whose reference-input indices follow the evidence, if
 * any. */
export type ValidationTracePhaseStep = Readonly<{
  phase: ValidationTracePhase;
  prepare: ValidationTracePhaseHolder;
  semantic: ValidationTracePhaseHolder;
  evidenceCbors: readonly string[];
  itemYield?: ValidationTracePhaseItemYield;
  yieldList?: readonly ValidationTracePhaseYield[];
}>;

/**
 * The resolution of the step of `phase` whose one-step argument selects
 * `resolverIndex` and the group-local `semanticResolverIndex` with the
 * transition `transitionCbor` beside the auxiliary evidence
 * `auxiliaryCbor`, to the successor whose work witness is
 * `successorWitnessCbor` (none when the step rejects); or why the runner
 * does not drive it.
 */
export const planValidationTracePhaseStep = ({
  phase,
  resolverIndex,
  semanticResolverIndex,
  transitionCbor,
  auxiliaryCbor,
  successorWitnessCbor,
  redeemerChain = false,
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
}): ValidationTracePhaseStep | Readonly<{ reason: string }> => {
  const resolution = PHASE_RESOLUTIONS[phase];
  if (resolverIndex !== resolution.resolverIndex)
    throw new Error(`validation-trace ${phase} step selects another resolver`);
  const semantic: ValidationTracePhaseHolder | undefined =
    resolution.semantics[semanticResolverIndex];
  if (semantic === undefined)
    return {
      reason: `a ${phase} step for semantic resolver ${semanticResolverIndex.toString()}`,
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
    return redeemerChain
      ? step([])
      : {
          reason: `a ${phase} step for semantic resolver ${semanticResolverIndex.toString()}`,
        };
  const derived = LEDGER_OUTPUT_PROOF_SEMANTICS[semantic];
  if (derived !== undefined) {
    const { evidenceCbors, yieldList } = derived.evidence({
      transitionCbor,
      auxiliaryCbor,
      successorWitnessCbor,
    });
    return Object.freeze({ ...step(evidenceCbors), yieldList });
  }
  const evidence = PHASE_SEMANTIC_EVIDENCE[semantic];
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
  const { fields } = auxiliary as Constr<Data>;
  const fieldCbors = fields.map((_, index) =>
    plutusConstrFieldCbor(auxiliaryCbor, [index]),
  );
  if (evidence.carriage === undefined) return step(fieldCbors);
  const carriage = fields[evidence.carriage];
  if (
    !(carriage instanceof Constr) ||
    Number(carriage.index) !== INLINE_CARRIAGE
  )
    return { reason: `a ${phase} item of a field delivered by reference` };
  const preimage = carriage.fields[0];
  if (typeof preimage !== "string")
    throw new Error(`validation-trace ${phase} inline carriage is not bytes`);
  const length = preimage.length / 2;
  if (length > evidence.inlineMaxPreimageBytes)
    return {
      reason: `a ${phase} item of a ${length.toString()}-byte field, over the ${evidence.inlineMaxPreimageBytes.toString()}-byte inline ${phase} limit`,
    };
  return step(
    fieldCbors,
    evidence.itemYields === undefined
      ? undefined
      : itemYieldOf(evidence.itemYields, preimage, fields[1]),
  );
};

/** The semantic resolver's hop of the thread of prover `prover`: it
 * verifies the transition `transitionCbor` beside the auxiliary evidence
 * whose fields are `evidenceCbors`, and the yield `itemYield` invokes
 * beside them, or the yields `yieldList`, whose reference-input indices
 * follow them, if any, and awards the step. */
export const phaseSemanticHop = ({
  prover,
  transitionCbor,
  evidenceCbors,
  itemYield,
  yieldList,
}: {
  readonly prover: string;
  readonly transitionCbor: string;
  readonly evidenceCbors: readonly string[];
  readonly itemYield?: ValidationTracePhaseYieldReads;
  readonly yieldList?: ValidationTracePhaseYieldListReads;
}) => {
  const reads =
    itemYield === undefined
      ? yieldList
      : { references: [itemYield.reference], scripts: [itemYield.script] };
  const indexOf = (context: RedeemerContext, reference: UTxO) =>
    SDK.requireReferenceInputIndex(context, reference, "phase yield");
  return {
    next: "award" as const,
    datum: stepDatum(prover, constr(0, [integer(1)])),
    redeemer: (
      inputIndex: bigint,
      outputIndex: bigint,
      context: RedeemerContext,
    ) =>
      continued(
        constr(0, [
          integer(inputIndex),
          integer(outputIndex),
          transitionCbor,
          ...evidenceCbors,
          ...(itemYield === undefined
            ? []
            : [
                integer(indexOf(context, itemYield.reference)),
                integer(itemYield.kind),
              ]),
          ...(yieldList === undefined
            ? []
            : [
                Data.to(
                  yieldList.references.map((reference) =>
                    indexOf(context, reference),
                  ),
                ),
              ]),
        ]),
      ),
    proverOnly: false,
    ...(reads === undefined || reads.references.length === 0
      ? {}
      : {
          reads: {
            references: [...reads.references],
            yields: [...reads.scripts],
          },
        }),
  };
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
