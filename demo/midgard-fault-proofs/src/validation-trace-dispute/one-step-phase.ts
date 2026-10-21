/** The one-step resolution of a step of a validation phase whose resolver
 * is the generic phase resolver and whose semantic resolver awards the step
 * in one hop: past the boundary, the phase resolver commits the transition
 * and its auxiliary evidence and hands the step to the semantic resolver
 * the evidence selects, which verifies the transition beside that evidence
 * (none, or one item of a field whose preimage the evidence carries
 * inline) and awards the step. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import { NO_AUXILIARY_WITNESS_CBOR } from "./one-step-canonical-decode.js";
import { constr, continued, integer, stepDatum } from "./one-step-data.js";

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
 * of its semantic resolvers, by the group-local index the evidence selects. */
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
} as const satisfies Readonly<
  Partial<
    Record<
      keyof typeof SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS,
      Readonly<{
        resolverIndex: number;
        prepare: ValidationTracePhaseHolder;
        semantics: readonly ValidationTracePhaseHolder[];
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

/** The longest field preimage whose `Inline` carriage an input-sets item
 * step's resolution carries: the phase resolver's move and the semantic
 * resolver's each carry it whole, and the longer of the two lands within
 * the transaction size limit less its 512-byte reserve even when our wallet
 * pays it
 * (tests/validation-trace-dispute-one-step-input-sets-inline-cap.test.ts).
 * An input-sets field is a whole number of 40-byte out-refs, and this is
 * the longest such field that fits. */
export const VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES = 13_683;

/** The auxiliary evidence each semantic resolver that takes any verifies
 * the transition beside: its constructor among the auxiliary witnesses, the
 * position among its fields of the carriage that delivers the field
 * preimage the item is read from, and the longest preimage the step's
 * resolution carries inline. A step over a longer field is not driven: it
 * waits under the one-step-required alarm. Every other semantic resolver
 * takes `NoAuxiliaryWitness`. */
const PHASE_SEMANTIC_EVIDENCE: Readonly<
  Partial<
    Record<
      ValidationTracePhaseHolder,
      Readonly<{
        auxiliary: number;
        carriage: number;
        inlineMaxPreimageBytes: number;
      }>
    >
  >
> = Object.freeze({
  // `TransactionFieldChunkWitness { field_index, item_index, carriage }`.
  inputSetsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES,
  },
});

/** `Inline`'s constructor among the field carriages. */
const INLINE_CARRIAGE = 0;

/** A phase step as its resolution needs it: the holders the thread visits
 * past the boundary, and the fields of the auxiliary evidence the semantic
 * resolver takes beside the transition, as their Data's CBOR (none when it
 * takes `NoAuxiliaryWitness`). */
export type ValidationTracePhaseStep = Readonly<{
  phase: ValidationTracePhase;
  prepare: ValidationTracePhaseHolder;
  semantic: ValidationTracePhaseHolder;
  evidenceCbors: readonly string[];
}>;

/**
 * The resolution of the step of `phase` whose one-step argument selects
 * `resolverIndex` and the group-local `semanticResolverIndex` with the
 * auxiliary evidence `auxiliaryCbor`; or why the runner does not drive it.
 */
export const planValidationTracePhaseStep = ({
  phase,
  resolverIndex,
  semanticResolverIndex,
  auxiliaryCbor,
}: {
  readonly phase: ValidationTracePhase;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly auxiliaryCbor: string;
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
  const step = (evidenceCbors: readonly string[]) =>
    Object.freeze({
      phase,
      prepare: resolution.prepare,
      semantic,
      evidenceCbors: Object.freeze(evidenceCbors),
    });
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
    fields.map((_, index) => plutusConstrFieldCbor(auxiliaryCbor, [index])),
  );
};

/** The semantic resolver's hop of the thread of prover `prover`: it
 * verifies the transition `transitionCbor` beside the auxiliary evidence
 * whose fields are `evidenceCbors`, and awards the step. */
export const phaseSemanticHop = ({
  prover,
  transitionCbor,
  evidenceCbors,
}: {
  readonly prover: string;
  readonly transitionCbor: string;
  readonly evidenceCbors: readonly string[];
}) => ({
  next: "award" as const,
  datum: stepDatum(prover, constr(0, [integer(1)])),
  redeemer: (inputIndex: bigint, outputIndex: bigint) =>
    continued(
      constr(0, [
        integer(inputIndex),
        integer(outputIndex),
        transitionCbor,
        ...evidenceCbors,
      ]),
    ),
  proverOnly: false,
});

/** Where each phase holder's datum keeps our successor's hash: the
 * resolution's challenger successor hash, behind the prepared state at the
 * semantic resolver. */
export const PHASE_CHALLENGER_SUCCESSOR_HASH = Object.freeze(
  Object.fromEntries(
    Object.values(PHASE_RESOLUTIONS).flatMap(({ prepare, semantics }) => [
      [prepare, [1, 0, 3]],
      ...semantics.map((holder) => [holder, [1, 0, 1, 3]] as const),
    ]),
  ) as Readonly<Record<ValidationTracePhaseHolder, readonly number[]>>,
);
