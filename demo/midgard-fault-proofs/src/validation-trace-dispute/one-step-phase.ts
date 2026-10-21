/** The one-step resolution of a step of a validation phase whose resolver
 * is the generic phase resolver and whose semantic resolver awards the step
 * in one hop: past the boundary, the phase resolver commits the transition
 * and its auxiliary evidence and hands the step to the semantic resolver
 * the evidence selects, which verifies the transition beside that evidence
 * (none, or one item of a field whose preimage the evidence carries
 * inline, with a required signer's proof of signing beside it, or with the
 * yield that verifies the item by its kind invoked beside it) and awards
 * the step. */
import { parseMidgardVersionedScriptHeader } from "@al-ft/midgard-core";
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type RedeemerContext,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import { NO_AUXILIARY_WITNESS_CBOR } from "./one-step-canonical-decode.js";
import { constr, continued, integer, stepDatum } from "./one-step-data.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

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

/** The longest field preimage whose `Inline` carriage an input-sets item
 * step's resolution carries: the phase resolver's move and the semantic
 * resolver's each carry it whole, and the longer of the two lands within
 * the transaction size limit less its 512-byte reserve even when our wallet
 * pays it
 * (tests/validation-trace-dispute-one-step-input-sets-inline-cap.test.ts).
 * An input-sets field is a whole number of 40-byte out-refs, and this is
 * the longest such field that fits. */
export const VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES = 13_683;

/** The same for a signatures step over an address-witness item, a field
 * of 103-byte witnesses
 * (tests/validation-trace-dispute-one-step-signatures-address-inline-cap.test.ts). */
export const VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES = 13_701;

/** The same for a signatures step over a required-signer item, a field of
 * 30-byte key hashes, which may repeat one signer
 * (tests/validation-trace-dispute-one-step-signatures-required-inline-cap.test.ts). */
export const VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES = 13_623;

/** The same for a phase-A native scripts step over a witness script, a
 * field of versioned scripts of any length, so measured to the byte
 * (tests/validation-trace-dispute-one-step-phase-a-native-item-inline-cap.test.ts). */
export const VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES = 13_659;

/** The yields an item semantic resolver invokes beside the item it reads,
 * by the item's kind, `yield_kind`: the native yield verifies a native
 * script's item, or the rejection of an item whose versioned-script header
 * is malformed; the foreign yield verifies any other script's. */
const PHASE_A_NATIVE_ITEM_YIELDS = Object.freeze([
  "phaseANativeItemNative",
  "phaseANativeItemForeign",
] as const);

export type ValidationTracePhaseYield =
  (typeof PHASE_A_NATIVE_ITEM_YIELDS)[number];

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
        /** The yields it invokes by the item's kind; the item's index is
         * the evidence's field 1. */
        itemYields?: readonly ValidationTracePhaseYield[];
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
  signaturesAddressItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_SIGNATURES_ADDRESS_INLINE_MAX_PREIMAGE_BYTES,
  },
  // `RequiredSignerItemWitness { carriage, signer_proof }`.
  signaturesRequiredItemSemantic: {
    auxiliary: 2,
    carriage: 0,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_SIGNATURES_REQUIRED_INLINE_MAX_PREIMAGE_BYTES,
  },
  phaseANativeScriptsItemSemantic: {
    auxiliary: 1,
    carriage: 2,
    inlineMaxPreimageBytes:
      VALIDATION_TRACE_PHASE_A_NATIVE_ITEM_INLINE_MAX_PREIMAGE_BYTES,
    itemYields: PHASE_A_NATIVE_ITEM_YIELDS,
  },
});

/** The yields the semantic resolvers of `phase` invoke. */
export const phaseYields = (
  phase: ValidationTracePhase,
): readonly ValidationTracePhaseYield[] =>
  PHASE_RESOLUTIONS[phase].semantics.flatMap((holder) =>
    holder === undefined
      ? []
      : (PHASE_SEMANTIC_EVIDENCE[holder]?.itemYields ?? []),
  );

/** `Inline`'s constructor among the field carriages. */
const INLINE_CARRIAGE = 0;

/** The yield a semantic resolver invokes beside the item it reads: its
 * `yield_kind`, and which yield that is. */
export type ValidationTracePhaseItemYield = Readonly<{
  kind: number;
  name: ValidationTracePhaseYield;
}>;

/** A phase step as its resolution needs it: the holders the thread visits
 * past the boundary, the fields of the auxiliary evidence the semantic
 * resolver takes beside the transition, as their Data's CBOR (none when it
 * takes `NoAuxiliaryWitness`), and the yield it invokes beside the item,
 * if any. */
export type ValidationTracePhaseStep = Readonly<{
  phase: ValidationTracePhase;
  prepare: ValidationTracePhaseHolder;
  semantic: ValidationTracePhaseHolder;
  evidenceCbors: readonly string[];
  itemYield?: ValidationTracePhaseItemYield;
}>;

/** The yield among `yields` that verifies the item `itemIndex` of the field
 * `preimage`, by its versioned-script header (`versioned_script_header_v1`):
 * the first, native, for a native script or a malformed header, the second,
 * foreign, for any other script. */
const itemYieldOf = (
  yields: readonly ValidationTracePhaseYield[],
  preimage: string,
  itemIndex: Data | undefined,
): ValidationTracePhaseItemYield => {
  const item =
    typeof itemIndex === "bigint"
      ? decodeMidgardFieldPreimage(Buffer.from(preimage, "hex"))[
          Number(itemIndex)
        ]
      : undefined;
  if (item === undefined)
    throw new Error("validation-trace item is not in its field");
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  const kind = header === null || header.languageTag === 0 ? 0 : 1;
  const name = yields[kind];
  if (name === undefined)
    throw new Error(`validation-trace item has no yield of kind ${kind}`);
  return Object.freeze({ kind, name });
};

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
    evidence.itemYields === undefined
      ? undefined
      : itemYieldOf(evidence.itemYields, preimage, fields[1]),
  );
};

/** The yield a semantic resolver's hop invokes, as the hop reads it: its
 * kind, its reference under its role's authentication token, and its
 * withdrawal script. */
export type ValidationTracePhaseYieldReads = Readonly<{
  kind: number;
  reference: UTxO;
  script: Script;
}>;

/** What the hop of the step `step` reads of `deployment` for the yield its
 * semantic resolver invokes beside the item; undefined when it invokes
 * none. */
export const phaseYieldReads = (
  deployment: ValidationTraceDisputeDeployment,
  step: ValidationTracePhaseStep,
): ValidationTracePhaseYieldReads | undefined => {
  if (step.itemYield === undefined) return undefined;
  const { kind, name } = step.itemYield;
  const reference = deployment.references.oneStep?.phaseYields?.[name];
  if (reference === undefined)
    throw new Error(
      `validation-trace deployment does not publish the ${name} yield`,
    );
  return Object.freeze({
    kind,
    reference,
    script: deployment.chain.yields[name].withdrawalScript,
  });
};

/** The semantic resolver's hop of the thread of prover `prover`: it
 * verifies the transition `transitionCbor` beside the auxiliary evidence
 * whose fields are `evidenceCbors`, and the yield `itemYield` invokes
 * beside them, if any, and awards the step. */
export const phaseSemanticHop = ({
  prover,
  transitionCbor,
  evidenceCbors,
  itemYield,
}: {
  readonly prover: string;
  readonly transitionCbor: string;
  readonly evidenceCbors: readonly string[];
  readonly itemYield?: ValidationTracePhaseYieldReads;
}) => ({
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
              integer(
                SDK.requireReferenceInputIndex(
                  context,
                  itemYield.reference,
                  "item yield",
                ),
              ),
              integer(itemYield.kind),
            ]),
      ]),
    ),
  proverOnly: false,
  ...(itemYield === undefined
    ? {}
    : {
        reads: {
          references: [itemYield.reference],
          yields: [itemYield.script],
        },
      }),
});

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
