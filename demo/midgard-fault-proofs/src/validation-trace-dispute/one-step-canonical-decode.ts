/** The one-step resolution of a canonical-decode step of ours, past the
 * boundary: the canonical-decode resolver, which commits to the transition
 * alone, hands the step to its empty semantic resolver, which awards it in
 * one hop, or to its item semantic resolver, which installs the transition
 * into the item's staging chain: the source stage binds the field's
 * commitment and length, the observe stage opens the item through the field
 * door from the carriage the step's evidence delivers (a field too long to
 * carry inline, from our published copy of it: observe-copy.ts), the proof
 * stage derives the item's encoding, and the settlement awards the step
 * whose successor that encoding implies. Each stage's successor is the
 * staging record the stage derives, which we derive here from our own
 * trace. */
import {
  decodeMidgardFieldPreimage,
  decodeSingleCbor,
  midgardFieldCommitment,
  midgardFieldItemOrderIsValid,
} from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  canonicalCborArgumentHeaderSize,
  canonicalFieldItemEncodedLength,
  type ValidationMachineWorkWitness,
} from "@al-ft/midgard-validation";
import {
  Constr,
  Data,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  bytes,
  constr,
  continued,
  integer,
  stepDatum,
} from "./one-step-data.js";

/** `NoAuxiliaryWitness`: what the canonical-decode resolver commits beside
 * the transition. */
export const NO_AUXILIARY_WITNESS_CBOR = constr(0, []);

/** `resolver_index(CanonicalDecode)` (validation-resolution-v1). */
export const CANONICAL_DECODE_RESOLVER_INDEX = 0;

/** The canonical-decode group's semantic resolvers. */
export const CANONICAL_DECODE_EMPTY_SEMANTIC_RESOLVER_INDEX = 0;
export const CANONICAL_DECODE_ITEM_SEMANTIC_RESOLVER_INDEX = 1;

/** `TransactionFieldItemWitness`'s constructor among the auxiliary
 * witnesses, and `Inline`'s and `RawUtxo`'s among the field carriages. */
const TRANSACTION_FIELD_ITEM_AUXILIARY = 28;
const INLINE_CARRIAGE = 0;
const RAW_UTXO_CARRIAGE = 1;

/** The longest field preimage whose `Inline` carriage the observe move
 * carries within the transaction size limit less its 512-byte reserve,
 * measured on the move paid from our wallet (the canonical-decode-8
 * observe-copy test). The observe move of an item of a longer field reads
 * the field from our published bytes-only copy of it instead (`RawUtxo`). */
export const VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES = 13_620;

/** The holders of a canonical-decode step's resolution, in the order the
 * thread visits them, with the deployment manifest's name for each. */
export const CANONICAL_DECODE_CONTRACT_NAMES = Object.freeze({
  canonicalDecodePrepare: "validationTraceDisputeCanonicalDecodePrepare",
  canonicalDecodeEmpty: "validationTraceDisputeCanonicalDecodeEmptySemantic",
  canonicalDecodeItem: "validationTraceDisputeCanonicalDecodeItemSemantic",
  canonicalDecodeItemSource: "validationTraceDisputeCanonicalDecodeItemSource",
  canonicalDecodeItemObserve:
    "validationTraceDisputeCanonicalDecodeItemObserve",
  canonicalDecodeItemProof: "validationTraceDisputeCanonicalDecodeItemProof",
  canonicalDecodeItemSettlement:
    "validationTraceDisputeCanonicalDecodeItemSettlement",
} as const);

export type ValidationTraceCanonicalDecodeHolder =
  keyof typeof CANONICAL_DECODE_CONTRACT_NAMES;

export const CANONICAL_DECODE_HOLDERS = Object.freeze(
  Object.keys(CANONICAL_DECODE_CONTRACT_NAMES),
) as readonly ValidationTraceCanonicalDecodeHolder[];

export const isValidationTraceCanonicalDecodeHolder = (
  holder: string,
): holder is ValidationTraceCanonicalDecodeHolder =>
  Object.hasOwn(CANONICAL_DECODE_CONTRACT_NAMES, holder);

type Chain = Pick<
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  "prepareResolvers" | "semanticResolvers" | "canonicalDecodeItemStages"
>;

/** The validator of each canonical-decode holder on `chain`. */
export const canonicalDecodeValidators = (
  chain: Chain,
): Readonly<
  Record<ValidationTraceCanonicalDecodeHolder, SDK.SpendingValidator>
> => {
  const semantic = (index: number) => {
    const resolver =
      chain.semanticResolvers[
        SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS.canonicalDecode[index]!
      ];
    if (resolver === undefined)
      throw new Error(
        "validation-trace deployment has no canonical-decode semantic resolver",
      );
    return resolver;
  };
  const stages = chain.canonicalDecodeItemStages;
  return Object.freeze({
    canonicalDecodePrepare:
      chain.prepareResolvers[CANONICAL_DECODE_RESOLVER_INDEX],
    canonicalDecodeEmpty: semantic(
      CANONICAL_DECODE_EMPTY_SEMANTIC_RESOLVER_INDEX,
    ),
    canonicalDecodeItem: semantic(
      CANONICAL_DECODE_ITEM_SEMANTIC_RESOLVER_INDEX,
    ),
    canonicalDecodeItemSource: stages.source,
    canonicalDecodeItemObserve: stages.observe,
    canonicalDecodeItemProof: stages.proof,
    canonicalDecodeItemSettlement: stages.settlement,
  });
};

/** What each item stage records, as the Data CBOR the stage derives. */
export type ValidationTraceCanonicalDecodeItem = Readonly<{
  /** The carriage the observe stage opens the item from. */
  carriageCbor: string;
  /** `CanonicalDecodeItemSourceV1`: the field's commitment and length. */
  sourceCbor: string;
  /** `CanonicalDecodeItemObservationV1`: the field's item count, the
   * item's length, and whether the item follows its predecessor in the
   * field's order. */
  observationCbor: string;
  /** `CanonicalDecodeItemProofV1`: the active item count, whether the
   * item's width is legal, and the field's encoded length past it. */
  proofCbor: string;
  /** The field preimage, when it is too long for the observe move to carry
   * inline: the move reads it from our published copy. */
  copied: Buffer | undefined;
}>;

/** A canonical-decode step as its resolution needs it: the evidence hash
 * the resolver commits (the transition beside `NoAuxiliaryWitness`), and an
 * item step's staging records; or why the runner does not drive it. */
export type ValidationTraceCanonicalDecodeStep =
  | Readonly<{
      preparedEvidenceHash: string;
      item: ValidationTraceCanonicalDecodeItem | undefined;
    }>
  | Readonly<{ driven: false; reason: string }>;

const asInteger = (value: unknown, label: string): number => {
  if (typeof value !== "bigint" && typeof value !== "number")
    throw new Error(`canonical-decode control ${label} is not an integer`);
  return Number(value);
};

/**
 * The canonical-decode step whose witness in our trace is `witness`, with
 * the evidence `auxiliaryCbor` delivers at the semantic resolver
 * `semanticResolverIndex`; `evidenceHash` hashes a transition beside an
 * auxiliary witness the way the resolver does.
 */
export const planValidationTraceCanonicalDecodeStep = ({
  witness,
  semanticResolverIndex,
  transitionCbor,
  auxiliaryCbor,
  evidenceHash,
}: {
  readonly witness: ValidationMachineWorkWitness;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: string;
  readonly auxiliaryCbor: string;
  readonly evidenceHash: (transition: string, auxiliary: string) => string;
}): ValidationTraceCanonicalDecodeStep => {
  const preparedEvidenceHash = evidenceHash(
    transitionCbor,
    NO_AUXILIARY_WITNESS_CBOR,
  );
  if (semanticResolverIndex === CANONICAL_DECODE_EMPTY_SEMANTIC_RESOLVER_INDEX)
    return { preparedEvidenceHash, item: undefined };
  const auxiliary = witness.auxiliary;
  const evidence = Data.from(auxiliaryCbor);
  if (
    semanticResolverIndex !== CANONICAL_DECODE_ITEM_SEMANTIC_RESOLVER_INDEX ||
    auxiliary?.kind !== "transactionFieldItem" ||
    !(evidence instanceof Constr) ||
    Number(evidence.index) !== TRANSACTION_FIELD_ITEM_AUXILIARY
  )
    throw new Error(
      "validation-trace canonical-decode step is neither empty nor a complete item",
    );
  const carriageCbor = plutusConstrFieldCbor(auxiliaryCbor, [0]);
  const carriage = Data.from(carriageCbor);
  if (
    !(carriage instanceof Constr) ||
    Number(carriage.index) !== INLINE_CARRIAGE
  )
    return {
      driven: false,
      reason: "a canonical-decode item delivered by reference",
    };
  const preimage = auxiliary.fieldPreimage;
  const control = decodeSingleCbor(witness.cbor);
  if (!Array.isArray(control) || control.length !== 9)
    throw new Error("canonical-decode control is not its 9 fields");
  const [fieldIndex, itemIndex, chunkIndex, itemCount, encodedLength] = [
    asInteger(control[4], "field_index"),
    asInteger(control[5], "item_index"),
    asInteger(control[6], "chunk_index"),
    asInteger(control[7], "item_count"),
    asInteger(control[8], "encoded_length"),
  ];
  const items = decodeMidgardFieldPreimage(preimage);
  const item = items[itemIndex];
  if (item === undefined)
    throw new Error("canonical-decode item is not in its field");
  const firstItem =
    itemIndex === 0 &&
    chunkIndex === 0 &&
    itemCount === -1 &&
    encodedLength === 0;
  const activeItemCount = firstItem ? items.length : itemCount;
  const orderIsValid =
    itemIndex === 0 ||
    midgardFieldItemOrderIsValid(fieldIndex, items[itemIndex - 1]!, item);
  const encoded = orderIsValid
    ? canonicalFieldItemEncodedLength(fieldIndex, item.length)
    : null;
  const before = firstItem
    ? canonicalCborArgumentHeaderSize(activeItemCount)
    : encodedLength;
  return {
    preparedEvidenceHash,
    item: Object.freeze({
      carriageCbor,
      sourceCbor: constr(0, [
        bytes(midgardFieldCommitment(preimage).toString("hex")),
        integer(preimage.length),
      ]),
      observationCbor: constr(0, [
        integer(items.length),
        integer(item.length),
        constr(orderIsValid ? 1 : 0, []),
      ]),
      proofCbor: constr(0, [
        integer(activeItemCount),
        constr(encoded === null ? 0 : 1, []),
        integer(encoded === null ? 0 : before + encoded),
      ]),
      copied:
        preimage.length > VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES
          ? Buffer.from(preimage)
          : undefined,
    }),
  };
};

/** A hop of a canonical-decode step's resolution, as one-step-builders.ts
 * builds it. */
export type CanonicalDecodeHop = Readonly<{
  next: ValidationTraceCanonicalDecodeHolder | "award";
  datum: string;
  redeemer: (
    inputIndex: bigint,
    outputIndex: bigint,
    context: RedeemerContext,
  ) => string;
  proverOnly: boolean;
  reads?: Readonly<{ references: readonly UTxO[]; yields: readonly [] }>;
}>;

/** The staging record at each stage nests the one before it, one level
 * deeper, behind its version: `{ version, previous, record }`. */
const staged = (datum: string, recordCbor: string) =>
  constr(0, [integer(1), plutusConstrFieldCbor(datum, [1, 0]), recordCbor]);

const indices = (inputIndex: bigint, outputIndex: bigint) => [
  integer(inputIndex),
  integer(outputIndex),
];

/**
 * The hop of the thread with datum `datum`, of prover `prover`, held by
 * the canonical-decode holder `holder`, in the resolution of the step whose
 * transition is `transitionCbor` and whose plan is `step`; the observe hop
 * of an item whose field is `copied` reads it from our published `copy`.
 */
export const canonicalDecodeHop = ({
  holder,
  datum,
  prover,
  semanticResolverIndex,
  transitionCbor,
  step,
  copy,
}: {
  readonly holder: ValidationTraceCanonicalDecodeHolder;
  readonly datum: string;
  readonly prover: string;
  readonly semanticResolverIndex: number;
  readonly transitionCbor: string;
  readonly step: Extract<
    ValidationTraceCanonicalDecodeStep,
    { preparedEvidenceHash: string }
  >;
  readonly copy?: UTxO | undefined;
}): CanonicalDecodeHop => {
  const item = (): ValidationTraceCanonicalDecodeItem => {
    if (step.item === undefined)
      throw new Error(
        `validation-trace ${holder} holds a thread whose step is not a canonical-decode item`,
      );
    return step.item;
  };
  const won = stepDatum(prover, constr(0, [integer(1)]));
  switch (holder) {
    case "canonicalDecodePrepare":
      return {
        next:
          step.item === undefined
            ? "canonicalDecodeEmpty"
            : "canonicalDecodeItem",
        datum: stepDatum(
          prover,
          constr(0, [
            integer(1),
            plutusConstrFieldCbor(datum, [1, 0]),
            bytes(step.preparedEvidenceHash),
          ]),
        ),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [
              ...indices(inputIndex, outputIndex),
              integer(semanticResolverIndex),
              transitionCbor,
            ]),
          ),
        proverOnly: true,
      };
    case "canonicalDecodeEmpty":
      return {
        next: "award",
        datum: won,
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [...indices(inputIndex, outputIndex), transitionCbor]),
          ),
        proverOnly: false,
      };
    case "canonicalDecodeItem":
      item();
      return {
        next: "canonicalDecodeItemSource",
        datum: stepDatum(prover, staged(datum, transitionCbor)),
        redeemer: (inputIndex, outputIndex) =>
          continued(
            constr(0, [...indices(inputIndex, outputIndex), transitionCbor]),
          ),
        proverOnly: true,
      };
    case "canonicalDecodeItemSource":
      return {
        next: "canonicalDecodeItemObserve",
        datum: stepDatum(prover, staged(datum, item().sourceCbor)),
        redeemer: (inputIndex, outputIndex) =>
          continued(constr(0, indices(inputIndex, outputIndex))),
        proverOnly: true,
      };
    case "canonicalDecodeItemObserve": {
      const { observationCbor, carriageCbor, copied } = item();
      if (copied === undefined)
        return {
          next: "canonicalDecodeItemProof",
          datum: stepDatum(prover, staged(datum, observationCbor)),
          redeemer: (inputIndex, outputIndex) =>
            continued(
              constr(0, [...indices(inputIndex, outputIndex), carriageCbor]),
            ),
          proverOnly: true,
        };
      if (copy === undefined)
        throw new Error(
          `validation-trace observe of a ${copied.length.toString()}-byte field has no published copy of it`,
        );
      return {
        next: "canonicalDecodeItemProof",
        datum: stepDatum(prover, staged(datum, observationCbor)),
        redeemer: (inputIndex, outputIndex, context) =>
          continued(
            constr(0, [
              ...indices(inputIndex, outputIndex),
              constr(RAW_UTXO_CARRIAGE, [
                integer(
                  SDK.requireReferenceInputIndex(context, copy, "field copy"),
                ),
              ]),
            ]),
          ),
        proverOnly: true,
        reads: { references: [copy], yields: [] },
      };
    }
    case "canonicalDecodeItemProof":
      return {
        next: "canonicalDecodeItemSettlement",
        datum: stepDatum(prover, staged(datum, item().proofCbor)),
        redeemer: (inputIndex, outputIndex) =>
          continued(constr(0, indices(inputIndex, outputIndex))),
        proverOnly: true,
      };
    case "canonicalDecodeItemSettlement":
      item();
      return {
        next: "award",
        datum: won,
        redeemer: (inputIndex, outputIndex) =>
          continued(constr(0, indices(inputIndex, outputIndex))),
        proverOnly: false,
      };
  }
};

/** Where each canonical-decode holder's datum keeps our successor's hash:
 * the resolution's challenger successor hash, behind one more staging level
 * at each item stage. */
export const CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH = Object.freeze({
  canonicalDecodePrepare: [1, 0, 3],
  canonicalDecodeEmpty: [1, 0, 1, 3],
  canonicalDecodeItem: [1, 0, 1, 3],
  canonicalDecodeItemSource: [1, 0, 1, 1, 3],
  canonicalDecodeItemObserve: [1, 0, 1, 1, 1, 3],
  canonicalDecodeItemProof: [1, 0, 1, 1, 1, 1, 3],
  canonicalDecodeItemSettlement: [1, 0, 1, 1, 1, 1, 1, 3],
} as const satisfies Readonly<
  Record<ValidationTraceCanonicalDecodeHolder, readonly number[]>
>);
