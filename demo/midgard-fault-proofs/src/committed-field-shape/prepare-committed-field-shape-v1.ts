import {
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  type MidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core";
import {
  type CommittedFieldClaimV1,
  committedFieldShapeEvidenceFromCommittedFieldV1,
  type CommittedFieldShapeEvidenceV1,
  type CommittedFieldShapeStep02State,
  committedFieldShapeStep02StateFromEvidenceV1,
  isCommittedFieldShapeViolationV1,
  MIDGARD_COMMITTED_FIELD_COUNT_V1,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1,
} from "@al-ft/midgard-sdk";

export const COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1 = {
  FieldIndexOutOfRange: "fieldIndexOutOfRange",
  NonConvictingField: "nonConvictingField",
  NoViolation: "noViolation",
} as const;

export type CommittedFieldShapePrepareErrorCodeV1 =
  (typeof COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1)[keyof typeof COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1];

export class CommittedFieldShapePrepareErrorV1 extends Error {
  readonly code: CommittedFieldShapePrepareErrorCodeV1;

  constructor(code: CommittedFieldShapePrepareErrorCodeV1, message: string) {
    super(`committed-field-shape prepare [${code}]: ${message}`);
    this.name = "CommittedFieldShapePrepareErrorV1";
    this.code = code;
  }
}

export type CommittedFieldShapeClassifiedFieldV1 = {
  readonly fieldIndex: number;
  readonly fieldName:
    | "spendInputs"
    | "referenceInputs"
    | "outputs"
    | "requiredObservers"
    | "requiredSigners"
    | "mint"
    | "scriptTxWits"
    | "addrTxWits"
    | "redeemerTxWits";
  readonly claimKind: "body" | "witness";
  readonly preimage: Buffer;
  readonly evidence: CommittedFieldShapeEvidenceV1;
};

const canonicalFieldsV1 = (
  tx: MidgardNativeTxCanonicalV1,
): readonly (readonly [
  CommittedFieldShapeClassifiedFieldV1["fieldName"],
  "body" | "witness",
  Buffer,
])[] => [
  ["spendInputs", "body", Buffer.from(tx.body.spendInputsPreimageCbor)],
  ["referenceInputs", "body", Buffer.from(tx.body.referenceInputsPreimageCbor)],
  ["outputs", "body", Buffer.from(tx.body.outputsPreimageCbor)],
  [
    "requiredObservers",
    "body",
    Buffer.from(tx.body.requiredObserversPreimageCbor),
  ],
  ["requiredSigners", "body", Buffer.from(tx.body.requiredSignersPreimageCbor)],
  ["mint", "body", Buffer.from(tx.body.mintPreimageCbor)],
  [
    "scriptTxWits",
    "witness",
    Buffer.from(tx.witnessSet.scriptTxWitsPreimageCbor),
  ],
  ["addrTxWits", "witness", Buffer.from(tx.witnessSet.addrTxWitsPreimageCbor)],
  [
    "redeemerTxWits",
    "witness",
    Buffer.from(tx.witnessSet.redeemerTxWitsPreimageCbor),
  ],
];

/** Classifies all nine §2.5 committed fields in positional order. */
export const classifyCommittedFieldShapeFieldsV1 = (
  tx: MidgardNativeTxCanonicalV1,
): readonly CommittedFieldShapeClassifiedFieldV1[] => {
  const full = materializeMidgardNativeTxFromCanonicalV1(tx);
  const badTxId = computeMidgardNativeTxIdV1(full).toString("hex");
  return Object.freeze(
    canonicalFieldsV1(tx).map(([fieldName, claimKind, preimage], fieldIndex) =>
      Object.freeze({
        fieldIndex,
        fieldName,
        claimKind,
        preimage,
        evidence: committedFieldShapeEvidenceFromCommittedFieldV1({
          badTxId,
          fieldIndex,
          committedPreimage: preimage,
        }),
      }),
    ),
  );
};

export type PreparedCommittedFieldShapeV1 = {
  readonly evidence: CommittedFieldShapeEvidenceV1;
  readonly claim: CommittedFieldClaimV1;
  readonly step02State: CommittedFieldShapeStep02State;
  readonly classifiedFields: readonly CommittedFieldShapeClassifiedFieldV1[];
};

/**
 * Selects one convicting field (caller-pinned or first in positional order)
 * and emits the exact claim/state the two validators speak.
 */
export const prepareCommittedFieldShapeFromCanonicalTxV1 = ({
  tx,
  fieldIndex,
}: {
  readonly tx: MidgardNativeTxCanonicalV1;
  readonly fieldIndex?: number;
}): PreparedCommittedFieldShapeV1 => {
  if (
    fieldIndex !== undefined &&
    (!Number.isInteger(fieldIndex) ||
      fieldIndex < 0 ||
      fieldIndex >= MIDGARD_COMMITTED_FIELD_COUNT_V1)
  ) {
    throw new CommittedFieldShapePrepareErrorV1(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1.FieldIndexOutOfRange,
      `field index ${String(fieldIndex)} is outside 0..${(MIDGARD_COMMITTED_FIELD_COUNT_V1 - 1).toString()}`,
    );
  }
  const classifiedFields = classifyCommittedFieldShapeFieldsV1(tx);
  const selected =
    fieldIndex === undefined
      ? classifiedFields.find(({ evidence }) => evidence.isViolation)
      : classifiedFields[fieldIndex];
  if (selected === undefined) {
    throw new CommittedFieldShapePrepareErrorV1(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1.NoViolation,
      "none of the transaction's nine committed fields violates the shape rule",
    );
  }
  if (!selected.evidence.isViolation) {
    throw new CommittedFieldShapePrepareErrorV1(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1.NonConvictingField,
      `field ${selected.fieldIndex.toString()} earns non-convicting verdict ${selected.evidence.verdictName}`,
    );
  }
  const carriage = {
    Inline: { preimage: selected.evidence.committedPreimage },
  } as const;
  const claim: CommittedFieldClaimV1 =
    selected.fieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1
      ? {
          BodyFieldClaim: {
            field_index: BigInt(selected.fieldIndex),
            carriage,
          },
        }
      : (() => {
          const witness = deriveMidgardNativeTxWitnessSetCompactV1(
            tx.witnessSet,
          );
          return {
            WitnessFieldClaim: {
              field_index: BigInt(selected.fieldIndex),
              witness_set: {
                addr_tx_wits_hash: Buffer.from(witness.addrTxWitsHash).toString(
                  "hex",
                ),
                script_tx_wits_hash: Buffer.from(
                  witness.scriptTxWitsHash,
                ).toString("hex"),
                redeemer_tx_wits_hash: Buffer.from(
                  witness.redeemerTxWitsHash,
                ).toString("hex"),
              },
              carriage,
            },
          };
        })();
  const step02State = committedFieldShapeStep02StateFromEvidenceV1(
    selected.evidence,
  );
  if (
    !isCommittedFieldShapeViolationV1({
      fieldIndex: Number(step02State.field_index),
      verdict: Number(step02State.verdict),
    })
  ) {
    throw new CommittedFieldShapePrepareErrorV1(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES_V1.NonConvictingField,
      "derived step-02 state is not finalizable",
    );
  }
  return Object.freeze({
    evidence: selected.evidence,
    claim,
    step02State,
    classifiedFields,
  });
};
