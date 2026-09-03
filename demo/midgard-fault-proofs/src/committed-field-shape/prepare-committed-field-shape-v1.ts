import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  type CommittedFieldClaim,
  type CommittedFieldShapeEvidence,
  committedFieldShapeEvidenceFromCommittedField,
  type CommittedFieldShapeStep02State,
  committedFieldShapeStep02StateFromEvidence,
  isCommittedFieldShapeViolation,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX,
} from "@al-ft/midgard-sdk";

export const COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES = {
  FieldIndexOutOfRange: "fieldIndexOutOfRange",
  NonConvictingField: "nonConvictingField",
  NoViolation: "noViolation",
} as const;

export type CommittedFieldShapePrepareErrorCode =
  (typeof COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES)[keyof typeof COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES];

export class CommittedFieldShapePrepareError extends Error {
  readonly code: CommittedFieldShapePrepareErrorCode;

  constructor(code: CommittedFieldShapePrepareErrorCode, message: string) {
    super(`committed-field-shape prepare [${code}]: ${message}`);
    this.name = "CommittedFieldShapePrepareErrorV1";
    this.code = code;
  }
}

export type CommittedFieldShapeClassifiedField = {
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
  readonly evidence: CommittedFieldShapeEvidence;
};

const canonicalFields = (
  tx: MidgardNativeTxCanonical,
): readonly (readonly [
  CommittedFieldShapeClassifiedField["fieldName"],
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
export const classifyCommittedFieldShapeFields = (
  tx: MidgardNativeTxCanonical,
): readonly CommittedFieldShapeClassifiedField[] => {
  const full = materializeMidgardNativeTxFromCanonical(tx);
  const badTxId = computeMidgardNativeTxId(full).toString("hex");
  return Object.freeze(
    canonicalFields(tx).map(([fieldName, claimKind, preimage], fieldIndex) =>
      Object.freeze({
        fieldIndex,
        fieldName,
        claimKind,
        preimage,
        evidence: committedFieldShapeEvidenceFromCommittedField({
          badTxId,
          fieldIndex,
          committedPreimage: preimage,
        }),
      }),
    ),
  );
};

export type PreparedCommittedFieldShape = {
  readonly evidence: CommittedFieldShapeEvidence;
  readonly claim: CommittedFieldClaim;
  readonly step02State: CommittedFieldShapeStep02State;
  readonly classifiedFields: readonly CommittedFieldShapeClassifiedField[];
};

/**
 * Selects one convicting field (caller-pinned or first in positional order)
 * and emits the exact claim/state the two validators speak.
 */
export const prepareCommittedFieldShapeFromCanonicalTx = ({
  tx,
  fieldIndex,
}: {
  readonly tx: MidgardNativeTxCanonical;
  readonly fieldIndex?: number;
}): PreparedCommittedFieldShape => {
  if (
    fieldIndex !== undefined &&
    (!Number.isInteger(fieldIndex) ||
      fieldIndex < 0 ||
      fieldIndex >= MIDGARD_COMMITTED_FIELD_COUNT)
  ) {
    throw new CommittedFieldShapePrepareError(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES.FieldIndexOutOfRange,
      `field index ${String(fieldIndex)} is outside 0..${(MIDGARD_COMMITTED_FIELD_COUNT - 1).toString()}`,
    );
  }
  const classifiedFields = classifyCommittedFieldShapeFields(tx);
  const selected =
    fieldIndex === undefined
      ? classifiedFields.find(({ evidence }) => evidence.isViolation)
      : classifiedFields[fieldIndex];
  if (selected === undefined) {
    throw new CommittedFieldShapePrepareError(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES.NoViolation,
      "none of the transaction's nine committed fields violates the shape rule",
    );
  }
  if (!selected.evidence.isViolation) {
    throw new CommittedFieldShapePrepareError(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES.NonConvictingField,
      `field ${selected.fieldIndex.toString()} earns non-convicting verdict ${selected.evidence.verdictName}`,
    );
  }
  const carriage = {
    Inline: { preimage: selected.evidence.committedPreimage },
  } as const;
  const claim: CommittedFieldClaim =
    selected.fieldIndex < MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX
      ? {
          BodyFieldClaim: {
            field_index: BigInt(selected.fieldIndex),
            carriage,
          },
        }
      : (() => {
          const witness = deriveMidgardNativeTxWitnessSetCompact(tx.witnessSet);
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
  const step02State = committedFieldShapeStep02StateFromEvidence(
    selected.evidence,
  );
  if (
    !isCommittedFieldShapeViolation({
      fieldIndex: Number(step02State.field_index),
      verdict: Number(step02State.verdict),
    })
  ) {
    throw new CommittedFieldShapePrepareError(
      COMMITTED_FIELD_SHAPE_PREPARE_ERROR_CODES.NonConvictingField,
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
