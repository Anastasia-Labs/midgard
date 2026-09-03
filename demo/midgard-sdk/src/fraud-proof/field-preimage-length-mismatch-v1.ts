import { decodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxSchema, HeaderSchema } from "../ledger-state.js";
import type { RejectionReason as RejectionReasonType } from "../rejection-reason-v1.js";
import { RejectionReasonSchema } from "../rejection-reason-v1.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { CommittedFieldClaimSchema } from "./canonical-decodability-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "./proof-thread-substrate-v1.js";

export const FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY =
  "fieldPreimageLengthMismatch" as const;
export const FIELD_PREIMAGE_LENGTH_MISMATCH_PROPOSED_ID = "00000020" as const;
export const MIDGARD_FIELD_PREIMAGE_COUNT = 9;
export const MIDGARD_MAX_FIELD_PREIMAGE_BYTES = 32_768;

export type FieldPreimageLengthEvidence = Readonly<{
  category: typeof FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY;
  transactionId: string;
  fieldIndex: number;
  declaredLength: number;
  actualLength: number;
  fieldPreimage: string;
  faultHolds: boolean;
}>;

const coordinate = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value;
};

const transactionId = (value: string): string => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error("transactionId must be canonical 32-byte lowercase hex");
  }
  return value;
};

export const fieldPreimageLengthMismatchFaultHolds = ({
  fieldIndex,
  declaredLength,
  actualLength,
}: {
  readonly fieldIndex: number;
  readonly declaredLength: number;
  readonly actualLength: number;
}): boolean => {
  coordinate(fieldIndex, "fieldIndex");
  coordinate(declaredLength, "declaredLength");
  coordinate(actualLength, "actualLength");
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  if (actualLength > MIDGARD_MAX_FIELD_PREIMAGE_BYTES) {
    throw new Error("field preimage exceeds the V1 consensus bound");
  }
  return declaredLength !== actualLength;
};

export const prepareFieldPreimageLengthEvidence = ({
  transactionId: txId,
  fieldIndex,
  fieldPreimageLengthsCbor,
  fieldPreimage,
}: {
  readonly transactionId: string;
  readonly fieldIndex: number;
  readonly fieldPreimageLengthsCbor: Uint8Array;
  readonly fieldPreimage: Uint8Array;
}): FieldPreimageLengthEvidence => {
  coordinate(fieldIndex, "fieldIndex");
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  const declaredLength = decodeMidgardNativeTxProofFieldLengths(
    fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  const actualLength = fieldPreimage.length;
  const faultHolds = fieldPreimageLengthMismatchFaultHolds({
    fieldIndex,
    declaredLength,
    actualLength,
  });
  return Object.freeze({
    category: FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY,
    transactionId: transactionId(txId),
    fieldIndex,
    declaredLength,
    actualLength,
    fieldPreimage: Buffer.from(fieldPreimage).toString("hex"),
    faultHolds,
  });
};

export const fieldPreimageLengthReason = (
  fieldIndex: number,
): RejectionReasonType => {
  coordinate(fieldIndex, "fieldIndex");
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  return {
    FieldPreimageLengthMismatch: { field_index: BigInt(fieldIndex) },
  };
};

export const requireFieldPreimageLengthSubject = (
  subject: VerdictSubject,
  fieldIndex: number,
): VerdictSubject => {
  if (!verdictSubjectIsCanonical(subject)) {
    throw new Error("field-preimage-length subject is not canonical");
  }
  if (subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE) {
    const reason = subject.rejection_reason;
    if (
      reason === null ||
      typeof reason !== "object" ||
      !("FieldPreimageLengthMismatch" in reason) ||
      reason.FieldPreimageLengthMismatch.field_index !== BigInt(fieldIndex)
    ) {
      throw new Error(
        "forced rejection has another typed reason or field coordinate",
      );
    }
  }
  return subject;
};

export const fieldPreimageLengthTerminalContradiction = ({
  subject,
  evidence,
}: {
  readonly subject: VerdictSubject;
  readonly evidence: FieldPreimageLengthEvidence;
}): boolean => {
  requireFieldPreimageLengthSubject(subject, evidence.fieldIndex);
  if (subject.transaction_id !== evidence.transactionId) {
    throw new Error("evidence transaction differs from the verdict subject");
  }
  return terminalVerdictContradiction(subject, evidence.faultHolds);
};

export const FieldPreimageLengthVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: H32Schema,
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

/** `step_01.Args`: dispatches accepted and forced subjects to distinct scripts. */
export const FieldPreimageLengthStep01ArgsSchema = Data.Enum([
  Data.Object({
    BindAccepted: Data.Object({
      inclusion: NativeTxInclusionCarriageSchema,
      claim: CommittedFieldClaimSchema,
    }),
  }),
  Data.Object({
    RecordForced: Data.Object({
      direction: Data.Integer(),
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export const FieldPreimageLengthStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export const FieldPreimageLengthStep01RedeemerSchema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep01ArgsSchema);

/** `step_02.State`, shared by the two physical authentication scripts. */
export const FieldPreimageLengthStep02StateSchema = Data.Enum([
  Data.Object({
    BoundSource: Data.Object({
      subject: FieldPreimageLengthVerdictSubjectSchema,
      source_cbor: Data.Bytes(),
    }),
  }),
  Data.Object({
    PendingForced: Data.Object({ direction: Data.Integer() }),
  }),
]);
export const FieldPreimageLengthStep02DatumSchema = faultProofStepDatumSchema(
  FieldPreimageLengthStep02StateSchema,
);

export const FieldPreimageLengthForcedMembershipSchema =
  rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxSchema);
export const FieldPreimageLengthStep02ArgsSchema = Data.Enum([
  Data.Object({
    AuthenticateAccepted: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      claim: CommittedFieldClaimSchema,
    }),
  }),
  Data.Object({
    AuthenticateForced: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: FieldPreimageLengthForcedMembershipSchema,
      claim: CommittedFieldClaimSchema,
    }),
  }),
]);
export const FieldPreimageLengthStep02RedeemerSchema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep02ArgsSchema);

/** `step_03.State`, the constant-size terminal input. */
export const FieldPreimageLengthStateSchema = Data.Object({
  subject: FieldPreimageLengthVerdictSubjectSchema,
  field_index: Data.Integer(),
  declared_length: Data.Integer(),
  actual_length: Data.Integer(),
});

export type FieldPreimageLengthState = Data.Static<
  typeof FieldPreimageLengthStateSchema
>;
export const FieldPreimageLengthState =
  FieldPreimageLengthStateSchema as unknown as FieldPreimageLengthState;
export const FieldPreimageLengthStep03DatumSchema = faultProofStepDatumSchema(
  FieldPreimageLengthStateSchema,
);
export const FieldPreimageLengthStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const FieldPreimageLengthStep03RedeemerSchema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep03ArgsSchema);

export const fieldPreimageLengthState = ({
  subject,
  evidence,
}: {
  readonly subject: VerdictSubject;
  readonly evidence: FieldPreimageLengthEvidence;
}): FieldPreimageLengthState => {
  requireFieldPreimageLengthSubject(subject, evidence.fieldIndex);
  if (subject.transaction_id !== evidence.transactionId) {
    throw new Error("evidence transaction differs from the verdict subject");
  }
  return {
    subject,
    field_index: BigInt(evidence.fieldIndex),
    declared_length: BigInt(evidence.declaredLength),
    actual_length: BigInt(evidence.actualLength),
  };
};
