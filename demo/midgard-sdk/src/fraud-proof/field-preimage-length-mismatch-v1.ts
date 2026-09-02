import { decodeMidgardNativeTxProofFieldLengthsV1 } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, OutputReferenceSchema } from "@/common.js";
import { ForcedInclusionTxV1Schema, HeaderV1Schema } from "@/ledger-state.js";
import type { RejectionReasonV1 as RejectionReasonV1Type } from "@/rejection-reason-v1.js";
import { RejectionReasonV1Schema } from "@/rejection-reason-v1.js";
import { rootMembershipProofSchema } from "@/transition-trace.js";

import { CommittedFieldClaimV1Schema } from "./canonical-decodability-v1.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "./proof-thread-substrate-v1.js";

export const FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY_V1 =
  "fieldPreimageLengthMismatch" as const;
export const FIELD_PREIMAGE_LENGTH_MISMATCH_PROPOSED_ID_V1 =
  "00000020" as const;
export const MIDGARD_FIELD_PREIMAGE_COUNT_V1 = 9;
export const MIDGARD_MAX_FIELD_PREIMAGE_BYTES_V1 = 32_768;

export type FieldPreimageLengthEvidenceV1 = Readonly<{
  category: typeof FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY_V1;
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

export const fieldPreimageLengthMismatchFaultHoldsV1 = ({
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
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT_V1) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  if (actualLength > MIDGARD_MAX_FIELD_PREIMAGE_BYTES_V1) {
    throw new Error("field preimage exceeds the V1 consensus bound");
  }
  return declaredLength !== actualLength;
};

export const prepareFieldPreimageLengthEvidenceV1 = ({
  transactionId: txId,
  fieldIndex,
  fieldPreimageLengthsCbor,
  fieldPreimage,
}: {
  readonly transactionId: string;
  readonly fieldIndex: number;
  readonly fieldPreimageLengthsCbor: Uint8Array;
  readonly fieldPreimage: Uint8Array;
}): FieldPreimageLengthEvidenceV1 => {
  coordinate(fieldIndex, "fieldIndex");
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT_V1) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  const declaredLength = decodeMidgardNativeTxProofFieldLengthsV1(
    fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  const actualLength = fieldPreimage.length;
  const faultHolds = fieldPreimageLengthMismatchFaultHoldsV1({
    fieldIndex,
    declaredLength,
    actualLength,
  });
  return Object.freeze({
    category: FIELD_PREIMAGE_LENGTH_MISMATCH_CATEGORY_V1,
    transactionId: transactionId(txId),
    fieldIndex,
    declaredLength,
    actualLength,
    fieldPreimage: Buffer.from(fieldPreimage).toString("hex"),
    faultHolds,
  });
};

export const fieldPreimageLengthReasonV1 = (
  fieldIndex: number,
): RejectionReasonV1Type => {
  coordinate(fieldIndex, "fieldIndex");
  if (fieldIndex >= MIDGARD_FIELD_PREIMAGE_COUNT_V1) {
    throw new Error("fieldIndex is outside the nine-field V1 subject space");
  }
  return {
    FieldPreimageLengthMismatch: { field_index: BigInt(fieldIndex) },
  };
};

export const requireFieldPreimageLengthSubjectV1 = (
  subject: VerdictSubjectV1,
  fieldIndex: number,
): VerdictSubjectV1 => {
  if (!verdictSubjectIsCanonicalV1(subject)) {
    throw new Error("field-preimage-length subject is not canonical");
  }
  if (subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1) {
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

export const fieldPreimageLengthTerminalContradictionV1 = ({
  subject,
  evidence,
}: {
  readonly subject: VerdictSubjectV1;
  readonly evidence: FieldPreimageLengthEvidenceV1;
}): boolean => {
  requireFieldPreimageLengthSubjectV1(subject, evidence.fieldIndex);
  if (subject.transaction_id !== evidence.transactionId) {
    throw new Error("evidence transaction differs from the verdict subject");
  }
  return terminalVerdictContradictionV1(subject, evidence.faultHolds);
};

export const FieldPreimageLengthVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: H32Schema,
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

/** `step_01.Args`: dispatches accepted and forced subjects to distinct scripts. */
export const FieldPreimageLengthStep01ArgsV1Schema = Data.Enum([
  Data.Object({
    BindAccepted: Data.Object({
      inclusion: NativeTxInclusionCarriageSchema,
      claim: CommittedFieldClaimV1Schema,
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
export const FieldPreimageLengthStep01DatumV1Schema = faultProofStepDatumSchema(
  Data.Any(),
);
export const FieldPreimageLengthStep01RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep01ArgsV1Schema);

/** `step_02.State`, shared by the two physical authentication scripts. */
export const FieldPreimageLengthStep02StateV1Schema = Data.Enum([
  Data.Object({
    BoundSource: Data.Object({
      subject: FieldPreimageLengthVerdictSubjectV1Schema,
      source_cbor: Data.Bytes(),
    }),
  }),
  Data.Object({
    PendingForced: Data.Object({ direction: Data.Integer() }),
  }),
]);
export const FieldPreimageLengthStep02DatumV1Schema = faultProofStepDatumSchema(
  FieldPreimageLengthStep02StateV1Schema,
);

export const FieldPreimageLengthForcedMembershipV1Schema =
  rootMembershipProofSchema(OutputReferenceSchema, ForcedInclusionTxV1Schema);
export const FieldPreimageLengthStep02ArgsV1Schema = Data.Enum([
  Data.Object({
    AuthenticateAccepted: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      claim: CommittedFieldClaimV1Schema,
    }),
  }),
  Data.Object({
    AuthenticateForced: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderV1Schema,
      membership: FieldPreimageLengthForcedMembershipV1Schema,
      claim: CommittedFieldClaimV1Schema,
    }),
  }),
]);
export const FieldPreimageLengthStep02RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep02ArgsV1Schema);

/** `step_03.State`, the constant-size terminal input. */
export const FieldPreimageLengthStateV1Schema = Data.Object({
  subject: FieldPreimageLengthVerdictSubjectV1Schema,
  field_index: Data.Integer(),
  declared_length: Data.Integer(),
  actual_length: Data.Integer(),
});

export type FieldPreimageLengthStateV1 = Data.Static<
  typeof FieldPreimageLengthStateV1Schema
>;
export const FieldPreimageLengthStateV1 =
  FieldPreimageLengthStateV1Schema as unknown as FieldPreimageLengthStateV1;
export const FieldPreimageLengthStep03DatumV1Schema = faultProofStepDatumSchema(
  FieldPreimageLengthStateV1Schema,
);
export const FieldPreimageLengthStep03ArgsV1Schema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export const FieldPreimageLengthStep03RedeemerV1Schema =
  faultProofStepRedeemerSchema(FieldPreimageLengthStep03ArgsV1Schema);

export const fieldPreimageLengthStateV1 = ({
  subject,
  evidence,
}: {
  readonly subject: VerdictSubjectV1;
  readonly evidence: FieldPreimageLengthEvidenceV1;
}): FieldPreimageLengthStateV1 => {
  requireFieldPreimageLengthSubjectV1(subject, evidence.fieldIndex);
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
