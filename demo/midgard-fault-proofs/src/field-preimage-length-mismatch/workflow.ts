import { createHash } from "node:crypto";

import {
  decodeMidgardNativeTxProofFieldLengths,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";

export const FIELD_PREIMAGE_LENGTH_WORKFLOW =
  "midgard-field-preimage-length-mismatch-workflow-v1" as const;

export type FieldPreimageLengthDirection =
  | "wrongfulAcceptance"
  | "wrongfulRejection";

export type PreparedFieldPreimageLengthWorkflow = Readonly<{
  schemaVersion: typeof FIELD_PREIMAGE_LENGTH_WORKFLOW;
  headerHash: string;
  transactionId: string;
  sourceKind: "normal" | "forced";
  direction: FieldPreimageLengthDirection;
  fieldIndex: number;
  declaredLength: number;
  actualLength: number;
  preimageHex: string;
  carriage: "Inline" | "RawUtxo" | "Certified";
  evidenceDigest: string;
}>;

const exactHex = (value: string, bytes: number, label: string): string => {
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(value)) {
    throw new Error(`${label} must be canonical ${bytes.toString()}-byte hex`);
  }
  return value;
};

export const prepareFieldPreimageLengthWorkflow = ({
  headerHash,
  transactionId,
  direction,
  sourceKind,
  fieldIndex,
  fieldPreimageLengthsCbor,
  fieldPreimage,
  forcedRejectionReason,
}: {
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: FieldPreimageLengthDirection;
  readonly sourceKind: "normal" | "forced";
  readonly fieldIndex: number;
  readonly fieldPreimageLengthsCbor: Uint8Array;
  readonly fieldPreimage: Uint8Array;
  readonly forcedRejectionReason?: unknown;
}): PreparedFieldPreimageLengthWorkflow => {
  if (!Number.isInteger(fieldIndex) || fieldIndex < 0 || fieldIndex >= 9) {
    throw new Error("field index is outside 0..8");
  }
  if (direction === "wrongfulRejection" && sourceKind !== "forced")
    throw new Error("wrongful rejection requires a forced source");
  const declaredLength = decodeMidgardNativeTxProofFieldLengths(
    fieldPreimageLengthsCbor,
  )[fieldIndex]!;
  const actualLength = fieldPreimage.length;
  if (actualLength > 32_768) {
    throw new Error("field preimage exceeds the V1 consensus bound");
  }
  const reason = forcedRejectionReason;
  if (direction === "wrongfulRejection") {
    if (
      typeof reason !== "object" ||
      reason === null ||
      Array.isArray(reason) ||
      Object.keys(reason).length !== 1 ||
      !("FieldPreimageLengthMismatch" in reason)
    ) {
      throw new Error(
        "forced rejection must carry only FieldPreimageLengthMismatch",
      );
    }
    const payload = (
      reason as {
        readonly FieldPreimageLengthMismatch?: {
          readonly field_index?: unknown;
        };
      }
    ).FieldPreimageLengthMismatch;
    if (
      typeof payload !== "object" ||
      payload === null ||
      Object.keys(payload).length !== 1 ||
      payload.field_index !== BigInt(fieldIndex)
    ) {
      throw new Error("forced rejection field coordinate differs");
    }
  } else if (reason !== undefined) {
    throw new Error("wrongful acceptance must not carry a rejection reason");
  }
  const mismatch = declaredLength !== actualLength;
  if (
    (direction === "wrongfulAcceptance" && !mismatch) ||
    (direction === "wrongfulRejection" && mismatch)
  ) {
    throw new Error("evidence does not contradict the selected verdict");
  }
  const preimageHex = Buffer.from(fieldPreimage).toString("hex");
  const normalizedHeader = exactHex(headerHash, 28, "header hash");
  const normalizedTx = exactHex(transactionId, 32, "transaction id");
  const evidenceDigest = createHash("sha256")
    .update(FIELD_PREIMAGE_LENGTH_WORKFLOW)
    .update(sourceKind)
    .update(direction)
    .update(normalizedHeader, "hex")
    .update(normalizedTx, "hex")
    .update(Buffer.from([fieldIndex]))
    .update(Buffer.from(fieldPreimageLengthsCbor))
    .update(Buffer.from(fieldPreimage))
    .digest("hex");
  return Object.freeze({
    schemaVersion: FIELD_PREIMAGE_LENGTH_WORKFLOW,
    headerHash: normalizedHeader,
    transactionId: normalizedTx,
    direction,
    sourceKind,
    fieldIndex,
    declaredLength,
    actualLength,
    preimageHex,
    carriage: selectMidgardFieldCarriageTier(actualLength),
    evidenceDigest,
  });
};

export type FieldPreimageLengthAction =
  | "init"
  | "dispatch"
  | "authenticate"
  | "finalize"
  | "remove"
  | "complete";

export const FIELD_PREIMAGE_LENGTH_PHYSICAL_SCRIPTS = Object.freeze([
  {
    role: "firstStep",
    title: "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
    parameters: [
      "accepted_step_02_validator_script_hash",
      "forced_step_02_validator_script_hash",
      "computation_thread_token_policy_id",
      "hub_oracle",
    ],
  },
  {
    role: "acceptedAuthenticator",
    title:
      "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
    parameters: [
      "step_03_validator_script_hash",
      "computation_thread_token_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  },
  {
    role: "forcedAuthenticator",
    title:
      "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
    parameters: [
      "step_03_validator_script_hash",
      "computation_thread_token_policy_id",
      "field_preimage_certificate_policy_id",
    ],
  },
  {
    role: "terminal",
    title: "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
    parameters: [
      "fraud_proof_token_policy_id",
      "fraud_proof_token_address",
      "computation_thread_token_policy_id",
    ],
  },
] as const);

export type FieldPreimageLengthSubmissionKind =
  | Exclude<FieldPreimageLengthAction, "complete">
  | "cancelDispatch"
  | "cancelAuthentication"
  | "cancelTerminal";
