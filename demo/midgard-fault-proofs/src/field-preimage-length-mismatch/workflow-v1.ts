import { createHash } from "node:crypto";

import {
  decodeMidgardNativeTxProofFieldLengthsV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";

export const FIELD_PREIMAGE_LENGTH_WORKFLOW_V1 =
  "midgard-field-preimage-length-mismatch-workflow-v1" as const;

export type FieldPreimageLengthDirectionV1 =
  | "wrongfulAcceptance"
  | "wrongfulRejection";

export type PreparedFieldPreimageLengthWorkflowV1 = Readonly<{
  schemaVersion: typeof FIELD_PREIMAGE_LENGTH_WORKFLOW_V1;
  headerHash: string;
  transactionId: string;
  direction: FieldPreimageLengthDirectionV1;
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

export const prepareFieldPreimageLengthWorkflowV1 = ({
  headerHash,
  transactionId,
  direction,
  fieldIndex,
  fieldPreimageLengthsCbor,
  fieldPreimage,
  forcedRejectionReason,
}: {
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: FieldPreimageLengthDirectionV1;
  readonly fieldIndex: number;
  readonly fieldPreimageLengthsCbor: Uint8Array;
  readonly fieldPreimage: Uint8Array;
  readonly forcedRejectionReason?: unknown;
}): PreparedFieldPreimageLengthWorkflowV1 => {
  if (!Number.isInteger(fieldIndex) || fieldIndex < 0 || fieldIndex >= 9) {
    throw new Error("field index is outside 0..8");
  }
  const declaredLength = decodeMidgardNativeTxProofFieldLengthsV1(
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
    .update(FIELD_PREIMAGE_LENGTH_WORKFLOW_V1)
    .update(direction)
    .update(normalizedHeader, "hex")
    .update(normalizedTx, "hex")
    .update(Buffer.from([fieldIndex]))
    .update(Buffer.from(fieldPreimageLengthsCbor))
    .update(Buffer.from(fieldPreimage))
    .digest("hex");
  return Object.freeze({
    schemaVersion: FIELD_PREIMAGE_LENGTH_WORKFLOW_V1,
    headerHash: normalizedHeader,
    transactionId: normalizedTx,
    direction,
    fieldIndex,
    declaredLength,
    actualLength,
    preimageHex,
    carriage: selectMidgardFieldCarriageTierV1(actualLength),
    evidenceDigest,
  });
};

export type FieldPreimageLengthActionV1 =
  | "init"
  | "dispatch"
  | "authenticate"
  | "finalize"
  | "remove"
  | "complete";

export const FIELD_PREIMAGE_LENGTH_PHYSICAL_SCRIPTS_V1 = Object.freeze([
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

export type FieldPreimageLengthSubmissionKindV1 =
  | Exclude<FieldPreimageLengthActionV1, "complete">
  | "cancelDispatch"
  | "cancelAuthentication"
  | "cancelTerminal";

export type FieldPreimageLengthJournalV1 = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflowV1;
  confirmed: readonly Exclude<FieldPreimageLengthActionV1, "complete">[];
  transactionIds: Readonly<
    Partial<Record<FieldPreimageLengthActionV1, string>>
  >;
}>;

const ORDER: readonly Exclude<FieldPreimageLengthActionV1, "complete">[] = [
  "init",
  "dispatch",
  "authenticate",
  "finalize",
  "remove",
];

export const nextFieldPreimageLengthActionV1 = (
  journal: FieldPreimageLengthJournalV1,
): FieldPreimageLengthActionV1 => {
  for (const action of ORDER) {
    if (!journal.confirmed.includes(action)) return action;
  }
  return "complete";
};

export const reconcileFieldPreimageLengthJournalV1 = ({
  journal,
  action,
  transactionId,
  confirmedOnChain,
}: {
  readonly journal: FieldPreimageLengthJournalV1;
  readonly action: Exclude<FieldPreimageLengthActionV1, "complete">;
  readonly transactionId: string;
  readonly confirmedOnChain: boolean;
}): FieldPreimageLengthJournalV1 => {
  if (action !== nextFieldPreimageLengthActionV1(journal)) {
    throw new Error("journal action differs from authenticated chain state");
  }
  const txId = exactHex(transactionId, 32, "submitted transaction id");
  const existing = journal.transactionIds[action];
  if (existing !== undefined && existing !== txId) {
    throw new Error("submitted transaction identity changed across restart");
  }
  return Object.freeze({
    prepared: journal.prepared,
    confirmed: confirmedOnChain
      ? Object.freeze([...journal.confirmed, action])
      : journal.confirmed,
    transactionIds: Object.freeze({
      ...journal.transactionIds,
      [action]: txId,
    }),
  });
};

/** Durable runner boundary: the store is flushed after every captured identity. */
export const runFieldPreimageLengthWorkflowV1 = async ({
  load,
  save,
  submit,
  observeConfirmed,
}: {
  readonly load: () => Promise<FieldPreimageLengthJournalV1>;
  readonly save: (journal: FieldPreimageLengthJournalV1) => Promise<void>;
  readonly submit: (
    action: Exclude<FieldPreimageLengthActionV1, "complete">,
    prepared: PreparedFieldPreimageLengthWorkflowV1,
  ) => Promise<string>;
  readonly observeConfirmed: (
    action: Exclude<FieldPreimageLengthActionV1, "complete">,
    transactionId: string,
  ) => Promise<boolean>;
}): Promise<FieldPreimageLengthJournalV1> => {
  let journal = await load();
  while (true) {
    const action = nextFieldPreimageLengthActionV1(journal);
    if (action === "complete") return journal;
    const known = journal.transactionIds[action];
    const txId = known ?? (await submit(action, journal.prepared));
    journal = reconcileFieldPreimageLengthJournalV1({
      journal,
      action,
      transactionId: txId,
      confirmedOnChain: await observeConfirmed(action, txId),
    });
    await save(journal);
    if (!journal.confirmed.includes(action)) return journal;
  }
};
