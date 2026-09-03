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
  fieldIndex,
  fieldPreimageLengthsCbor,
  fieldPreimage,
  forcedRejectionReason,
}: {
  readonly headerHash: string;
  readonly transactionId: string;
  readonly direction: FieldPreimageLengthDirection;
  readonly fieldIndex: number;
  readonly fieldPreimageLengthsCbor: Uint8Array;
  readonly fieldPreimage: Uint8Array;
  readonly forcedRejectionReason?: unknown;
}): PreparedFieldPreimageLengthWorkflow => {
  if (!Number.isInteger(fieldIndex) || fieldIndex < 0 || fieldIndex >= 9) {
    throw new Error("field index is outside 0..8");
  }
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

export type FieldPreimageLengthJournal = Readonly<{
  prepared: PreparedFieldPreimageLengthWorkflow;
  confirmed: readonly Exclude<FieldPreimageLengthAction, "complete">[];
  transactionIds: Readonly<Partial<Record<FieldPreimageLengthAction, string>>>;
}>;

const ORDER: readonly Exclude<FieldPreimageLengthAction, "complete">[] = [
  "init",
  "dispatch",
  "authenticate",
  "finalize",
  "remove",
];

export const nextFieldPreimageLengthAction = (
  journal: FieldPreimageLengthJournal,
): FieldPreimageLengthAction => {
  for (const action of ORDER) {
    if (!journal.confirmed.includes(action)) return action;
  }
  return "complete";
};

export const reconcileFieldPreimageLengthJournal = ({
  journal,
  action,
  transactionId,
  confirmedOnChain,
}: {
  readonly journal: FieldPreimageLengthJournal;
  readonly action: Exclude<FieldPreimageLengthAction, "complete">;
  readonly transactionId: string;
  readonly confirmedOnChain: boolean;
}): FieldPreimageLengthJournal => {
  if (action !== nextFieldPreimageLengthAction(journal)) {
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
export const runFieldPreimageLengthWorkflow = async ({
  load,
  save,
  submit,
  observeConfirmed,
}: {
  readonly load: () => Promise<FieldPreimageLengthJournal>;
  readonly save: (journal: FieldPreimageLengthJournal) => Promise<void>;
  readonly submit: (
    action: Exclude<FieldPreimageLengthAction, "complete">,
    prepared: PreparedFieldPreimageLengthWorkflow,
  ) => Promise<string>;
  readonly observeConfirmed: (
    action: Exclude<FieldPreimageLengthAction, "complete">,
    transactionId: string,
  ) => Promise<boolean>;
}): Promise<FieldPreimageLengthJournal> => {
  let journal = await load();
  while (true) {
    const action = nextFieldPreimageLengthAction(journal);
    if (action === "complete") return journal;
    const known = journal.transactionIds[action];
    const txId = known ?? (await submit(action, journal.prepared));
    journal = reconcileFieldPreimageLengthJournal({
      journal,
      action,
      transactionId: txId,
      confirmedOnChain: await observeConfirmed(action, txId),
    });
    await save(journal);
    if (!journal.confirmed.includes(action)) return journal;
  }
};
