import { Option } from "effect";

import {
  arrayOf,
  booleanValue,
  exactRecord,
  nonEmptyString,
  oneOf,
  openRecord,
} from "../artifact-schema.js";
import { PendingBlockFinalizationsDB } from "../database/index.js";
import { parseEventId } from "./command-utils.js";

export const RECONCILIATION_SCHEMA_VERSION =
  "midgard-e2e-reconciliation-v1" as const;

export type ReconciliationStatus =
  | "satisfied"
  | "pending"
  | "repaired"
  | "blocked"
  | "ambiguous"
  | "failed";

export type ReconciliationEvidence = {
  readonly kind: string;
  readonly detail: Readonly<Record<string, unknown>>;
};

export type ReconciliationResult = {
  readonly schemaVersion: typeof RECONCILIATION_SCHEMA_VERSION;
  readonly milestone: string;
  readonly target: Readonly<Record<string, unknown>>;
  readonly status: ReconciliationStatus;
  readonly safeToRetryOriginalStep: boolean;
  readonly evidence: readonly ReconciliationEvidence[];
  readonly nextAction: string | null;
  readonly repairActions: readonly string[];
};

const parseReconciliationEvidence = (
  value: unknown,
  label: string,
): ReconciliationEvidence => {
  const input = exactRecord(value, label, ["kind", "detail"]);
  return {
    kind: nonEmptyString(input.kind, `${label}.kind`),
    // Evidence detail is deliberately open because each milestone has a
    // different diagnostic payload. The enclosing evidence record is exact.
    detail: openRecord(input.detail, `${label}.detail`),
  };
};

export const RECONCILIATION_MILESTONES = [
  "phas-registered",
  "reference-scripts-complete",
  "deposit-projected",
  "tx-committed",
  "da-attested",
  "block-committed",
  "local-finalization",
  "merge-complete",
] as const;

type ReconciliationMilestone = (typeof RECONCILIATION_MILESTONES)[number];

const canonicalString = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (parsed !== parsed.trim()) {
    throw new Error(`${label} must not contain surrounding whitespace`);
  }
  return parsed;
};

const lowerHex = (
  value: unknown,
  label: string,
  byteLength: number,
): string => {
  const parsed = canonicalString(value, label);
  if (parsed.length !== byteLength * 2 || !/^[0-9a-f]+$/u.test(parsed)) {
    throw new Error(
      `${label} must be ${byteLength.toString()} bytes of lowercase hexadecimal`,
    );
  }
  return parsed;
};

const parseReconciliationTarget = (
  value: unknown,
  milestone: ReconciliationMilestone,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (milestone === "phas-registered") {
    const target = exactRecord(value, label, ["rewardAddress", "scriptHash"]);
    return {
      rewardAddress: canonicalString(
        target.rewardAddress,
        `${label}.rewardAddress`,
      ),
      scriptHash: lowerHex(target.scriptHash, `${label}.scriptHash`, 28),
    };
  }
  if (milestone === "reference-scripts-complete") {
    const target = exactRecord(value, label, [
      "scope",
      "address",
      "authPolicyId",
    ]);
    if (target.scope !== "node-runtime") {
      throw new Error(`${label}.scope must be node-runtime`);
    }
    return {
      scope: "node-runtime",
      address: canonicalString(target.address, `${label}.address`),
      authPolicyId: lowerHex(target.authPolicyId, `${label}.authPolicyId`, 28),
    };
  }
  if (milestone === "deposit-projected") {
    const target = exactRecord(value, label, [], ["eventId", "cardanoTxHash"]);
    if (target.eventId === undefined && target.cardanoTxHash === undefined) {
      throw new Error(`${label} must identify eventId or cardanoTxHash`);
    }
    const eventId =
      target.eventId === undefined
        ? undefined
        : canonicalString(target.eventId, `${label}.eventId`);
    if (
      eventId !== undefined &&
      (!/^(?:[0-9a-f]{2})+$/u.test(eventId) ||
        parseEventId(eventId, `${label}.eventId`).toString("hex") !== eventId)
    ) {
      throw new Error(
        `${label}.eventId must be canonical OutputReference CBOR`,
      );
    }
    return {
      ...(eventId === undefined ? {} : { eventId }),
      ...(target.cardanoTxHash === undefined
        ? {}
        : {
            cardanoTxHash: lowerHex(
              target.cardanoTxHash,
              `${label}.cardanoTxHash`,
              32,
            ),
          }),
    };
  }
  const target = exactRecord(value, label, [
    milestone === "tx-committed" ? "txHash" : "headerHash",
  ]);
  return milestone === "tx-committed"
    ? { txHash: lowerHex(target.txHash, `${label}.txHash`, 32) }
    : { headerHash: lowerHex(target.headerHash, `${label}.headerHash`, 28) };
};

export const parseReconciliationResult = (
  value: unknown,
): ReconciliationResult => {
  const label = "E2E reconciliation";
  const input = exactRecord(value, label, [
    "schemaVersion",
    "milestone",
    "target",
    "status",
    "safeToRetryOriginalStep",
    "evidence",
    "nextAction",
    "repairActions",
  ]);
  if (input.schemaVersion !== RECONCILIATION_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${RECONCILIATION_SCHEMA_VERSION}`,
    );
  }
  const milestone = oneOf(
    input.milestone,
    `${label}.milestone`,
    RECONCILIATION_MILESTONES,
  );
  const parsed: ReconciliationResult = {
    schemaVersion: RECONCILIATION_SCHEMA_VERSION,
    milestone,
    target: parseReconciliationTarget(
      input.target,
      milestone,
      `${label}.target`,
    ),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "pending",
      "repaired",
      "blocked",
      "ambiguous",
      "failed",
    ]),
    safeToRetryOriginalStep: booleanValue(
      input.safeToRetryOriginalStep,
      `${label}.safeToRetryOriginalStep`,
    ),
    evidence: arrayOf(
      input.evidence,
      `${label}.evidence`,
      parseReconciliationEvidence,
    ),
    nextAction:
      input.nextAction === null
        ? null
        : nonEmptyString(input.nextAction, `${label}.nextAction`),
    repairActions: arrayOf(
      input.repairActions,
      `${label}.repairActions`,
      (entry, entryLabel) =>
        oneOf(entry, entryLabel, [
          "register_phas_membership_reward_account",
          "ensure_node_runtime_reference_scripts",
          "backfill_missing_da_payload",
          "recover_local_finalization",
          "merge_action",
        ]),
    ),
  };
  if (
    new Set(parsed.repairActions).size !== parsed.repairActions.length ||
    ((parsed.status === "ambiguous" ||
      parsed.status === "blocked" ||
      parsed.status === "failed") &&
      parsed.safeToRetryOriginalStep) ||
    (parsed.status === "satisfied" && parsed.nextAction !== null) ||
    (parsed.status === "repaired" &&
      (parsed.repairActions.length === 0 || parsed.nextAction !== null))
  ) {
    throw new Error(
      `${label} status, retry, or repair binding is inconsistent`,
    );
  }
  return parsed;
};

export const evidence = (
  kind: string,
  detail: Readonly<Record<string, unknown>>,
): ReconciliationEvidence => ({ kind, detail });

type ReconciliationResultInput = Pick<
  ReconciliationResult,
  "milestone" | "target" | "status"
> &
  Partial<
    Pick<
      ReconciliationResult,
      "safeToRetryOriginalStep" | "evidence" | "nextAction" | "repairActions"
    >
  >;

export const result = ({
  milestone,
  target,
  status,
  safeToRetryOriginalStep = false,
  evidence: evidenceEntries = [],
  nextAction = null,
  repairActions = [],
}: ReconciliationResultInput): ReconciliationResult =>
  parseReconciliationResult({
    schemaVersion: RECONCILIATION_SCHEMA_VERSION,
    milestone,
    target,
    status,
    safeToRetryOriginalStep,
    evidence: evidenceEntries,
    nextAction,
    repairActions,
  });

export const bufferHex = (value: Buffer | null | undefined): string | null =>
  value === null || value === undefined ? null : value.toString("hex");

export const optionRecordEvidence = (
  record: Option.Option<PendingBlockFinalizationsDB.Record>,
): ReconciliationEvidence =>
  evidence(
    "pending_block_finalization",
    Option.isNone(record)
      ? { present: false }
      : {
          present: true,
          headerHash:
            record.value[
              PendingBlockFinalizationsDB.Columns.HEADER_HASH
            ].toString("hex"),
          submittedTxHash: bufferHex(
            record.value[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH],
          ),
          status: record.value[PendingBlockFinalizationsDB.Columns.STATUS],
          depositEventIds: record.value.depositEventIds.map((id) =>
            id.toString("hex"),
          ),
          forcedTransactionEventIds: record.value.forcedTransactionEventIds.map(
            (id) => id.toString("hex"),
          ),
          withdrawalEventIds: record.value.withdrawalEventIds.map((id) =>
            id.toString("hex"),
          ),
          txIds: record.value.mempoolTxIds.map((id) => id.toString("hex")),
        },
  );
