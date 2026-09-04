import { createHash } from "node:crypto";

import { Effect } from "effect";

import { ConfigError } from "../services/config.js";

export const PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV =
  "PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN";
export const PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK_ENV =
  "PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK";
export const PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV =
  "PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID";
export const PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK =
  "I_ACKNOWLEDGE_ISOLATED_PHASE1_ACCEPT_CRASH";
export const PHASE1_ACCEPT_CRASH_CHECKPOINT_EVENT =
  "phase1_accept_commit_before_write_behind";

type ArmedCheckpoint = {
  readonly tokenHash: string;
  readonly expectedTxIdHex: string;
  readonly database: string;
};

let armedCheckpoint: ArmedCheckpoint | null | undefined;

const checkpointId = (token: string): string =>
  createHash("sha256").update(token).digest("hex");

export const phase1AcceptCrashCheckpointMarker = (
  token: string,
  expectedTxIdHex: string,
): string =>
  JSON.stringify({
    event: PHASE1_ACCEPT_CRASH_CHECKPOINT_EVENT,
    checkpoint_id: checkpointId(token),
    expected_tx_id: expectedTxIdHex.toLowerCase(),
  }).slice(0, -1);

const resolveCheckpoint = (): ArmedCheckpoint | null => {
  const token = process.env[PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV];
  const acknowledgement = process.env[PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK_ENV];
  const expectedTxId = process.env[PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV];
  const configured = [token, acknowledgement, expectedTxId].filter(
    (value) => value !== undefined && value !== "",
  ).length;
  if (configured === 0) {
    return null;
  }
  if (configured !== 3) {
    throw new Error(
      "Phase 1 accept-crash checkpoint requires token, acknowledgement, and expected tx id together",
    );
  }
  if (process.env.NODE_ENV !== "test" && process.env.NODE_ENV !== "emulator") {
    throw new Error(
      "Phase 1 accept-crash checkpoint is permitted only under NODE_ENV=test or NODE_ENV=emulator",
    );
  }
  if (acknowledgement !== PHASE1_ACCEPT_CRASH_CHECKPOINT_ACK) {
    throw new Error(
      "Phase 1 accept-crash checkpoint acknowledgement is invalid",
    );
  }
  if (token === undefined || !/^[A-Za-z0-9_-]{32,128}$/u.test(token)) {
    throw new Error(
      "Phase 1 accept-crash checkpoint token must contain 32-128 safe characters",
    );
  }
  if (expectedTxId === undefined || !/^[0-9a-fA-F]{64}$/u.test(expectedTxId)) {
    throw new Error(
      "Phase 1 accept-crash checkpoint expected tx id must be 32-byte hex",
    );
  }
  const database = process.env.POSTGRES_DB ?? "";
  if (!/^midgard_phase1_crash_[a-z0-9_]+$/u.test(database)) {
    throw new Error(
      "Phase 1 accept-crash checkpoint requires an isolated POSTGRES_DB named midgard_phase1_crash_*",
    );
  }
  return {
    tokenHash: checkpointId(token),
    expectedTxIdHex: expectedTxId.toLowerCase(),
    database,
  };
};

/**
 * Validates and arms the destructive operator checkpoint before node startup
 * performs any database work. Production/default runs resolve to a cached
 * null and add no logging or crash behavior to accepted transactions.
 */
export const assertPhase1AcceptCrashCheckpointConfiguration: Effect.Effect<
  void,
  ConfigError
> = Effect.try({
  try: () => {
    armedCheckpoint = resolveCheckpoint();
  },
  catch: (cause) =>
    new ConfigError({
      message: "Invalid Phase 1 accept-crash checkpoint configuration",
      cause,
      fieldsAndValues: [
        ["NODE_ENV", process.env.NODE_ENV ?? ""],
        ["POSTGRES_DB", process.env.POSTGRES_DB ?? ""],
        [
          PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV,
          process.env[PHASE1_ACCEPT_CRASH_CHECKPOINT_TOKEN_ENV]
            ? "<redacted-present>"
            : "<absent>",
        ],
        [
          PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV,
          process.env[PHASE1_ACCEPT_CRASH_CHECKPOINT_TX_ID_ENV] ?? "<absent>",
        ],
      ],
    }),
});

/**
 * Emits the deterministic supervisor marker only when the explicitly armed
 * target was part of the just-committed authoritative accept transaction.
 */
export const emitPhase1AcceptCommitCheckpoint = (
  acceptedTxIds: readonly Buffer[],
): Effect.Effect<void, never> =>
  Effect.sync(() => {
    if (armedCheckpoint === undefined) {
      armedCheckpoint = resolveCheckpoint();
    }
    const checkpoint = armedCheckpoint;
    if (checkpoint === null) {
      return;
    }
    const acceptedTxIdsHex = acceptedTxIds.map((txId) => txId.toString("hex"));
    if (!acceptedTxIdsHex.includes(checkpoint.expectedTxIdHex)) {
      return;
    }
    process.stdout.write(
      `${JSON.stringify({
        event: PHASE1_ACCEPT_CRASH_CHECKPOINT_EVENT,
        checkpoint_id: checkpoint.tokenHash,
        expected_tx_id: checkpoint.expectedTxIdHex,
        accepted_tx_ids: acceptedTxIdsHex,
        accepted_count: acceptedTxIdsHex.length,
        database: checkpoint.database,
      })}\n`,
    );
  });
