import { createHash, randomUUID } from "node:crypto";

import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import type { Database } from "../src/services/database.js";
import { provideDatabaseLayers } from "./utils.js";

export const hash = (seed: string) =>
  createHash("sha256").update(seed).digest("hex");

export const input = (): Omit<Journal.Row, "revision"> => {
  const nonce = hash(randomUUID());
  return {
    submission_id: `reservation-${randomUUID()}`,
    kind: "Deposit",
    policy_id: "aa".repeat(28),
    wallet_address: "reservation-test-wallet",
    intent_hash: "bb".repeat(32),
    nonce_out_ref: `${nonce}#0`,
    request: {
      payloadCbor: "d87980",
      reclaimAuthCbor: "d87980",
      assets: { lovelace: "25000000" },
      structuralLovelace: "2500000",
      structuralRefundKey: "cc".repeat(28),
      nonce: {
        txHash: nonce,
        outputIndex: 0,
        address: "reservation-test-wallet",
        assets: { lovelace: "30000000" },
      },
    },
    checkpoint: { requestHash: "dd".repeat(32) },
  };
};

/** A completed body spending `txHashes`#0, with a TTL slot when given. */
export const attempt = (
  txHashes: readonly string[],
  ttl?: number,
  phase: SDK.EventHistorySubmissionAttempt["phase"] = "Admission",
): SDK.EventHistorySubmissionAttempt => {
  const transactionCbor = `84a${ttl === undefined ? 3 : 4}008${txHashes.length}${txHashes
    .map((txHash) => `825820${txHash}00`)
    .join("")}01800200${
    ttl === undefined ? "" : `031a${ttl.toString(16).padStart(8, "0")}`
  }a0f5f6`;
  return {
    phase,
    outputIndex: 0,
    transactionCbor,
    txHash: CML.hash_transaction(
      CML.Transaction.from_cbor_hex(transactionCbor).body(),
    ).to_hex(),
  };
};

export const holdings = (submissionId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ out_ref: string }>`SELECT out_ref
      FROM event_history_submission_inputs WHERE submission_id = ${submissionId}`;
    return rows.map((row) => row.out_ref).sort();
  });

/** A reservation row as the code before release left it behind. */
export const leftBehind = (outRef: string, submissionId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO event_history_submission_inputs (out_ref, submission_id)
      VALUES (${outRef}, ${submissionId})`;
  });

export const run = <E>(
  effect: Effect.Effect<void, E, SqlClient.SqlClient | Database>,
) => Effect.runPromise(provideDatabaseLayers(effect));

/** Polls until a session of the test database holds (`granted`) or waits on
 * an advisory lock. */
export const untilAdvisoryLock = async (granted: boolean) => {
  const deadline = Date.now() + 30_000;
  const found = Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql<{ found: boolean }>`SELECT count(*) > 0 AS found FROM pg_locks
      WHERE locktype = 'advisory' AND granted = ${granted} AND database =
        (SELECT oid FROM pg_database WHERE datname = current_database())`,
  );
  while (!(await Effect.runPromise(provideDatabaseLayers(found)))[0]?.found) {
    if (Date.now() > deadline) throw new Error("No advisory lock session");
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};
