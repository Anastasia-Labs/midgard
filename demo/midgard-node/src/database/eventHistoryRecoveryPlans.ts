import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { NativeMpfCanonicalRootRecovery } from "../services/mpf-native-owner/protocol.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import type { Checkpoint } from "./eventHistoryJournal.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_recovery_plans";
const fail = (message: string) =>
  Effect.fail(new DatabaseError({ table, message, cause: undefined }));
const digest = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const isHash = (value: string) => /^[0-9a-f]{64}$/u.test(value);

/** Immutable SQL/native operation identity. Evidence is freshly revalidated on
 * every attempt; it is deliberately not part of the operation ID so a crash
 * after the native CAS can resume at a later canonical checkpoint. */
export type HistoryRecoveryIntent = Readonly<{
  bindingDigest: string;
  manifestId: string;
  headerHash: string;
  signedTransactionHash: string;
  signedTransactionCborSha256: string;
  expectedRoot: string;
  targetRoot: string;
  /** Digest of the exact retained journal and incarnation-bound memberships. */
  journalDigest: string;
}>;
export type HistoryRecoveryPlan = Readonly<{
  recoveryId: string;
  intent: HistoryRecoveryIntent;
  evidenceDigest: string;
  checkpointRevision: string;
  state: "prepared" | "applied";
  native: NativeMpfCanonicalRootRecovery;
}>;

const lockCheckpoint = (checkpoint: Checkpoint) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    if (token.deploymentIdentity !== checkpoint.manifestId)
      return yield* fail("Recovery plan deployment changed");
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`SELECT 1 FROM event_history_cursor
      WHERE binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")}
        AND manifest_id = ${Buffer.from(checkpoint.manifestId, "hex")}
        AND revision = ${checkpoint.revision}
        AND head_hash = ${Buffer.from(checkpoint.head.id, "hex")}
        AND snapshot_digest = ${Buffer.from(checkpoint.capture.snapshotDigest, "hex")}
      FOR UPDATE`;
    if (rows.length !== 1)
      return yield* fail("Recovery plan checkpoint changed");
    return token;
  });

/** Call only after checking freshly admitted branch evidence and the exact
 * journal/native baseline. This persists intent BEFORE any native mutation.
 * A different checkpoint can refresh evidence for the identical operation,
 * never substitute another header, root or incarnation under the same ID. */
export const prepareHistoryRecoveryPlan = (
  checkpoint: Checkpoint,
  intent: HistoryRecoveryIntent,
  evidenceDigest: string,
) =>
  Effect.gen(function* () {
    intent = Object.freeze({ ...intent });
    const token = yield* lockCheckpoint(checkpoint);
    if (
      intent.bindingDigest !== checkpoint.bindingDigest ||
      intent.manifestId !== checkpoint.manifestId ||
      !/^[0-9a-f]{56}$/u.test(intent.headerHash) ||
      !Object.entries(intent).every(
        ([key, value]) => key === "headerHash" || isHash(value),
      ) ||
      !isHash(evidenceDigest)
    )
      return yield* fail("Invalid recovery plan identity");
    // Copy primitives before the first asynchronous boundary involving caller
    // data; the persisted canonical document is also the returned immutable view.
    const payload = eventHistoryCanonicalJson({
      domain: "midgard-history-recovery-intent-v1",
      ...intent,
    });
    const copied = Object.freeze({ ...intent });
    const recoveryId = digest(payload);
    const sql = yield* SqlClient.SqlClient;
    const conflicting = yield* sql`SELECT 1 FROM event_history_recovery_plans
      WHERE binding_digest = ${Buffer.from(intent.bindingDigest, "hex")}
        AND state = 'prepared' AND recovery_id <> ${Buffer.from(recoveryId, "hex")} FOR UPDATE`;
    if (conflicting.length !== 0)
      return yield* fail(
        "A different durable native recovery must be resolved first",
      );
    const rows = yield* sql<{
      state: "prepared" | "applied";
      intent: string;
    }>`INSERT INTO event_history_recovery_plans
      (recovery_id, binding_digest, manifest_id, header_hash, intent, evidence_digest,
       checkpoint_revision, head_hash, snapshot_digest, owner_generation, state)
      VALUES (${Buffer.from(recoveryId, "hex")}, ${Buffer.from(intent.bindingDigest, "hex")},
        ${Buffer.from(intent.manifestId, "hex")}, ${Buffer.from(intent.headerHash, "hex")}, ${payload},
        ${Buffer.from(evidenceDigest, "hex")}, ${checkpoint.revision}, ${Buffer.from(checkpoint.head.id, "hex")},
        ${Buffer.from(checkpoint.capture.snapshotDigest, "hex")}, ${token.generation}, 'prepared')
      ON CONFLICT (recovery_id) DO UPDATE SET
        evidence_digest = EXCLUDED.evidence_digest, checkpoint_revision = EXCLUDED.checkpoint_revision,
        head_hash = EXCLUDED.head_hash, snapshot_digest = EXCLUDED.snapshot_digest,
        owner_generation = EXCLUDED.owner_generation, updated_at = NOW()
      WHERE event_history_recovery_plans.intent = EXCLUDED.intent
      RETURNING state, intent`;
    if (rows.length !== 1 || rows[0]!.intent !== payload)
      return yield* fail(
        "Durable recovery plan conflicts with retained identity",
      );
    return Object.freeze({
      recoveryId,
      intent: copied,
      evidenceDigest,
      checkpointRevision: checkpoint.revision,
      state: rows[0]!.state,
      native: Object.freeze({
        recoveryId,
        expectedRoot: copied.expectedRoot,
        targetRoot: copied.targetRoot,
      }),
    }) satisfies HistoryRecoveryPlan;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to prepare history recovery plan"),
  );

/** Select a retained native base only after fresh branch/queue authorization.
 * Journal status does not reveal whether native promotion already committed.
 * A prepared operation wins over a new diagnostic observation: after its CAS,
 * deriving a new R0-to-R0 identity would strand the original R1-to-R0 receipt.
 */
export const prepareRetainedNativeHistoryRecoveryPlan = (
  checkpoint: Checkpoint,
  intent: Omit<HistoryRecoveryIntent, "expectedRoot">,
  evidenceDigest: string,
  native: Readonly<{ durableRoot: string; candidateRoot: string }>,
) =>
  Effect.gen(function* () {
    const captured = Object.freeze({ ...intent });
    const observed = Object.freeze({ ...native });
    yield* lockCheckpoint(checkpoint);
    if (
      !isHash(observed.durableRoot) ||
      !isHash(observed.candidateRoot) ||
      (observed.durableRoot !== captured.targetRoot &&
        observed.durableRoot !== observed.candidateRoot)
    )
      return yield* fail(
        "Native recovery root is outside the authenticated journal",
      );
    const sql = yield* SqlClient.SqlClient;
    const retained = yield* sql<{
      intent: string;
    }>`SELECT intent FROM event_history_recovery_plans
      WHERE binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")}
        AND state = 'prepared' FOR UPDATE`;
    let expectedRoot = observed.durableRoot;
    if (retained.length !== 0) {
      if (retained.length !== 1)
        return yield* fail("Multiple prepared native recovery operations");
      const prior = yield* Effect.try({
        try: () =>
          JSON.parse(retained[0]!.intent) as { expectedRoot?: unknown },
        catch: () =>
          new DatabaseError({
            table,
            message: "Malformed retained native recovery identity",
            cause: undefined,
          }),
      });
      if (
        prior === null ||
        typeof prior.expectedRoot !== "string" ||
        (prior.expectedRoot !== captured.targetRoot &&
          prior.expectedRoot !== observed.candidateRoot) ||
        (observed.durableRoot !== prior.expectedRoot &&
          observed.durableRoot !== captured.targetRoot) ||
        eventHistoryCanonicalJson({
          domain: "midgard-history-recovery-intent-v1",
          ...captured,
          expectedRoot: prior.expectedRoot,
        }) !== retained[0]!.intent
      )
        return yield* fail(
          "Retained native recovery requires a different disposition",
        );
      expectedRoot = prior.expectedRoot;
    }
    return yield* prepareHistoryRecoveryPlan(
      checkpoint,
      { ...captured, expectedRoot },
      evidenceDigest,
    );
  });

/** The production coordinator calls this only after native restore succeeds.
 * Journal disposition, dependent inverse SQL and this receipt commit together.
 * A failed/superseded transaction leaves the native operation resumable by ID.
 */
export const applyHistoryRecoveryPlan = <A, E, R>(
  checkpoint: Checkpoint,
  plan: HistoryRecoveryPlan,
  repair: Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const identity = eventHistoryCanonicalJson({
      domain: "midgard-history-recovery-intent-v1",
      ...plan.intent,
    });
    if (
      digest(identity) !== plan.recoveryId ||
      plan.native.recoveryId !== plan.recoveryId ||
      plan.native.expectedRoot !== plan.intent.expectedRoot ||
      plan.native.targetRoot !== plan.intent.targetRoot ||
      plan.checkpointRevision !== checkpoint.revision
    )
      return yield* fail("Recovery application immutable identity changed");
    const token = yield* lockCheckpoint(checkpoint);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      state: "prepared" | "applied";
    }>`SELECT state FROM event_history_recovery_plans
      WHERE recovery_id = ${Buffer.from(plan.recoveryId, "hex")}
        AND intent = ${identity}
        AND binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")}
        AND manifest_id = ${Buffer.from(checkpoint.manifestId, "hex")}
        AND checkpoint_revision = ${checkpoint.revision}
        AND head_hash = ${Buffer.from(checkpoint.head.id, "hex")}
        AND snapshot_digest = ${Buffer.from(checkpoint.capture.snapshotDigest, "hex")}
        AND evidence_digest = ${Buffer.from(plan.evidenceDigest, "hex")}
        AND owner_generation = ${token.generation}
      FOR UPDATE`;
    if (rows.length !== 1)
      return yield* fail("Recovery application evidence changed");
    if (rows[0]!.state === "applied") return;
    yield* repair;
    yield* sql`UPDATE event_history_recovery_plans SET state = 'applied', updated_at = NOW()
      WHERE recovery_id = ${Buffer.from(plan.recoveryId, "hex")} AND state = 'prepared'`;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to apply history recovery plan"),
  );
