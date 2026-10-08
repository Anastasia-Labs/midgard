import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { Checkpoint } from "./eventHistoryJournal.js";
import {
  DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN,
  parseDisplacementCompensationIntent,
} from "./eventHistoryRecoveryPlans.displacement-compensation.js";
import {
  CORRECTION_REWIND_RECOVERY_DOMAIN,
  type CorrectionRewindIntent,
  type CorrectionRewindRecoveryPlan,
  type DependentRecoveryPlan,
  digest,
  DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
  fail,
  freezeRewindIntent,
  type HistoryRecoveryDomain,
  type HistoryRecoveryIntent,
  type HistoryRecoveryKind,
  historyRecoveryKind,
  isHash,
  isHeaderHash,
  lockCheckpoint,
  persistRecoveryPlan,
  planIdentity,
  prepareHistoryRecoveryPlan,
  table,
  validRewindIntent,
} from "./eventHistoryRecoveryPlans.prepare-history-recovery-plan.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

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
  domain: HistoryRecoveryDomain,
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
        !isHash(prior.expectedRoot)
      )
        return yield* fail("Malformed retained native recovery identity");
      if (
        (prior.expectedRoot !== captured.targetRoot &&
          prior.expectedRoot !== observed.candidateRoot) ||
        (observed.durableRoot !== prior.expectedRoot &&
          observed.durableRoot !== captured.targetRoot)
      )
        return yield* Effect.fail(
          new DatabaseError({
            table,
            message:
              "Retained native recovery requires a different disposition",
            cause: { nativeRecoveryRefusal: "root" },
          }),
        );
      if (
        eventHistoryCanonicalJson({
          domain,
          ...captured,
          expectedRoot: prior.expectedRoot,
        }) !== retained[0]!.intent
      )
        return yield* Effect.fail(
          new DatabaseError({
            table,
            message:
              "Retained native recovery requires a different disposition",
            cause: { nativeRecoveryRefusal: "identity" },
          }),
        );
      expectedRoot = prior.expectedRoot;
    }
    return yield* prepareHistoryRecoveryPlan(
      checkpoint,
      { ...captured, expectedRoot },
      evidenceDigest,
      domain,
    );
  });

/** Deletes the binding's prepared `domain` plan for `headerHash`, whose
 * operation the caller proved moot at this checkpoint, in the caller's owned
 * recovery transaction. Its native CAS may already have run: the caller's
 * disposition must make the native root consistent again without it. Any
 * other retained operation is refused. */
export const discardPreparedHistoryRecoveryPlan = (
  checkpoint: Checkpoint,
  domain: HistoryRecoveryDomain,
  headerHash: string,
) =>
  Effect.gen(function* () {
    yield* lockCheckpoint(checkpoint);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ recovery_id: Buffer; intent: string }>`
      SELECT recovery_id, intent FROM event_history_recovery_plans
      WHERE binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")}
        AND state = 'prepared' FOR UPDATE`;
    if (rows.length !== 1)
      return yield* fail("No single prepared native recovery to discard");
    let decoded: Record<string, unknown> | null;
    try {
      decoded = JSON.parse(rows[0]!.intent) as Record<string, unknown> | null;
    } catch {
      decoded = null;
    }
    if (decoded?.domain !== domain || decoded.headerHash !== headerHash)
      return yield* fail(
        "The prepared native recovery is not the operation to discard",
      );
    yield* sql`DELETE FROM event_history_recovery_plans
      WHERE recovery_id = ${rows[0]!.recovery_id} AND state = 'prepared'`;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to discard a prepared recovery"),
  );

/** The single prepared native recovery of this binding, if any, decoded by
 * domain. A plan of a kind only removed services prepared (signed-header
 * recovery, signed-intent release, displacement) is still reported by kind,
 * header, the native root its CAS moves from and the journal digest it
 * binds, so a reader can name it; no service resumes it any more. An
 * undecodable retained identity fails closed. */
export const retainedPreparedRecoveryPlan = (bindingDigest: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      intent: string;
      recovery_id: Buffer;
    }>`SELECT intent, recovery_id FROM event_history_recovery_plans
      WHERE binding_digest = ${Buffer.from(bindingDigest, "hex")}
        AND state = 'prepared'`;
    if (rows.length === 0) return undefined;
    if (rows.length !== 1)
      return yield* fail("Multiple prepared native recovery operations");
    const decoded = yield* Effect.try({
      try: () => JSON.parse(rows[0]!.intent) as Record<string, unknown>,
      catch: () =>
        new DatabaseError({
          table,
          message: "Malformed retained native recovery identity",
          cause: undefined,
        }),
    });
    if (decoded?.domain === DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN) {
      const { domain: _domain, ...fields } = decoded;
      const intent = parseDisplacementCompensationIntent(fields);
      if (
        intent === undefined ||
        eventHistoryCanonicalJson(decoded) !== rows[0]!.intent ||
        digest(rows[0]!.intent) !== rows[0]!.recovery_id.toString("hex")
      )
        return yield* fail("Malformed retained displacement compensation");
      return {
        kind: "displacement_compensation" as const,
        recoveryId: rows[0]!.recovery_id.toString("hex"),
        intent,
      };
    }
    const historyKind = historyRecoveryKind(decoded?.domain);
    if (historyKind !== undefined) {
      if (
        !isHeaderHash(decoded.headerHash) ||
        typeof decoded.expectedRoot !== "string" ||
        !isHash(decoded.expectedRoot)
      )
        return yield* fail(
          "Malformed retained signed-header recovery identity",
        );
      if (
        decoded.domain === DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN &&
        (typeof decoded.operationNonce !== "string" ||
          !isHash(decoded.operationNonce) ||
          !Array.isArray(decoded.displacedHeaderHashes) ||
          decoded.displacedHeaderHashes.length === 0 ||
          !decoded.displacedHeaderHashes.every(isHeaderHash) ||
          new Set(decoded.displacedHeaderHashes).size !==
            decoded.displacedHeaderHashes.length ||
          typeof decoded.targetRoot !== "string" ||
          !isHash(decoded.targetRoot) ||
          typeof decoded.journalDigest !== "string" ||
          !isHash(decoded.journalDigest) ||
          digest(rows[0]!.intent) !== rows[0]!.recovery_id.toString("hex"))
      )
        return yield* fail("Malformed retained displacement recovery identity");
      return {
        kind: historyKind,
        headerHash: decoded.headerHash,
        expectedRoot: decoded.expectedRoot,
        // The journal it binds, for the service that resumes it.
        journalDigest:
          typeof decoded.journalDigest === "string" &&
          isHash(decoded.journalDigest)
            ? decoded.journalDigest
            : undefined,
        ...(decoded.domain === DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN && {
          recoveryId: rows[0]!.recovery_id.toString("hex"),
          operationNonce: decoded.operationNonce as string,
          displacementIntent: Object.freeze({
            ...Object.fromEntries(
              Object.entries(decoded).filter(([key]) => key !== "domain"),
            ),
            displacedHeaderHashes: Object.freeze([
              ...(decoded.displacedHeaderHashes as string[]),
            ]),
          }) as unknown as HistoryRecoveryIntent,
          targetRoot: decoded.targetRoot as string,
          displacedHeaderHashes:
            decoded.displacedHeaderHashes as readonly string[],
        }),
      };
    }
    if (decoded?.domain !== CORRECTION_REWIND_RECOVERY_DOMAIN)
      return yield* fail("Retained native recovery has an unknown domain");
    const { domain: _domain, ...fields } = decoded;
    const intent = freezeRewindIntent(
      fields as unknown as CorrectionRewindIntent,
    );
    if (
      !validRewindIntent(intent) ||
      eventHistoryCanonicalJson({
        domain: CORRECTION_REWIND_RECOVERY_DOMAIN,
        ...intent,
      }) !== rows[0]!.intent
    )
      return yield* fail("Malformed retained correction rewind identity");
    return { kind: "correction_rewind" as const, intent };
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read retained recovery plan"),
  );

/** Persist a correction rewind BEFORE its native restore. The caller checked
 * fresh correction authority, the exact removed journal chain and the durable
 * native root; resuming passes the retained intent back verbatim. */
export const prepareCorrectionRewindRecoveryPlan = (
  checkpoint: Checkpoint,
  intent: CorrectionRewindIntent,
  evidenceDigest: string,
) =>
  Effect.gen(function* () {
    const copied = freezeRewindIntent(intent);
    if (
      copied.bindingDigest !== checkpoint.bindingDigest ||
      copied.manifestId !== checkpoint.manifestId ||
      !validRewindIntent(copied) ||
      !isHash(evidenceDigest)
    ) {
      yield* lockCheckpoint(checkpoint);
      return yield* fail("Invalid correction rewind identity");
    }
    const { recoveryId, state } = yield* persistRecoveryPlan(
      checkpoint,
      CORRECTION_REWIND_RECOVERY_DOMAIN,
      copied,
      evidenceDigest,
      copied.headerHash,
    );
    return Object.freeze({
      kind: "correction_rewind" as const,
      recoveryId,
      intent: copied,
      evidenceDigest,
      checkpointRevision: checkpoint.revision,
      state,
      native: Object.freeze({
        recoveryId,
        expectedRoot: copied.expectedRoot,
        targetRoot: copied.targetRoot,
      }),
    }) satisfies CorrectionRewindRecoveryPlan;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to prepare correction rewind plan"),
  );

/** The production coordinator calls this only after native restore succeeds.
 * Journal disposition, dependent inverse SQL and this receipt commit together.
 * A failed/superseded transaction leaves the native operation resumable by ID.
 */
export const applyHistoryRecoveryPlan = <A, E, R>(
  checkpoint: Checkpoint,
  plan: DependentRecoveryPlan,
  repair: Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const identity = planIdentity(plan);
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

/** A native recovery that has reset the native committed root to its target:
 * a correction rewind or a signed-header recovery in state `applied`. */
export type AppliedNativeRecovery = Readonly<{
  recoveryId: string;
  kind: "correction_rewind" | "displacement_compensation" | HistoryRecoveryKind;
  targetRoot: string;
}>;

export type AppliedRecoveryRow = { recovery_id: Buffer; intent: string };
