import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

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

const SIGNED_HEADER_RECOVERY_DOMAIN = "midgard-history-recovery-intent-v1";
export const CORRECTION_REWIND_RECOVERY_DOMAIN =
  "midgard-history-correction-rewind-intent-v1";
type RecoveryPlanDomain =
  | typeof SIGNED_HEADER_RECOVERY_DOMAIN
  | typeof CORRECTION_REWIND_RECOVERY_DOMAIN;

/** One block the rewind resolves, in the order its payloads are reincluded
 * (earliest first). A `removed` member's header was removed from the state
 * queue by the admitted correction `transitionDigest`. An `unlanded` member is
 * a locally journaled descendant whose commit never reached L1 and never can:
 * the correction `transitionDigest` consumed the exact queue node its commit
 * spends. Every `removed` member precedes every `unlanded` one. */
export type CorrectionRewindMemberKind = "removed" | "unlanded";
export type CorrectionRewindMember = Readonly<{
  headerHash: string;
  transitionDigest: string;
  kind: CorrectionRewindMemberKind;
}>;
/** Immutable identity of a native rewind to the replay base of the earliest
 * removed block, with every removed block's payload reinclusion. The member
 * list is part of the identity so an interrupted rewind resumes exactly the
 * blocks it restored the native root for. */
export type CorrectionRewindIntent = Readonly<{
  bindingDigest: string;
  manifestId: string;
  /** The earliest removed block; its replay base is the target root. */
  headerHash: string;
  members: readonly CorrectionRewindMember[];
  expectedRoot: string;
  targetRoot: string;
  /** Digest of the removed chain's immutable journal identities. */
  journalDigest: string;
}>;
export type CorrectionRewindRecoveryPlan = Readonly<{
  kind: "correction_rewind";
  recoveryId: string;
  intent: CorrectionRewindIntent;
  evidenceDigest: string;
  checkpointRevision: string;
  state: "prepared" | "applied";
  native: NativeMpfCanonicalRootRecovery;
}>;
export type DependentRecoveryPlan =
  | HistoryRecoveryPlan
  | CorrectionRewindRecoveryPlan;

const isHeaderHash = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{56}$/u.test(value);
const freezeRewindIntent = (
  intent: CorrectionRewindIntent,
): CorrectionRewindIntent =>
  Object.freeze({
    bindingDigest: intent.bindingDigest,
    manifestId: intent.manifestId,
    headerHash: intent.headerHash,
    members: Object.freeze(
      intent.members.map((member) =>
        Object.freeze({
          headerHash: member.headerHash,
          transitionDigest: member.transitionDigest,
          kind: member.kind,
        }),
      ),
    ),
    expectedRoot: intent.expectedRoot,
    targetRoot: intent.targetRoot,
    journalDigest: intent.journalDigest,
  });
const validRewindIntent = (intent: CorrectionRewindIntent) =>
  isHash(intent.bindingDigest) &&
  isHash(intent.manifestId) &&
  isHeaderHash(intent.headerHash) &&
  Array.isArray(intent.members) &&
  intent.members.length > 0 &&
  intent.members[0]!.headerHash === intent.headerHash &&
  intent.members[0]!.kind === "removed" &&
  new Set(intent.members.map(({ headerHash }) => headerHash)).size ===
    intent.members.length &&
  intent.members.every(
    (member, index) =>
      isHeaderHash(member.headerHash) &&
      isHash(member.transitionDigest) &&
      (member.kind === "removed" || member.kind === "unlanded") &&
      (member.kind === "unlanded" ||
        intent.members[index - 1]?.kind !== "unlanded"),
  ) &&
  isHash(intent.expectedRoot) &&
  isHash(intent.targetRoot) &&
  isHash(intent.journalDigest);
const planIdentity = (plan: DependentRecoveryPlan) =>
  "kind" in plan
    ? eventHistoryCanonicalJson({
        domain: CORRECTION_REWIND_RECOVERY_DOMAIN,
        ...plan.intent,
      })
    : eventHistoryCanonicalJson({
        domain: SIGNED_HEADER_RECOVERY_DOMAIN,
        ...plan.intent,
      });

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

/** Persists one immutable operation identity under its own domain, BEFORE
 * any native mutation, and returns its retained state. A different checkpoint
 * can refresh evidence for the identical operation, never substitute another
 * header, root or incarnation under the same ID. */
const persistRecoveryPlan = (
  checkpoint: Checkpoint,
  domain: RecoveryPlanDomain,
  intent: Readonly<{ bindingDigest: string }>,
  evidenceDigest: string,
  headerHash: string,
) =>
  Effect.gen(function* () {
    const token = yield* lockCheckpoint(checkpoint);
    // Copy primitives before the first asynchronous boundary involving caller
    // data; the persisted canonical document is also the returned immutable view.
    const payload = eventHistoryCanonicalJson({ domain, ...intent });
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
        ${Buffer.from(checkpoint.manifestId, "hex")}, ${Buffer.from(headerHash, "hex")}, ${payload},
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
    return { recoveryId, state: rows[0]!.state };
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
    if (
      intent.bindingDigest !== checkpoint.bindingDigest ||
      intent.manifestId !== checkpoint.manifestId ||
      !/^[0-9a-f]{56}$/u.test(intent.headerHash) ||
      !Object.entries(intent).every(
        ([key, value]) => key === "headerHash" || isHash(value),
      ) ||
      !isHash(evidenceDigest)
    ) {
      yield* lockCheckpoint(checkpoint);
      return yield* fail("Invalid recovery plan identity");
    }
    const copied = Object.freeze({ ...intent });
    const { recoveryId, state } = yield* persistRecoveryPlan(
      checkpoint,
      SIGNED_HEADER_RECOVERY_DOMAIN,
      copied,
      evidenceDigest,
      copied.headerHash,
    );
    return Object.freeze({
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
          domain: SIGNED_HEADER_RECOVERY_DOMAIN,
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

/** The single prepared native recovery of this binding, if any, decoded by
 * domain. A signed-header plan is reported by kind and header: the service
 * that prepared it for that header resumes it. An undecodable retained
 * identity fails closed. */
export const retainedPreparedRecoveryPlan = (bindingDigest: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      intent: string;
    }>`SELECT intent FROM event_history_recovery_plans
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
    if (decoded?.domain === SIGNED_HEADER_RECOVERY_DOMAIN) {
      if (!isHeaderHash(decoded.headerHash))
        return yield* fail(
          "Malformed retained signed-header recovery identity",
        );
      return {
        kind: "signed_header" as const,
        headerHash: decoded.headerHash,
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
  kind: "correction_rewind" | "signed_header";
  targetRoot: string;
}>;

type AppliedRecoveryRow = { recovery_id: Buffer; intent: string };

/**
 * The newest applied native recovery whose application is later than the
 * creation of `journalHeaderHash`'s journal (the newest overall when no
 * journal is given).
 *
 * Only this node's own commits advance the native root, and applying a
 * recovery resets it to the recovery's target root, which can be a foreign
 * block's post-state. Whichever of the two is later fixes the native committed
 * point. Recovery refuses to run while a journal is active, so a journal
 * created before a recovery was already finalized (or abandoned by it) when
 * the plan applied. The exception is a journal abandoned and later revived
 * (it carries a correction digest): a replaced block whose signed commit won
 * its slot is revived after the replacement's plan and advances the native
 * root when it finalizes, so its own last update orders it instead. An
 * applied plan that cannot be decoded fails closed.
 */
export const retrieveAppliedRecoveryAfterJournal = (
  journalHeaderHash: Buffer | undefined,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* journalHeaderHash === undefined
      ? sql<AppliedRecoveryRow>`SELECT recovery_id, intent
          FROM event_history_recovery_plans
          WHERE state = 'applied'
          ORDER BY updated_at DESC, recovery_id DESC LIMIT 1`
      : sql<AppliedRecoveryRow>`SELECT recovery_id, intent
          FROM event_history_recovery_plans
          WHERE state = 'applied'
            AND updated_at > (SELECT CASE
                WHEN correction_transition_digest IS NULL THEN created_at
                ELSE updated_at END
              FROM pending_block_finalizations
              WHERE header_hash = ${journalHeaderHash})
          ORDER BY updated_at DESC, recovery_id DESC LIMIT 1`;
    if (rows.length === 0) return Option.none<AppliedNativeRecovery>();
    const recoveryId = rows[0]!.recovery_id.toString("hex");
    const undecodable = new DatabaseError({
      table,
      message: "Applied native recovery has no decodable target root",
      cause: `recovery_id=${recoveryId}`,
    });
    const decoded = yield* Effect.try({
      try: () => JSON.parse(rows[0]!.intent) as Record<string, unknown> | null,
      catch: () => undecodable,
    });
    const kind =
      decoded?.domain === CORRECTION_REWIND_RECOVERY_DOMAIN
        ? ("correction_rewind" as const)
        : decoded?.domain === SIGNED_HEADER_RECOVERY_DOMAIN
          ? ("signed_header" as const)
          : undefined;
    const targetRoot = decoded?.targetRoot;
    if (
      kind === undefined ||
      typeof targetRoot !== "string" ||
      !isHash(targetRoot)
    )
      return yield* Effect.fail(undecodable);
    return Option.some<AppliedNativeRecovery>({ recoveryId, kind, targetRoot });
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read applied native recovery"),
  );

/** Headers removed by every correction rewind this deployment ever prepared or
 * applied. Once a rewind moved the native root off a removed block, that
 * removal must stand: the node has no forward path that re-applies a removed
 * block, so an authenticated view that no longer removes one of these headers
 * is an integrity failure, never a state to reconcile silently. */
export const correctionRewindRemovedHeaders = (manifestId: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ intent: string }>`
      SELECT intent FROM event_history_recovery_plans
      WHERE manifest_id = ${Buffer.from(manifestId, "hex")}
      ORDER BY created_at, recovery_id`;
    const headers = new Map<string, string>();
    for (const { intent } of rows) {
      let decoded: Record<string, unknown>;
      try {
        decoded = JSON.parse(intent) as Record<string, unknown>;
      } catch {
        return yield* fail("Malformed retained native recovery identity");
      }
      if (decoded?.domain !== CORRECTION_REWIND_RECOVERY_DOMAIN) continue;
      const { domain: _domain, ...fields } = decoded;
      const parsed = freezeRewindIntent(
        fields as unknown as CorrectionRewindIntent,
      );
      if (!validRewindIntent(parsed))
        return yield* fail("Malformed retained correction rewind identity");
      for (const member of parsed.members)
        if (member.kind === "removed")
          headers.set(member.headerHash, member.transitionDigest);
    }
    return headers;
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read correction rewind plans"),
  );
