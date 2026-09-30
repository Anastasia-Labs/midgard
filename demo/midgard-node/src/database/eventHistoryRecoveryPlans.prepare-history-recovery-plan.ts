import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { NativeMpfCanonicalRootRecovery } from "../services/mpf-native-owner/protocol.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import type { Checkpoint } from "./eventHistoryJournal.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const table = "event_history_recovery_plans";

export const fail = (message: string) =>
  Effect.fail(new DatabaseError({ table, message, cause: undefined }));

export const digest = (value: string) =>
  createHash("sha256").update(value).digest("hex");

export const isHash = (value: string) => /^[0-9a-f]{64}$/u.test(value);

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
  /** The service whose operation this is: a signed-header recovery or a
   * signed-intent release. Each resumes only its own retained plan. */
  domain: HistoryRecoveryDomain;
  recoveryId: string;
  intent: HistoryRecoveryIntent;
  evidenceDigest: string;
  checkpointRevision: string;
  state: "prepared" | "applied";
  native: NativeMpfCanonicalRootRecovery;
}>;

export const SIGNED_HEADER_RECOVERY_DOMAIN =
  "midgard-history-recovery-intent-v1";

/** The expired signed-intent release (replacement or revival of a missed
 * signed commit). It shares the signed-header intent shape but is a different
 * operation, so it never shares an identity with a signed-header recovery of
 * the same header. */
export const SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN =
  "midgard-history-signed-intent-release-intent-v1";

export const CORRECTION_REWIND_RECOVERY_DOMAIN =
  "midgard-history-correction-rewind-intent-v1";

export type HistoryRecoveryDomain =
  | typeof SIGNED_HEADER_RECOVERY_DOMAIN
  | typeof SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN;

type RecoveryPlanDomain =
  | HistoryRecoveryDomain
  | typeof CORRECTION_REWIND_RECOVERY_DOMAIN;

const HISTORY_RECOVERY_KIND = {
  [SIGNED_HEADER_RECOVERY_DOMAIN]: "signed_header",
  [SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN]: "signed_intent_release",
} as const;

export type HistoryRecoveryKind =
  (typeof HISTORY_RECOVERY_KIND)[HistoryRecoveryDomain];

export const historyRecoveryKind = (domain: unknown) =>
  domain === SIGNED_HEADER_RECOVERY_DOMAIN ||
  domain === SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN
    ? HISTORY_RECOVERY_KIND[domain]
    : undefined;

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

export const isHeaderHash = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{56}$/u.test(value);

export const freezeRewindIntent = (
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

export const validRewindIntent = (intent: CorrectionRewindIntent) =>
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

export const planIdentity = (plan: DependentRecoveryPlan) =>
  "kind" in plan
    ? eventHistoryCanonicalJson({
        domain: CORRECTION_REWIND_RECOVERY_DOMAIN,
        ...plan.intent,
      })
    : eventHistoryCanonicalJson({
        domain: plan.domain,
        ...plan.intent,
      });

export const lockCheckpoint = (checkpoint: Checkpoint) =>
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
export const persistRecoveryPlan = (
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
  domain: HistoryRecoveryDomain,
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
      !isHash(evidenceDigest) ||
      historyRecoveryKind(domain) === undefined
    ) {
      yield* lockCheckpoint(checkpoint);
      return yield* fail("Invalid recovery plan identity");
    }
    const copied = Object.freeze({ ...intent });
    const { recoveryId, state } = yield* persistRecoveryPlan(
      checkpoint,
      domain,
      copied,
      evidenceDigest,
      copied.headerHash,
    );
    return Object.freeze({
      domain,
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
