import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import {
  DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN,
  parseDisplacementCompensationIntent,
} from "./eventHistoryRecoveryPlans.displacement-compensation.js";
import {
  CORRECTION_REWIND_RECOVERY_DOMAIN,
  type CorrectionRewindIntent,
  digest,
  fail,
  freezeRewindIntent,
  historyRecoveryKind,
  isHash,
  table,
  validRewindIntent,
} from "./eventHistoryRecoveryPlans.prepare-history-recovery-plan.js";
import {
  type AppliedNativeRecovery,
  type AppliedRecoveryRow,
} from "./eventHistoryRecoveryPlans.prepare-retained-native-history-recovery-plan.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

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
        : decoded?.domain === DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN
          ? ("displacement_compensation" as const)
          : historyRecoveryKind(decoded?.domain);
    if (kind === "displacement_compensation") {
      const { domain: _domain, ...fields } = decoded!;
      if (
        parseDisplacementCompensationIntent(fields) === undefined ||
        digest(eventHistoryCanonicalJson(decoded)) !== recoveryId
      )
        return yield* Effect.fail(undecodable);
    }
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
      if (decoded?.domain === DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN) {
        const { domain: _domain, ...fields } = decoded;
        const compensation = parseDisplacementCompensationIntent(fields);
        if (compensation === undefined)
          return yield* fail(
            "Malformed retained displacement compensation identity",
          );
        for (const member of compensation.suffixMembers)
          if (member.kind === "removed")
            headers.set(member.headerHash, member.transitionDigest);
        continue;
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
