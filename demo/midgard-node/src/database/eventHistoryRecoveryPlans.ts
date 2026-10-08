/**
 * The native recovery plans earlier node versions retained in
 * `event_history_recovery_plans`. No service prepares one now: the
 * landed-block rebase (`landed-blocks/rebase.ts`) moves native MPF and the
 * working ledger without a plan. What remains is read:
 *
 * - the plan domains, by which `landed-blocks/retired-plans.ts` discards a
 *   prepared plan an earlier version left behind;
 * - the newest applied plan, which `mpf-audit` reads as a possible native
 *   committed point (`retrieveAppliedRecoveryAfterJournal`).
 */
import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

const table = "event_history_recovery_plans";

export const SIGNED_HEADER_RECOVERY_DOMAIN =
  "midgard-history-recovery-intent-v1";
export const SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN =
  "midgard-history-signed-intent-release-intent-v1";
export const DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN =
  "midgard-history-displaced-block-revival-intent-v1";
export const DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN =
  "midgard-history-displacement-compensation-intent-v1";
export const CORRECTION_REWIND_RECOVERY_DOMAIN =
  "midgard-history-correction-rewind-intent-v1";

/** Each plan domain's kind, as logs and the audit name it. */
export const RECOVERY_PLAN_KINDS: ReadonlyMap<string, string> = new Map([
  [SIGNED_HEADER_RECOVERY_DOMAIN, "signed_header"],
  [SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN, "signed_intent_release"],
  [DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN, "displaced_block_revival"],
  [DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN, "displacement_compensation"],
  [CORRECTION_REWIND_RECOVERY_DOMAIN, "correction_rewind"],
]);

/** An applied native recovery: it reset the native committed root to its target. */
export type AppliedNativeRecovery = Readonly<{
  recoveryId: string;
  kind: string;
  targetRoot: string;
}>;

const isHash = (value: unknown): value is string =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);

/**
 * The newest applied native recovery whose application is later than the
 * creation of `journalHeaderHash`'s journal (the newest overall when no
 * journal is given).
 *
 * Applying a recovery reset the native root to the recovery's target root,
 * which can be a foreign block's post-state; only this node's own commits
 * otherwise advanced it. Whichever of the two is later fixes the native
 * committed point. A journal abandoned and later revived (it carries a
 * correction digest) is ordered by its own last update instead. An applied
 * plan of an unknown domain, without a target root, or (a displacement
 * compensation) whose identity is not the digest of its intent fails closed.
 */
export const retrieveAppliedRecoveryAfterJournal = (
  journalHeaderHash: Buffer | undefined,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* journalHeaderHash === undefined
      ? sql<{ recovery_id: Buffer; intent: string }>`
          SELECT recovery_id, intent FROM event_history_recovery_plans
          WHERE state = 'applied'
          ORDER BY updated_at DESC, recovery_id DESC LIMIT 1`
      : sql<{ recovery_id: Buffer; intent: string }>`
          SELECT recovery_id, intent FROM event_history_recovery_plans
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
    const domain = decoded?.domain;
    const kind =
      typeof domain === "string" ? RECOVERY_PLAN_KINDS.get(domain) : undefined;
    const targetRoot = decoded?.targetRoot;
    if (
      kind === undefined ||
      !isHash(targetRoot) ||
      (domain === DISPLACEMENT_COMPENSATION_RECOVERY_DOMAIN &&
        createHash("sha256")
          .update(eventHistoryCanonicalJson(decoded))
          .digest("hex") !== recoveryId)
    )
      return yield* Effect.fail(undecodable);
    return Option.some<AppliedNativeRecovery>({ recoveryId, kind, targetRoot });
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read applied native recovery"),
  );
