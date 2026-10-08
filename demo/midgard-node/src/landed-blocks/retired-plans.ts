/**
 * Prepared recovery plans of the retired kinds (I3, N4): a signed-header
 * recovery, a signed-intent release, a displaced-block revival, a
 * displacement compensation or a correction rewind that a node prepared
 * before the services that resumed them were deleted. No service resumes
 * one now, so while one is retained the history owner's reconciliation
 * stays pending (`pendingHistoryLedgerDisposition`), for good.
 *
 * The landed-block rebase covers what each one was preparing. A prepared
 * plan committed nothing in SQL (its journal disposition and its applied
 * receipt commit together), so its journals are as they were, and the own
 * journal disposition (`own-journals.ts`) settles them from the landed
 * chain and S6:
 *
 * - a signed-header recovery or a signed-intent release abandoned a signed
 *   journal that cannot land: disposed of once S6 derives its commit dead,
 *   or once its block or its base leaves the landed chain, and followed if
 *   it lands;
 * - a displaced-block revival revived an abandoned block that landed, and
 *   abandoned the applied blocks a rollback displaced: the landed block is
 *   revived from its processed row, and a displaced one is disposed of as
 *   removed, or as built on a base whose successor slot another block took;
 * - a displacement compensation reopened the unlanded signed suffix of such
 *   a chain: disposed of as dead or as built on a base that left;
 * - a correction rewind restored the native root below the blocks a landed
 *   correction removed and reopened their journals and unlanded
 *   descendants: a removed block's journal is disposed of as removed, and a
 *   descendant as built on a base that left (whichever lands wins: it is
 *   revived if it lands after all).
 *
 * Its native restore may have run. So a retained one makes the rebase due:
 * the rebase moves the native MPF to the landed target from wherever the
 * restore left it, and deletes the plan in its SQL transaction. A crash in
 * between keeps the plan, and the next rebase finds the native MPF already
 * at the target.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { RECOVERY_PLAN_KINDS } from "../database/eventHistoryRecoveryPlans.js";
import { sqlErrorToDatabaseError } from "../database/utils/common.js";

const table = "event_history_recovery_plans";

/** Every plan domain is retired: no service prepares or resumes one. */
const RETIRED_KINDS = RECOVERY_PLAN_KINDS;

/** A retained prepared plan of a retired kind. */
export type RetiredPlan = Readonly<{
  recoveryId: string;
  kind: string;
  headerHash: string;
}>;

const domainOf = (intent: string): unknown => {
  try {
    return (JSON.parse(intent) as { domain?: unknown } | null)?.domain;
  } catch {
    return undefined;
  }
};

/**
 * The prepared plans of the retired kinds, under every binding. A plan of
 * a kept kind, an applied receipt and an identity that does not decode are
 * not among them (their readers judge those).
 */
export const retiredPlans = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    recovery_id: Buffer;
    header_hash: Buffer;
    intent: string;
  }>`SELECT recovery_id, header_hash, intent FROM event_history_recovery_plans
    WHERE state = 'prepared' ORDER BY recovery_id`;
  return rows.flatMap((row): RetiredPlan[] => {
    const domain = domainOf(row.intent);
    const kind =
      typeof domain === "string" ? RETIRED_KINDS.get(domain) : undefined;
    return kind === undefined
      ? []
      : [
          {
            recoveryId: row.recovery_id.toString("hex"),
            kind,
            headerHash: row.header_hash.toString("hex"),
          },
        ];
  });
}).pipe(
  sqlErrorToDatabaseError(table, "Failed to read retired recovery plans"),
);

/** Deletes `plans` while still prepared, in the caller's transaction. */
export const discardRetiredPlans = (plans: readonly RetiredPlan[]) =>
  Effect.gen(function* () {
    if (plans.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM event_history_recovery_plans
      WHERE state = 'prepared' AND recovery_id IN ${sql.in(
        plans.map((plan) => Buffer.from(plan.recoveryId, "hex")),
      )}`;
    for (const plan of plans)
      yield* Effect.logWarning(
        `Discarded a prepared ${plan.kind} recovery plan for header ${plan.headerHash}: no service resumes it, and the landed-block rebase settled its journals and the native MPF`,
      );
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to discard retired recovery plans"),
  );
