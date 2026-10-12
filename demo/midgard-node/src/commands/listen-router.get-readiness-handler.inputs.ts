import { SqlClient } from "@effect/sql/SqlClient";
import { Effect } from "effect";

import {
  DaPayloadPublicationsDB,
  MempoolDB,
  MutationJobsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import {
  PENDING_FINALIZATION_AGE_BOUND_MS,
  retrieveActiveJournalAges,
} from "../database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  L1_OWN_BLOCK_FORCED_ORDER_ORPHANED,
  readOwnLandedForcedOrphans,
} from "../forced-orders/own-landed-orphans.js";

/**
 * Every database read `/readyz` makes, run only after the connectivity probe
 * passed. A read failing here means the database is not serving, which
 * readiness reports as `db_unhealthy`, never as a server error.
 */
export const readReadinessDatabaseState = (
  deploymentIdentityDigest: Buffer | undefined,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient;
    yield* sql`SELECT 1 AS ok`;
    return {
      durableAdmissionBacklog: yield* TxAdmissionsDB.countBacklog,
      durableAdmissionOldestAgeMs: yield* TxAdmissionsDB.oldestQueuedAgeMs,
      unfinishedMutationJobs: yield* MutationJobsDB.countUnfinished,
      daPublicationConflicts: yield* DaPayloadPublicationsDB.conflictCount(
        15,
        deploymentIdentityDigest,
      ),
      mempoolTxCount: yield* MempoolDB.retrieveTxCount,
      leaseInspection: yield* StateQueueMutationLeasesDB.inspect({
        recentLimit: 3,
      }),
      journalAges: yield* retrieveActiveJournalAges,
      ownLandedForcedOrphans: (yield* readOwnLandedForcedOrphans).length,
    };
  });

/** The degradation a landed own block with a forced order that left the
 * chain reports: the node follows the block, so it stays ready. */
export const ownLandedForcedOrphanDetail = (
  count: number,
): string | undefined =>
  count > 0
    ? `${L1_OWN_BLOCK_FORCED_ORDER_ORPHANED}:${count.toString()}`
    : undefined;

/** The first line of a database failure, for the public body. */
export const readinessDatabaseError = (error: unknown): string => {
  const message =
    error instanceof Error
      ? error.message
      : typeof error === "object" && error !== null && "message" in error
        ? String((error as { readonly message: unknown }).message)
        : String(error);
  return (message.split("\n", 1)[0] ?? "").slice(0, 200);
};

/** How long a provider may keep failing, after its last success, before
 * readiness goes unready on it. Until then the failure is a detail: requests
 * reach L1 through the shared provider retry, which outlasts a transient. */
export const READINESS_L1_PROVIDER_UNHEALTHY_AFTER_MS = 5 * 60_000;

/** The provider-failure bound readiness applies: never shorter than the
 * ledger-tip staleness bound (`L1_NODE_BEHIND_MAX_MS`), so a tip gap that
 * bound still calls honest stays a detail. */
export const readinessL1ProviderUnhealthyAfterMs = (
  nodeBehindMaxMs: number | undefined,
): number =>
  Math.max(
    READINESS_L1_PROVIDER_UNHEALTHY_AFTER_MS,
    nodeBehindMaxMs !== undefined && Number.isFinite(nodeBehindMaxMs)
      ? nodeBehindMaxMs
      : 0,
  );

/** The local node transport is not ready; the suffix is its unready reason
 * (`node_unreachable`, `sidecar_unavailable`, ...). */
export const L1_TRANSPORT_UNREADY = "l1_transport_unready";

/** The provider's readiness reason, or the detail a still-recent failure
 * reports instead. Both the latest success and the latest exact (HubOracle)
 * success must lie within the bound for a failure to stay a detail. */
export const l1ProviderReadiness = ({
  healthy,
  lastSuccessAtMs,
  lastExactSuccessAtMs,
  nowMs,
  unhealthyAfterMs = READINESS_L1_PROVIDER_UNHEALTHY_AFTER_MS,
}: {
  readonly healthy: boolean;
  readonly lastSuccessAtMs: number;
  readonly lastExactSuccessAtMs: number;
  readonly nowMs: number;
  readonly unhealthyAfterMs?: number;
}): { readonly reason?: string; readonly detail?: string } => {
  if (healthy) return {};
  if (lastSuccessAtMs <= 0 || lastExactSuccessAtMs <= 0)
    return { reason: "provider_query_unhealthy:l1-provider" };
  const failingForMs = Math.max(
    0,
    nowMs - Math.min(lastSuccessAtMs, lastExactSuccessAtMs),
  );
  return failingForMs > unhealthyAfterMs
    ? { reason: "provider_query_unhealthy:l1-provider" }
    : {
        detail: `provider_query_degraded:l1-provider:${failingForMs.toString()}`,
      };
};

/** A degradation detail once the oldest unfinished journal outlives the
 * bound; an honest commit finalizes, or its signed intent is replaced, well
 * within it. Admission does not wait on it, so it never makes the node
 * unready. */
export const pendingFinalizationAgeDetail = (
  pendingFinalizationAgeMs: number | null,
  boundMs: number = PENDING_FINALIZATION_AGE_BOUND_MS,
): string | undefined =>
  pendingFinalizationAgeMs !== null && pendingFinalizationAgeMs > boundMs
    ? `pending_finalization_age:${pendingFinalizationAgeMs.toString()}:${boundMs.toString()}`
    : undefined;
