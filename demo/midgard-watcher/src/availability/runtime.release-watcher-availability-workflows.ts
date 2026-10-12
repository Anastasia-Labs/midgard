import { randomUUID } from "node:crypto";

import {
  type AvailabilityOperationJournal,
  type AvailabilityOperationRecord,
} from "@al-ft/midgard-core/availability-operation-journal";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import {
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherReleasedHeaderProof,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.exact-record.js";
import { type WatcherDaBondPoolObservation } from "./pool-observation.js";

/** Queued headers merged or removed on L1 at release finality, by hash. */
export type WatcherReleasedHeadersReader = (
  observation: WatcherAuthenticatedStateQueueObservation,
) => Promise<ReadonlyMap<string, WatcherReleasedHeaderProof>>;

/** True when `header` is unattested or its DA is published. */
export const availabilityUndecidable = ({
  daAvailability,
}: WatcherAuthenticatedStateQueueObservation["finalizedHeaders"][number]): boolean =>
  daAvailability === "Unattested" || "Published" in daAvailability;

/**
 * Deletes from `pending` every header `read` proves merged or removed: it can
 * no longer be challenged and its DA may be pruned, so it is never pending,
 * snapshotted, alerted or actuated.
 */
export const dropReleasedHeaders = async (
  pending: Set<string>,
  observation: WatcherAuthenticatedStateQueueObservation,
  read: WatcherReleasedHeadersReader | undefined,
): Promise<void> => {
  for (const headerHash of ((await read?.(observation)) ?? new Map()).keys())
    pending.delete(headerHash);
};

/**
 * A due Open (or the preparation of its challenger coin) the wallet could not
 * fund this reconciliation. Reported, never blocking (spec #685 E5): the
 * watcher still takes every other admitted step, such as a live challenge's
 * settle, close or Timeout. Amounts are decimal lovelace strings, since the
 * status is served as JSON.
 */
export type WatcherAvailabilityOpenRefusal = Readonly<{
  headerHash: string;
  reason: "insufficient-availability-capital";
  requiredLovelace: string;
  availableLovelace: string;
  detail: string;
}>;

/**
 * A Timeout not built this reconciliation because the DA bond pool could not
 * be read at the source tip, or no authentic pool was found there. The
 * Timeout never falls back to the finalized snapshot's pool; it is retried on
 * the next reconciliation. Reported, never blocking: the watcher still takes
 * the next admitted step (spec #685 E5, P9(6)).
 */
export type WatcherAvailabilityTimeoutDeferral = Readonly<{
  headerHash: string;
  reason: "tip-pool-unavailable";
  detail: string;
}>;

/**
 * A step the availability journal's workflow rule refused this
 * reconciliation, with the journal's reason. Reported, never blocking. The
 * refusal of every step while the actor has a live workflow in another
 * deployment appears here, naming that deployment and header, as does the
 * expected refusal of a preparation for a header whose own Open landed.
 */
export type WatcherAvailabilityWorkflowRefusal = Readonly<{
  headerHash: string;
  action: string;
  detail: string;
}>;

/**
 * A workflow row released this reconciliation (P20): this actor's challenge
 * of `headerHash` in `deployment` ended in a terminal step someone else
 * landed, a finalized, verified transaction `txHash` that burned the header's
 * queue node or closed its challenge. Reported, never blocking.
 */
export type WatcherAvailabilityWorkflowRelease = Readonly<{
  deployment: string;
  headerHash: string;
  reason: SDK.DaAvailabilityWorkflowRelease["reason"];
  txHash: string;
  spendPoint: string;
}>;

/**
 * A workflow row whose release check failed this reconciliation: a reader
 * error, a node chain past the hop cap, or the journal refusing the release.
 * The row is kept and checked again on the next reconciliation; the tick
 * goes on (P9(6)).
 */
export type WatcherAvailabilityWorkflowReleaseDeferral = Readonly<{
  deployment: string;
  headerHash: string;
  detail: string;
}>;

export type WatcherAvailabilityStatus = Readonly<{
  phase: "ready" | "waiting" | "blocked" | "closed";
  pendingHeaders: readonly string[];
  action?: string;
  txHash?: string;
  detail?: string;
  /**
   * The pooled DA bond from the last successful pool read, taken on every
   * reconciliation whether or not a header is pending, and kept when a later
   * reconciliation fails. It never changes `phase` (spec #685 E5).
   */
  pool?: WatcherDaBondPoolObservation;
  /**
   * Why the last reconciliation's pool read failed, absent once a read
   * succeeds. Reported only: a failed pool read never changes `phase` or
   * blocks an action (spec #685 E5).
   */
  poolReadFailure?: string;
  /** Withheld Attested headers whose Open deadline passed unchallenged. */
  missedOpenDeadlines?: readonly string[];
  /** Due Opens refused for lack of wallet capital in the last reconciliation. */
  openRefused?: readonly WatcherAvailabilityOpenRefusal[];
  /** Timeouts deferred for want of an authentic pool at the tip. */
  timeoutsDeferred?: readonly WatcherAvailabilityTimeoutDeferral[];
  /** Steps the journal's workflow rule refused in the last reconciliation. */
  workflowRefused?: readonly WatcherAvailabilityWorkflowRefusal[];
  /** Workflow rows released on someone else's terminal step (P20). */
  workflowReleased?: readonly WatcherAvailabilityWorkflowRelease[];
  /** Workflow rows whose release check failed and were kept. */
  workflowReleaseDeferred?: readonly WatcherAvailabilityWorkflowReleaseDeferral[];
}>;

/**
 * The deployment profile's Open window (`timing.da_challenge_window_ms`),
 * compiled into the availability validator and bound by the signed release.
 */
export const DA_CHALLENGE_WINDOW_MS = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
);

export type WatcherAvailabilityStatusTransition = Readonly<{
  status: WatcherAvailabilityStatus;
  observationDigest: string;
  nativePoint: WatcherAuthenticatedStateQueueObservation["nativePoint"];
  elapsedMs: number;
  observedAt: string;
}>;

export type WatcherAvailabilityRuntime = Readonly<{
  reconcile(
    observation: WatcherAuthenticatedStateQueueObservation,
    actuate: boolean,
  ): Promise<void>;
  /** Headers in `merged` are skipped: L1 already merged them. */
  pendingAvailabilityHeaders(
    observation: WatcherAuthenticatedStateQueueObservation,
    merged?: ReadonlySet<string>,
  ): Promise<ReadonlySet<string>>;
  invalidateForRollback(point?: WatcherNativeChainSyncPoint): void;
  invalidateForShutdown(): void;
  status(): WatcherAvailabilityStatus;
  /** True while a header still needs attestation or the last attempt was blocked. */
  close(): Promise<void>;
}>;

export const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`Availability action requires ${label}`);
  return value;
};

export const plainAda = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).length === 1 &&
  (utxo.assets.lovelace ?? 0n) > 0n;

/** The ledger's collateral input cap; the SDK builders select within it. */
export const WATCHER_AVAILABILITY_MAX_COLLATERAL_INPUTS = 3;

/**
 * The wallet cannot fund an availability step: too little plain-ADA
 * collateral, working capital, or no single coin large enough to prepare the
 * exact challenger coin.
 */
export class WatcherAvailabilityCapitalShortfall extends Error {
  readonly requiredLovelace: bigint;
  readonly availableLovelace: bigint;
  constructor(
    message: string,
    requiredLovelace: bigint,
    availableLovelace: bigint,
  ) {
    super(message);
    this.name = "WatcherAvailabilityCapitalShortfall";
    this.requiredLovelace = requiredLovelace;
    this.availableLovelace = availableLovelace;
  }
}

/**
 * The DA bond pool could not be read at the source tip for a Timeout, or no
 * authentic pool was found there. The Timeout is skipped this reconciliation
 * and reported (P13(2)); it never falls back to the finalized snapshot's pool.
 */
export class WatcherAvailabilityTimeoutPoolUnavailable extends Error {
  constructor(message: string) {
    super(message);
    this.name = "WatcherAvailabilityTimeoutPoolUnavailable";
  }
}

/**
 * Why the availability journal's workflow rule refuses a step for this
 * header now, or undefined when it admits it. The journal keeps one challenge
 * workflow per header: it refuses every step while the actor has a live
 * workflow in another deployment, and a preparation for a header whose own
 * Open landed. A refused lease (another owner holds the actor) propagates.
 */
export const watcherAvailabilityWorkflowRefusal = (
  journal: Pick<
    AvailabilityOperationJournal,
    "acquire" | "assertWorkflow" | "assertLease" | "release"
  >,
  step: Readonly<{
    actor: string;
    deploymentIdentity: string;
    headerHash: string;
    action: string;
    nowMs: number;
  }>,
): string | undefined => {
  const lease = journal.acquire(step.actor, randomUUID(), step.nowMs, 60_000);
  try {
    journal.assertWorkflow(
      lease,
      step.deploymentIdentity,
      step.headerHash,
      step.action,
      step.nowMs,
    );
    return undefined;
  } catch (cause) {
    journal.assertLease(lease, step.nowMs);
    return cause instanceof Error ? cause.message : String(cause);
  } finally {
    journal.release(lease);
  }
};

/**
 * Releases every workflow row of `actor`, in any deployment, whose challenge
 * ended in a terminal step someone else landed (P20). Each row's evidence is
 * walked from the actor's own confirmed Open on L1 (`findRelease`), and the
 * journal re-checks its own facts before retiring the row. A failed check
 * keeps the row, is reported, and never stops the other rows or the tick.
 *
 * Confirmation depth is not finality, so a released row whose terminal is
 * not yet `automaticRecoveryMaxDepth` deep is re-walked first on every pass:
 * found again, its evidence is refreshed; found no longer, the row is live
 * again and reported as deferred.
 */
export const releaseWatcherAvailabilityWorkflows = async (
  journal: Pick<
    AvailabilityOperationJournal,
    | "acquire"
    | "release"
    | "workflows"
    | "releaseWorkflow"
    | "unsettledReleases"
    | "reviveWorkflow"
  >,
  actor: string,
  findRelease: (
    openIntent: AvailabilityOperationRecord,
    headerHash: string,
  ) => Promise<SDK.DaAvailabilityWorkflowRelease | undefined>,
  assertCurrent: () => void,
): Promise<
  Readonly<{
    released: readonly WatcherAvailabilityWorkflowRelease[];
    deferred: readonly WatcherAvailabilityWorkflowReleaseDeferral[];
  }>
> => {
  const released: WatcherAvailabilityWorkflowRelease[] = [];
  const deferred: WatcherAvailabilityWorkflowReleaseDeferral[] = [];
  const recoveryDepth =
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
  // A revoked generation aborts the pass before any journal write.
  const leased = (
    mutate: (lease: ReturnType<typeof journal.acquire>, nowMs: number) => void,
  ) => {
    const nowMs = Date.now();
    const lease = journal.acquire(actor, randomUUID(), nowMs, 60_000);
    try {
      mutate(lease, nowMs);
    } finally {
      journal.release(lease);
    }
  };
  for (const row of journal.unsettledReleases(actor)) {
    const where = {
      deployment: row.deploymentIdentity,
      headerHash: row.headerHash,
    };
    const defer = (cause: unknown) =>
      deferred.push({
        ...where,
        detail: cause instanceof Error ? cause.message : String(cause),
      });
    let release: SDK.DaAvailabilityWorkflowRelease | undefined;
    try {
      release = await findRelease(row.open, row.headerHash);
    } catch (cause) {
      defer(cause);
      continue;
    }
    assertCurrent();
    try {
      leased((lease, nowMs) =>
        release === undefined
          ? journal.reviveWorkflow(
              lease,
              where.deployment,
              where.headerHash,
              nowMs,
            )
          : journal.releaseWorkflow(
              lease,
              where.deployment,
              where.headerHash,
              { openIntentId: row.open.intent.id, ...release, recoveryDepth },
              nowMs,
            ),
      );
    } catch (cause) {
      defer(cause);
      continue;
    }
    if (release === undefined)
      defer(
        `The terminal transaction ${row.release.txHash} that released this workflow is no longer canonical; the workflow is live again`,
      );
  }
  for (const row of journal.workflows(actor)) {
    const where = {
      deployment: row.deploymentIdentity,
      headerHash: row.headerHash,
    };
    const defer = (cause: unknown) =>
      deferred.push({
        ...where,
        detail: cause instanceof Error ? cause.message : String(cause),
      });
    let found:
      | Readonly<{
          openIntentId: string;
          release: SDK.DaAvailabilityWorkflowRelease;
        }>
      | undefined;
    try {
      for (const open of row.confirmedOpens) {
        const release = await findRelease(open, row.headerHash);
        if (release !== undefined) {
          found = { openIntentId: open.intent.id, release };
          break;
        }
      }
    } catch (cause) {
      defer(cause);
      continue;
    }
    if (found === undefined) continue;
    const { openIntentId, release } = found;
    assertCurrent();
    try {
      leased((lease, nowMs) =>
        journal.releaseWorkflow(
          lease,
          row.deploymentIdentity,
          row.headerHash,
          { openIntentId, ...release, recoveryDepth },
          nowMs,
        ),
      );
    } catch (cause) {
      defer(cause);
      continue;
    }
    released.push({
      ...where,
      reason: found.release.reason,
      txHash: found.release.txHash,
      spendPoint: found.release.spendPoint,
    });
  }
  return { released, deferred };
};
