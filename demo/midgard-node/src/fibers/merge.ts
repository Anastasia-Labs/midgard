import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Data, Effect, Either, Exit, Option, Ref, Schedule } from "effect";

import {
  MempoolDB,
  MutationJobsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  runHistoryProducer,
  UnownedHistoryFixture,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import {
  Database,
  Globals,
  L1ControlPlaneTimeoutError,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlane,
  withL1ControlPlaneWaitTimeout,
} from "../services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
  type StateQueueSnapshot,
} from "../services/state-queue-topology.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  type MergeReadinessStatus,
  mergeSubmitValidityEvidence,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import {
  buildAndSubmitMergeTx,
  type CanonicalMergeCandidateReadiness,
  captureMergeLocalLedgerGate,
  type ConfirmedMergeFinalization,
  fetchCanonicalMergeCandidateReadiness,
  mergeSemanticSkipResult,
} from "../transactions/state-queue/merge-to-confirmed-state.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "../transactions/utils.js";
import {
  checkSlotAwareDueWork,
  clearSlotAwareDueWork,
  listSlotAwareDueWork,
} from "./slot-aware-due-work.js";

/**
 * Background merge flow for confirmed state-queue blocks.
 *
 * The merge fiber switches to the dedicated merge wallet and submits the
 * on-chain merge transaction that folds confirmed queue state into the next
 * durable checkpoint.
 */

export type MergeActionResult =
  | {
      readonly status: "merged";
      readonly postMergeSnapshot: StateQueueSnapshot;
      readonly headerHash: string;
      readonly txHash: string;
      readonly trigger: "threshold" | "manual" | "final_tail_auto_merge";
    }
  | {
      readonly status:
        | Exclude<MergeReadinessStatus, "ready">
        | "skipped_state_queue_lease_busy"
        | "skipped_l1_control_plane_busy";
      readonly reason: string;
      readonly headerHash?: string;
      readonly queueLength?: number;
      readonly minQueueLength?: number;
      readonly readyAfterUnixTime?: number;
      readonly nowUnixTime?: number;
    };

type CurrentMergeDueWorkEvidence = {
  readonly key: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
  readonly headerHash: string;
  readonly validFromSlot: number;
  readonly targetSlot: number;
};

export const SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS = 30_000;

export const withScheduledMergeControlPlaneWait = <A, E, R>({
  globals,
  effect,
  waitTimeoutMs = SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS,
}: {
  readonly globals: Globals;
  readonly effect: Effect.Effect<A, E, R>;
  readonly waitTimeoutMs?: number;
}): Effect.Effect<Option.Option<A>, E | Error, R> =>
  withL1ControlPlaneWaitTimeout(
    globals,
    {
      scope: "state_queue_merge",
      waitTimeoutMs,
      maxHoldMs: 180_000,
    },
    effect,
  );

const registeredMergeDueWorkSkip = (
  currentEvidence: CurrentMergeDueWorkEvidence,
): Effect.Effect<MergeActionResult | undefined, never, Lucid> =>
  Effect.gen(function* () {
    const entries = listSlotAwareDueWork().filter(
      (entry) => entry.kind === "merge_submit_validity",
    );
    if (entries.length === 0) {
      return undefined;
    }
    const lucid = yield* Lucid;
    const slotSnapshot = yield* Effect.either(lucid.submitSlotSnapshot());
    for (const entry of entries) {
      if (entry.key !== currentEvidence.key) {
        clearSlotAwareDueWork(entry.kind, entry.key);
        yield* Effect.logInfo(
          `🔸 Clearing stale merge due work before re-plan (key=${entry.key},current_key=${currentEvidence.key},header=${currentEvidence.headerHash},valid_from_slot=${currentEvidence.validFromSlot.toString()},target_slot=${currentEvidence.targetSlot.toString()}).`,
        );
        continue;
      }
      const decision = checkSlotAwareDueWork({
        kind: entry.kind,
        key: entry.key,
        currentSlot:
          slotSnapshot._tag === "Right"
            ? slotSnapshot.right.currentSlot
            : undefined,
        dependencyKey: currentEvidence.dependencyKey,
        invalidationKey: currentEvidence.invalidationKey,
      });
      switch (decision.status) {
        case "skip": {
          const reason = `merge_due_work_not_due,key=${entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${entry.dueSlot.toString()},wait_ms=${entry.waitMs.toString()}`;
          yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
          return {
            status: "skipped_oldest_block_local_ledger_not_ready",
            reason,
          } satisfies MergeActionResult;
        }
        case "due":
          yield* Effect.logInfo(
            `🔸 Waking merge due work (key=${entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${entry.dueSlot.toString()}).`,
          );
          break;
        case "invalidated":
          yield* Effect.logInfo(
            `🔸 Clearing merge due work before re-plan (key=${entry.key},reason=${decision.reason}).`,
          );
          break;
        case "missing":
          break;
      }
    }
    return undefined;
  });

type SemanticCandidate = Extract<
  CanonicalMergeCandidateReadiness,
  { readonly status: "candidate" }
>;

type SemanticCandidateSkip = Exclude<
  SemanticCandidate["readiness"],
  { readonly status: "ready" }
>;

type MergeCandidateChangedResult = {
  readonly status: "skipped_merge_candidate_changed";
  readonly reason: string;
  readonly headerHash?: string;
  readonly readyAfterUnixTime?: number;
  readonly nowUnixTime?: number;
};

const logSemanticSkip = (
  phase: "before lease" | "after leased recheck",
  readiness: SemanticCandidateSkip,
): Effect.Effect<void> => {
  const message =
    readiness.status === "skipped_oldest_block_unattested"
      ? "oldest block is not DA-attested yet"
      : readiness.status === "skipped_oldest_block_proven_fraud"
        ? "oldest block has completed fraud and requires state correction"
        : "oldest block is not mature yet";
  return Effect.logInfo(
    `🔸 Skipping merge ${phase} because ${message} (${readiness.reason}).`,
  );
};

const mergeActionSemanticSkipResult = (
  readiness: SemanticCandidateSkip,
): MergeActionResult => mergeSemanticSkipResult(readiness) as MergeActionResult;

const mergeValidFromSlot = (
  lucid: LucidEvolution,
  validFromUnixTime: number,
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.try({
    try: () => {
      const slot = Number(lucid.unixTimeToSlot(validFromUnixTime));
      if (!Number.isSafeInteger(slot) || slot < 0) {
        throw new Error(`invalid slot=${slot.toString()}`);
      }
      return slot;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to convert merge pre-lease valid-from time to a slot",
        cause,
      }),
  });

const changedCandidateResult = ({
  preLeaseCandidate,
  leasedCandidate,
}: {
  readonly preLeaseCandidate: SemanticCandidate;
  readonly leasedCandidate: CanonicalMergeCandidateReadiness;
}): MergeCandidateChangedResult => {
  const preLeaseIdentity = preLeaseCandidate.readiness.candidateIdentity;
  const leasedIdentity =
    leasedCandidate.status === "candidate"
      ? leasedCandidate.readiness.candidateIdentity
      : leasedCandidate.reason;
  const readiness =
    leasedCandidate.status === "candidate"
      ? leasedCandidate.readiness
      : preLeaseCandidate.readiness;
  return {
    status: "skipped_merge_candidate_changed",
    reason: `preflight_candidate=${preLeaseIdentity},leased_candidate=${leasedIdentity}`,
    headerHash: readiness.headerHash,
    readyAfterUnixTime: readiness.readyAfterUnixTime,
    nowUnixTime: readiness.nowUnixTime,
  };
};

/**
 * Runs one merge attempt, optionally bypassing the queue-length guard for
 * explicit recovery/administrative flows.
 */
type MergeTrigger = Extract<
  MergeActionResult,
  { readonly status: "merged" }
>["trigger"];

/** A merge confirmed on L1 during this attempt, with its local finalization's
 * exit, recorded even if the attempt was interrupted afterwards. */
type ConfirmedMerge = ConfirmedMergeFinalization & {
  readonly trigger: MergeTrigger;
};

const mergeActionWithL1ControlPlaneHeld = (
  force: boolean,
  expectedHeaderHash: string | undefined,
  confirmed: Ref.Ref<Option.Option<ConfirmedMerge>>,
): Effect.Effect<
  MergeActionResult,
  | SDK.CmlDeserializationError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LinkedListError
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
  | DatabaseError
  | TxConfirmError
  | TxSubmitError
  | TxSignError,
  Lucid | MidgardContracts | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
    const [
      initialUnconfirmedSubmittedBlockTxHash,
      initialLocalFinalizationPending,
      initialResetInProgress,
    ] = yield* Effect.all(
      [
        Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
        Ref.get(globals.LOCAL_FINALIZATION_PENDING),
        Ref.get(globals.RESET_IN_PROGRESS),
      ],
      { concurrency: "unbounded" },
    );
    if (initialLocalFinalizationPending) {
      const reason = "local_finalization_pending=true";
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_local_finalization_pending",
        reason,
      } satisfies MergeActionResult;
    }
    if (initialUnconfirmedSubmittedBlockTxHash !== "") {
      const reason = `submitted_tx=${initialUnconfirmedSubmittedBlockTxHash}`;
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_unresolved_commitment",
        reason,
      } satisfies MergeActionResult;
    }
    if (initialResetInProgress) {
      const reason = "reset_in_progress=true";
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_reset_in_progress",
        reason,
      } satisfies MergeActionResult;
    }
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const { stateQueue: stateQueueAuthValidator } = contracts;

    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const minQueueLength =
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING;
    const preLeaseCandidate = yield* fetchCanonicalMergeCandidateReadiness(
      lucid.api,
      fetchConfig,
      contracts,
    );
    if (
      expectedHeaderHash !== undefined &&
      (preLeaseCandidate.status !== "candidate" ||
        preLeaseCandidate.readiness.headerHash !== expectedHeaderHash)
    ) {
      // A targeted merge may only fold the block it names. The leased
      // candidate-identity recheck below then binds the merged block to this
      // pre-lease candidate.
      const reason = `expected_header=${expectedHeaderHash},oldest_candidate=${
        preLeaseCandidate.status === "candidate"
          ? preLeaseCandidate.readiness.headerHash
          : preLeaseCandidate.reason
      }`;
      yield* Effect.logInfo(
        `🔸 Skipping targeted merge because the oldest block is not the target (${reason}).`,
      );
      return {
        status: "skipped_merge_candidate_changed",
        reason,
        ...(preLeaseCandidate.status === "candidate"
          ? { headerHash: preLeaseCandidate.readiness.headerHash }
          : {}),
      } satisfies MergeActionResult;
    }
    if (
      preLeaseCandidate.status === "candidate" &&
      preLeaseCandidate.readiness.status !== "ready"
    ) {
      yield* logSemanticSkip("before lease", preLeaseCandidate.readiness);
      return mergeActionSemanticSkipResult(preLeaseCandidate.readiness);
    }
    if (preLeaseCandidate.status === "candidate") {
      const validFromSlot = yield* mergeValidFromSlot(
        lucid.api,
        preLeaseCandidate.readiness.validFromUnixTime,
      );
      const currentMergeDueWorkEvidence = mergeSubmitValidityEvidence({
        headerHash: preLeaseCandidate.readiness.headerHash,
        validFromSlot,
        candidateIdentity: preLeaseCandidate.readiness.candidateIdentity,
      });
      const mergeDueWorkSkip = yield* registeredMergeDueWorkSkip(
        currentMergeDueWorkEvidence,
      );
      if (mergeDueWorkSkip !== undefined) {
        return mergeDueWorkSkip;
      }
      const preLeaseLocalLedgerGate = yield* captureMergeLocalLedgerGate({
        lucid: lucid.api,
        nodeConfig,
        validFromUnixTime: preLeaseCandidate.readiness.validFromUnixTime,
        headerHash: preLeaseCandidate.readiness.headerHash,
        candidateIdentity: preLeaseCandidate.readiness.candidateIdentity,
        submitSlotSnapshot: lucid.submitSlotSnapshot,
      });
      if (preLeaseLocalLedgerGate.status === "retry_later") {
        return {
          status: "skipped_oldest_block_local_ledger_not_ready",
          headerHash: preLeaseCandidate.readiness.headerHash,
          reason: preLeaseLocalLedgerGate.reason,
          readyAfterUnixTime: preLeaseCandidate.readiness.readyAfterUnixTime,
          nowUnixTime: Date.now(),
        } satisfies MergeActionResult;
      }
    }
    const leaseResult = yield* StateQueueMutationLeasesDB.tryWithLease(
      "state_queue_merge",
      (leaseToken) =>
        Effect.gen(function* () {
          const leasedCandidate = yield* fetchCanonicalMergeCandidateReadiness(
            lucid.api,
            fetchConfig,
            contracts,
          );
          if (
            preLeaseCandidate.status === "candidate" &&
            (leasedCandidate.status !== "candidate" ||
              leasedCandidate.readiness.candidateIdentity !==
                preLeaseCandidate.readiness.candidateIdentity)
          ) {
            const changed = changedCandidateResult({
              preLeaseCandidate,
              leasedCandidate,
            });
            yield* Effect.logInfo(
              `🔸 Skipping merge after leased recheck because the oldest block candidate changed (${changed.reason}).`,
            );
            return changed;
          }
          if (
            leasedCandidate.status === "candidate" &&
            leasedCandidate.readiness.status !== "ready"
          ) {
            yield* logSemanticSkip(
              "after leased recheck",
              leasedCandidate.readiness,
            );
            return mergeActionSemanticSkipResult(leasedCandidate.readiness);
          }
          if (leasedCandidate.status === "candidate") {
            const leasedLocalLedgerGate = yield* captureMergeLocalLedgerGate({
              lucid: lucid.api,
              nodeConfig,
              validFromUnixTime: leasedCandidate.readiness.validFromUnixTime,
              leaseToken,
              headerHash: leasedCandidate.readiness.headerHash,
              candidateIdentity: leasedCandidate.readiness.candidateIdentity,
              submitSlotSnapshot: lucid.submitSlotSnapshot,
            });
            if (leasedLocalLedgerGate.status === "retry_later") {
              return {
                status: "skipped_oldest_block_local_ledger_not_ready",
                headerHash: leasedCandidate.readiness.headerHash,
                reason: leasedLocalLedgerGate.reason,
                readyAfterUnixTime:
                  leasedCandidate.readiness.readyAfterUnixTime,
                nowUnixTime: Date.now(),
              } satisfies MergeActionResult;
            }
          }

          const preMergeSnapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            stateQueueAuthValidator,
            "manual_status",
          );
          yield* refreshStateQueueGlobalsFromSnapshot(
            globals,
            preMergeSnapshot,
          );
          const [
            unconfirmedSubmittedBlockTxHash,
            localFinalizationPending,
            resetInProgress,
            durableAdmissionBacklog,
            mempoolTxCount,
            unfinishedMutationJobs,
          ] = yield* Effect.all(
            [
              Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
              Ref.get(globals.LOCAL_FINALIZATION_PENDING),
              Ref.get(globals.RESET_IN_PROGRESS),
              TxAdmissionsDB.countBacklog,
              MempoolDB.retrieveTxCount,
              MutationJobsDB.countUnfinished,
            ],
            { concurrency: "unbounded" },
          );
          const queueLength = Math.max(
            0,
            preMergeSnapshot.topology.parsedNodeCount - 1,
          );
          const preflight = planMergePreflight({
            force,
            queueLength,
            minQueueLength,
            unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
            localFinalizationPending,
            resetInProgress,
            durableAdmissionBacklog,
            mempoolTxCount,
            unfinishedMutationJobs,
          });
          if (preflight.status !== "ready") {
            if (preflight.status === "tail_eligible_final_merge") {
              yield* Effect.logInfo(
                `🔸 Auto-merging mature final tail below batch threshold if hard merge checks pass (${preflight.reason}).`,
              );
            } else {
              yield* Effect.logInfo(
                `🔸 Skipping merge (${preflight.status}; ${preflight.reason}).`,
              );
              return {
                status: preflight.status,
                reason: preflight.reason,
                queueLength: preflight.queueLength,
                minQueueLength: preflight.minQueueLength,
              } satisfies MergeActionResult;
            }
          }
          const trigger: MergeTrigger = force
            ? "manual"
            : preflight.status === "tail_eligible_final_merge"
              ? "final_tail_auto_merge"
              : "threshold";
          yield* lucid.switchToOperatorsMergingWallet;
          yield* StateQueueMutationLeasesDB.revalidate(leaseToken);
          const mergeTxResult = yield* buildAndSubmitMergeTx(
            lucid.api,
            fetchConfig,
            contracts,
            {
              bypassQueueLengthGuard: preflight.bypassQueueLengthGuard,
              leaseToken,
              // The permit was proven at registration, before an unbounded
              // wait for the L1 control plane; history recovery may have
              // revoked it since. Re-prove it under the lease right before
              // the transaction leaves, since the local finalization after
              // the L1 confirmation cannot write without it.
              assertSubmitAuthority: () =>
                StateQueueMutationLeasesDB.revalidate(leaseToken).pipe(
                  Effect.zipRight(withHistoryWrite(Effect.void)),
                ),
              onConfirmedFinalization: (outcome) =>
                Ref.set(confirmed, Option.some({ ...outcome, trigger })),
              referenceScriptsAddress: lucid.referenceScriptsAddress,
              submitSlotSnapshot: lucid.submitSlotSnapshot,
            },
          );
          if (mergeTxResult.status !== "merged") {
            return {
              status: mergeTxResult.status,
              reason: mergeTxResult.reason,
              ...(mergeTxResult.headerHash === undefined
                ? {}
                : { headerHash: mergeTxResult.headerHash }),
              ...(mergeTxResult.queueLength === undefined
                ? {}
                : { queueLength: mergeTxResult.queueLength }),
              ...(mergeTxResult.minQueueLength === undefined
                ? {}
                : { minQueueLength: mergeTxResult.minQueueLength }),
              ...(mergeTxResult.readyAfterUnixTime === undefined
                ? {}
                : { readyAfterUnixTime: mergeTxResult.readyAfterUnixTime }),
              ...(mergeTxResult.nowUnixTime === undefined
                ? {}
                : { nowUnixTime: mergeTxResult.nowUnixTime }),
            } satisfies MergeActionResult;
          }
          const snapshot = yield* fetchStateQueueSnapshotProgram(
            lucid.api,
            stateQueueAuthValidator,
            "post_merge",
          );
          yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
          yield* Effect.logInfo(
            `🔸 Refreshed live state-queue tail after merge: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
          );
          return {
            status: "merged",
            postMergeSnapshot: snapshot,
            headerHash: mergeTxResult.headerHash,
            txHash: mergeTxResult.txHash,
            trigger,
          } satisfies MergeActionResult;
        }),
      {
        ttlMs: nodeConfig.STATE_QUEUE_MUTATION_LEASE_TTL_MS,
        renewIntervalMs:
          nodeConfig.STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS,
      },
    );
    if (leaseResult._tag === "Busy") {
      const reason = StateQueueMutationLeasesDB.describeActiveLease(
        leaseResult.activeLease,
      );
      yield* Effect.logInfo(
        `🔸 Skipping merge because the state-queue mutation lease is busy (${reason}).`,
      );
      return {
        status: "skipped_state_queue_lease_busy",
        reason,
      } satisfies MergeActionResult;
    }
    return leaseResult.value;
  });

/**
 * The merge could not take the history producer permit, so none of its work
 * ran: no L1 read, no transaction, no local write. A standalone process (no
 * history owner) and a node whose history owner is not Ready both land here.
 */
export class MergeProducerPermitUnavailable extends Data.TaggedError(
  "MergeProducerPermitUnavailable",
)<{
  readonly message: string;
  readonly cause: unknown;
}> {}

/**
 * Runs `work` under the history producer permit. A merge finalizes locally by
 * writing history rows (confirmed ledger, block rows, deposit/withdrawal/forced
 * statuses) and every such write requires a registered producer.
 *
 * With a history owner in `Globals` the work always registers with it, even
 * when the caller already holds a permit, since producer registration nests.
 * Without an owner, only the explicit model-fixture capability runs the work
 * unregistered (its writes still pass `withHistoryWrite`'s fixture gate).
 *
 * A registration that refuses before the work starts fails with
 * `MergeProducerPermitUnavailable`. Once the work ran, its own failure wins,
 * with its type, even over a supersession found by the owner's trailing
 * currency check; a successful work whose trailing check fails reports that
 * check's failure.
 */
const withMergeHistoryProducer = <A, E, R>(
  work: Effect.Effect<A, E, R>,
): Effect.Effect<
  A,
  E | DatabaseError | MergeProducerPermitUnavailable,
  R | Globals | Database
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    if (owner === undefined) {
      const fixture = yield* Effect.serviceOption(UnownedHistoryFixture);
      if (Option.isSome(fixture)) return yield* work;
    }
    const ran = yield* Ref.make<Option.Option<Either.Either<A, E>>>(
      Option.none(),
    );
    const registration = yield* runHistoryProducer(
      Effect.either(work).pipe(
        Effect.tap((outcome) => Ref.set(ran, Option.some(outcome))),
      ),
    ).pipe(Effect.either);
    const outcome = yield* Ref.get(ran);
    if (Option.isNone(outcome)) {
      return yield* Effect.fail(
        new MergeProducerPermitUnavailable({
          message:
            "The merge needs the history producer permit, which this process could not take",
          cause: Either.isLeft(registration)
            ? registration.left
            : "registration returned without running the merge",
        }),
      );
    }
    if (Either.isLeft(outcome.value))
      return yield* Effect.fail(outcome.value.left);
    if (Either.isLeft(registration))
      return yield* Effect.fail(registration.left);
    return outcome.value.right;
  });

export type MergeActionOptions = {
  /**
   * Merge only if the oldest queued block is this header; otherwise skip with
   * `skipped_merge_candidate_changed` without building a transaction.
   */
  readonly expectedHeaderHash?: string;
};

/**
 * The single entry point for every merge trigger: the scheduled fiber, the
 * admin `GET /merge` route and `reconcile merge-complete --repair`.
 *
 * It holds the history producer permit for the whole attempt, so no caller can
 * reach the local finalization writes without it, and it runs under the
 * process-wide L1 control plane, so scheduled and manual merges in one process
 * are serialized. The state-queue mutation lease taken inside additionally
 * serializes merges against every other process sharing the database.
 */
export const mergeAction = (
  force: boolean = false,
  { expectedHeaderHash }: MergeActionOptions = {},
) =>
  withMergeHistoryProducer(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.HEARTBEAT_MERGE, Date.now());
      const confirmed = yield* Ref.make(Option.none<ConfirmedMerge>());
      const attempt = force
        ? withL1ControlPlane(
            globals,
            { scope: "state_queue_merge", maxHoldMs: 180_000 },
            mergeActionWithL1ControlPlaneHeld(
              true,
              expectedHeaderHash,
              confirmed,
            ),
          )
        : scheduledMergeAttempt(globals, expectedHeaderHash, confirmed);
      return yield* attempt.pipe(
        Effect.catchIf(
          (error): error is L1ControlPlaneTimeoutError =>
            error instanceof L1ControlPlaneTimeoutError,
          (timeout) => reportConfirmedMergeOverHoldTimeout(confirmed, timeout),
        ),
      );
    }),
  );

/**
 * The L1 control plane's hold timeout interrupts a merge attempt that runs
 * too long, but a merge already confirmed on L1 finishes its local
 * finalization regardless (it is uninterruptible), so the timeout must not
 * relabel it. A finalization that completed reports the merge, with a fresh
 * read of the state queue; one that failed reports its own failure. Only an
 * attempt interrupted before its merge was confirmed reports the timeout.
 */
const reportConfirmedMergeOverHoldTimeout = (
  confirmed: Ref.Ref<Option.Option<ConfirmedMerge>>,
  timeout: L1ControlPlaneTimeoutError,
) =>
  Effect.gen(function* () {
    const settled = yield* Ref.get(confirmed);
    if (Option.isNone(settled)) return yield* Effect.fail(timeout);
    const { exit, headerHash, txHash, trigger } = settled.value;
    if (Exit.isFailure(exit)) {
      yield* Effect.logError(
        `🔸 Merge confirmed on L1 but its local finalization failed while the L1 control-plane hold timed out; reporting the finalization failure (header=${headerHash},tx=${txHash},timeout=${timeout.message}).`,
      );
      return yield* Effect.failCause(exit.cause);
    }
    yield* Effect.logWarning(
      `🔸 Merge completed its local finalization past the L1 control-plane hold timeout; reporting the merge (header=${headerHash},tx=${txHash},timeout=${timeout.message}).`,
    );
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const globals = yield* Globals;
    const snapshot = yield* fetchStateQueueSnapshotProgram(
      lucid.api,
      contracts.stateQueue,
      "post_merge",
    );
    yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
    return {
      status: "merged",
      postMergeSnapshot: snapshot,
      headerHash,
      txHash,
      trigger,
    } satisfies MergeActionResult;
  });

const scheduledMergeAttempt = (
  globals: Globals,
  expectedHeaderHash: string | undefined,
  confirmed: Ref.Ref<Option.Option<ConfirmedMerge>>,
) =>
  Effect.gen(function* () {
    const attempt = yield* withScheduledMergeControlPlaneWait({
      globals,
      effect: mergeActionWithL1ControlPlaneHeld(
        false,
        expectedHeaderHash,
        confirmed,
      ),
    });
    if (Option.isSome(attempt)) {
      return attempt.value;
    }
    const reason = `l1_control_plane_wait_exceeded_ms=${SCHEDULED_MERGE_CONTROL_PLANE_WAIT_MS.toString()}`;
    yield* Effect.logInfo(
      `🔸 Skipping scheduled merge because the L1 control-plane remained busy (${reason}).`,
    );
    return {
      status: "skipped_l1_control_plane_busy",
      reason,
    } satisfies MergeActionResult;
  });

/**
 * Fiber wrapper that repeats merge attempts on the provided schedule.
 */
export const mergeFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Lucid | MidgardContracts | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟠 Merge fiber started.");
    const action = mergeAction().pipe(
      Effect.withSpan("merge-confirmed-state-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
