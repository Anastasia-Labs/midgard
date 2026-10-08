import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

import {
  MempoolDB,
  MutationJobsDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  awaitPostMergeSnapshot,
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeSubmitValidityEvidence,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import {
  buildAndSubmitMergeTx,
  captureMergeLocalLedgerGate,
  fetchCanonicalMergeCandidateReadiness,
  finalizeLandedMergesProgram,
} from "../transactions/state-queue/merge-to-confirmed-state.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "../transactions/utils.js";
import {
  changedCandidateResult,
  type ConfirmedMerge,
  logSemanticSkip,
  MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS,
  MERGE_LOCAL_FINALIZATION_RESERVE_MS,
  type MergeActionResult,
  mergeActionSemanticSkipResult,
  type MergeTrigger,
  mergeValidFromSlot,
  registeredMergeDueWorkSkip,
} from "./merge.registered-merge-due-work-skip.js";

export const mergeActionWithL1ControlPlaneHeld = (
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
  Lucid | MidgardContracts | Database | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    // The L1 control plane's hold began when this attempt acquired it.
    const confirmationDeadlineMs =
      Date.now() +
      MERGE_L1_CONTROL_PLANE_MAX_HOLD_MS -
      MERGE_LOCAL_FINALIZATION_RESERVE_MS;
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
    // Finalize any merge that landed without its local finalization before
    // deciding anything else, so even an attempt that skips catches it up.
    // Its failure fails this attempt, and the attempt runs under the producer
    // permit, so only while the history owner is Ready. A merge landing after
    // this read is finalized by buildAndSubmitMergeTx before it builds on it.
    yield* finalizeLandedMergesProgram(fetchConfig);
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
          nowUnixTime: preLeaseCandidate.readiness.nowUnixTime,
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
                nowUnixTime: leasedCandidate.readiness.nowUnixTime,
              } satisfies MergeActionResult;
            }
          }

          const preMergeSnapshot = yield* landedStateQueueSnapshot(
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
          const queueLength = preMergeSnapshot.blockCount;
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
              confirmationDeadlineMs,
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
          const snapshot = yield* awaitPostMergeSnapshot(
            stateQueueAuthValidator,
            mergeTxResult.headerHash,
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
