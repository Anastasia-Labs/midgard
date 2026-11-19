import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";

import {
  MempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Globals } from "../services/globals.globals.js";
import { Database, NodeConfig } from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  landedStateQueueSnapshot,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/landed-state-queue.js";
import { Lucid } from "../services/lucid.js";
import { MidgardContracts } from "../services/midgard-contracts.make-midgard-contract-runtime.js";
import {
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeSubmitValidityEvidence,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import {
  buildAndSubmitMergeTx,
  captureMergeLocalLedgerGate,
  fetchCanonicalMergeCandidateReadiness,
} from "../transactions/state-queue/merge-to-confirmed-state.js";
import type { SubmittedMergeTx } from "../transactions/state-queue/merge-to-confirmed-state.merge-options.js";
import {
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "../transactions/utils.await-required-output-visibility.js";
import { TxExpiredError } from "../transactions/utils.tx-expiry.js";
import {
  changedCandidateResult,
  logSemanticSkip,
  type MergeActionResult,
  mergeActionSemanticSkipResult,
  type MergeTrigger,
  mergeValidFromSlot,
  registeredMergeDueWorkSkip,
  type SubmittedMerge,
} from "./merge.registered-merge-due-work-skip.js";

/**
 * The part of a merge attempt that holds the L1 control plane: the candidate
 * checks, the lease, and the merge transaction's build, sign, submit and L1
 * confirmation. It ends with the skip, or with the submitted merge, whose
 * landing the caller awaits after the hold.
 */
export const mergeActionWithL1ControlPlaneHeld = (
  onSubmitted: (submitted: SubmittedMerge) => Effect.Effect<void>,
): Effect.Effect<
  MergeActionResult | SubmittedMerge,
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
  | TxExpiredError
  | TxSubmitError
  | TxSignError,
  Lucid | MidgardContracts | Database | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const startedAtMs = Date.now();
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    yield* Ref.set(globals.HEARTBEAT_MERGE, startedAtMs);
    const initialUnconfirmedSubmittedBlockTxHash = yield* Ref.get(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
    );
    if (initialUnconfirmedSubmittedBlockTxHash !== "") {
      const reason = `submitted_tx=${initialUnconfirmedSubmittedBlockTxHash}`;
      yield* Effect.logInfo(`🔸 Skipping merge (${reason}).`);
      return {
        status: "skipped_unresolved_commitment",
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
            durableAdmissionBacklog,
            mempoolTxCount,
          ] = yield* Effect.all(
            [
              Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
              TxAdmissionsDB.countBacklog,
              MempoolDB.retrieveTxCount,
            ],
            { concurrency: "unbounded" },
          );
          const queueLength = preMergeSnapshot.blockCount;
          const preflight = planMergePreflight({
            queueLength,
            minQueueLength,
            unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
            durableAdmissionBacklog,
            mempoolTxCount,
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
          const trigger: MergeTrigger =
            preflight.status === "tail_eligible_final_merge"
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
              // The lease was proven before the build; re-prove it right
              // before the transaction leaves.
              assertSubmitAuthority: () =>
                StateQueueMutationLeasesDB.revalidate(leaseToken),
              onSubmitted: (submitted: SubmittedMergeTx) =>
                onSubmitted({
                  status: "submitted",
                  ...submitted,
                  startedAtMs,
                  trigger,
                }),
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
          return {
            status: "submitted",
            headerHash: mergeTxResult.headerHash,
            txHash: mergeTxResult.txHash,
            startedAtMs,
            trigger,
          } satisfies SubmittedMerge;
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
