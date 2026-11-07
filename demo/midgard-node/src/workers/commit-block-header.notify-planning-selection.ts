import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import type { ContractDeploymentIdentityValue } from "../services/midgard-contracts.js";
import type { NotifyCommitWorkerParent } from "./commit-block-header.commit-explicit-block-header-program.js";
import { stateQueueBaseHeaderHash } from "./commit-block-header/state-queue.js";
import { makeCommitPlanningNotice } from "./commit-planning-notice.js";
import type { WorkerInput } from "./utils/commit-block-header.js";
import type { AdditiveCommitFramePlan } from "./utils/commit-block-planner.additive-frame.js";
import type { CommitEventInstant } from "./utils/commit-block-planner.event-window.js";

export const notifyCommitPlanningSelection = (input: {
  readonly notify: NotifyCommitWorkerParent | undefined;
  readonly workerInput: WorkerInput;
  readonly parent: SDK.StateQueueUTxO | undefined;
  readonly identity: ContractDeploymentIdentityValue;
  readonly baseRoot: string;
  readonly startTimeMs: number;
  readonly blockTimeMs: number;
  readonly canonicalHorizonMs: number;
  readonly eligibleCutoffMs: number;
  readonly groups: readonly CommitEventInstant[];
  readonly plan: Extract<AdditiveCommitFramePlan, { kind: "selected" }>;
}) =>
  Effect.gen(function* () {
    if (input.notify === undefined) return;
    if (
      input.workerInput.history === undefined ||
      input.parent === undefined ||
      input.identity.manifestId === undefined
    )
      return yield* input.notify({
        type: "CommitPlanningNotice",
        status: "unavailable",
        reason:
          input.workerInput.history === undefined
            ? "missing_follower_permit"
            : input.parent === undefined
              ? "missing_admitted_parent"
              : "deployment_identity_unavailable",
      });
    const baseHeaderHash = yield* stateQueueBaseHeaderHash(input.parent);
    const eligibleEventCountLowerBounds = input.groups.reduce(
      (sum, group) => ({
        deposits: sum.deposits + group.deposits,
        withdrawals: sum.withdrawals + group.withdrawals,
        forcedTransactions: sum.forcedTransactions + group.forcedTransactions,
      }),
      { deposits: 0, withdrawals: 0, forcedTransactions: 0 },
    );
    const notice = makeCommitPlanningNotice({
      mode: input.plan.window.mode,
      observedAtMs: Date.now(),
      authority: {
        view: input.workerInput.history.view,
        epoch: input.workerInput.history.epoch,
        baseHeaderHash,
        baseUtxosRoot: input.baseRoot,
        deploymentFingerprint: input.identity.manifestId,
      },
      coverageStartTimeMs: input.startTimeMs,
      selectedEndTimeMs: input.plan.window.endTimeMs,
      blockTimeMs: input.blockTimeMs,
      canonicalHorizonMs: input.canonicalHorizonMs,
      eligibleCutoffMs: input.eligibleCutoffMs,
      eligibleEventCountLowerBounds,
      selectedEventCounts: input.plan.window.counts,
      includedInstantCount: input.plan.window.includedInstantCount,
      selectedOrdinaryTxCount: input.plan.candidateCount,
      rescue: input.plan.rescue ?? null,
    });
    yield* input.notify(notice);
  });
