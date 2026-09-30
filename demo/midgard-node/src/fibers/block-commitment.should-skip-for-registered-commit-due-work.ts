import { Effect, Ref } from "effect";

import {
  type CommitPipelinePhase,
  Globals,
  Lucid,
  MidgardContracts,
} from "../services/index.js";
import type { EarliestCommitSchedulerPlan } from "../workers/utils/commit-block-planner.js";
import { fetchRealStateQueueWitnessContext } from "../workers/utils/scheduler-refresh.js";
import {
  BLOCK_COMMITMENT_DUE_WORK_KEY,
  BLOCK_COMMITMENT_DUE_WORK_KIND,
} from "./block-commitment.promote-or-recover-native-mpf.js";
import {
  clearRegisteredCommitDueWork,
  isDetailedSchedulerAlignmentDueWork,
  planPreLeaseCommitSchedulerDueWork,
  preLeaseCommitSchedulerTargetMs,
  shouldSkipForDetailedSchedulerDueWork,
} from "./block-commitment.should-skip-for-detailed-scheduler-due-work.js";
import {
  checkSlotAwareDueWork,
  peekSlotAwareDueWork,
  registerSlotAwareDueWork,
} from "./slot-aware-due-work.js";

export const shouldSkipForRegisteredCommitDueWork = Effect.gen(function* () {
  const entry = peekSlotAwareDueWork(
    BLOCK_COMMITMENT_DUE_WORK_KIND,
    BLOCK_COMMITMENT_DUE_WORK_KEY,
  );
  if (entry === undefined) {
    return false;
  }
  if (isDetailedSchedulerAlignmentDueWork(entry)) {
    return yield* shouldSkipForDetailedSchedulerDueWork;
  }
  const freshPlan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
  if (freshPlan._tag === "Left") {
    clearRegisteredCommitDueWork();
    yield* Effect.logWarning(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease evidence failed: ${String(freshPlan.left)}`,
    );
    return false;
  }
  if (freshPlan.right.status !== "register_due_work") {
    clearRegisteredCommitDueWork();
    yield* Effect.logInfo(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease evidence no longer proves scheduler due-work (status=${freshPlan.right.status},reason=${freshPlan.right.reason}).`,
    );
    return false;
  }
  if (
    freshPlan.right.dueWork.key !== entry.key ||
    freshPlan.right.dueWork.dueSlot !== entry.dueSlot
  ) {
    clearRegisteredCommitDueWork();
    yield* Effect.logInfo(
      `🔹 Clearing block commitment due work before re-plan because fresh pre-lease due-work changed (old_due_slot=${entry.dueSlot.toString()},fresh_due_slot=${freshPlan.right.dueWork.dueSlot.toString()}).`,
    );
    return false;
  }
  const decision = checkSlotAwareDueWork({
    kind: BLOCK_COMMITMENT_DUE_WORK_KIND,
    key: BLOCK_COMMITMENT_DUE_WORK_KEY,
    currentSlot: freshPlan.right.dueWork.observedSlot,
    dependencyKey: freshPlan.right.dueWork.dependencyKey,
    invalidationKey: freshPlan.right.dueWork.invalidationKey,
  });
  switch (decision.status) {
    case "skip":
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because registered due work is not due discovery_stage=pre_lease (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()},wait_ms=${decision.entry.waitMs.toString()},dependency_key=${freshPlan.right.dueWork.dependencyKey}).`,
      );
      return true;
    case "due":
      yield* Effect.logInfo(
        `🔹 Waking block commitment due work discovery_stage=pre_lease (kind=${decision.entry.kind},key=${decision.entry.key},current_slot=${decision.currentSlot.toString()},due_slot=${decision.entry.dueSlot.toString()}).`,
      );
      return false;
    case "invalidated":
      yield* Effect.logInfo(
        `🔹 Clearing block commitment due work before re-plan discovery_stage=pre_lease (${decision.reason}).`,
      );
      return false;
    case "missing":
      return false;
  }
});

const registerPreLeaseCommitSchedulerDueWork = (
  plan: Extract<EarliestCommitSchedulerPlan, { status: "register_due_work" }>,
) =>
  Effect.sync(() => registerSlotAwareDueWork(plan.dueWork)).pipe(
    Effect.andThen((entry) =>
      Effect.logInfo(
        `🔹 Registered slot-aware due work before block commitment lease discovery_stage=${plan.discoveryStage} (kind=${entry.kind},key=${entry.key},current_slot=${entry.observedSlot.toString()},due_slot=${entry.dueSlot.toString()},due_at_ms=${entry.dueAtMs.toString()},wait_ms=${entry.waitMs.toString()},slot_source=${entry.slotSource},dependency_key=${entry.dependencyKey}).`,
      ),
    ),
  );

export const registerPreLeaseCommitSchedulerDueWorkIfProven = Effect.gen(
  function* () {
    const plan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
    if (plan._tag === "Left") {
      yield* Effect.logWarning(
        `🔹 Earliest block commitment scheduler preflight failed; skipping this tick before the mutation lease: ${String(plan.left)}`,
      );
      return true;
    }
    if (plan.right.status === "register_due_work") {
      yield* registerPreLeaseCommitSchedulerDueWork(plan.right);
      return true;
    }
    if (plan.right.status === "ambiguous") {
      yield* Effect.logInfo(
        `🔹 Earliest block commitment scheduler preflight ambiguous; continuing to full planner (reason=${plan.right.reason}).`,
      );
    }
    return false;
  },
);

export const shouldRunPreLeaseSchedulerAlignment = (
  plan: EarliestCommitSchedulerPlan,
): boolean =>
  (plan.status === "proceed" &&
    (plan.reason === "scheduler_submit_timing_ready" ||
      plan.reason === "current_operator_already_active")) ||
  (plan.status === "ambiguous" &&
    plan.reason === "scheduler_has_no_active_operator");

const alignCommitSchedulerBeforeMutationWorker = Effect.gen(function* () {
  const plan = yield* Effect.either(planPreLeaseCommitSchedulerDueWork);
  if (plan._tag === "Left") {
    yield* Effect.logWarning(
      `🔹 Pre-lease scheduler alignment probe failed; skipping this tick before the mutation lease: ${String(plan.left)}`,
    );
    return true;
  }
  if (plan.right.status === "register_due_work") {
    yield* registerPreLeaseCommitSchedulerDueWork(plan.right);
    return true;
  }
  if (!shouldRunPreLeaseSchedulerAlignment(plan.right)) {
    return false;
  }
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const alignedEndTime = preLeaseCommitSchedulerTargetMs(Date.now());
  const alignment = yield* Effect.either(
    fetchRealStateQueueWitnessContext(
      lucid.api,
      contracts,
      alignedEndTime,
      undefined,
      lucid.referenceScriptsAddress,
      lucid.submitSlotSnapshot,
      true,
    ),
  );
  if (alignment._tag === "Left") {
    yield* Effect.logWarning(
      `🔹 Pre-lease scheduler alignment failed; skipping this tick before the mutation lease: ${String(alignment.left)}`,
    );
    return true;
  }
  if ("dueWork" in alignment.right) {
    registerSlotAwareDueWork(alignment.right.dueWork);
    yield* Effect.logInfo(
      `🔹 Registered slot-aware due work from pre-lease scheduler alignment (kind=${alignment.right.dueWork.kind},key=${alignment.right.dueWork.key},current_slot=${alignment.right.dueWork.observedSlot.toString()},due_slot=${alignment.right.dueWork.dueSlot.toString()},wait_ms=${alignment.right.dueWork.waitMs.toString()},dependency_key=${alignment.right.dueWork.dependencyKey}).`,
    );
    return true;
  }
  yield* Effect.logInfo(
    `🔹 Scheduler alignment completed before block commitment mutation worker (reason=${plan.right.reason}).`,
  );
  return false;
});

type CommitPhaseAcquireResult =
  | {
      readonly acquired: true;
    }
  | {
      readonly acquired: false;
      readonly activePhase: CommitPipelinePhase;
    };

const acquireCommitPipelinePhase = (
  globals: Globals,
  targetPhase: Exclude<CommitPipelinePhase, "idle">,
): Effect.Effect<CommitPhaseAcquireResult> =>
  Ref.modify(
    globals.COMMIT_PIPELINE_PHASE,
    (activePhase): [CommitPhaseAcquireResult, CommitPipelinePhase] =>
      activePhase === "idle"
        ? [{ acquired: true }, targetPhase]
        : [{ acquired: false, activePhase }, activePhase],
  );

export const tryAcquireCommitSchedulerAlignmentPhase = (
  globals: Globals,
): Effect.Effect<CommitPhaseAcquireResult> =>
  acquireCommitPipelinePhase(globals, "scheduler_alignment");

export const releaseCommitSchedulerAlignmentPhase = (
  globals: Globals,
): Effect.Effect<void> => Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle");

export const tryAcquireCommitMutationWorkerPhase = (
  globals: Globals,
): Effect.Effect<CommitPhaseAcquireResult> =>
  Effect.gen(function* () {
    const acquired = yield* acquireCommitPipelinePhase(
      globals,
      "mutation_worker",
    );
    if (acquired.acquired) {
      yield* Ref.set(globals.COMMIT_WORKER_ACTIVE, true);
    }
    return acquired;
  });

export const releaseCommitMutationWorkerPhase = (
  globals: Globals,
): Effect.Effect<void> =>
  Effect.all(
    [
      Ref.set(globals.COMMIT_WORKER_ACTIVE, false),
      Ref.set(globals.COMMIT_PIPELINE_PHASE, "idle"),
    ],
    { discard: true },
  );

export const alignCommitSchedulerBeforeMutationWorkerIfIdle = Effect.gen(
  function* () {
    const globals = yield* Globals;
    const acquired = yield* tryAcquireCommitSchedulerAlignmentPhase(globals);
    if (!acquired.acquired) {
      yield* Effect.logInfo(
        `🔹 Skipping block commitment trigger because commit pipeline phase is already active (phase=${acquired.activePhase}).`,
      );
      return true;
    }
    return yield* alignCommitSchedulerBeforeMutationWorker.pipe(
      Effect.ensuring(releaseCommitSchedulerAlignmentPhase(globals)),
    );
  },
);
