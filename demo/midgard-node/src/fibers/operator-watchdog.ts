/**
 * Operator watchdog: notices a missed shift and submits the strike (or, at the
 * strike limit, the forced retirement) that hands the shift on. Layer 1 never
 * advances on its own; without this fiber a dead operator stalls the chain
 * until someone runs the CLI verbs by hand.
 *
 * Cadence: the fiber runs on the block-commitment schedule and defers itself
 * through the slot-aware due-work registry. A shift that cannot be struck for
 * another 40 minutes costs one directory read, not one per tick, and a node
 * with nothing to watch re-reads the directory once a minute.
 *
 * Scope: the watchdog strikes on the commitment-gap threshold only. A
 * neglected deposit or withdrawal can make a shift strikable earlier; that
 * threshold needs the neglected event as a witness and is left to the SDK
 * planner's `neglectedEvent` option, which this fiber does not supply.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, type Schedule } from "effect";

import { errorMessage } from "../commands/cli-runtime.js";
import { verifyConfiguredDeploymentManifestProgram } from "../commands/contract-deployment-info.js";
import {
  canonicalSlotConfigForLucid,
  unixTimeToSlotForConfig,
} from "../lucid-time.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlaneIfAvailable,
} from "../services/index.js";
import {
  configuredOperatorEconomicsProgram,
  retireOperatorProgram,
} from "../transactions/operators/exit.js";
import { OperatorFundingShortfall } from "../transactions/operators/funding-preflight.js";
import {
  planTakeoverProgram,
  resolveOwnOperatorKeyHashProgram,
  submitInactivityStrikeProgram,
  type TakeoverError,
} from "../transactions/operators/takeover.js";
import {
  decideOperatorWatchdogAction,
  recordOperatorWatchdogSkip,
  recordOperatorWatchdogTakeover,
  type WatchdogTakeoverPlan,
} from "./operator-watchdog-policy.js";
import {
  checkSlotAwareDueWork,
  registerSlotAwareDueWork,
} from "./slot-aware-due-work.js";

const DUE_WORK_KIND = "operator_watchdog" as const;
const DUE_WORK_KEY = "takeover";

/**
 * How long a tick with nothing to act on waits before reading the directory
 * again: the node holds the shift itself, is not active, the scheduler names
 * nobody, the takeover is blocked, or the provider could not be read.
 */
export const OPERATOR_WATCHDOG_IDLE_RECHECK_MS = 60_000;

/**
 * The scheduler UTxO this node's last takeover spent. A provider that lags
 * behind its own submission still serves that UTxO for a while; planning
 * against it would rebuild and resubmit the same takeover on a spent input.
 */
let lastSpentSchedulerRef: string | null = null;

const schedulerRefOf = (snapshot: SDK.OperatorDirectorySnapshot): string =>
  `${snapshot.scheduler.utxo.txHash}#${snapshot.scheduler.utxo.outputIndex.toString()}`;

/**
 * Maps a wall-clock target to the slot at which it becomes current. When Lucid
 * exposes no slot configuration (emulator), one-second slots are assumed.
 */
const unixTimeToSlotOrFallback = (
  lucid: Parameters<typeof canonicalSlotConfigForLucid>[0],
  unixTimeMs: number,
  currentSlot: number,
  waitMs: number,
): number => {
  try {
    return unixTimeToSlotForConfig(
      unixTimeMs,
      canonicalSlotConfigForLucid(lucid),
    );
  } catch {
    return currentSlot + Math.ceil(waitMs / 1000);
  }
};

const toSafeNumber = (value: bigint): number =>
  value > BigInt(Number.MAX_SAFE_INTEGER)
    ? Number.MAX_SAFE_INTEGER
    : Number(value);

/**
 * Projects the SDK planner's result onto the policy's view of it.
 */
const toWatchdogPlan = (
  plan: SDK.InactivityTakeoverPlan,
): WatchdogTakeoverPlan => {
  switch (plan.kind) {
    case "no-shift":
      return { kind: "no-shift" };
    case "not-yet":
      return {
        kind: "not-yet",
        currentOperator: plan.currentOperator,
        thresholdMs: toSafeNumber(plan.thresholdMs),
      };
    case "blocked":
      // A blocked plan (e.g. a registered operator may still activate, or
      // the successor node is missing) is treated as "no shift to take" this
      // tick; the next tick re-plans from a fresh snapshot.
      return { kind: "no-shift" };
    case "strikes-exhausted":
      return {
        kind: "strikes-exhausted",
        currentOperator: plan.currentOperator,
        thresholdMs: toSafeNumber(plan.thresholdMs),
      };
    case "ready":
      return {
        kind: "ready",
        currentOperator: plan.currentOperator,
        newOperatorKey: plan.newOperatorKey,
        thresholdMs: toSafeNumber(plan.thresholdMs),
      };
  }
};

/**
 * Defers the next directory read until `untilMs`. The registry's dependency
 * and invalidation keys are descriptive here: the check runs before the
 * directory is read, so there is nothing yet to compare them against.
 */
const deferWatchdog = (input: {
  readonly lucid: Parameters<typeof canonicalSlotConfigForLucid>[0];
  readonly currentSlot: number;
  readonly nowMs: number;
  readonly untilMs: number;
  readonly reason: string;
  readonly dependencyKey: string;
}): Effect.Effect<void> => {
  const waitMs = Math.max(0, input.untilMs - input.nowMs);
  const dueSlot = unixTimeToSlotOrFallback(
    input.lucid,
    input.untilMs,
    input.currentSlot,
    waitMs,
  );
  registerSlotAwareDueWork({
    kind: DUE_WORK_KIND,
    key: DUE_WORK_KEY,
    callerLabel: "operator-watchdog",
    reason: input.reason,
    observedSlot: input.currentSlot,
    dueSlot,
    dueAtMs: input.untilMs,
    waitMs,
    slotSource: "lucid_current_slot",
    dependencyKey: input.dependencyKey,
    invalidationKey: input.reason,
  });
  return Effect.logInfo(
    `🐕 Operator watchdog waiting (${input.reason}) until ${new Date(input.untilMs).toISOString()} (wait_ms=${waitMs.toString()}, due_slot=${dueSlot.toString()}).`,
  );
};

const operatorWatchdogTick: Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | NodeConfig
> = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

  const currentSlot = lucid.api.currentSlot();
  const due = checkSlotAwareDueWork({
    kind: DUE_WORK_KIND,
    key: DUE_WORK_KEY,
    currentSlot,
  });
  if (due.status === "skip") {
    return;
  }

  yield* lucid.switchToOperatorsMainWallet;
  const prepared = yield* Effect.either(
    Effect.all([
      resolveOwnOperatorKeyHashProgram(lucid.api),
      planTakeoverProgram(lucid.api, contracts),
    ]),
  );
  if (prepared._tag === "Left") {
    yield* Effect.logWarning(
      `🐕 Operator watchdog could not read the operator directory this tick: ${errorMessage(prepared.left)}`,
    );
    const nowMs = Date.now();
    return yield* deferWatchdog({
      lucid: lucid.api,
      currentSlot,
      nowMs,
      untilMs: nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS,
      reason: "directory_read_failed",
      dependencyKey: "provider",
    });
  }
  const [ownOperatorKey, planning] = prepared.right;
  const nowMs = Number(planning.nowMs);
  const schedulerRef = schedulerRefOf(planning.snapshot);
  const defer = (reason: string, untilMs: number) =>
    deferWatchdog({
      lucid: lucid.api,
      currentSlot,
      nowMs,
      untilMs,
      reason,
      dependencyKey: `scheduler=${schedulerRef}`,
    });

  if (schedulerRef === lastSpentSchedulerRef) {
    return yield* defer(
      "provider_behind_own_takeover",
      nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS,
    );
  }

  const decision = decideOperatorWatchdogAction({
    enabled: nodeConfig.OPERATOR_WATCHDOG_ENABLED,
    nowMs,
    patienceMs: nodeConfig.OPERATOR_WATCHDOG_PATIENCE_MS,
    ownOperatorKey,
    ownOperatorIsActive:
      SDK.findNodeByKey(planning.snapshot.active, ownOperatorKey) !== undefined,
    plan: toWatchdogPlan(planning.plan),
  });

  switch (decision.action) {
    case "idle": {
      const reason =
        planning.plan.kind === "blocked"
          ? `takeover_blocked:${planning.plan.reason}`
          : decision.reason;
      if (planning.plan.kind === "blocked") {
        yield* Effect.logInfo(
          `🐕 Operator watchdog idle: takeover blocked (${planning.plan.reason}: ${planning.plan.detail}).`,
        );
      }
      return yield* defer(reason, nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS);
    }
    case "wait":
      return yield* defer(decision.reason, decision.untilMs);
    case "strike":
    case "force_retire": {
      if (
        planning.plan.kind !== "ready" &&
        planning.plan.kind !== "strikes-exhausted"
      ) {
        return;
      }
      const plan = planning.plan;
      type TakeoverOutcome = {
        readonly kind: "strike" | "force_retire";
        readonly txHash: string;
        readonly detail: string;
      };
      const submission: Effect.Effect<
        TakeoverOutcome,
        TakeoverError,
        NodeConfig
      > =
        plan.kind === "ready"
          ? submitInactivityStrikeProgram(
              lucid.api,
              contracts,
              lucid.referenceScriptsAddress,
              { ...planning, plan },
              { label: `operator-watchdog strike (${decision.tier})` },
            ).pipe(
              Effect.map((result) => ({
                kind: "strike",
                txHash: result.txHash,
                detail: `struck ${result.skippedOperator} → ${result.newOperator} (strikes=${result.struckInactivityStrikes.toString()}, tier=${result.tier})`,
              })),
            )
          : configuredOperatorEconomicsProgram.pipe(
              Effect.flatMap((economics) =>
                retireOperatorProgram(
                  lucid.api,
                  contracts,
                  lucid.referenceScriptsAddress,
                  {
                    snapshot: planning.snapshot,
                    operatorKeyHash: plan.currentOperator,
                    mode: "forced-inactivity",
                    economics,
                  },
                  {
                    label: `operator-watchdog force-retire (${decision.tier})`,
                  },
                ),
              ),
              Effect.map((result) => ({
                kind: "force_retire",
                txHash: result.txHash,
                detail: `force-retired ${plan.currentOperator} (retired bond=${result.retiredBondLovelace.toString()})`,
              })),
            );
      const guarded = yield* Effect.either(
        withL1ControlPlaneIfAvailable(
          globals,
          { scope: "operator_watchdog", maxHoldMs: 180_000 },
          Effect.either(submission),
        ),
      );
      if (guarded._tag === "Left") {
        recordOperatorWatchdogSkip({
          reason: "control_plane_hold_timeout",
          atMs: Date.now(),
        });
        yield* Effect.logWarning(
          `🐕 Operator watchdog gave up the L1 control plane: ${errorMessage(guarded.left)}`,
        );
        return;
      }
      const outcome = guarded.right;
      if (outcome._tag === "None") {
        yield* Effect.logInfo(
          "🐕 Operator watchdog skipped this tick: the L1 control plane is busy.",
        );
        return;
      }
      const result = outcome.value;
      if (result._tag === "Left") {
        const error = result.left;
        const reason =
          error instanceof OperatorFundingShortfall
            ? "insufficient_funds"
            : "submission_failed";
        recordOperatorWatchdogSkip({ reason, atMs: Date.now() });
        yield* Effect.logWarning(
          `🐕 Operator watchdog could not ${decision.action.replace("_", "-")} ${decision.skippedOperator} (${reason}): ${errorMessage(error)}`,
        );
        return;
      }
      lastSpentSchedulerRef = schedulerRef;
      recordOperatorWatchdogTakeover({
        txHash: result.right.txHash,
        atMs: Date.now(),
        kind: result.right.kind,
      });
      yield* Effect.logInfo(
        `🐕 Operator watchdog ${result.right.detail}; tx=${result.right.txHash}`,
      );
      return;
    }
  }
});

export const operatorWatchdogFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    if (!nodeConfig.OPERATOR_WATCHDOG_ENABLED) {
      yield* Effect.logInfo(
        "🐕 Operator watchdog disabled (OPERATOR_WATCHDOG_ENABLED=false); missed shifts must be struck by hand.",
      );
      return;
    }
    // Every lifecycle verb verifies the deployment manifest before it acts;
    // the watchdog verifies once at start, since the manifest cannot change
    // without a redeploy and a restart.
    const verification = yield* Effect.either(
      verifyConfiguredDeploymentManifestProgram,
    );
    if (verification._tag === "Left" || !verification.right.ok) {
      const detail =
        verification._tag === "Left"
          ? errorMessage(verification.left)
          : verification.right.mismatches.join("; ");
      yield* Effect.logError(
        `🐕 Operator watchdog not started: deployment manifest verification failed (${detail}); missed shifts must be struck by hand.`,
      );
      return;
    }
    yield* Effect.logInfo(
      `🐕 Operator watchdog fiber started (patience_ms=${nodeConfig.OPERATOR_WATCHDOG_PATIENCE_MS.toString()}).`,
    );
    const action = operatorWatchdogTick.pipe(
      Effect.withSpan("operator-watchdog-fiber"),
      // The tick's error channel is `never`; anything caught here is a defect.
      Effect.catchAllCause(Effect.logError),
    );
    yield* Effect.repeat(action, schedule);
  });
