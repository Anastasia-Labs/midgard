/**
 * Operator watchdog: notices a missed shift and submits the strike (or, at the
 * strike limit, the forced retirement) that hands the shift on. Layer 1 never
 * advances on its own; without this fiber a dead operator stalls the chain
 * until someone runs the CLI verbs by hand.
 *
 * Directory: the tick plans from the operator set the follower-change driver
 * publishes (`Globals.OPERATOR_SET`, NC14): the registered and active lists,
 * the scheduler, the hub oracle and the landed state queue's tail, brought
 * up to date from the facts that changed. The retired list is never read
 * whole; a forced retirement reads only its insertion anchor, by asset name.
 * No provider read plans a takeover.
 *
 * Cadence: the fiber runs on the block-commitment schedule and defers itself
 * through the slot-aware due-work registry. A shift that cannot be struck for
 * many minutes yet costs one plan, not one per tick, and a node with nothing
 * to watch re-plans once a minute.
 *
 * Evidence: a strike needs an undelivered user event, a live deposit,
 * withdrawal or tx order whose inclusion time is after the state-queue tail's
 * end time. The tick reads the earliest one the chain would admit from the
 * follower's projections (`neglectedUserEvent`) and the strike cites it; its
 * threshold is that inclusion time plus `user_events_negligence_timeout`, but
 * never inside the shift's grace period. With none, the shift has no due L1
 * work: an operator below the strike cap is not struck however long it stays
 * idle, and the tick re-plans a minute later. A forced retirement at the
 * strike cap needs no event: the chain checks the strike count alone.
 *
 * A strike that fails on its citation does not wedge the tick: a script
 * refusal passes over that citation at once, any other failure after
 * `MAX_STRIKE_ATTEMPTS_PER_CITATION` attempts, and the next plan cites the
 * next citable event (`recordCitationFailure`). The record is per scheduler
 * UTxO and bounded, and each pass-over is logged and recorded by name.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, type Schedule } from "effect";

import { errorMessage } from "../commands/cli-runtime.js";
import { verifyConfiguredDeploymentManifestProgram } from "../commands/contract-deployment-info.js";
import { l1SlotNow } from "../l1-heads.js";
import {
  publishedDirectoryOf,
  publishedNeglectedUserEventProgram,
  publishedRetiredAnchorProgram,
} from "../l1-operator-set/index.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "../lucid-time.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  withL1ControlPlaneIfAvailable,
} from "../services/index.js";
import { intentPlanAt } from "../services/intent-journal.intent.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  configuredOperatorEconomicsProgram,
  retireOperatorProgram,
} from "../transactions/operators/exit.js";
import { OperatorFundingShortfall } from "../transactions/operators/funding-preflight.js";
import {
  planTakeoverFrom,
  resolveOwnOperatorKeyHashProgram,
  submitInactivityStrikeProgram,
  type TakeoverError,
} from "../transactions/operators/takeover.js";
import {
  deferWatchdog,
  DUE_WORK_KEY,
  DUE_WORK_KIND,
  toWatchdogPlan,
} from "./operator-watchdog.defer.js";
import {
  makeManifestStrikeGate,
  type ManifestGateDecision,
} from "./operator-watchdog.manifest-gate.js";
import {
  type CitationFailures,
  citationFailuresAt,
  decideOperatorWatchdogAction,
  emptyCitationFailures,
  isScriptRefusal,
  recordCitationFailure,
  recordOperatorWatchdogSkip,
  recordOperatorWatchdogTakeover,
} from "./operator-watchdog-policy.js";
import { checkSlotAwareDueWork } from "./slot-aware-due-work.js";

/**
 * How long a tick with nothing to act on waits before planning again: the
 * node holds the shift itself, is not active, the scheduler names nobody,
 * no user event is undelivered, the takeover is blocked, or the operator set
 * is not available.
 */
export const OPERATOR_WATCHDOG_IDLE_RECHECK_MS = 60_000;

/**
 * The scheduler UTxO this node's last takeover spent. The operator set shows
 * it live until the follower sees the takeover land; planning against it
 * would rebuild and resubmit the same takeover on a spent input.
 */
let lastSpentSchedulerRef: string | null = null;

/** The citations strikes failed on, against the current scheduler UTxO. */
let citationFailures: CitationFailures = emptyCitationFailures;

export const resetWatchdogCitationFailuresForTests = (): void => {
  citationFailures = emptyCitationFailures;
};

const schedulerRefOf = (snapshot: SDK.OperatorDirectorySnapshot): string =>
  `${snapshot.scheduler.utxo.txHash}#${snapshot.scheduler.utxo.outputIndex.toString()}`;

/**
 * One watchdog tick. `beforeStrike` runs before any strike or forced
 * retirement is built; a refusal defers the watchdog until the time it names
 * and strikes nobody. `whileNoStrikeDue` runs on an idle or waiting tick, so
 * a reason the gate raised clears without a strike falling due; a wait is cut
 * short to the time it returns. Neither has a default, so a caller that
 * passes a gate cannot leave its reasons without a clear.
 */
export const makeOperatorWatchdogTick = <R = never>(
  beforeStrike: (
    nowMs: number,
  ) => Effect.Effect<ManifestGateDecision, never, R>,
  whileNoStrikeDue: (
    nowMs: number,
  ) => Effect.Effect<number | undefined, never, R>,
): Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | NodeConfig | R | IntentJournal
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const globals = yield* Globals;

    // The L1 `slotNow` (plan §3.6) both gates the deferral and dates every
    // decision below; while it is unknown the tick does nothing and the next
    // one retries.
    const slotNow = yield* Effect.either(l1SlotNow(lucid.api));
    if (slotNow._tag === "Left") {
      yield* Effect.logWarning(
        `🐕 Operator watchdog waiting for the L1 slot: ${slotNow.left.message}`,
      );
      return;
    }
    const currentSlot = slotNow.right;
    const due = checkSlotAwareDueWork({
      kind: DUE_WORK_KIND,
      key: DUE_WORK_KEY,
      currentSlot,
    });
    if (due.status === "skip") {
      return;
    }

    // The shared Lucid's selected wallet belongs to the L1 control-plane holder
    // (a merge signs with the merge wallet), so the tick reads with the
    // configured operator identity and selects the operator wallet only once it
    // holds the permit.
    const published = yield* Ref.get(globals.OPERATOR_SET);
    const directory = publishedDirectoryOf(published);
    const deferUnplanned = (reason: string, detail: string) =>
      Effect.gen(function* () {
        yield* Effect.logWarning(
          `🐕 Operator watchdog could not plan from the operator set this tick (${reason}): ${detail}`,
        );
        const nowMs = slotToUnixTimeForLucidOrEmulatorFallback(
          lucid.api,
          currentSlot,
        );
        yield* deferWatchdog({
          lucid: lucid.api,
          currentSlot,
          nowMs,
          untilMs: nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS,
          reason,
          dependencyKey: "operator_set",
        });
      });
    if (directory.kind === "unavailable")
      return yield* deferUnplanned(directory.reason, directory.detail);
    // S5: a plan from the published set records under the set's view.
    const intentPlan = intentPlanAt(directory.view);
    citationFailures = citationFailuresAt(
      citationFailures,
      schedulerRefOf(directory.snapshot),
    );
    const prepared = yield* Effect.either(
      Effect.all([
        resolveOwnOperatorKeyHashProgram(lucid.operatorMainAddress),
        publishedNeglectedUserEventProgram(
          published,
          directory.snapshot.stateQueueTail.endTime,
          citationFailures.excluded,
        ).pipe(
          Effect.flatMap((neglectedEvent) =>
            planTakeoverFrom(lucid.api, directory.snapshot, intentPlan, {
              neglectedEvent,
            }),
          ),
        ),
      ]),
    );
    if (prepared._tag === "Left")
      return yield* deferUnplanned(
        "takeover_plan_failed",
        errorMessage(prepared.left),
      );
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
        "operator_set_behind_own_takeover",
        nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS,
      );
    }

    const decision = decideOperatorWatchdogAction({
      enabled: nodeConfig.OPERATOR_WATCHDOG_ENABLED,
      nowMs,
      patienceMs: nodeConfig.OPERATOR_WATCHDOG_PATIENCE_MS,
      ownOperatorKey,
      ownOperatorIsActive:
        SDK.findNodeByKey(planning.snapshot.active, ownOperatorKey) !==
        undefined,
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
        yield* whileNoStrikeDue(nowMs);
        return yield* defer(reason, nowMs + OPERATOR_WATCHDOG_IDLE_RECHECK_MS);
      }
      case "wait": {
        // A wait can last a whole shift; a raised gate reason retries sooner.
        const retryAtMs = yield* whileNoStrikeDue(nowMs);
        return yield* defer(
          decision.reason,
          retryAtMs === undefined
            ? decision.untilMs
            : Math.min(decision.untilMs, retryAtMs),
        );
      }
      case "strike":
      case "force_retire": {
        if (
          planning.plan.kind !== "ready" &&
          planning.plan.kind !== "strikes-exhausted"
        ) {
          return;
        }
        const plan = planning.plan;
        // L1 time, as every deferral of the tick.
        const gate = yield* beforeStrike(nowMs);
        if (!gate.ok) {
          recordOperatorWatchdogSkip({ reason: gate.reason, atMs: Date.now() });
          return yield* defer(gate.reason, gate.untilMs);
        }
        type TakeoverOutcome = {
          readonly kind: "strike" | "force_retire";
          readonly txHash: string;
          readonly detail: string;
        };
        const submission: Effect.Effect<
          TakeoverOutcome,
          TakeoverError,
          NodeConfig | IntentJournal
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
            : Effect.all([
                configuredOperatorEconomicsProgram,
                publishedRetiredAnchorProgram(published, plan.currentOperator),
              ]).pipe(
                Effect.flatMap(([economics, anchor]) =>
                  retireOperatorProgram(
                    lucid.api,
                    contracts,
                    lucid.referenceScriptsAddress,
                    {
                      // The retirement inserts after one retired node; it is
                      // the only one the snapshot needs.
                      snapshot: { ...planning.snapshot, retired: [anchor] },
                      intentPlan: planning.intentPlan,
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
            lucid.switchToOperatorsMainWallet.pipe(
              Effect.zipRight(Effect.either(submission)),
            ),
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
          let reason: string = "submission_failed";
          let citation = "";
          if (error instanceof OperatorFundingShortfall) {
            reason = "insufficient_funds";
          } else if (plan.kind === "ready") {
            // The failure counts against the citation, never against the
            // funds: a later event is no cheaper to cite.
            const citationId = SDK.neglectedUserEventCitationId(
              plan.neglectedEvent,
            );
            const recorded = recordCitationFailure(citationFailures, {
              schedulerRef,
              citationId,
              refused: isScriptRefusal(error),
            });
            citationFailures = recorded.failures;
            reason = recorded.reason;
            citation = `, citing ${citationId}`;
          }
          recordOperatorWatchdogSkip({ reason, atMs: Date.now() });
          yield* Effect.logWarning(
            `🐕 Operator watchdog could not ${decision.action.replace("_", "-")} ${decision.skippedOperator} (${reason}${citation}): ${errorMessage(error)}`,
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

/** The tick without a strike gate, for callers that verified the deployment
 * manifest themselves. */
export const operatorWatchdogTick = makeOperatorWatchdogTick(
  () => Effect.succeed({ ok: true }),
  () => Effect.succeed(undefined),
);

export const operatorWatchdogFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | Lucid | MidgardContracts | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    if (!nodeConfig.OPERATOR_WATCHDOG_ENABLED) {
      yield* Effect.logInfo(
        "🐕 Operator watchdog disabled (OPERATOR_WATCHDOG_ENABLED=false); missed shifts must be struck by hand.",
      );
      // Membership is the operator-set hook's, in the follower-change
      // driver. Disabling takeover never disables removal detection.
      return;
    }
    // Every lifecycle verb verifies the deployment manifest before it acts;
    // the watchdog verifies it before its first strike, retries a failed
    // verification, and re-verifies a raised reason on ticks with no strike
    // due (see `makeManifestStrikeGate`).
    const globals = yield* Globals;
    const gate = yield* makeManifestStrikeGate(
      globals,
      verifyConfiguredDeploymentManifestProgram,
    );
    yield* Effect.logInfo(
      `🐕 Operator watchdog fiber started (patience_ms=${nodeConfig.OPERATOR_WATCHDOG_PATIENCE_MS.toString()}).`,
    );
    const action = makeOperatorWatchdogTick(
      gate.beforeStrike,
      gate.whileNoStrikeDue,
    ).pipe(
      Effect.withSpan("operator-watchdog-fiber"),
      // The tick's error channel is `never`; anything caught here is a defect.
      Effect.catchAllCause(Effect.logError),
    );
    yield* Effect.repeat(action, schedule);
  });
