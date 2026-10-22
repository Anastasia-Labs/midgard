/**
 * Node-side programs behind stalled-operator recovery: plan a takeover from
 * the live directory, submit an inactivity strike, or force-retire an operator
 * that has exhausted its strikes. Shared by the watchdog fiber and the manual
 * CLI verbs so both act on identical logic.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type L1SlotUnknownError } from "../../l1-heads.js";
import {
  type IntentJournal,
  type IntentPlan,
  journaledIntent,
  openPlan,
} from "../../services/intent-journal.js";
import { alignedUnixTimeStrictlyAfter } from "../../workers/utils/commit-end-time.js";
import { resolveL1NowMs } from "../register-active-operator/clock.js";
import { handleSignSubmit } from "../utils.js";
import { readSelectedWalletViewInputs } from "../utils.wallet-view.js";
import {
  OPERATOR_TX_VALIDITY_WINDOW_MS,
  type OperatorExitError,
  resolveOperatorScriptRefsProgram,
} from "./exit.js";
import {
  presetOperatorWalletInputs,
  requireOperatorFundingProgram,
} from "./funding-preflight.js";

export type TakeoverError = OperatorExitError | SDK.SchedulerError;

/**
 * The payment key hash of an operator wallet address. Callers pass the address
 * derived from the configured seed, not the shared Lucid's selected wallet,
 * which belongs to whichever L1 control-plane holder selected it last.
 */
export const resolveOwnOperatorKeyHashProgram = (
  address: string,
): Effect.Effect<string, SDK.StateQueueError> => {
  const credential = paymentCredentialOf(address);
  return credential.type === "Key"
    ? Effect.succeed(credential.hash)
    : Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet must use a payment key credential",
          cause: address,
        }),
      );
};

/** The first slot boundary at or after `unixTimeMs`. */
export const alignedUnixTimeAtOrAfter = (
  lucid: LucidEvolution,
  unixTimeMs: bigint,
): bigint =>
  BigInt(alignedUnixTimeStrictlyAfter(lucid, Number(unixTimeMs) - 1));

export type TakeoverPlanning = {
  readonly nowMs: bigint;
  readonly snapshot: SDK.OperatorDirectorySnapshot;
  readonly plan: SDK.InactivityTakeoverPlan;
  /** The intent plan (S5) opened before `snapshot` was read. */
  readonly intentPlan: IntentPlan;
  /** The timing parameters `plan` was made under; the defaults when unset. */
  readonly params?: SDK.InactivityTimingParameters;
};

/**
 * Plans a takeover of the current shift in `snapshot` as of the chain clock.
 * `neglectedEvent` is the undelivered user event the strike cites, or null
 * when there is none, in which case nobody can be struck.
 */
export const planTakeoverFrom = (
  lucid: LucidEvolution,
  snapshot: SDK.OperatorDirectorySnapshot,
  intentPlan: IntentPlan,
  options: {
    readonly neglectedEvent: SDK.NeglectedUserEventClaim | null;
    readonly params?: SDK.InactivityTimingParameters;
    readonly nowMs?: bigint;
  },
): Effect.Effect<TakeoverPlanning, L1SlotUnknownError> =>
  Effect.gen(function* () {
    const nowMs = options.nowMs ?? (yield* resolveL1NowMs(lucid));
    const plan = SDK.planInactivityTakeover({
      snapshot,
      nowMs,
      neglectedEvent: options.neglectedEvent,
      params: options.params,
      validityWindowMs: OPERATOR_TX_VALIDITY_WINDOW_MS,
      alignValidFrom: (candidate) => alignedUnixTimeAtOrAfter(lucid, candidate),
    });
    return { nowMs, snapshot, plan, intentPlan, params: options.params };
  });

/**
 * Reads the directory and the user events from the provider and plans a
 * takeover of the current shift (the CLI's read; the watchdog plans from the
 * follower's operator set and projections). The strike cites the earliest
 * user event after the directory's state-queue tail, if any.
 */
export const planTakeoverProgram = (
  lucid: LucidEvolution,
  contracts: SDK.OperatorDirectoryValidators &
    Pick<SDK.MidgardValidators, "eventHistory" | "txOrder">,
  options: Omit<Parameters<typeof planTakeoverFrom>[3], "neglectedEvent"> = {},
): Effect.Effect<
  TakeoverPlanning,
  SDK.OperatorDirectorySnapshotError | SDK.LucidError | L1SlotUnknownError,
  IntentJournal
> =>
  Effect.gen(function* () {
    // S5: the plan opens before the directory read.
    const intentPlan = yield* openPlan;
    const snapshot = yield* SDK.fetchOperatorDirectorySnapshotProgram(
      lucid,
      contracts,
    );
    const neglectedEvent = yield* SDK.fetchNeglectedUserEventProgram(
      lucid,
      contracts,
      snapshot.stateQueueTail.endTime,
    );
    return yield* planTakeoverFrom(lucid, snapshot, intentPlan, {
      ...options,
      neglectedEvent,
    });
  });

export type ReadyTakeoverPlan = Extract<
  SDK.InactivityTakeoverPlan,
  { kind: "ready" }
>;

export type StrikeSubmission = {
  readonly txHash: string;
  readonly skippedOperator: string;
  readonly newOperator: string;
  readonly struckInactivityStrikes: bigint;
  readonly tier: SDK.InactivityTakeoverTier;
};

/**
 * The ready plan re-made from `planning`'s snapshot and citation as of the L1
 * `slotNow` at build time. A plan's validity window starts from the clock it
 * was planned at, and the reads and gates between planning and building can
 * outlast the window: the ledger then refuses the strike as expired. The same
 * planner picks the bounds again, so every on-chain constraint on them holds
 * as it did when planned.
 */
const replanAtBuildTime = (
  lucid: LucidEvolution,
  planning: TakeoverPlanning & { readonly plan: ReadyTakeoverPlan },
): Effect.Effect<ReadyTakeoverPlan, SDK.SchedulerError> =>
  planTakeoverFrom(lucid, planning.snapshot, planning.intentPlan, {
    neglectedEvent: planning.plan.neglectedEvent,
    params: planning.params,
  }).pipe(
    Effect.mapError(
      (cause) =>
        new SDK.SchedulerError({
          message: "The strike needs the L1 slot at build time",
          cause,
        }),
    ),
    Effect.flatMap(({ plan }) =>
      plan.kind === "ready"
        ? Effect.succeed(plan)
        : Effect.fail(
            new SDK.SchedulerError({
              message: `The strike is no longer ready at build time (${plan.kind === "blocked" ? `blocked: ${plan.reason}` : plan.kind})`,
              cause: plan,
            }),
          ),
    ),
  );

/**
 * Builds, signs over the selected wallet's view, and submits the strike a
 * ready plan describes, with its validity window chosen at build time
 * (`replanAtBuildTime`). The wallet only pays the fee, from its view.
 */
export const submitInactivityStrikeProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  planning: TakeoverPlanning & { readonly plan: ReadyTakeoverPlan },
  options: { readonly label?: string } = {},
): Effect.Effect<StrikeSubmission, TakeoverError, IntentJournal> =>
  Effect.gen(function* () {
    const label = options.label ?? "strike-inactive-operator";
    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
    });
    const { snapshot } = planning;
    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["scheduler", "active-operators"],
    );
    const presetWalletInputs = yield* presetOperatorWalletInputs(
      label,
      yield* readSelectedWalletViewInputs(lucid, label),
    );
    // Every read is done: the window starts from the L1 time of the build.
    const plan = yield* replanAtBuildTime(lucid, planning);
    const result = yield* SDK.buildStrikeInactiveOperatorTxProgram({
      lucid,
      scheduler: contracts.scheduler,
      activeOperators: contracts.activeOperators,
      schedulerInput: snapshot.scheduler.utxo,
      hubOracleRefInput: snapshot.hubOracle.utxo,
      stateQueueTailRefInput: snapshot.stateQueueTail.utxo,
      skippedOperatorKeyHash: plan.currentOperator,
      skippedOperatorNode: plan.skippedNode,
      skippedOperatorDatum: plan.skippedOperatorDatum,
      newOperatorKeyHash: plan.newOperatorKey,
      newStartTime: plan.newStartTime,
      witnesses: plan.witnesses,
      neglectedEvent: {
        kind: plan.neglectedEvent.kind,
        utxo: plan.neglectedEvent.utxo,
      },
      validFrom: plan.validity.validFrom,
      validTo: plan.validity.validTo,
      schedulerSpendingScriptRef: scriptRefs.spending.scheduler,
      activeOperatorsSpendingScriptRef: scriptRefs.spending["active-operators"],
      presetWalletInputs,
    });
    yield* Effect.logInfo(
      `${label}: striking ${plan.currentOperator} (tier=${plan.tier}, strikes→${result.struckInactivityStrikes.toString()}, new_operator=${plan.newOperatorKey}, valid=[${plan.validity.validFrom.toString()},${plan.validity.validTo.toString()}))`,
    );
    const txHash = yield* handleSignSubmit(
      lucid,
      result.tx,
      journaledIntent(
        "takeover",
        `takeover:${plan.currentOperator}:${plan.newStartTime.toString()}`,
        planning.intentPlan,
      ),
      { label },
    );
    return {
      txHash,
      skippedOperator: plan.currentOperator,
      newOperator: plan.newOperatorKey,
      struckInactivityStrikes: result.struckInactivityStrikes,
      tier: plan.tier,
    };
  });
