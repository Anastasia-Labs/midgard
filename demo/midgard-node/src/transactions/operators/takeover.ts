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

import { alignedUnixTimeStrictlyAfter } from "../../workers/utils/commit-end-time.js";
import { currentTimeMsForLucidOrEmulatorFallback } from "../register-active-operator/clock.js";
import { handleSignSubmit } from "../utils.js";
import {
  OPERATOR_TX_VALIDITY_WINDOW_MS,
  type OperatorExitError,
  resolveOperatorScriptRefsProgram,
} from "./exit.js";
import { requireOperatorFundingProgram } from "./funding-preflight.js";

export type TakeoverError = OperatorExitError | SDK.SchedulerError;

/**
 * The payment key hash of the wallet currently selected in `lucid`.
 */
export const resolveOwnOperatorKeyHashProgram = (
  lucid: LucidEvolution,
): Effect.Effect<string, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const credential = paymentCredentialOf(address);
    if (credential.type !== "Key") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Operator wallet must use a payment key credential",
          cause: address,
        }),
      );
    }
    return credential.hash;
  });

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
};

/**
 * Reads the directory and plans a takeover of the current shift as of the
 * chain clock. `neglectedEvent` narrows the threshold to a specific neglected
 * user event when the caller has one.
 */
export const planTakeoverProgram = (
  lucid: LucidEvolution,
  contracts: SDK.OperatorDirectoryValidators,
  options: {
    readonly neglectedEvent?: SDK.NeglectedUserEventClaim;
    readonly params?: SDK.InactivityTimingParameters;
    readonly nowMs?: bigint;
  } = {},
): Effect.Effect<TakeoverPlanning, SDK.OperatorDirectorySnapshotError> =>
  Effect.gen(function* () {
    const snapshot = yield* SDK.fetchOperatorDirectorySnapshotProgram(
      lucid,
      contracts,
    );
    const nowMs =
      options.nowMs ?? currentTimeMsForLucidOrEmulatorFallback(lucid);
    const plan = SDK.planInactivityTakeover({
      snapshot,
      nowMs,
      neglectedEvent: options.neglectedEvent,
      params: options.params,
      validityWindowMs: OPERATOR_TX_VALIDITY_WINDOW_MS,
      alignValidFrom: (candidate) => alignedUnixTimeAtOrAfter(lucid, candidate),
    });
    return { nowMs, snapshot, plan };
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
 * Builds, signs with the selected wallet, and submits the strike a ready plan
 * describes. The wallet only pays the fee.
 */
export const submitInactivityStrikeProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  planning: TakeoverPlanning & { readonly plan: ReadyTakeoverPlan },
  options: { readonly label?: string } = {},
): Effect.Effect<StrikeSubmission, TakeoverError> =>
  Effect.gen(function* () {
    const label = options.label ?? "strike-inactive-operator";
    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
    });
    const { plan, snapshot } = planning;
    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["scheduler", "active-operators"],
    );
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
      neglectedEvent:
        plan.neglectedEvent === undefined
          ? undefined
          : { kind: plan.neglectedEvent.kind, utxo: plan.neglectedEvent.utxo },
      validFrom: plan.validity.validFrom,
      validTo: plan.validity.validTo,
      schedulerSpendingScriptRef: scriptRefs.spending.scheduler,
      activeOperatorsSpendingScriptRef: scriptRefs.spending["active-operators"],
    });
    yield* Effect.logInfo(
      `${label}: striking ${plan.currentOperator} (tier=${plan.tier}, strikes→${result.struckInactivityStrikes.toString()}, new_operator=${plan.newOperatorKey}, valid=[${plan.validity.validFrom.toString()},${plan.validity.validTo.toString()}))`,
    );
    const txHash = yield* handleSignSubmit(lucid, result.tx, { label });
    return {
      txHash,
      skippedOperator: plan.currentOperator,
      newOperator: plan.newOperatorKey,
      struckInactivityStrikes: result.struckInactivityStrikes,
      tier: plan.tier,
    };
  });
