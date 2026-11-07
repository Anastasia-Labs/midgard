import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { alignedUnixTimeAtOrAfter } from "../../src/transactions/operators/takeover.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../../src/transactions/reference-scripts.js";
import {
  advanceEmulatorPastUnixTime,
  alignedUnixTimeAtOrBefore,
  type InactivityOperatorAccount,
  type OperatorInactivityFixture,
  STRIKE_VALIDITY_WINDOW_MS,
} from "./operator-inactivity.build-deployment-snapshot.js";

// ---------------------------------------------------------------------------
// Directory reads and scheduler appointment
// ---------------------------------------------------------------------------

export const requirePrimaryOperator = (
  fixture: OperatorInactivityFixture,
): InactivityOperatorAccount => {
  const primary = fixture.operators[0];
  if (primary === undefined) {
    throw new Error("Fixture carries no operators");
  }
  return primary;
};

export const fetchInactivityDirectorySnapshot = (
  fixture: OperatorInactivityFixture,
): Promise<SDK.OperatorDirectorySnapshot> =>
  Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(fixture.lucid, fixture.contracts),
  );

/**
 * The undelivered user event an honest strike cites, read from the emulator
 * ledger the way a provider reader does; null on an idle network.
 */
export const fetchNeglectedUserEvent = (
  fixture: OperatorInactivityFixture,
  snapshot: SDK.OperatorDirectorySnapshot,
): Promise<SDK.NeglectedUserEventClaim | null> =>
  Effect.runPromise(
    SDK.fetchNeglectedUserEventProgram(
      fixture.lucid,
      fixture.contracts,
      snapshot.stateQueueTail.endTime,
    ),
  );

export const fetchSchedulerDatum = async (
  fixture: OperatorInactivityFixture,
): Promise<SDK.SchedulerDatum> =>
  (await fetchInactivityDirectorySnapshot(fixture)).scheduler.datum;

const requireStrikeScriptRefs = async (
  fixture: OperatorInactivityFixture,
): Promise<{
  readonly scheduler: UTxO;
  readonly activeOperators: UTxO;
}> => {
  const resolved = await Effect.runPromise(
    fetchReferenceScriptUtxosProgram(
      fixture.lucid,
      fixture.referenceScriptsAddress,
      [
        {
          name: "scheduler spending",
          script: fixture.contracts.scheduler.spendingScript,
        },
        {
          name: "active-operators spending",
          script: fixture.contracts.activeOperators.spendingScript,
        },
      ],
      fixture.contracts.referenceScriptAuth,
    ),
  );
  return {
    scheduler: referenceScriptByName(resolved, "scheduler spending"),
    activeOperators: referenceScriptByName(
      resolved,
      "active-operators spending",
    ),
  };
};

/**
 * Drives the scheduler out of `NoActiveOperators` with `AppointFirstOperator`.
 * On-chain that endpoint can only appoint the last node of the active list, so
 * the appointed operator is always the greatest key hash — the head of the
 * scheduler's descending rotation.
 */
export const appointFirstSchedulerOperator = async (
  fixture: OperatorInactivityFixture,
): Promise<{
  readonly operatorKeyHash: string;
  readonly startTime: bigint;
  readonly txHash: string;
}> => {
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  if (snapshot.scheduler.datum !== "NoActiveOperators") {
    throw new Error("The scheduler already has an appointed operator");
  }
  const activeTail = SDK.findTailNode(snapshot.active);
  const registeredWitness = SDK.findTailNode(snapshot.registered);
  if (activeTail === undefined || activeTail.datum.key === "Empty") {
    throw new Error("The active-operators list has no appointable tail node");
  }
  if (registeredWitness === undefined) {
    throw new Error("The registered-operators list has no tail element");
  }
  const operatorKeyHash = activeTail.datum.key.Key.key;
  const validFrom = alignedUnixTimeAtOrBefore(
    fixture.lucid,
    BigInt(fixture.emulator.now()),
  );
  const validTo = validFrom + STRIKE_VALIDITY_WINDOW_MS;
  const startTime = validTo - 1n;
  const scriptRefs = await requireStrikeScriptRefs(fixture);
  const { tx } = await Effect.runPromise(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: fixture.lucid,
      scheduler: fixture.contracts.scheduler,
      operatorKeyHash: requirePrimaryOperator(fixture).keyHash,
      schedulerInput: snapshot.scheduler.utxo,
      refreshedDatum: {
        ActiveOperator: { operator: operatorKeyHash, start_time: startTime },
      } as SDK.SchedulerDatum,
      validFrom,
      validTo,
      selection: {
        kind: "AppointFirst",
        activeNode: { utxo: activeTail.utxo },
        registeredWitnessNode: { utxo: registeredWitness.utxo },
      },
      schedulerSpendingScriptRef: scriptRefs.scheduler,
    }),
  );
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  return { operatorKeyHash, startTime, txHash };
};

// ---------------------------------------------------------------------------
// Strike submission
// ---------------------------------------------------------------------------

export type StrikeAttemptOptions = {
  /**
   * The event the strike cites. Omitted, the strike cites the earliest
   * undelivered user event on the ledger, and there must be one.
   */
  readonly neglectedEvent?: SDK.NeglectedUserEventClaim;
  /**
   * Timing parameters for the plan only. Passing a set that differs from the
   * deployment's compiled-in constants is how a negative test reaches an
   * on-chain check the honest planner refuses to plan towards.
   */
  readonly params?: SDK.InactivityTimingParameters;
  /**
   * Plans against a hypothetical instant instead of the emulator clock, so a
   * scenario can obtain the witnesses for a strike it then deliberately dates
   * too early.
   */
  readonly planNowMs?: bigint;
  /** Skips the wait that moves the emulator past the inactivity threshold. */
  readonly skipThresholdWait?: boolean;
  readonly validity?: SDK.InactivityTakeoverValidity;
  readonly newStartTime?: bigint;
  readonly newOperatorKeyHash?: string;
  readonly witnesses?: SDK.InactivityTakeoverWitnesses;
  readonly adversarialOverrides?: SDK.StrikeInactiveOperatorAdversarialOverrides;
};

export type PreparedStrike = {
  readonly plan: Extract<SDK.InactivityTakeoverPlan, { kind: "ready" }>;
  readonly config: SDK.BuildStrikeInactiveOperatorTxConfig;
  readonly snapshot: SDK.OperatorDirectorySnapshot;
};

/**
 * Plans the strike the scheduler's current operator has earned, advancing the
 * emulator past the inactivity threshold first unless told not to.
 */
export const prepareInactivityStrike = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<PreparedStrike> => {
  const plannedSnapshot = await fetchInactivityDirectorySnapshot(fixture);
  const current = SDK.schedulerCurrentOperator(plannedSnapshot.scheduler);
  if (current === null) {
    throw new Error("The scheduler holds no active operator to strike");
  }
  const neglectedEvent =
    options.neglectedEvent ??
    (await fetchNeglectedUserEvent(fixture, plannedSnapshot));
  if (neglectedEvent === null) {
    throw new Error(
      "No user event is undelivered, so no strike can be planned: submit a neglected event first",
    );
  }
  if (options.skipThresholdWait !== true) {
    const threshold = SDK.computeInactivityThreshold({
      shiftStartMs: current.startTime,
      stateQueueTailEndTimeMs: plannedSnapshot.stateQueueTail.endTime,
      neglectedEvent,
      params: options.params,
    });
    if (threshold.kind === "unsatisfiable") {
      throw new Error(
        `Inactivity threshold is unsatisfiable: ${threshold.detail}`,
      );
    }
    advanceEmulatorPastUnixTime(fixture.emulator, threshold.thresholdMs);
  }
  // Re-read after the clock moved: awaiting slots does not change the ledger,
  // but the snapshot is what the plan's witnesses point at.
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const plan = SDK.planInactivityTakeover({
    snapshot,
    nowMs: options.planNowMs ?? BigInt(fixture.emulator.now()),
    neglectedEvent,
    params: options.params,
    validityWindowMs: STRIKE_VALIDITY_WINDOW_MS,
    alignValidFrom: (candidate) =>
      alignedUnixTimeAtOrAfter(fixture.lucid, candidate),
  });
  if (plan.kind !== "ready") {
    throw new Error(
      `Expected a ready inactivity takeover plan, got ${plan.kind}${
        plan.kind === "blocked" ? `: ${plan.detail}` : ""
      }`,
    );
  }
  const scriptRefs = await requireStrikeScriptRefs(fixture);
  const validity = options.validity ?? plan.validity;
  return {
    plan,
    snapshot,
    config: {
      lucid: fixture.lucid,
      scheduler: fixture.contracts.scheduler,
      activeOperators: fixture.contracts.activeOperators,
      schedulerInput: snapshot.scheduler.utxo,
      hubOracleRefInput: snapshot.hubOracle.utxo,
      stateQueueTailRefInput: snapshot.stateQueueTail.utxo,
      skippedOperatorKeyHash: plan.currentOperator,
      skippedOperatorNode: plan.skippedNode,
      skippedOperatorDatum: plan.skippedOperatorDatum,
      newOperatorKeyHash: options.newOperatorKeyHash ?? plan.newOperatorKey,
      newStartTime: options.newStartTime ?? validity.validTo - 1n,
      witnesses: options.witnesses ?? plan.witnesses,
      neglectedEvent: { kind: neglectedEvent.kind, utxo: neglectedEvent.utxo },
      validFrom: validity.validFrom,
      validTo: validity.validTo,
      schedulerSpendingScriptRef: scriptRefs.scheduler,
      activeOperatorsSpendingScriptRef: scriptRefs.activeOperators,
      adversarialOverrides: options.adversarialOverrides,
    },
  };
};

export type StrikeSubmission = {
  readonly plan: Extract<SDK.InactivityTakeoverPlan, { kind: "ready" }>;
  readonly result: SDK.StrikeInactiveOperatorTxResult;
  readonly txHash: string;
};

export const submitInactivityStrike = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<StrikeSubmission> => {
  const prepared = await prepareInactivityStrike(fixture, options);
  const result = await Effect.runPromise(
    SDK.buildStrikeInactiveOperatorTxProgram(prepared.config),
  );
  const signed = await result.tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  return { plan: prepared.plan, result, txHash };
};

/**
 * Builder pre-flight failures (a missing witness, an ambiguous output
 * selector) look nothing like a validator refusal, so a negative test has to
 * be able to tell them apart.
 */
export const BUILDER_PREFLIGHT_MARKERS = [
  "expected exactly one matching redeemer purpose",
  "is missing from final tx inputs",
  "is missing from final tx reference inputs",
  "output selector matched multiple outputs",
  "output is missing from final tx outputs",
  "expected own spend purpose",
  "expected exactly one",
  "resolved inconsistent",
] as const;
