import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  activeOperatorNodeUnit,
  advanceEmulatorPastUnixTime,
  alignedUnixTimeAtOrBefore,
  appointFirstSchedulerOperator,
  expectInactivityStrikeRefusal,
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  prepareInactivityStrike,
  STRIKE_VALIDITY_WINDOW_MS,
  submitInactivityStrike,
  submitNeglectedDeposit,
  submitNeglectedWithdrawal,
  submitUnauthenticatedHistoryNodeCopy,
} from "./helpers/operator-inactivity.js";

export const EMULATOR_REQUIRED_BOND_LOVELACE = 900_000_000n;

export const requireOperator = (
  fixture: OperatorInactivityFixture,
  index: number,
): string => {
  const operator = fixture.operators[index];
  if (operator === undefined) {
    throw new Error(`Fixture has no operator at index ${index}`);
  }
  return operator.keyHash;
};

export const activeNodeDatum = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<SDK.ActiveOperatorDatum> => {
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const node = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
  if (node === undefined || node.active === null) {
    throw new Error(`Operator ${operatorKeyHash} has no active node`);
  }
  return node.active;
};

export const activeNodeUtxo = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<UTxO> => {
  const [utxo] = await fixture.lucid.utxosAtWithUnit(
    fixture.contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit(fixture.contracts, operatorKeyHash),
  );
  if (utxo === undefined) {
    throw new Error(`Operator ${operatorKeyHash} has no active node UTxO`);
  }
  return utxo;
};

export const requireActiveOperatorShift = async (
  fixture: OperatorInactivityFixture,
): Promise<{ readonly operator: string; readonly startTime: bigint }> => {
  const datum = await fetchSchedulerDatum(fixture);
  if (datum === "NoActiveOperators") {
    throw new Error("The scheduler holds no active operator");
  }
  return {
    operator: datum.ActiveOperator.operator,
    startTime: datum.ActiveOperator.start_time,
  };
};

/**
 * The deployed validators carry no traces, so a refusal is pinned by where
 * and how it failed. The strike spends two scripts, so the failing `Spend[n]`
 * must be the scheduler's own input: a crash in the active-operators spend
 * would otherwise mask a scheduler that accepted the shape. And it must be a
 * failed `expect` (the validator crashed), not a builtin deserialisation
 * failure, which is how the pre-fix decoder crashed on any history node.
 */
const expectSchedulerRefusal = (
  message: string,
  schedulerInputIndex: bigint,
): void => {
  expect(message).toMatch(
    new RegExp(
      `failed script execution Spend\\[${schedulerInputIndex.toString()}\\] the validator crashed`,
    ),
  );
  expect(message).not.toMatch(/failed to deserialise/);
};

/** The honest strike's layout, built at the current ledger state. */
const honestStrikeLayout = async (
  fixture: OperatorInactivityFixture,
  neglectedEvent: SDK.NeglectedUserEventClaim,
): Promise<SDK.StrikeInactiveOperatorLayout> => {
  const honest = await prepareInactivityStrike(fixture, { neglectedEvent });
  const { layout } = await Effect.runPromise(
    SDK.buildStrikeInactiveOperatorTxProgram(honest.config),
  );
  expect(layout.neglectedUserEventRefInputIndex).not.toBe(undefined);
  return layout;
};

/**
 * A neglected deposit or withdrawal licenses a strike from its event-history
 * Order node's `inclusion_time` plus the negligence timeout, and only from an
 * authentic node of its own history list. Each refusal differs from the
 * honest strike in exactly one respect, so the honest success is what ties
 * the refusal to that respect.
 */
export const expectNeglectedEventStrike = async (
  kind: "Deposit" | "Withdrawal",
): Promise<void> => {
  const fixture = await initOperatorInactivityFixture(3);
  const appointed = await appointFirstSchedulerOperator(fixture);
  // The event goes in straight after the appointment on purpose: its
  // threshold (event validTo + event wait + negligence) must fall before the
  // shift ends (appointment validTo + shift). Under preprod-testing that
  // leaves only a few minutes, so extra emulator time between the two calls
  // can make the neglected threshold unsatisfiable.
  const neglected =
    kind === "Deposit"
      ? await submitNeglectedDeposit(fixture)
      : await submitNeglectedWithdrawal(fixture);

  // Under every shipped profile a neglected event can never license an
  // *earlier* strike than the plain commitment gap: on-chain `inclusion_time
  // >= last_state_queue_elements_end_time` and
  // `user_events_negligence_timeout (1_200_000) >=
  // max_inactivity_between_block_commitments (1_200_000)`, so the neglected
  // term dominates. This event is therefore the binding term.
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  expect(neglected.inclusionTimeMs).toBeGreaterThanOrEqual(
    snapshot.stateQueueTail.endTime,
  );
  const withEvent = SDK.computeInactivityThreshold({
    shiftStartMs: appointed.startTime,
    stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
    neglectedEvent: neglected,
  });
  const withoutEvent = SDK.computeInactivityThreshold({
    shiftStartMs: appointed.startTime,
    stateQueueTailEndTimeMs: snapshot.stateQueueTail.endTime,
  });
  if (withEvent.kind !== "threshold" || withoutEvent.kind !== "threshold") {
    throw new Error("Both thresholds should be satisfiable");
  }
  expect(withEvent.thresholdMs).toBeGreaterThanOrEqual(
    withoutEvent.thresholdMs,
  );
  expect(withEvent.source).toBe("neglected-user-event");

  // Too early: dated at the last slot at or before the event's threshold.
  // That instant is past the plain commitment gap's threshold, the shift's
  // grace period and the event's own inclusion time, so only the negligence
  // window added to `inclusion_time` refuses it. This must run before any
  // step below moves the emulator past the threshold.
  const earlyValidFrom = alignedUnixTimeAtOrBefore(
    fixture.lucid,
    withEvent.thresholdMs,
  );
  expect(earlyValidFrom).toBeGreaterThan(neglected.inclusionTimeMs);
  expect(earlyValidFrom).toBeGreaterThan(withoutEvent.thresholdMs);
  advanceEmulatorPastUnixTime(fixture.emulator, earlyValidFrom - 3_000n);
  expect(BigInt(fixture.emulator.now())).toBeLessThanOrEqual(
    withEvent.thresholdMs,
  );
  const earlyRefusal = await expectInactivityStrikeRefusal(fixture, {
    neglectedEvent: neglected,
    skipThresholdWait: true,
    // Plans as if the threshold had passed, purely to obtain the witnesses,
    // then dates the range where it really is.
    planNowMs: withEvent.thresholdMs + 2n,
    validity: {
      validFrom: earlyValidFrom,
      validTo: earlyValidFrom + STRIKE_VALIDITY_WINDOW_MS,
    },
  });

  // The wallet is unchanged since the early attempt, so the honest build
  // selects the same inputs and its scheduler index is that attempt's too.
  const honestLayout = await honestStrikeLayout(fixture, neglected);
  expectSchedulerRefusal(earlyRefusal, honestLayout.schedulerInputIndex);

  // Mis-indexed: the redeemer points the validator at the hub oracle, which
  // is neither at the history list's address nor under its policy.
  expectSchedulerRefusal(
    await expectInactivityStrikeRefusal(fixture, {
      neglectedEvent: neglected,
      adversarialOverrides: {
        neglectedUserEventRefInputIndex: honestLayout.hubOracleRefInputIndex,
      },
    }),
    honestLayout.schedulerInputIndex,
  );

  // Wrong family: the same authentic node claimed under the other variant,
  // which authenticates against the other history list's policy and address.
  expectSchedulerRefusal(
    await expectInactivityStrikeRefusal(fixture, {
      neglectedEvent: {
        ...neglected,
        kind: kind === "Deposit" ? "Withdrawal" : "Deposit",
      },
    }),
    honestLayout.schedulerInputIndex,
  );

  // Unauthenticated: the same datum at the same address, without the NFT.
  // Submitting the copy changes the wallet, so the index is re-derived.
  const forged = await submitUnauthenticatedHistoryNodeCopy(fixture, neglected);
  const forgedStateLayout = await honestStrikeLayout(fixture, neglected);
  expectSchedulerRefusal(
    await expectInactivityStrikeRefusal(fixture, { neglectedEvent: forged }),
    forgedStateLayout.schedulerInputIndex,
  );

  const submission = await submitInactivityStrike(fixture, {
    neglectedEvent: neglected,
  });
  expect(submission.plan.thresholdSource).toBe("neglected-user-event");
  expect(submission.plan.neglectedEvent?.utxo.txHash).toBe(
    neglected.utxo.txHash,
  );
  expect(submission.result.layout.neglectedUserEventRefInputIndex).not.toBe(
    undefined,
  );
  expect(submission.result.struckInactivityStrikes).toBe(1n);
  expect((await requireActiveOperatorShift(fixture)).operator).toBe(
    requireOperator(fixture, 1),
  );
};
