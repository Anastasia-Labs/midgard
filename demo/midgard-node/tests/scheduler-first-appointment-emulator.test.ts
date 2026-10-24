/**
 * First scheduler appointment (`AppointFirstOperator`) against the real
 * compiled scheduler validator, driven by the node's own window and start-time
 * resolution.
 *
 * The commit target the node appoints towards is `Date.now() + buffer`, which
 * falls mid-slot. The ledger carries a transaction's upper bound as a slot
 * (Lucid floors a millisecond validTo to its enclosing slot), and the validator
 * requires the appointed `start_time` to equal the inclusive upper bound it
 * sees. Both polarities below spend the same scheduler with the same validity
 * interval and differ only in the output datum's `start_time`, so the only
 * on-chain check that can separate them is
 * `validate_unscheduled_new_shifts_start_time` (6g).
 *
 * The emulator does not run phase-2 scripts on submit; `localUPLCEval` runs the
 * deployed scheduler script while the transaction is completed, so a refusal
 * surfaces as a `failed script execution Spend[n]` build error.
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
} from "../src/transactions/reference-scripts.js";
import { COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/workers/utils/commit-end-time.js";
import {
  captureSchedulerSlotSnapshot,
  resolveRefreshedSchedulerStartTime,
  resolveSchedulerFirstAppointmentValidityWindow,
  resolveSchedulerRefreshWitnessSelection,
  schedulerStateCoversCommitTarget,
} from "../src/workers/utils/scheduler-refresh.js";
import {
  fetchInactivityDirectorySnapshot,
  fetchSchedulerDatum,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
} from "./helpers/operator-inactivity.js";

/** Mid-slot offset of the live commit target that exposed the defect. */
const MID_SLOT_OFFSET_MS = 327;

type FirstAppointmentPlan = {
  readonly operatorKeyHash: string;
  readonly targetCommitEndTime: bigint;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly startTime: bigint;
  readonly selection: SDK.SchedulerRefreshWitnessSelection;
  readonly schedulerInput: UTxO;
  readonly schedulerSpendingScriptRef: UTxO;
};

const planFirstAppointment = async (
  fixture: OperatorInactivityFixture,
): Promise<FirstAppointmentPlan> => {
  const operator = fixture.operators[0];
  if (operator === undefined) {
    throw new Error("Fixture carries no operators");
  }
  // The first-appointment window backdates validFrom by 30 s. Lucid anchors an
  // emulator's slot mapping at the instant the instance was created, and the
  // evaluator refuses a slot before that anchor, so let the clock run first.
  fixture.emulator.awaitSlot(60);
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  expect(snapshot.scheduler.datum).toBe("NoActiveOperators");

  const slotSnapshot = captureSchedulerSlotSnapshot(fixture.lucid);
  const targetCommitEndTime = BigInt(
    fixture.emulator.now() +
      COMMIT_MINIMUM_FUTURE_BUFFER_MS +
      MID_SLOT_OFFSET_MS,
  );
  const selection = resolveSchedulerRefreshWitnessSelection({
    currentOperator: "",
    targetOperator: operator.keyHash,
    activeNodes: snapshot.active,
    registeredNodes: snapshot.registered,
    allowGenesisRewind: true,
  });
  if (selection.kind !== "AppointFirst") {
    throw new Error(`Expected AppointFirst, got ${selection.kind}`);
  }
  const { validFrom, validTo } = resolveSchedulerFirstAppointmentValidityWindow(
    fixture.lucid,
    targetCommitEndTime,
    slotSnapshot,
  );
  const startTime = resolveRefreshedSchedulerStartTime({
    selection,
    currentSchedulerState: undefined,
    validFrom,
    validTo,
  });
  const scriptRefs = await Effect.runPromise(
    fetchReferenceScriptUtxosProgram(
      fixture.lucid,
      fixture.referenceScriptsAddress,
      [
        {
          name: "scheduler spending",
          script: fixture.contracts.scheduler.spendingScript,
        },
      ],
      fixture.contracts.referenceScriptAuth,
    ),
  );
  return {
    operatorKeyHash: operator.keyHash,
    targetCommitEndTime,
    validFrom,
    validTo,
    startTime,
    selection: {
      kind: "AppointFirst",
      activeNode: { utxo: selection.activeNode.utxo },
      registeredWitnessNode: { utxo: selection.registeredWitnessNode.utxo },
    },
    schedulerInput: snapshot.scheduler.utxo,
    schedulerSpendingScriptRef: referenceScriptByName(
      scriptRefs,
      "scheduler spending",
    ),
  };
};

const buildFirstAppointment = (
  fixture: OperatorInactivityFixture,
  plan: FirstAppointmentPlan,
  overrides: { readonly validTo?: bigint; readonly startTime?: bigint } = {},
) =>
  Effect.runPromise(
    SDK.buildUnsignedSchedulerRefreshTxProgram({
      lucid: fixture.lucid,
      scheduler: fixture.contracts.scheduler,
      operatorKeyHash: plan.operatorKeyHash,
      schedulerInput: plan.schedulerInput,
      refreshedDatum: {
        ActiveOperator: {
          operator: plan.operatorKeyHash,
          start_time: overrides.startTime ?? plan.startTime,
        },
      },
      validFrom: plan.validFrom,
      validTo: overrides.validTo ?? plan.validTo,
      selection: plan.selection,
      schedulerSpendingScriptRef: plan.schedulerSpendingScriptRef,
    }),
  );

const expectSchedulerScriptRefusal = async (
  attempt: Promise<unknown>,
): Promise<void> => {
  let message: string | undefined;
  try {
    await attempt;
  } catch (cause) {
    message = String(
      cause instanceof Error ? (cause.stack ?? cause.message) : cause,
    );
  }
  if (message === undefined) {
    throw new Error("Expected the scheduler appointment to be refused");
  }
  // The scheduler input is the only script input; a refusal anywhere else,
  // or a builder pre-flight guard, would not carry this marker.
  expect(message).toMatch(/failed script execution\s+Spend\[\d+\]/);
  expect(message).not.toContain("output is missing from final tx outputs");
};

describe("scheduler first appointment on the real scheduler validator", () => {
  it("appoints the first operator for a mid-slot commit target", async () => {
    const fixture = await initOperatorInactivityFixture(1);
    const plan = await planFirstAppointment(fixture);

    const built = await buildFirstAppointment(fixture, plan);
    const signed = await built.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await fixture.lucid.awaitTx(txHash);

    // The emulator clock sits on a slot boundary and the buffer is whole
    // seconds, so the target is exactly MID_SLOT_OFFSET_MS past a boundary and
    // validTo is the boundary the ledger presents.
    expect(plan.targetCommitEndTime - plan.validTo).toBe(
      BigInt(MID_SLOT_OFFSET_MS),
    );
    expect(plan.validTo).toBe(
      BigInt(
        fixture.lucid.slotToUnixTime(
          fixture.lucid.unixTimeToSlot(Number(plan.validTo)),
        ),
      ),
    );

    expect(await fetchSchedulerDatum(fixture)).toEqual({
      ActiveOperator: {
        operator: plan.operatorKeyHash,
        start_time: plan.startTime,
      },
    });
    expect(
      schedulerStateCoversCommitTarget({
        currentSchedulerState: {
          operator: plan.operatorKeyHash,
          startTime: plan.startTime,
        },
        operatorKeyHash: plan.operatorKeyHash,
        targetStartTime: plan.targetCommitEndTime,
      }),
    ).toBe(true);
  });

  it("refuses a start_time derived from the unaligned commit target", async () => {
    const fixture = await initOperatorInactivityFixture(1);
    const plan = await planFirstAppointment(fixture);

    // The pre-fix shape: validTo is the raw mid-slot target and start_time is
    // `validTo - 1`. Lucid floors that validTo to the honest plan's slot, so
    // the transaction differs from the honest one only in start_time.
    expect(fixture.lucid.unixTimeToSlot(Number(plan.targetCommitEndTime))).toBe(
      fixture.lucid.unixTimeToSlot(Number(plan.validTo)),
    );
    await expectSchedulerScriptRefusal(
      buildFirstAppointment(fixture, plan, {
        validTo: plan.targetCommitEndTime,
        startTime: plan.targetCommitEndTime - 1n,
      }),
    );

    // Either side of the inclusive upper bound is refused as well.
    await expectSchedulerScriptRefusal(
      buildFirstAppointment(fixture, plan, { startTime: plan.startTime - 1n }),
    );
    await expectSchedulerScriptRefusal(
      buildFirstAppointment(fixture, plan, { startTime: plan.startTime + 1n }),
    );

    expect(await fetchSchedulerDatum(fixture)).toBe("NoActiveOperators");
  });
});
