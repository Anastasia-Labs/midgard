import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { detectTransitionTraceFaults } from "../src/transition-trace/detect.js";
import {
  phaseForStepIndex,
  traceStepPhaseFault,
} from "../src/transition-trace/phase-band.js";
import { buildForcedAndL2Block } from "./support/transition-trace-phase-band.forced-and-l2-block.js";
import {
  buildPayloadFixture,
  depositEventKey,
  forcedEventKey,
  reconstruct,
  withdrawalEventKey,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  eventToStepEntry,
  forcedTx,
  outRef,
  withdrawalInfo,
} from "./transition-trace-challenger.native-material.js";

const counts = (
  withdrawalCount: bigint,
  forcedTransactionCount: bigint,
  l2TransactionCount: bigint,
  depositCount: bigint,
) => {
  const total =
    withdrawalCount +
    forcedTransactionCount +
    l2TransactionCount +
    depositCount;
  return {
    withdrawalCount,
    forcedTransactionCount,
    l2TransactionCount,
    depositCount,
    totalEventCount: total,
    transitionStepCount: total,
  };
};

const bandIssues = (
  detections: Awaited<ReturnType<typeof detectTransitionTraceFaults>>,
) =>
  detections.filter(
    ({ invariant }) =>
      invariant === "event_to_step_phase_band" ||
      invariant === "trace_phase_matches_event_key",
  );

describe("transition-trace phase bands (twin of proof.ak phase_for_step_index)", () => {
  it("places every step index in its header band, and nothing outside the event range", () => {
    const header = counts(2n, 1n, 2n, 1n);
    expect(
      [-1n, 0n, 1n, 2n, 3n, 4n, 5n, 6n].map((index) =>
        phaseForStepIndex(header, index),
      ),
    ).toEqual([
      undefined,
      "Withdrawal",
      "Withdrawal",
      "ForcedTransaction",
      "L2Transaction",
      "L2Transaction",
      "Deposit",
      undefined,
    ]);
    // Empty bands are skipped: with no withdrawals or forced transactions,
    // step 0 is already in the L2 band.
    expect(phaseForStepIndex(counts(0n, 0n, 1n, 1n), 0n)).toBe("L2Transaction");
    expect(phaseForStepIndex(counts(0n, 0n, 1n, 1n), 1n)).toBe("Deposit");
    expect(phaseForStepIndex(counts(0n, 0n, 0n, 0n), 0n)).toBeUndefined();
    // The on-chain function fails its expects on a negative count and past
    // the last deposit even when total_event_count is larger.
    expect(
      phaseForStepIndex({ ...counts(1n, 0n, 0n, 0n), depositCount: -1n }, 0n),
    ).toBeUndefined();
    expect(
      phaseForStepIndex({ ...counts(1n, 0n, 0n, 0n), totalEventCount: 3n }, 1n),
    ).toBeUndefined();
  });

  it("reports a band violation before an event-key phase violation, and neither outside the bands", () => {
    const header = counts(1n, 0n, 0n, 1n);
    const step = (
      step_index: bigint,
      phase: SDK.TransitionPhase,
      event_key: SDK.EventKey,
    ) => ({ step_index, phase, event_key });
    const withdrawal = withdrawalEventKey(outRef(1));
    const deposit = depositEventKey(outRef(2));
    expect(
      traceStepPhaseFault(header, step(0n, "Withdrawal", withdrawal)),
    ).toBeUndefined();
    expect(
      traceStepPhaseFault(header, step(1n, "Deposit", deposit)),
    ).toBeUndefined();
    expect(
      traceStepPhaseFault(header, step(0n, "Deposit", deposit))?.invariant,
    ).toBe("event_to_step_phase_band");
    expect(
      traceStepPhaseFault(header, step(1n, "Deposit", withdrawal))?.invariant,
    ).toBe("trace_phase_matches_event_key");
    expect(
      traceStepPhaseFault(header, step(2n, "Deposit", withdrawal)),
    ).toBeUndefined();
  });

  it("proves an L2 transaction stepped ahead of a forced transaction while event_to_step agrees", async () => {
    const block = await buildForcedAndL2Block({ forcedLast: true });
    const steps = [...block.reconstruction.transitionTrace]
      .map(({ value }) => value)
      .sort((left, right) => Number(left.step_index - right.step_index));
    expect(steps.map(({ phase }) => phase)).toEqual([
      "L2Transaction",
      "ForcedTransaction",
    ]);
    for (const step of steps) {
      const mapped = block.reconstruction.eventToStep.find(
        ({ key }) =>
          Data.to(key, SDK.EventKey) === Data.to(step.event_key, SDK.EventKey),
      );
      expect(mapped?.value).toEqual({
        step_index: step.step_index,
        phase: step.phase,
      });
    }

    const detections = await detectTransitionTraceFaults(block.reconstruction);
    expect(
      detections.map(({ kind, invariant, buildable }) => ({
        kind,
        invariant,
        buildable,
      })),
    ).toEqual(
      [0, 1].map(() => ({
        kind: "eventToStepMismatch",
        invariant: "event_to_step_phase_band",
        buildable: true,
      })),
    );
    for (const detection of detections) {
      if (!detection.buildable) throw new Error("unbuildable band detection");
      if (!("EventToStepMismatch" in detection.fault)) {
        throw new Error("band detection is not an EventToStepMismatch");
      }
      expect(
        "EventToStepMembership" in
          detection.fault.EventToStepMismatch.event_to_step,
      ).toBe(true);
    }
  });

  it("proves a trace step whose phase disagrees with its own event key", async () => {
    const withdrawalKey = withdrawalEventKey(outRef(5));
    const fixture = await buildPayloadFixture({
      deposits: [
        encodedEntry({
          key: outRef(6),
          keySchema: SDK.OutputReference as never,
          value: depositInfo(6),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: withdrawalKey,
          phase: "Deposit",
          pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
      eventToStep: [
        eventToStepEntry(withdrawalKey, { step_index: 0n, phase: "Deposit" }),
      ],
    });
    const detections = await detectTransitionTraceFaults(
      await reconstruct(fixture),
    );
    expect(
      bandIssues(detections).map(({ kind, invariant }) => ({
        kind,
        invariant,
      })),
    ).toEqual([
      {
        kind: "eventToStepMismatch",
        invariant: "trace_phase_matches_event_key",
      },
    ]);
  });

  it("stays silent on honest blocks in canonical phase order", async () => {
    const honestForcedAndL2 = await buildForcedAndL2Block({
      forcedLast: false,
    });
    expect(
      await detectTransitionTraceFaults(honestForcedAndL2.reconstruction),
    ).toEqual([]);

    const withdrawalId = outRef(11);
    const txOrderId = outRef(12);
    const depositId = outRef(13);
    const events = [
      [withdrawalEventKey(withdrawalId), "Withdrawal"],
      [forcedEventKey(txOrderId), "ForcedTransaction"],
      [depositEventKey(depositId), "Deposit"],
    ] as const;
    const multiPhase = await buildPayloadFixture({
      withdrawals: [
        encodedEntry({
          key: withdrawalId,
          keySchema: SDK.OutputReference as never,
          value: withdrawalInfo(20),
          valueSchema: SDK.WithdrawalInfoSchema,
        }),
      ],
      forcedTransactions: [
        encodedEntry({
          key: txOrderId,
          keySchema: SDK.OutputReference as never,
          value: forcedTx(30),
          valueSchema: SDK.ForcedInclusionTxV1Schema,
        }),
      ],
      deposits: [
        encodedEntry({
          key: depositId,
          keySchema: SDK.OutputReference as never,
          value: depositInfo(40),
          valueSchema: SDK.DepositInfoSchema,
        }),
      ],
      steps: events.map(([event_key, phase], index) => ({
        schema_version: 1n,
        step_index: BigInt(index),
        event_key,
        phase,
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      })),
      eventToStep: events.map(([eventKey, phase], index) =>
        eventToStepEntry(eventKey, { step_index: BigInt(index), phase }),
      ),
    });
    expect(
      await detectTransitionTraceFaults(await reconstruct(multiPhase)),
    ).toEqual([]);
  });
});
