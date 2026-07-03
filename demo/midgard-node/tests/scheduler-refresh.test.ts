import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  captureSchedulerSlotSnapshot,
  filterLocallyConsumedUtxos,
  type NodeUtxoWithDatum,
  requireExistingSchedulerWitnessUtxo,
  resolveSchedulerFirstAppointmentValidityWindow,
  resolveSchedulerRefreshValidityWindow,
  resolveSchedulerRefreshWitnessSelection,
  SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS,
  schedulerRefreshDueWorkFromSubmitTiming,
  schedulerSlotSnapshotFromSubmitSlot,
} from "@/workers/utils/scheduler-refresh.js";

const mkUtxo = (txHash: string, outputIndex: number): UTxO =>
  ({
    txHash,
    outputIndex,
    address: "addr_test1vr0dummy",
    assets: { lovelace: 5_000_000n },
    datum: undefined,
    datumHash: undefined,
    scriptRef: undefined,
  }) as UTxO;

const mkNode = (
  txHash: string,
  outputIndex: number,
  datum: SDK.LinkedListNodeView,
): NodeUtxoWithDatum => ({
  utxo: mkUtxo(txHash, outputIndex),
  datum,
});

const customSlotLucid = {
  config: () => ({
    network: "Custom",
    provider: { time: 0, slot: 0 },
  }),
  unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
};

const schedulerStartForShiftBoundary = (shiftBoundaryMs: number): bigint =>
  BigInt(shiftBoundaryMs) - SDK.SHIFT_DURATION_MS;

describe("scheduler refresh witness selection", () => {
  const activeRoot = mkNode("00".repeat(32), 0, {
    key: "Empty",
    next: { Key: { key: "aa" } },
    data: "00" as SDK.LinkedListNodeView["data"],
  });
  const activeHead = mkNode("11".repeat(32), 0, {
    key: { Key: { key: "aa" } },
    next: { Key: { key: "bb" } },
    data: "00" as SDK.LinkedListNodeView["data"],
  });
  const activeTail = mkNode("22".repeat(32), 0, {
    key: { Key: { key: "bb" } },
    next: "Empty",
    data: "00" as SDK.LinkedListNodeView["data"],
  });

  it("selects Advance when the target operator precedes the current operator", () => {
    const selection = resolveSchedulerRefreshWitnessSelection({
      currentOperator: "bb",
      targetOperator: "aa",
      activeNodes: [activeRoot, activeHead, activeTail],
      registeredNodes: [activeRoot],
      allowGenesisRewind: false,
    });

    expect(selection.kind).toBe("Advance");
    expect(selection.activeNode.utxo.txHash).toBe(activeHead.utxo.txHash);
  });

  it("selects Rewind when the current operator is the active head and the target is the tail", () => {
    const registeredRoot = mkNode("33".repeat(32), 0, {
      key: "Empty",
      next: { Key: { key: "cc" } },
      data: "00" as SDK.LinkedListNodeView["data"],
    });
    const registeredTail = mkNode("44".repeat(32), 0, {
      key: { Key: { key: "cc" } },
      next: "Empty",
      data: "00" as SDK.LinkedListNodeView["data"],
    });

    const selection = resolveSchedulerRefreshWitnessSelection({
      currentOperator: "aa",
      targetOperator: "bb",
      activeNodes: [activeRoot, activeHead, activeTail],
      registeredNodes: [registeredRoot, registeredTail],
      allowGenesisRewind: false,
    });

    expect(selection.kind).toBe("Rewind");
    if (selection.kind !== "Rewind") {
      throw new Error("expected rewind selection");
    }
    expect(selection.activeRootNode.utxo.txHash).toBe(activeRoot.utxo.txHash);
    expect(selection.registeredWitnessNode.utxo.txHash).toBe(
      registeredTail.utxo.txHash,
    );
  });

  it("selects AppointFirst from the canonical empty scheduler operator", () => {
    const registeredRoot = mkNode("55".repeat(32), 0, {
      key: "Empty",
      next: "Empty",
      data: "00" as SDK.LinkedListNodeView["data"],
    });

    const selection = resolveSchedulerRefreshWitnessSelection({
      currentOperator: "",
      targetOperator: "bb",
      activeNodes: [activeRoot, activeHead, activeTail],
      registeredNodes: [registeredRoot],
      allowGenesisRewind: true,
    });

    expect(selection.kind).toBe("AppointFirst");
    if (selection.kind !== "AppointFirst") {
      throw new Error("expected appoint-first selection");
    }
    expect(selection.activeNode.utxo.txHash).toBe(activeTail.utxo.txHash);
    expect(selection.registeredWitnessNode.utxo.txHash).toBe(
      registeredRoot.utxo.txHash,
    );
  });

  it("rejects an operator that is not next in schedule order", () => {
    expect(() =>
      resolveSchedulerRefreshWitnessSelection({
        currentOperator: "bb",
        targetOperator: "bb",
        activeNodes: [activeRoot, activeHead, activeTail],
        registeredNodes: [activeRoot],
        allowGenesisRewind: false,
      }),
    ).toThrow("cannot rewind scheduler");
  });

  it("encodes scheduler datums with a definite root array for deployed validators", () => {
    expect(
      SDK.encodeSchedulerDatumForChain({
        ActiveOperator: {
          operator: "aa",
          start_time: 42n,
        },
      } satisfies SDK.SchedulerDatum),
    ).toBe("d87a8241aa182a");
  });

  it("rejects missing scheduler roots instead of bootstrapping during commit witness resolution", async () => {
    const result = await Effect.runPromise(
      Effect.either(requireExistingSchedulerWitnessUtxo([], "scheduler-unit")),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toContain("Incomplete protocol deployment");
      expect(result.left.message).toContain("refusing commit-time scheduler");
    }
  });

  it("filters active-operator candidates consumed by a submitted scheduler refresh", () => {
    const stale = mkUtxo("66".repeat(32), 0);
    const fresh = mkUtxo("77".repeat(32), 1);

    expect(
      filterLocallyConsumedUtxos(
        [stale, fresh],
        [`${stale.txHash}#${stale.outputIndex}`],
      ),
    ).toEqual([fresh]);
  });

  it("captures a single current-slot snapshot for scheduler validity resolution", async () => {
    const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([operator]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);

    const snapshot = captureSchedulerSlotSnapshot(lucid, 1_779_150_000_000);

    expect(snapshot.currentSlot).toBe(lucid.currentSlot());
    expect(snapshot.currentSlotStartMs).toEqual(expect.any(Number));
    expect(snapshot.observedAtMs).toBe(1_779_150_000_000);
  });

  it("captures production scheduler slots from local submit-ledger snapshots", () => {
    const lucid = {
      currentSlot: () => {
        throw new Error("wall-clock Lucid slot must not be used");
      },
      config: () => ({
        network: "Custom",
        provider: { time: 20_000, slot: 20 },
      }),
    };

    const snapshot = schedulerSlotSnapshotFromSubmitSlot(lucid as never, {
      source: "local_ogmios_tip",
      currentSlot: 12,
      observedAtMs: 1_779_149_999_000,
      slotLengthMs: 1_000,
    });

    expect(snapshot).toEqual({
      currentSlot: 12,
      currentSlotStartMs: 12_000,
      observedAtMs: 1_779_149_999_000,
    });
  });

  it("backdates refresh validity when the scheduler shift boundary is already mature", () => {
    const window = resolveSchedulerRefreshValidityWindow(
      customSlotLucid as never,
      schedulerStartForShiftBoundary(40_000),
      {
        currentSlot: 100,
        currentSlotStartMs: 100_000,
        observedAtMs: 100_500,
      },
    );

    expect(window.validFrom).toBe(70_000n);
    expect(window.validTo - window.validFrom).toBe(8n * 60n * 1000n);
  });

  it("does not backdate refresh validity before the scheduler shift boundary", () => {
    const window = resolveSchedulerRefreshValidityWindow(
      customSlotLucid as never,
      schedulerStartForShiftBoundary(95_500),
      {
        currentSlot: 100,
        currentSlotStartMs: 100_000,
        observedAtMs: 100_500,
      },
    );

    expect(window.validFrom).toBe(96_000n);
    expect(window.validFrom).toBeGreaterThanOrEqual(95_500n);
    expect(window.validTo - window.validFrom).toBe(8n * 60n * 1000n);
  });

  it("keeps first-appointment validity within the on-chain short-range limit for production commit buffers", async () => {
    const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
    const emulator = new Emulator([operator]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    const snapshot = captureSchedulerSlotSnapshot(lucid);
    const targetCommitEndTime = BigInt(
      snapshot.currentSlotStartMs + 8 * 60 * 1000 + 1_000,
    );

    const window = resolveSchedulerFirstAppointmentValidityWindow(
      lucid,
      targetCommitEndTime,
      snapshot,
    );

    expect(window.validTo).toBeLessThanOrEqual(targetCommitEndTime);
    expect(window.validFrom).toBeGreaterThanOrEqual(
      BigInt(snapshot.currentSlotStartMs),
    );
    expect(window.validTo - window.validFrom).toBeLessThanOrEqual(
      8n * 60n * 1000n,
    );
  });

  it("keeps scheduler refresh confirmation wait tolerant of live preprod confirmation latency", () => {
    expect(SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS).toBe(5 * 60_000);
    expect(SCHEDULER_SUBMISSION_CONFIRMATION_TIMEOUT_MS).toBeLessThan(
      Number(8n * 60n * 1000n),
    );
  });

  it("routes not-due scheduler timing to block-commitment due work before transaction build", () => {
    const result = schedulerRefreshDueWorkFromSubmitTiming({
      plan: {
        status: "not_due",
        callerLabel: "scheduler-refresh",
        targetSlot: 30,
        dueSlot: 30,
        currentSlot: 20,
        observedSlot: 20,
        observedAtMs: 1_000,
        deltaSlots: 10,
        waitMs: 10_000,
        slotLengthMs: 1_000,
        slotSource: "local_ogmios_tip",
        invalidBeforeSlot: 28,
        invalidHereafterSlot: 40,
        reason: "wait_ms=10000,max_inline_wait_ms=5000",
        dependencyKey: "scheduler=tx#0,current_operator=aa",
        invalidationKey: "scheduler=tx#0,current_operator=aa",
      },
    });

    expect(result).toStrictEqual({
      type: "CommitTimingDueWork",
      dueWork: {
        kind: "commit_scheduler_refresh",
        key: "block_commitment",
        callerLabel: "scheduler-refresh",
        reason: "scheduler_transition_not_reached",
        observedSlot: 20,
        dueSlot: 30,
        dueAtMs: 11_000,
        waitMs: 10_000,
        slotSource: "local_ogmios_tip",
        dependencyKey: "scheduler=tx#0,current_operator=aa",
        invalidationKey: "scheduler=tx#0,current_operator=aa",
      },
    });
  });
});
