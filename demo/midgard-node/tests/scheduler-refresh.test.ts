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
  resolveSchedulerRefreshWitnessSelection,
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
});
