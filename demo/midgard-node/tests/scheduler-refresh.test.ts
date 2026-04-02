import { describe, expect, it } from "vitest";
import type { UTxO } from "@lucid-evolution/lucid";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  type NodeUtxoWithDatum,
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
  datum: SDK.NodeDatum,
): NodeUtxoWithDatum => ({
  utxo: mkUtxo(txHash, outputIndex),
  datum,
});

describe("scheduler refresh witness selection", () => {
  const activeRoot = mkNode("00".repeat(32), 0, {
    key: "Empty",
    next: { Key: { key: "aa" } },
    data: "00",
  });
  const activeHead = mkNode("11".repeat(32), 0, {
    key: { Key: { key: "aa" } },
    next: { Key: { key: "bb" } },
    data: "00",
  });
  const activeTail = mkNode("22".repeat(32), 0, {
    key: { Key: { key: "bb" } },
    next: "Empty",
    data: "00",
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
      data: "00",
    });
    const registeredTail = mkNode("44".repeat(32), 0, {
      key: { Key: { key: "cc" } },
      next: "Empty",
      data: "00",
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

  it("selects a genesis Rewind from the canonical empty scheduler operator", () => {
    const registeredRoot = mkNode("55".repeat(32), 0, {
      key: "Empty",
      next: "Empty",
      data: "00",
    });

    const selection = resolveSchedulerRefreshWitnessSelection({
      currentOperator: "",
      targetOperator: "bb",
      activeNodes: [activeRoot, activeHead, activeTail],
      registeredNodes: [registeredRoot],
      allowGenesisRewind: true,
    });

    expect(selection.kind).toBe("Rewind");
    if (selection.kind !== "Rewind") {
      throw new Error("expected rewind selection");
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
});
