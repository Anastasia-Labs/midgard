import type { UTxO } from "@lucid-evolution/lucid";
import {
  fraudProofAddress,
  header,
} from "midgard-watcher/tests/published-da-target-consumption.scenario";
import { describe, expect, it } from "vitest";

import {
  leftQueueByMerge,
  leftQueueDaTarget,
  onQueueOrMerged,
  type QueueRootState,
  rebuildMergedRootBlock,
} from "./state-queue-root-anchor.js";
import {
  ATTESTED,
  headerHash,
  mergedHistory,
  mergedRoot,
  nodeOutput,
  otherHash,
  rootOutput,
} from "./state-queue-root-anchor.test-support.js";

const left = (history: ReturnType<typeof mergedHistory>) =>
  leftQueueByMerge({ ...history.lookup, headerHash });

describe("a block that left the queue by a merge", () => {
  it("is shown by the spend whose root holds it, and by no other spend", async () => {
    expect(await left(mergedHistory())).toBe(true);
    const otherRoot = [rootOutput(otherHash, null)];
    expect(await left(mergedHistory({ mergeOutputs: otherRoot }))).toBe(false);
    expect(await left(mergedHistory({ mergeSkipsNode: true }))).toBe(false);
    // A merge leaves exactly one root.
    const twoRoots = [
      rootOutput(headerHash, null),
      rootOutput(headerHash, null),
    ];
    expect(await left(mergedHistory({ mergeOutputs: twoRoots }))).toBe(false);
  });

  it("refuses a spender that is not the exact valid included body", async () => {
    await expect(left(mergedHistory({ mergeValid: false }))).rejects.toThrow(
      "is not the exact valid included body",
    );
  });

  it("is found whatever order Kupo lists two spends of one slot in", async () => {
    // Kupo lists the merge first, so a stable sort by slot leaves it first.
    const history = mergedHistory({ sameSlot: true });
    expect(await left(history)).toBe(true);
    await expect(
      leftQueueDaTarget({ headerHash, root: mergedRoot, ...history.lookup }),
    ).resolves.toEqual({ kind: "merged", txHash: history.attestTxHash });
    await expect(
      rebuildMergedRootBlock({ root: mergedRoot, ...history.lookup }),
    ).resolves.toEqual({ header, commitTxHash: history.attestTxHash });
  });
});

describe("a DA target whose node left the queue", () => {
  const target = (
    history: ReturnType<typeof mergedHistory>,
    root: QueueRootState = mergedRoot,
  ) => leftQueueDaTarget({ headerHash, root, ...history.lookup });

  it("merged once the root holds it or names it as parent, with the output that held its attested node", async () => {
    const history = mergedHistory();
    const merged = { kind: "merged", txHash: history.attestTxHash };
    await expect(target(history)).resolves.toEqual(merged);
    const child = { ...mergedRoot, headerHash: otherHash };
    await expect(
      target(history, { ...child, prevHeaderHash: headerHash }),
    ).resolves.toEqual(merged);
  });

  it("stays absent otherwise, so its removal is authenticated", async () => {
    const neither = { ...mergedRoot, headerHash: otherHash };
    await expect(target(mergedHistory(), neither)).resolves.toEqual({
      kind: "absent",
    });
  });

  it("refuses a merged node that is not attested, is another block's, or holds another header", async () => {
    const unattested = mergedHistory({
      attestedNode: nodeOutput(headerHash, null),
    });
    await expect(target(unattested)).rejects.toThrow("is not attested");
    // A node's key is its token's name, so another block's node is keyed by
    // that block.
    await expect(
      target(
        mergedHistory({
          attestedNode: nodeOutput(otherHash, null, header, ATTESTED),
        }),
      ),
    ).rejects.toThrow(`does not hold merged block ${headerHash}`);
    const otherHeader = { ...header, endTime: header.endTime + 1n };
    await expect(
      target(
        mergedHistory({
          attestedNode: nodeOutput(headerHash, null, otherHeader, ATTESTED),
        }),
      ),
    ).rejects.toThrow("another block's header");
  });

  it("refuses a node output elsewhere than the state queue, or a body that is not its transaction", async () => {
    const node = nodeOutput(headerHash, null, header, ATTESTED);
    const away = { ...node, address: fraudProofAddress };
    await expect(target(mergedHistory({ attestedNode: away }))).rejects.toThrow(
      `does not hold merged block ${headerHash}`,
    );
    const history = mergedHistory();
    const { readConfirmedTransaction } = history.lookup;
    await expect(
      leftQueueDaTarget({
        headerHash,
        root: mergedRoot,
        ...history.lookup,
        readConfirmedTransaction: (txHash) =>
          readConfirmedTransaction(
            txHash === history.attestTxHash ? history.commitTxHash : txHash,
          ),
      }),
    ).rejects.toThrow("is not the exact valid included body");
  });
});

describe("a block checked on the queue after its commit", () => {
  const check = (history: ReturnType<typeof mergedHistory>, live: UTxO[]) =>
    onQueueOrMerged({
      ...history.lookup,
      headerHash,
      provider: { getUtxosWithUnit: async () => live },
    });

  it("is there as one live node, or merged since", async () => {
    const node = nodeOutput(headerHash, null);
    expect(await check(mergedHistory(), [node])).toBe(true);
    expect(await check(mergedHistory(), [])).toBe(true);
  });

  it("is missing when removed, or ambiguous with two live nodes", async () => {
    const removed = mergedHistory({
      mergeOutputs: [rootOutput(otherHash, null)],
    });
    expect(await check(removed, [])).toBe(false);
    const node = nodeOutput(headerHash, null);
    expect(await check(mergedHistory(), [node, node])).toBe(false);
  });
});
