import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import {
  header,
  stateQueueAddress,
} from "midgard-watcher/tests/published-da-target-consumption.scenario";
import { describe, expect, it } from "vitest";

import {
  awaitAttestedAtOrAfterStackQueueTail,
  awaitAttestedStackQueueTail,
  type StackQueueNode,
  type StackQueuePorts,
} from "./stack-journey-da.js";
import {
  type QueueRootState,
  rebuildMergedRootBlock,
  settledStateQueue,
  stagingAppendPoint,
} from "./state-queue-root-anchor.js";
import {
  ATTESTED,
  headerHash,
  kupoUrl,
  mergedHistory,
  mergedRoot,
  nodeOutput,
  nodeUnit,
  olderHash,
  otherHash,
  rootOutput,
  stateQueue,
} from "./state-queue-root-anchor.test-support.js";

describe("the merged block a root tail holds, rebuilt from L1", () => {
  it("is the header in the node output its merge spent, which hashes to the root", async () => {
    const history = mergedHistory();
    const rebuilt = await rebuildMergedRootBlock({
      root: mergedRoot,
      ...history.lookup,
    });
    expect(rebuilt).toEqual({ header, commitTxHash: history.attestTxHash });
  });

  it("refuses a header that does not hash to the root's", async () => {
    // A node keyed by the root's hash whose header is another block's.
    const history = mergedHistory({ hash: otherHash });
    await expect(
      rebuildMergedRootBlock({
        root: { ...mergedRoot, headerHash: otherHash },
        ...history.lookup,
      }),
    ).rejects.toThrow("header hash");
  });

  it("refuses a header whose UTxO root or end time is not the root's", async () => {
    const history = mergedHistory();
    const rebuild = (root: QueueRootState) =>
      rebuildMergedRootBlock({ root, ...history.lookup });
    await expect(
      rebuild({ ...mergedRoot, utxoRoot: "66".repeat(32) }),
    ).rejects.toThrow("UTxO root");
    await expect(
      rebuild({ ...mergedRoot, endTime: mergedRoot.endTime + 1n }),
    ).rejects.toThrow("end time");
  });

  it("refuses an output that does not hold the block's node, or a transaction that is not valid", async () => {
    const elsewhere = mergedHistory({
      attestedNode: nodeOutput(otherHash, null, header, ATTESTED),
    });
    await expect(
      rebuildMergedRootBlock({ root: mergedRoot, ...elsewhere.lookup }),
    ).rejects.toThrow(`does not hold merged block ${headerHash}`);
    const invalid = mergedHistory({ attestValid: false });
    await expect(
      rebuildMergedRootBlock({ root: mergedRoot, ...invalid.lookup }),
    ).rejects.toThrow("is not the exact valid included body");
  });

  it("refuses a genesis root, which holds no block to extend", async () => {
    let reads = 0;
    await expect(
      rebuildMergedRootBlock({
        root: { ...mergedRoot, headerHash: SDK.GENESIS_HEADER_HASH },
        ...mergedHistory().lookup,
        fetchImpl: async () => {
          reads += 1;
          return Response.json([]);
        },
      }),
    ).rejects.toThrow("no block to extend");
    expect(reads).toBe(0);
  });

  it("refuses without a reader of confirmed transactions", async () => {
    const { fetchImpl } = mergedHistory().lookup;
    await expect(
      rebuildMergedRootBlock({
        root: mergedRoot,
        kupoUrl,
        stateQueue,
        fetchImpl,
      }),
    ).rejects.toThrow("reads no confirmed transactions");
  });
});

/** The staging append point over a fake L1 holding `outputs`. */
const appendPoint = (outputs: UTxO[]) => {
  const provider = {
    getUtxosWithUnit: async (address: string, unit: string) =>
      outputs.filter(
        (output) => output.address === address && output.assets[unit] === 1n,
      ),
  };
  return stagingAppendPoint({
    provider,
    stateQueue,
    readNode: async (hash) =>
      (await provider.getUtxosWithUnit(stateQueueAddress, nodeUnit(hash)))[0],
  });
};

describe("the output a staged block is committed on after its parent", () => {
  const parent = { headerHash, header };

  it("is the parent's node while it is the exact tail", async () => {
    const node = nodeOutput(headerHash, null);
    const point = appendPoint([rootOutput(olderHash, headerHash), node]);
    expect(await point.tailOutput(parent)).toBe(node);
    expect(await point.queueHead()).toBe(node);
  });

  it("is the root once the parent merged and nothing follows it", async () => {
    const root = rootOutput(headerHash, null);
    const point = appendPoint([root]);
    expect(await point.tailOutput(parent)).toBe(root);
    // A commit on the root takes no head.
    expect(await point.queueHead()).toBeUndefined();
  });

  it("is absent for a root holding the hash when the block's header does not hash to it", async () => {
    const point = appendPoint([rootOutput(otherHash, null)]);
    expect(
      await point.tailOutput({ headerHash: otherHash, header }),
    ).toBeUndefined();
  });

  it("is absent once a newer block follows the parent, merged or not", async () => {
    expect(
      await appendPoint([
        rootOutput(olderHash, headerHash),
        nodeOutput(headerHash, otherHash),
        nodeOutput(otherHash, null),
      ]).tailOutput(parent),
    ).toBeUndefined();
    expect(
      await appendPoint([
        rootOutput(headerHash, otherHash),
        nodeOutput(otherHash, null),
      ]).tailOutput(parent),
    ).toBeUndefined();
  });

  it("is absent once the parent was removed, or its node holds another header", async () => {
    expect(
      await appendPoint([rootOutput(olderHash, null)]).tailOutput(parent),
    ).toBeUndefined();
    expect(
      await appendPoint([
        rootOutput(olderHash, headerHash),
        nodeOutput(headerHash, null, { ...header, endTime: 2n }),
      ]).tailOutput(parent),
    ).toBeUndefined();
  });
});

/** How often a tail wait polls the fake queue, and its attestation timeout. */
const POLL_MS = 10_000;
const TIMEOUT_MS = SDK.DA_ATTESTATION_TIMEOUT_MS;

const failure = (wait: Promise<unknown>) =>
  wait.then(
    () => undefined,
    (error: unknown) => error as Error,
  );

/**
 * The stack's queue read whole, root included: our node merges an attested
 * block once it matures, whoever's shift it is, so the root advances to it.
 */
const mergingQueue = (
  initial: readonly { attested: boolean }[],
  beforePoll: (queue: {
    blocks: StackQueueNode[];
    merge(): void;
    append(attested?: boolean): void;
  }) => void,
) => {
  let clock = 1_000_000_000;
  let pauses = 0;
  let root: QueueRootState = {
    headerHash: "ee".repeat(28),
    prevHeaderHash: "dd".repeat(28),
    utxoRoot: "55".repeat(32),
    endTime: BigInt(clock - 60_000),
  };
  const blocks: StackQueueNode[] = [];
  let made = 0;
  const append = (attested = false) => {
    made += 1;
    blocks.push({
      headerHash: made.toString(16).padStart(56, "0"),
      blockTime: BigInt(clock),
      attested,
    });
  };
  const merge = () => {
    const oldest = blocks.shift()!;
    root = {
      headerHash: oldest.headerHash,
      prevHeaderHash: root.headerHash,
      utxoRoot: "55".repeat(32),
      endTime: oldest.blockTime,
    };
  };
  for (const { attested } of initial) append(attested);
  const ports: StackQueuePorts<StackQueueNode> = {
    ...settledStateQueue({
      readQueue: async () => ({ root, blocks: [...blocks] }),
      decodeBlock: async (block: StackQueueNode) => block,
      mergedRoot: (_root, node) => node,
    }),
    now: () => clock,
    pause: async () => {
      pauses += 1;
      clock += POLL_MS;
      beforePoll({ blocks, merge, append });
    },
    attestationTimeoutMs: TIMEOUT_MS,
    onStage: () => undefined,
  };
  return { ports, root: () => root, pauses: () => pauses };
};

describe("an idle stack, whose every block merged into the root", () => {
  it("takes the root as the attested tail at once", async () => {
    const queue = mergingQueue([], () => undefined);
    const tail = await awaitAttestedStackQueueTail(queue.ports);
    expect(tail).toEqual({
      headerHash: queue.root().headerHash,
      blockTime: queue.root().endTime,
      attested: true,
    });
    expect(queue.pauses()).toBe(0);
    const settled = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    expect(settled.headerHash).toBe(queue.root().headerHash);
    expect(queue.pauses()).toBe(0);
  });

  it("returns the root holding a pinned tail that merged while it was awaited", async () => {
    const queue = mergingQueue([{ attested: false }], ({ blocks, merge }) => {
      blocks[0] = { ...blocks[0]!, attested: true };
      merge();
    });
    const block = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    expect(block.attested).toBe(true);
    expect(block.headerHash).toBe(queue.root().headerHash);
    expect(queue.pauses()).toBe(1);
  });

  it("returns the root that merged past a pinned tail, its child merged too", async () => {
    const queue = mergingQueue(
      [{ attested: false }],
      ({ blocks, append, merge }) => {
        blocks[0] = { ...blocks[0]!, attested: true };
        append(true);
        merge();
        merge();
      },
    );
    const block = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    // The root holds the pinned tail's child, whose parent it names.
    expect(block.headerHash).toBe(queue.root().headerHash);
    expect(queue.root().prevHeaderHash).toBe("1".padStart(56, "0"));
  });

  it("returns a newer attested block after a pinned tail that merged", async () => {
    const queue = mergingQueue([{ attested: false }], ({ blocks, merge }) => {
      blocks[0] = { ...blocks[0]!, attested: true };
      merge();
      blocks.push({
        headerHash: "ab".repeat(28),
        blockTime: 1n,
        attested: true,
      });
    });
    const block = await awaitAttestedAtOrAfterStackQueueTail(queue.ports);
    expect(block.headerHash).toBe("ab".repeat(28));
  });

  it("still refuses by name a pinned tail that was removed, not merged", async () => {
    const queue = mergingQueue([{ attested: false }], ({ blocks }) => {
      blocks.splice(0);
    });
    const error = await failure(
      awaitAttestedAtOrAfterStackQueueTail(queue.ports),
    );
    expect(error?.name).toBe("StackQueueNodeRemovedError");
  });
});
