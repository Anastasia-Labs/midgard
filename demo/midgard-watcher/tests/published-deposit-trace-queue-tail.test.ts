import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { PublishedTransactionSubmissionError } from "./support/published-block-actor.is-unanswered-submission.js";
import {
  appendPointAfter,
  publishedDepositTraceQueue,
} from "./support/published-deposit-trace.queue-tail.js";

const policyId = "c3".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Script",
  hash: "d4".repeat(28),
});
const parent = "a1".repeat(28);
const newer = "b2".repeat(28);
const older = "e5".repeat(28);
const rootUnit = toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);
const nodeUnit = (headerHash: string) =>
  toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash);
const key = (headerHash: string | null): SDK.LinkedListNodeView["next"] =>
  headerHash === null ? "Empty" : { Key: { key: headerHash } };

let nonce = 0;
const nextTxHash = () => (++nonce).toString(16).padStart(64, "0");

/** A root whose confirmed state holds `headerHash`, linked to `next`. */
const rootOutput = (headerHash: string, next: string | null): UTxO => ({
  txHash: nextTxHash(),
  outputIndex: 0,
  address,
  assets: { lovelace: 5_000_000n, [rootUnit]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: key(next),
    data: SDK.castConfirmedStateToData({
      headerHash,
      prevHeaderHash: older,
      utxoRoot: "55".repeat(32),
      startTime: 0n,
      endTime: 1_000n,
      protocolVersion: 0n,
    }) as SDK.LinkedListNodeView["data"],
  }),
});

const nodeOutput = (headerHash: string, next: string | null): UTxO => ({
  txHash: nextTxHash(),
  outputIndex: 0,
  address,
  assets: { lovelace: 5_000_000n, [nodeUnit(headerHash)]: 1n },
  datum: SDK.encodeLinkedListNodeView({
    key: key(headerHash),
    next: key(next),
    data: 0n,
  }),
});

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

describe("the output a block is committed on after its parent", () => {
  it("is the parent's node while it is the queue's tail", async () => {
    const node = nodeOutput(parent, null);
    expect(
      await appendPointAfter(parent, {
        node,
        root: async () => rootOutput(older, parent),
      }),
    ).toBe(node);
  });

  it("is the root once the parent merged and the queue is empty", async () => {
    const root = rootOutput(parent, null);
    expect(
      await appendPointAfter(parent, {
        node: undefined,
        root: async () => root,
      }),
    ).toBe(root);
  });

  it("is absent once a newer block follows the parent, merged or not", async () => {
    expect(
      await appendPointAfter(parent, {
        node: nodeOutput(parent, newer),
        root: async () => rootOutput(older, parent),
      }),
    ).toBeUndefined();
    expect(
      await appendPointAfter(parent, {
        node: undefined,
        root: async () => rootOutput(parent, newer),
      }),
    ).toBeUndefined();
  });

  it("is absent when the queue's last node is another block's", async () => {
    expect(
      await appendPointAfter(parent, {
        node: nodeOutput(newer, null),
        root: async () => rootOutput(older, newer),
      }),
    ).toBeUndefined();
  });

  it("is absent once the parent was removed rather than merged", async () => {
    expect(
      await appendPointAfter(parent, {
        node: undefined,
        root: async () => rootOutput(older, null),
      }),
    ).toBeUndefined();
  });
});

/** A trace queue whose outputs the test changes between reads. */
const traceQueue = (outputs: UTxO[]) =>
  publishedDepositTraceQueue({
    contracts: {
      stateQueue: { spendingScriptAddress: address, policyId },
    } as unknown as Parameters<
      typeof publishedDepositTraceQueue
    >[0]["contracts"],
    lucid: () =>
      ({
        utxosAt: async () => [...outputs],
      }) as unknown as LucidEvolution,
    liveTail: undefined,
  });

const block = {
  headerHash: newer,
  header: { prevHeaderHash: parent } as SDK.Header,
};

describe("a trace commit on its parent's append point", () => {
  it("commits again on the root when its parent merges under a refused commit", async () => {
    const outputs = [rootOutput(older, parent), nodeOutput(parent, null)];
    const queue = traceQueue(outputs);
    const anchors: { anchor: string; head: string | undefined }[] = [];
    const stages: string[] = [];
    const merged = rootOutput(parent, null);
    const txHash = await queue.commitAfterParent(
      block,
      async (anchor, head) => {
        anchors.push({
          anchor: outRef(anchor),
          head: head === undefined ? undefined : outRef(head),
        });
        if (anchors.length > 1) return "77".repeat(32);
        // Our node merges the parent between the read and the submission.
        outputs.splice(0, outputs.length, merged);
        throw new PublishedTransactionSubmissionError(
          "66".repeat(32),
          new Error("BadInputsUTxO"),
        );
      },
      (name) => stages.push(name),
    );
    expect(txHash).toBe("77".repeat(32));
    expect(anchors).toEqual([
      { anchor: anchors[0]!.anchor, head: anchors[0]!.anchor },
      { anchor: outRef(merged), head: undefined },
    ]);
    expect(anchors[0]!.anchor).not.toBe(outRef(merged));
    expect(stages.join("\n")).toContain("refused on spent anchor");
  });

  it("commits on the root when its parent merged before the first attempt", async () => {
    const merged = rootOutput(parent, null);
    const queue = traceQueue([merged]);
    const anchors: string[] = [];
    await queue.commitAfterParent(
      block,
      async (anchor, head) => {
        anchors.push(outRef(anchor));
        expect(head).toBeUndefined();
        return "77".repeat(32);
      },
      () => undefined,
    );
    expect(anchors).toEqual([outRef(merged)]);
  });

  it("throws a refused commit whose anchor was not spent", async () => {
    const queue = traceQueue([
      rootOutput(older, parent),
      nodeOutput(parent, null),
    ]);
    const refusal = new PublishedTransactionSubmissionError(
      "66".repeat(32),
      new Error("ScriptFailure"),
    );
    let attempts = 0;
    await expect(
      queue.commitAfterParent(
        block,
        async () => {
          attempts += 1;
          throw refusal;
        },
        () => undefined,
      ),
    ).rejects.toBe(refusal);
    expect(attempts).toBe(1);
  });

  it("refuses before committing when the queue no longer ends at the parent", async () => {
    // Removed, or replaced at the queue's end by another block's node.
    for (const outputs of [
      [rootOutput(older, null)],
      [rootOutput(older, older), nodeOutput(older, null)],
    ]) {
      let attempts = 0;
      await expect(
        traceQueue(outputs).commitAfterParent(
          block,
          async () => {
            attempts += 1;
            return "77".repeat(32);
          },
          () => undefined,
        ),
      ).rejects.toThrow(`no longer ends at ${parent}`);
      expect(attempts).toBe(0);
    }
  });
});
