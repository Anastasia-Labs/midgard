import { type OutRef } from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simStoreOptions,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type SlotTime,
  slotTimeMs,
} from "../../src/l1/follower/obligations.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import { headerHashOf } from "../../src/l1/follower/queue-derivation.js";
import {
  nodeDatum,
  queueOutput,
  rootDatum,
  SIM_QUEUE,
  SIM_SLOT_TIME,
  simHeader,
} from "./queue-sim.js";

const GENESIS_HASH = "00".repeat(28);

export type QueueChainNode = {
  outRef: OutRef;
  header: SDK.Header;
  hash: string;
  status: SDK.DaAvailabilityStateQueueStatus;
  link: string | null;
};

type QueueRoot = {
  outRef: OutRef;
  headerHash: string;
  endTimeMs: number;
  link: string | null;
};

/**
 * How many blocks `rollBack` can drop. The queue below older blocks is not
 * kept: a long history (the B5 bench's) would hold a copy of the whole
 * queue per block.
 */
export const QUEUE_CHAIN_ROLLBACK_BOUND = 64;

type QueueState = {
  root: QueueRoot | null;
  nodes: QueueChainNode[];
  merged: (QueueChainNode & { mergedAt: number })[];
  attested: number;
};

/**
 * A linear state queue on the simulator's chain: a root, then one append or
 * one attestation per block. The tail is relinked by each append, as the
 * validator requires. `rollBack` drops blocks and restores the queue as it
 * stood below them.
 */
export class QueueChain {
  readonly chain = new SimChain(
    simUniverse(),
    SIM_ORIGIN,
    simStoreOptions([committeeProjection(SIM_QUEUE)], 1, "sqlite").trackedSet,
  );
  private root: QueueRoot | null = null;
  readonly nodes: QueueChainNode[] = [];
  /** Headers merged into the root, oldest first, with the merge's height. */
  readonly merged: (QueueChainNode & { mergedAt: number })[] = [];
  private attested = 0;
  /**
   * The queue below each of the last `QUEUE_CHAIN_ROLLBACK_BOUND` blocks,
   * oldest first.
   */
  private readonly below: QueueState[] = [];

  /** `slotTime` dates each appended header's end time (default the sim's). */
  constructor(private readonly slotTime: SlotTime = SIM_SLOT_TIME) {}

  private snapshot(): QueueState {
    const node = (n: QueueChainNode): QueueChainNode => ({
      ...n,
      outRef: { ...n.outRef },
    });
    return {
      root: this.root === null ? null : { ...this.root },
      nodes: this.nodes.map(node),
      merged: this.merged.map((n) => ({ ...node(n), mergedAt: n.mergedAt })),
      attested: this.attested,
    };
  }

  /** Records the queue below the block the caller is about to add. */
  private beginBlock(): void {
    this.below.push(this.snapshot());
    if (this.below.length > QUEUE_CHAIN_ROLLBACK_BOUND) this.below.shift();
  }

  private forward(tx: SimTx) {
    const step = this.chain.forward([tx]);
    return { event: step.event, txHash: step.encoded.txHashes[0] as Buffer };
  }

  /** A block that touches nothing the queue holds. */
  empty() {
    this.beginBlock();
    return this.chain.forward([]).event;
  }

  /** Drops the top `depth` blocks; the queue is as it stood below them. */
  rollBack(depth: number) {
    if (depth > this.below.length)
      throw new RangeError(
        `cannot roll back ${depth.toString()} blocks: the queue below only the last ${this.below.length.toString()} is kept`,
      );
    const event = this.chain.backward(depth);
    const restored = this.below.splice(this.below.length - depth)[0];
    if (restored !== undefined) {
      this.root = restored.root;
      this.nodes.splice(0, this.nodes.length, ...restored.nodes);
      this.merged.splice(0, this.merged.length, ...restored.merged);
      this.attested = restored.attested;
    }
    return event;
  }

  init() {
    this.beginBlock();
    const step = this.forward({
      inputs: [this.chain.outsideInput()],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(GENESIS_HASH, 0, null),
        ),
      ],
      nonce: this.chain.nonce(),
    });
    this.root = {
      outRef: { txHash: step.txHash, index: 0 },
      headerHash: GENESIS_HASH,
      endTimeMs: 0,
      link: null,
    };
    return step.event;
  }

  append() {
    this.beginBlock();
    const root = this.root!;
    const tail = this.nodes.at(-1);
    const nonce = this.chain.nonce();
    const header = simHeader(
      nonce,
      tail?.hash ?? root.headerHash,
      slotTimeMs(this.chain.tip.point.slot + 3, this.slotTime),
    );
    const hash = headerHashOf(header);
    const relinked =
      tail === undefined
        ? queueOutput(
            SDK.STATE_QUEUE_ROOT_ASSET_NAME,
            rootDatum(root.headerHash, root.endTimeMs, hash),
          )
        : queueOutput(
            `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${tail.hash}`,
            nodeDatum(tail.header, tail.status, hash),
          );
    const step = this.forward({
      inputs: [tail?.outRef ?? root.outRef],
      outputs: [
        relinked,
        queueOutput(
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${hash}`,
          nodeDatum(header, "Unattested", null),
        ),
      ],
      nonce,
    });
    if (tail === undefined)
      this.root = {
        ...root,
        outRef: { txHash: step.txHash, index: 0 },
        link: hash,
      };
    else {
      tail.outRef = { txHash: step.txHash, index: 0 };
      tail.link = hash;
    }
    this.nodes.push({
      outRef: { txHash: step.txHash, index: 1 },
      header,
      hash,
      status: "Unattested",
      link: null,
    });
    return step.event;
  }

  /** Attests the oldest unattested node. */
  attest() {
    this.beginBlock();
    const node = this.nodes[this.attested]!;
    this.attested += 1;
    const nonce = this.chain.nonce();
    node.status = {
      Attested: { commitment_hash: nonce.toString(16).padStart(64, "0") },
    };
    const step = this.forward({
      inputs: [node.outRef],
      outputs: [
        queueOutput(
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${node.hash}`,
          nodeDatum(node.header, node.status, node.link),
        ),
      ],
      nonce,
    });
    node.outRef = { txHash: step.txHash, index: 0 };
    return step.event;
  }

  /** Merges the oldest node into the root: both spent, one new root. */
  merge() {
    this.beginBlock();
    const root = this.root!;
    const node = this.nodes.shift()!;
    this.attested = Math.max(0, this.attested - 1);
    const endTimeMs = Number(node.header.endTime);
    const step = this.forward({
      inputs: [root.outRef, node.outRef],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(node.hash, endTimeMs, node.link),
        ),
      ],
      nonce: this.chain.nonce(),
    });
    this.root = {
      outRef: { txHash: step.txHash, index: 0 },
      headerHash: node.hash,
      endTimeMs,
      link: node.link,
    };
    this.merged.push({ ...node, mergedAt: this.chain.tip.height });
    return step.event;
  }
}
