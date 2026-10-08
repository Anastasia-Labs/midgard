import { type DepthParameters, isFinal } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import type { ObservedStateQueueNode } from "../../src/domain.js";
import type {
  CommitteeL1Readiness,
  CommitteeL1Source,
} from "../../src/l1/follower/l1-follower.js";
import {
  headersAwaitingAttestation,
  type QueueRow,
  walkLandedQueue,
} from "../../src/l1/follower/landed-queue.js";
import { obligations } from "../../src/l1/follower/obligations.js";
import type { QueueExit } from "../../src/l1/follower/projection.js";
import { decodeQueueOutput } from "../../src/l1/follower/queue-derivation.js";
import { nodeDatum, rootDatum } from "../l1-follower/queue-sim-traffic.js";

/** cd and k of the fake chain: `minimalConfig`'s finality depth, and k = 2·cd. */
export const FAKE_L1_PARAMETERS: DepthParameters = {
  confirmationDepth: 2,
  securityParameter: 4,
};

/** The fake chain's tip height. */
export const FAKE_TIP_HEIGHT = 1_000;

const POLICY = "71".repeat(28);
const ROOT_OUT_REF = `${"00".repeat(32)}#0`;
const ROOT_HEADER_HASH = "00".repeat(28);

export type FakeL1Chain = Readonly<{
  /**
   * The live queue nodes, in list order. Each node's `chainPoint.depth`
   * places its output under the tip; its slot and block hash are kept.
   */
  fetchStateQueueNodes: () => Promise<readonly ObservedStateQueueNode[]>;
  /** Exits of stored headers no longer in the queue (default none). */
  exits?: () => readonly QueueExit[];
  /** The follower's named reasons (default: ready). */
  readiness?: () => readonly CommitteeL1Readiness[];
  /** The follower's cursor slot (default: the tip's). */
  cursorSlot?: () => number | null;
  /** Where a point stands (default: canonical at depth 1). */
  pointStatus?: CommitteeL1Source["pointStatus"];
  parameters?: DepthParameters;
}>;

const queueRow = (
  node: ObservedStateQueueNode,
  next: ObservedStateQueueNode | undefined,
): QueueRow => {
  const datum = nodeDatum(
    node.header,
    node.daAttestation,
    next === undefined
      ? null
      : next.linkedListKey === "Empty"
        ? null
        : next.linkedListKey,
  );
  const decoded = decodeQueueOutput(
    {
      address: Buffer.alloc(29),
      paymentCredential: null,
      stakeCredential: null,
      lovelace: 5_000_000n,
      assets: new Map([[POLICY, new Map([[node.assetName, 1n]])]]),
      datumHash: null,
      datum,
      scriptRef: null,
    },
    POLICY,
  )!;
  const createdHeight = FAKE_TIP_HEIGHT - (node.chainPoint.depth ?? 1) + 1;
  return {
    outRef: node.outRef,
    ...decoded,
    datumHex: datum.toString("hex"),
    createdSlot: node.chainPoint.slot ?? 1,
    createdHeight,
    createdTxIndex: 0,
    spentSlot: null,
  };
};

/**
 * A committee L1 source over a fixed list of queue nodes, built through the
 * real landed-queue walk, awaiting-header rule and obligations. Each node's
 * datum is encoded and decoded the way the follower's derivation stores it.
 */
export const fakeL1Source = (chain: FakeL1Chain): CommitteeL1Source => {
  const parameters = chain.parameters ?? FAKE_L1_PARAMETERS;
  return {
    parameters,
    readiness: () => chain.readiness?.() ?? [],
    cursorSlot: () => chain.cursorSlot?.() ?? FAKE_TIP_HEIGHT,
    pointStatus:
      chain.pointStatus ??
      (async () => ({ kind: "canonical", depth: 1 }) as never),
    readView: async ({ signed, exitsOf }) => {
      const nodes = await chain.fetchStateQueueNodes();
      const first = nodes[0];
      const rootDatumHex = rootDatum(
        ROOT_HEADER_HASH,
        0,
        first === undefined || first.linkedListKey === "Empty"
          ? null
          : first.linkedListKey,
      ).toString("hex");
      const root: QueueRow = {
        outRef: ROOT_OUT_REF,
        ...decodeQueueOutput(
          {
            address: Buffer.alloc(29),
            paymentCredential: null,
            stakeCredential: null,
            lovelace: 5_000_000n,
            assets: new Map([
              [POLICY, new Map([[SDK.STATE_QUEUE_ROOT_ASSET_NAME, 1n]])],
            ]),
            datumHash: null,
            datum: Buffer.from(rootDatumHex, "hex"),
            scriptRef: null,
          },
          POLICY,
        )!,
        datumHex: rootDatumHex,
        createdSlot: 0,
        createdHeight: 1,
        createdTxIndex: 0,
        spentSlot: null,
      };
      const rows = [
        root,
        ...nodes.map((node, index) => queueRow(node, nodes[index + 1])),
      ];
      const queue = walkLandedQueue(rows);
      const nodeBlocks = new Map<number, string>();
      for (const node of nodes)
        if (
          node.chainPoint.slot !== undefined &&
          node.chainPoint.blockHash !== undefined
        )
          nodeBlocks.set(node.chainPoint.slot, node.chainPoint.blockHash);
      const asked = new Set(exitsOf);
      return {
        at: { slot: FAKE_TIP_HEIGHT, height: FAKE_TIP_HEIGHT, generation: 0 },
        queue,
        awaiting: headersAwaitingAttestation(
          queue,
          FAKE_TIP_HEIGHT,
          parameters,
        ),
        obligations: obligations({
          signed,
          presence: queue.nodes.map((node) => ({
            headerHash: node.headerHash,
            firstCreatedHeight: node.createdHeight,
            liveStatus: node.daStatus,
          })),
          tipHeight: FAKE_TIP_HEIGHT,
          finalBlockTimeMs: null,
          pruned: false,
          parameters,
        }),
        finalQueueHeaderHashes: [
          ROOT_HEADER_HASH,
          ...queue.nodes
            .filter((node) =>
              isFinal(FAKE_TIP_HEIGHT - node.createdHeight + 1, parameters),
            )
            .map((node) => node.headerHash),
        ].sort(),
        finalSlot: null,
        exits: (chain.exits?.() ?? []).filter((exit) =>
          asked.has(exit.headerHash),
        ),
        nodeBlocks,
        prunedThroughSlot: 0,
      };
    },
  };
};
