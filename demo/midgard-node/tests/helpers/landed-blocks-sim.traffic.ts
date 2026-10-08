/**
 * State-queue traffic for the landed-block fork simulator (N3): the root is
 * initialized at the real genesis header and root; every appended node links
 * to the tail (hash, root, start time) and commits the universe's root for
 * its height and bit; a bad node (about one in ten) commits the other bit's
 * root, so its honest replay misses it; a late-DA node's payload is missing
 * on its first replay. The attested head merges into the root (a bad head
 * never does), and the tail can be removed.
 */
import {
  type ScenarioTraffic,
  type SimOutput,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  H_MAX,
  hasDeposit,
  type SimUniverse,
} from "./landed-blocks-sim.universe.js";
import {
  hex32,
  NODE_PREFIX,
  nodeDatum,
  queueOutput,
  ROOT_ASSET,
  simHeader,
} from "./state-queue-sim.fixtures.js";
import {
  isQueueOutput,
  type ModelElement,
  modelElement,
  walkModel,
} from "./state-queue-sim.model.js";

/** What the simulator knows of every header its traffic made. */
export type SimBlockInfo = Readonly<{
  h: number;
  b: number;
  bad: boolean;
  lateDa: boolean;
  prevHeaderHash: string | null;
}>;

export type SimRegistry = Map<string, SimBlockInfo>;

export const newSimRegistry = (): SimRegistry =>
  new Map([
    [
      SDK.GENESIS_HEADER_HASH,
      { h: 0, b: 0, bad: false, lateDa: false, prevHeaderHash: null },
    ],
  ]);

export type TrafficStats = {
  appends: number;
  badBlocks: number;
  lateDaBlocks: number;
  attests: number;
  merges: number;
  tailRemovals: number;
  /** Rolled-back appends the traffic landed again. */
  relandedAppends: number;
};

export const rootDatum = (
  state: SDK.ConfirmedState,
  link: string | null,
): Buffer =>
  Buffer.from(
    Data.to(
      {
        data: { Root: { data: Data.castTo(state, SDK.ConfirmedState) } },
        link,
      },
      SDK.LinkedListDatum,
    ),
    "hex",
  );

export const genesisState = (universe: SimUniverse): SDK.ConfirmedState => ({
  headerHash: SDK.GENESIS_HEADER_HASH,
  prevHeaderHash: SDK.GENESIS_HEADER_HASH,
  utxoRoot: universe.root(0, 0),
  startTime: 0n,
  endTime: 0n,
  protocolVersion: 1n,
});

/** A root element's confirmed state, decoded from its datum. */
export const confirmedStateOf = (output: SimOutput): SDK.ConfirmedState => {
  const datum = Data.from(output.datum!.toString("hex"), SDK.LinkedListDatum);
  if (!("Root" in datum.data)) throw new Error("not a queue root");
  return Data.castFrom(datum.data.Root.data, SDK.ConfirmedState);
};

/** The live queue at the chain's tip: the root's state and the walk. */
export const liveQueue = (
  utxos: readonly Readonly<{
    outRef: ModelElement["outRef"];
    output: SimOutput;
  }>[],
) => {
  const { root, nodes } = walkModel(
    utxos
      .filter((utxo) => isQueueOutput(utxo.output))
      .map((utxo) => modelElement(utxo.outRef, utxo.output)),
  );
  return {
    root:
      root === null
        ? null
        : { element: root, state: confirmedStateOf(root.output) },
    nodes,
  };
};

const linkedHeader = (
  universe: SimUniverse,
  nonce: number,
  parent: Readonly<{ headerHash: string; utxosRoot: string; endTime: bigint }>,
  h: number,
  committedBit: number,
): SDK.Header => ({
  ...simHeader(nonce, parent.headerHash),
  prevUtxosRoot: parent.utxosRoot,
  utxosRoot: universe.root(h, committedBit),
  depositCount: hasDeposit(h) ? 1n : 0n,
  totalEventCount: hasDeposit(h) ? 1n : 0n,
  startTime: parent.endTime,
  endTime: parent.endTime + 1_000n,
});

const relinkedNode = (element: ModelElement, link: string | null): SimOutput =>
  queueOutput(
    element.assetName,
    nodeDatum(element.header!, element.status ?? "Unattested", link),
  );

export const landedBlocksTraffic = (
  universe: SimUniverse,
  registry: SimRegistry,
  stats: TrafficStats,
): ScenarioTraffic => {
  // Appends a rollback removed: re-landing one (the same transaction, so
  // the same node) exercises a removed row relanding.
  const recentAppends: SimTx[] = [];
  return ({ chain, rng, claim }): SimTx[] => {
    const { root, nodes } = liveQueue(chain.live());
    const nonce = chain.nonce();
    if (root !== null) {
      const again = recentAppends.find((tx) => chain.isLive(tx.inputs[0]!));
      if (again !== undefined && rng.chance(0.7) && claim(again.inputs[0]!)) {
        stats.relandedAppends += 1;
        return [again];
      }
    }
    if (root === null)
      return [
        {
          inputs: [chain.outsideInput()],
          outputs: [
            queueOutput(ROOT_ASSET, rootDatum(genesisState(universe), null)),
          ],
          nonce,
        },
      ];
    const spend = (
      elements: readonly ModelElement[],
      tx: SimTx,
      count: () => void,
    ): SimTx[] => {
      if (!elements.every((element) => claim(element.outRef))) return [];
      count();
      return [tx];
    };
    const tail = nodes.at(-1);
    const tailInfo = tail === undefined ? undefined : registry.get(tail.key!);
    const parent =
      tail === undefined
        ? {
            headerHash: root.state.headerHash,
            utxosRoot: root.state.utxoRoot,
            endTime: root.state.endTime,
            h: registry.get(root.state.headerHash)!.h,
          }
        : {
            headerHash: tail.key!,
            utxosRoot: tail.header!.utxosRoot,
            endTime: tail.header!.endTime,
            h: tailInfo!.h,
          };
    const roll = rng.next();
    const removeTail = (): SimTx[] => {
      const before = nodes.at(-2) ?? root.element;
      return spend(
        [before, tail!],
        {
          inputs: [before.outRef, tail!.outRef],
          outputs: [
            before.header === null
              ? queueOutput(ROOT_ASSET, rootDatum(root.state, null))
              : relinkedNode(before, null),
          ],
          nonce,
        },
        () => (stats.tailRemovals += 1),
      );
    };
    if (tailInfo?.bad === true && roll < 0.6) return removeTail();
    if ((roll < 0.5 || nodes.length === 0) && parent.h < H_MAX) {
      const h = parent.h + 1;
      const b = nonce % 2;
      const bad = rng.chance(0.1);
      const lateDa = !bad && rng.chance(0.15);
      const header = linkedHeader(universe, nonce, parent, h, bad ? 1 - b : b);
      const hash = SDK.stateQueueHeaderHash(header);
      registry.set(hash, {
        h,
        b,
        bad,
        lateDa,
        prevHeaderHash: parent.headerHash,
      });
      const last = tail ?? root.element;
      const append: SimTx = {
        inputs: [last.outRef],
        outputs: [
          last.header === null
            ? queueOutput(ROOT_ASSET, rootDatum(root.state, hash))
            : relinkedNode(last, hash),
          queueOutput(
            NODE_PREFIX + hash,
            nodeDatum(header, "Unattested", null),
          ),
        ],
        nonce,
      };
      return spend([last], append, () => {
        recentAppends.unshift(append);
        recentAppends.splice(20);
        stats.appends += 1;
        if (bad) stats.badBlocks += 1;
        if (lateDa) stats.lateDaBlocks += 1;
      });
    }
    if (nodes.length === 0) return [];
    const unattested = nodes.filter((node) => node.status === "Unattested");
    if (roll < 0.65 && unattested.length > 0) {
      const node = rng.pick(unattested);
      return spend(
        [node],
        {
          inputs: [node.outRef],
          outputs: [
            queueOutput(
              node.assetName,
              nodeDatum(
                node.header!,
                { Attested: { commitment_hash: hex32(nonce) } },
                node.link,
              ),
            ),
          ],
          nonce,
        },
        () => (stats.attests += 1),
      );
    }
    const head = nodes[0]!;
    if (
      roll < 0.85 &&
      head.status !== "Unattested" &&
      registry.get(head.key!)?.bad !== true
    ) {
      const header = head.header!;
      return spend(
        [root.element, head],
        {
          inputs: [root.element.outRef, head.outRef],
          outputs: [
            queueOutput(
              ROOT_ASSET,
              rootDatum(
                {
                  headerHash: head.key!,
                  prevHeaderHash: header.prevHeaderHash,
                  utxoRoot: header.utxosRoot,
                  startTime: header.startTime,
                  endTime: header.endTime,
                  protocolVersion: 1n,
                },
                head.link,
              ),
            ),
          ],
          nonce,
        },
        () => (stats.merges += 1),
      );
    }
    return removeTail();
  };
};
