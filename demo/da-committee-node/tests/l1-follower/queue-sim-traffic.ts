// The committee's simulator traffic: the queue's datums, outputs and the
// honest state-queue transactions the fork simulator interleaves.
import {
  type DepthParameters,
  type OutRef,
  outRefKey,
} from "@al-ft/midgard-l1-follower";
import {
  type ScenarioTraffic,
  type SimChain,
  type SimOutput,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type SlotTime,
  slotTimeMs,
} from "../../src/l1/follower/obligations.js";
import type { CommitteeQueueParameters } from "../../src/l1/follower/queue-derivation.js";
import { headerHashOf } from "../../src/l1/follower/queue-derivation.js";
/** The simulator's rollback bound (the follower suites use the same). */
export const SIM_K = 6;

/** cd and k for the committee under the simulator. */
export const SIM_DEPTHS: DepthParameters = {
  confirmationDepth: 2,
  securityParameter: SIM_K,
};

/** One-second slots from a fixed zero time, so block times are exact. */
export const SIM_SLOT_TIME: SlotTime = {
  zeroTime: 1_600_000_000_000,
  zeroSlot: 0,
  slotLength: 1_000,
};

/** The queue lives at the simulator's tracked address, under its own policy. */
export const SIM_QUEUE: CommitteeQueueParameters = {
  stateQueueAddress: simUniverse().trackedAddress,
  stateQueuePolicyId: "71".repeat(28),
};

/**
 * The end time of a header whose commit is valid before slot `invalidAfter`:
 * one millisecond before that slot starts.
 */
export const commitEndTimeMs = (invalidAfter: number): number =>
  slotTimeMs(invalidAfter, SIM_SLOT_TIME) - 1;

/** The `invalidAfter` slot of a commit for a header ending at `endTimeMs`. */
export const commitInvalidAfter = (endTimeMs: number): number =>
  (endTimeMs + 1 - SIM_SLOT_TIME.zeroTime) / SIM_SLOT_TIME.slotLength +
  SIM_SLOT_TIME.zeroSlot;

const ROOT_ASSET = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
const GENESIS_HASH = "00".repeat(28);

const hex32 = (n: number): string => n.toString(16).padStart(64, "0");

/** A V1 header unique to `nonce`, chained to `prevHeaderHash`. */
export const simHeader = (
  nonce: number,
  prevHeaderHash: string,
  endTimeMs: number,
): SDK.Header => {
  const root = hex32(nonce);
  return {
    prevUtxosRoot: root,
    utxosRoot: root,
    withdrawalsRoot: root,
    forcedTransactionsRoot: root,
    transactionsRoot: root,
    depositsRoot: root,
    transitionTraceRoot: root,
    eventToStepRoot: root,
    validationTracesRoot: root,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
    startTime: BigInt(endTimeMs - 1_000),
    endTime: BigInt(endTimeMs),
    blockSlot: BigInt(nonce),
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash,
    operatorVkey: "ab".repeat(28),
    protocolVersion: 1n,
  };
};

const datumOf = (datum: SDK.LinkedListDatum): Buffer =>
  Buffer.from(Data.to(datum, SDK.LinkedListDatum), "hex");

export const rootDatum = (
  headerHash: string,
  endTimeMs: number,
  link: string | null,
): Buffer =>
  datumOf({
    data: {
      Root: {
        data: Data.castTo(
          {
            headerHash,
            prevHeaderHash: GENESIS_HASH,
            utxoRoot: hex32(0),
            startTime: 0n,
            endTime: BigInt(endTimeMs),
            protocolVersion: 1n,
          },
          SDK.ConfirmedState,
        ),
      },
    },
    link,
  });

export const nodeDatum = (
  header: SDK.Header,
  status: SDK.DaAvailabilityStateQueueStatus,
  link: string | null,
): Buffer =>
  datumOf({
    data: {
      Node: {
        data: Data.castTo(
          { header, da_attestation: status, proven_fraud: null },
          SDK.StateQueueNode,
        ),
      },
    },
    link,
  });

export const queueOutput = (assetName: string, datum: Buffer): SimOutput => ({
  address: SIM_QUEUE.stateQueueAddress,
  lovelace: 5_000_000n,
  assets: new Map([[SIM_QUEUE.stateQueuePolicyId, new Map([[assetName, 1n]])]]),
  datum,
});

/** Whether an output carries the queue policy (only queue traffic spends it). */
export const isQueueOutput = (output: SimOutput): boolean =>
  output.assets?.has(SIM_QUEUE.stateQueuePolicyId) === true;

/** A decoded queue element of the simulated ledger. */
type Element = Readonly<{
  outRef: OutRef;
  output: SimOutput;
  assetName: string;
  datum: SDK.LinkedListDatum;
  /** Root: the confirmed header hash; node: its header hash. */
  headerHash: string;
  header: SDK.Header | null;
  status: SDK.DaAvailabilityStateQueueStatus | null;
  /** Root only: the confirmed state's end time. */
  rootEndTimeMs: number;
}>;

export const decodeElement = (outRef: OutRef, output: SimOutput): Element => {
  const names = output.assets?.get(SIM_QUEUE.stateQueuePolicyId);
  const assetName = [...(names?.keys() ?? [])][0] ?? "";
  const datum = Data.from(
    (output.datum ?? Buffer.alloc(0)).toString("hex"),
    SDK.LinkedListDatum,
  );
  if ("Root" in datum.data) {
    const state = Data.castFrom(datum.data.Root.data, SDK.ConfirmedState);
    return {
      outRef,
      output,
      assetName,
      datum,
      headerHash: state.headerHash,
      header: null,
      status: null,
      rootEndTimeMs: Number(state.endTime),
    };
  }
  const node = Data.castFrom(datum.data.Node.data, SDK.StateQueueNode);
  return {
    outRef,
    output,
    assetName,
    datum,
    headerHash: headerHashOf(node.header),
    header: node.header,
    status: node.da_attestation,
    rootEndTimeMs: 0,
  };
};

/** The live queue as one list from its root, or null if there is no root. */
const liveList = (
  chain: SimChain,
): Readonly<{ root: Element; nodes: Element[] }> | null => {
  const elements = chain
    .live()
    .filter((utxo) => isQueueOutput(utxo.output))
    .map((utxo) => decodeElement(utxo.outRef, utxo.output));
  const root = elements.find((element) => element.header === null);
  if (root === undefined) return null;
  const byKey = new Map(
    elements
      .filter((element) => element.header !== null)
      .map((element) => [element.headerHash, element]),
  );
  const nodes: Element[] = [];
  let key = root.datum.link;
  while (key !== null) {
    const next = byKey.get(key);
    if (next === undefined) break;
    nodes.push(next);
    key = next.datum.link;
  }
  return { root, nodes };
};

const relinked = (element: Element, link: string | null): SimOutput => {
  if (element.header === null)
    return queueOutput(
      element.assetName,
      rootDatum(element.headerHash, element.rootEndTimeMs, link),
    );
  return queueOutput(
    element.assetName,
    nodeDatum(element.header, element.status ?? "Unattested", link),
  );
};

const commitmentHash = (nonce: number): string => hex32(nonce + 0xa77e57);

export type QueueTrafficOptions = Readonly<{
  /** Appends a node nothing links to, at this chance per block (P1's orphan). */
  orphanChance?: number;
}>;

/**
 * Honest state-queue traffic: at most one queue transaction per block (the
 * simulator's ledger view is the state before the block). It initializes the
 * root, appends headers, attests, merges an attested head and removes the
 * tail. A commit is valid before slot `invalidAfter`, two or three slots
 * past the block it is built for, and its header ends one millisecond before
 * that slot starts (the state-queue validator pins the header end time to
 * the commit's inclusive validity upper bound). After a rollback a new
 * append is a sibling of the header the rollback removed, or the removed
 * commit itself lands again while its interval still admits the next block.
 */
export const queueTraffic = (
  options: QueueTrafficOptions = {},
): ScenarioTraffic => {
  /** Every commit built, by its spent tail outref, to land it again. */
  const commits = new Map<string, SimTx>();
  return ({ chain, rng, claim }): SimTx[] => {
    const list = liveList(chain);
    const nonce = chain.nonce();
    const invalidAfter = chain.tip.point.slot + 3;
    const endTimeMs = commitEndTimeMs(invalidAfter);
    if (list === null) {
      return [
        {
          inputs: [chain.outsideInput()],
          outputs: [queueOutput(ROOT_ASSET, rootDatum(GENESIS_HASH, 0, null))],
          nonce,
        },
      ];
    }
    if (
      options.orphanChance !== undefined &&
      rng.chance(options.orphanChance)
    ) {
      const header = simHeader(nonce, GENESIS_HASH, endTimeMs);
      const hash = headerHashOf(header);
      return [
        {
          inputs: [chain.outsideInput()],
          outputs: [
            queueOutput(
              `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${hash}`,
              nodeDatum(header, "Unattested", null),
            ),
          ],
          nonce,
        },
      ];
    }
    const spend = (elements: readonly Element[], tx: SimTx): SimTx[] =>
      elements.every((element) => claim(element.outRef)) ? [tx] : [];
    const tail = list.nodes.at(-1) ?? list.root;
    const removed = commits.get(outRefKey(tail.outRef));
    if (
      removed !== undefined &&
      chain.nextSlot() < (removed.invalidAfter ?? 0) &&
      rng.chance(0.5)
    )
      return spend([tail], removed);
    const roll = rng.next();
    if (roll < 0.5 || list.nodes.length === 0) {
      const header = simHeader(nonce, tail.headerHash, endTimeMs);
      const hash = headerHashOf(header);
      const commit: SimTx = {
        inputs: [tail.outRef],
        outputs: [
          relinked(tail, hash),
          queueOutput(
            `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${hash}`,
            nodeDatum(header, "Unattested", null),
          ),
        ],
        invalidAfter,
        nonce,
      };
      const built = spend([tail], commit);
      if (built.length > 0) commits.set(outRefKey(tail.outRef), commit);
      return built;
    }
    const unattested = list.nodes.filter(
      (node) => node.status === "Unattested",
    );
    if (roll < 0.75 && unattested.length > 0) {
      const node = rng.pick(unattested);
      return spend([node], {
        inputs: [node.outRef],
        outputs: [
          queueOutput(
            node.assetName,
            nodeDatum(
              node.header!,
              { Attested: { commitment_hash: commitmentHash(nonce) } },
              node.datum.link,
            ),
          ),
        ],
        nonce,
      });
    }
    const head = list.nodes[0]!;
    if (roll < 0.9 && head.status !== "Unattested") {
      return spend([list.root, head], {
        inputs: [list.root.outRef, head.outRef],
        outputs: [
          queueOutput(
            ROOT_ASSET,
            rootDatum(
              head.headerHash,
              Number(head.header!.endTime),
              head.datum.link,
            ),
          ),
        ],
        nonce,
      });
    }
    const before = list.nodes.at(-2) ?? list.root;
    return spend([before, tail], {
      inputs: [before.outRef, tail.outRef],
      outputs: [relinked(before, null)],
      nonce,
    });
  };
};
