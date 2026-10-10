/**
 * The independent model of the landed state queue that the fork simulator
 * checks P1 against (N2): it decodes the queue outputs itself (the datum as
 * a linked-list datum, the asset name as root or node key) from the
 * canonical blocks and walks them, sharing nothing with P1's decode or walk.
 */
import {
  type BlockSummary,
  type OutRef,
  outRefKey,
} from "@al-ft/midgard-l1-follower";
import type { SimOutput } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  NODE_PREFIX,
  QUEUE_ADDRESS,
  QUEUE_POLICY,
} from "./state-queue-sim.fixtures.js";

export const isQueueOutput = (output: {
  readonly assets?: ReadonlyMap<string, unknown>;
}): boolean => output.assets?.has(QUEUE_POLICY) === true;

/** One queue output as the model reads it, independently of P1. */
export type ModelElement = Readonly<{
  outRef: OutRef;
  output: SimOutput;
  assetName: string;
  /** Null for the root. */
  key: string | null;
  link: string | null;
  header: SDK.Header | null;
  status: SDK.DaAvailabilityStateQueueStatus | null;
  rootHash: string;
}>;

export const modelElement = (
  outRef: OutRef,
  output: SimOutput,
): ModelElement => {
  const assetName = [...output.assets!.get(QUEUE_POLICY)!.keys()][0]!;
  const datum = Data.from(output.datum!.toString("hex"), SDK.LinkedListDatum);
  if ("Root" in datum.data) {
    const state = Data.castFrom(datum.data.Root.data, SDK.ConfirmedState);
    return {
      outRef,
      output,
      assetName,
      key: null,
      link: datum.link,
      header: null,
      status: null,
      rootHash: state.headerHash,
    };
  }
  const node = Data.castFrom(datum.data.Node.data, SDK.StateQueueNode);
  return {
    outRef,
    output,
    assetName,
    key: assetName.slice(NODE_PREFIX.length),
    link: datum.link,
    header: node.header,
    status: node.da_attestation,
    rootHash: "",
  };
};

export type ModelQueue = Readonly<{
  healthy: boolean;
  /** Null when healthy; only the reasons this traffic can produce. */
  reason: "no_root" | "orphan_node" | null;
  /** Root first, then the walk. */
  outRefs: readonly string[];
  /** Live queue outputs (under the policy). */
  policyOutputs: number;
  /** Live outputs at the queue address carrying no queue token. */
  thirdParty: number;
}>;

/** `<tx hash hex>#<index>`, as P1 names an output. */
const label = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

type Live = Map<string, { outRef: OutRef; output: SimOutput }>;

const liveAtAddress = (blocks: readonly BlockSummary[]): Live => {
  const live: Live = new Map();
  for (const block of blocks)
    for (const tx of block.txs) {
      if (!tx.isValid) continue;
      for (const input of tx.inputs) live.delete(outRefKey(input));
      tx.outputs.forEach((output, index) => {
        if (!output.address.equals(QUEUE_ADDRESS)) return;
        const outRef = { txHash: tx.hash, index };
        live.set(outRefKey(outRef), {
          outRef,
          output: {
            address: output.address,
            lovelace: output.lovelace,
            assets: output.assets,
            ...(output.datum === null ? {} : { datum: output.datum }),
          },
        });
      });
    }
  return live;
};

export const walkModel = (
  elements: readonly ModelElement[],
): Readonly<{ root: ModelElement | null; nodes: ModelElement[] }> => {
  const root = elements.find((element) => element.key === null) ?? null;
  if (root === null) return { root, nodes: [] };
  const byKey = new Map(
    elements.flatMap((element) =>
      element.key === null ? [] : [[element.key, element] as const],
    ),
  );
  const nodes: ModelElement[] = [];
  for (let key = root.link; key !== null; ) {
    const next = byKey.get(key);
    if (next === undefined) break;
    nodes.push(next);
    key = next.link;
  }
  return { root, nodes };
};

export const modelQueue = (blocks: readonly BlockSummary[]): ModelQueue => {
  const live = [...liveAtAddress(blocks).values()];
  const elements = live
    .filter((entry) => isQueueOutput(entry.output))
    .map((entry) => modelElement(entry.outRef, entry.output));
  const { root, nodes } = walkModel(elements);
  const outRefs =
    root === null
      ? []
      : [root, ...nodes].map((element) => label(element.outRef));
  const reached = root === null ? 0 : nodes.length + 1;
  const reason =
    root === null && elements.length > 0
      ? "no_root"
      : reached < elements.length
        ? "orphan_node"
        : null;
  return {
    healthy: root !== null && reason === null,
    reason,
    outRefs,
    policyOutputs: elements.length,
    thirdParty: live.length - elements.length,
  };
};
