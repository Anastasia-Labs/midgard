import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core/consensus-profile";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { daAvailabilityStateQueueStatusIdentity } from "./da-availability-state.js";
import { ConfirmedState } from "./ledger-state.confirmed-state-next-header-protocol-version.js";
import type { Header } from "./ledger-state.header-schema.js";
import {
  encodeHeaderCbor,
  StateQueueNode,
} from "./ledger-state.validate-header-transition-commitments-program.js";
import {
  LinkedListDatum,
  linkedListDatumToNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";

/**
 * Why an output carrying the state-queue policy cannot be read as a queue
 * element. A queue output keeps its problems: a malformed one makes the
 * landed queue unhealthy instead of being dropped.
 */
export type StateQueueOutputProblem =
  /** Not exactly one non-ADA asset, of quantity 1, under the queue policy. */
  | "malformed_nft"
  /** No inline datum, or one that is not a root or a V1 node. */
  | "malformed_datum"
  /** The node's linked-list key is not its header hash. */
  | "linked_list_key_mismatch"
  /** The node's asset name lacks the block prefix. */
  | "block_asset_prefix_mismatch"
  /** The node's asset name does not end in its header hash. */
  | "block_asset_suffix_mismatch";

/** The parts of an L1 output the decoder reads; hex is lowercase. */
export type StateQueueOutputValue = Readonly<{
  /** Non-ADA assets: policy id hex to asset name hex to quantity. */
  assets: ReadonlyMap<string, ReadonlyMap<string, bigint>>;
  /** The inline datum's exact CBOR, or null. */
  datum: Uint8Array | null;
}>;

/** One state-queue output as every role reads it. Hex is lowercase. */
export type DecodedStateQueueOutput = Readonly<{
  kind: "root" | "node" | "invalid";
  assetName: string | null;
  /** A node's linked-list key: its asset name without the block prefix. */
  nodeKey: string | null;
  /** The key the element links to; null at the tail. */
  nextKey: string | null;
  /** A node: blake2b-224 of its header. The root: the confirmed header hash. */
  headerHash: string | null;
  /** A node's DA status identity (`daAvailabilityStateQueueStatusIdentity`). */
  daStatus: string | null;
  endTimeMs: bigint | null;
  problems: readonly StateQueueOutputProblem[];
  /** The decoded datum; null for an invalid output. */
  datum: LinkedListDatum | null;
}>;

const invalid = (
  assetName: string | null,
  problem: StateQueueOutputProblem,
): DecodedStateQueueOutput => ({
  kind: "invalid",
  assetName,
  nodeKey: null,
  nextKey: null,
  headerHash: null,
  daStatus: null,
  endTimeMs: null,
  problems: [problem],
  datum: null,
});

/** The queue token's asset name, and whether the value is exactly that NFT. */
const queueToken = (
  value: StateQueueOutputValue,
  policyId: string,
): Readonly<{ assetName: string | null; single: boolean }> => {
  const entries = [...value.assets.entries()].flatMap(([policy, names]) =>
    [...names.entries()]
      .filter(([, quantity]) => quantity !== 0n)
      .map(([name, quantity]) => ({ policy, name, quantity })),
  );
  const ours = entries.find(({ policy }) => policy === policyId);
  const only = entries[0];
  return {
    assetName: ours?.name ?? null,
    single:
      entries.length === 1 &&
      only !== undefined &&
      only.policy === policyId &&
      only.quantity === 1n,
  };
};

const decodeLinkedList = (datum: Uint8Array): LinkedListDatum | null => {
  try {
    return Data.from(Buffer.from(datum).toString("hex"), LinkedListDatum);
  } catch {
    return null;
  }
};

const decodeRoot = (data: Data): ConfirmedState | null => {
  try {
    return Data.castFrom(data, ConfirmedState);
  } catch {
    return null;
  }
};

const decodeNode = (data: Data): StateQueueNode | null => {
  try {
    const node = Data.castFrom(data, StateQueueNode);
    return node.header.protocolVersion === BigInt(MIDGARD_PROTOCOL_VERSION)
      ? node
      : null;
  } catch {
    return null;
  }
};

/** blake2b-224 of a V1 header's canonical Plutus Data (its state-queue key). */
export const stateQueueHeaderHash = (header: Header): string =>
  Buffer.from(blake2b(encodeHeaderCbor(header), { dkLen: 28 })).toString("hex");

/**
 * Reads one output as a state-queue element. Returns null for an output
 * that carries nothing under the queue policy (a third party's payment to
 * the queue address, never part of the queue). A pure function of the
 * output.
 */
export const decodeStateQueueOutput = (
  value: StateQueueOutputValue,
  stateQueuePolicyId: string,
): DecodedStateQueueOutput | null => {
  const token = queueToken(value, stateQueuePolicyId);
  if (token.assetName === null) return null;
  if (!token.single) return invalid(token.assetName, "malformed_nft");
  const assetName = token.assetName;
  const datum = value.datum === null ? null : decodeLinkedList(value.datum);
  if (datum === null) return invalid(assetName, "malformed_datum");
  const nextKey = datum.link === null ? null : datum.link.toLowerCase();
  if ("Root" in datum.data) {
    const root = decodeRoot(datum.data.Root.data);
    if (root === null) return invalid(assetName, "malformed_datum");
    return {
      kind: "root",
      assetName,
      nodeKey: null,
      nextKey,
      headerHash: root.headerHash.toLowerCase(),
      daStatus: null,
      endTimeMs: root.endTime,
      problems: [],
      datum,
    };
  }
  const node = decodeNode(datum.data.Node.data);
  if (node === null) return invalid(assetName, "malformed_datum");
  const view = linkedListDatumToNodeView(datum, assetName);
  const nodeKey = view.key === "Empty" ? "" : view.key.Key.key.toLowerCase();
  const prefix = STATE_QUEUE_NODE_ASSET_NAME_PREFIX;
  const headerHash = stateQueueHeaderHash(node.header);
  const problems: StateQueueOutputProblem[] = [];
  if (nodeKey !== headerHash) problems.push("linked_list_key_mismatch");
  if (!assetName.startsWith(prefix))
    problems.push("block_asset_prefix_mismatch");
  else if (assetName.slice(prefix.length) !== headerHash)
    problems.push("block_asset_suffix_mismatch");
  return {
    kind: "node",
    assetName,
    nodeKey,
    nextKey,
    headerHash,
    daStatus: daAvailabilityStateQueueStatusIdentity(node.da_attestation),
    endTimeMs: node.header.endTime,
    problems,
    datum,
  };
};
