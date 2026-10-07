import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core/consensus-profile";
import {
  blake2b224,
  type DerivationHook,
  type OutputSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { COMMITTEE_QUEUE_TABLE } from "./queue-table.js";

/**
 * Why a state-queue output cannot be read as a queue element. An output
 * carrying the state-queue policy is a queue output whatever its shape; a
 * malformed one makes the landed queue unhealthy instead of being dropped.
 */
export type QueueOutputProblem =
  /** Not exactly one non-ADA asset, of quantity 1, under the queue policy. */
  | "malformed_nft"
  /** No inline datum, or one that is not a root or a V1 node. */
  | "malformed_datum"
  // The three node checks the current scanner makes (state-queue-scanner.ts).
  | "linked_list_key_mismatch"
  | "block_asset_prefix_mismatch"
  | "block_asset_suffix_mismatch";

/** One state-queue output as the committee reads it. Hex is lowercase. */
export type DecodedQueueOutput = Readonly<{
  kind: "root" | "node" | "invalid";
  assetName: string | null;
  /** A node's linked-list key: its asset name without the canonical prefix. */
  nodeKey: string | null;
  /** The key the element links to; null for the tail (or the empty queue's root). */
  nextKey: string | null;
  /** A node: blake2b-224 of its header. The root: the confirmed header hash. */
  headerHash: string | null;
  /** A node's DA status identity (`daAvailabilityStateQueueStatusIdentity`). */
  daStatus: string | null;
  endTimeMs: bigint | null;
  problems: readonly QueueOutputProblem[];
}>;

const invalid = (
  assetName: string | null,
  problem: QueueOutputProblem,
): DecodedQueueOutput => ({
  kind: "invalid",
  assetName,
  nodeKey: null,
  nextKey: null,
  headerHash: null,
  daStatus: null,
  endTimeMs: null,
  problems: [problem],
});

/** The single queue token's asset name, or null when the value is not one NFT. */
const queueToken = (
  output: OutputSummary,
  policyId: string,
): Readonly<{ assetName: string | null; single: boolean }> => {
  const entries = [...output.assets.entries()].flatMap(([policy, names]) =>
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

const decodeLinkedList = (datum: Buffer): SDK.LinkedListDatum | null => {
  try {
    return Data.from(datum.toString("hex"), SDK.LinkedListDatum);
  } catch {
    return null;
  }
};

const decodeRoot = (
  data: unknown,
): Readonly<{ headerHash: string; endTime: bigint }> | null => {
  try {
    const state = Data.castFrom(
      data as Parameters<typeof Data.castFrom>[0],
      SDK.ConfirmedState,
    );
    return {
      headerHash: state.headerHash.toLowerCase(),
      endTime: state.endTime,
    };
  } catch {
    return null;
  }
};

const decodeNode = (data: unknown): SDK.StateQueueNode | null => {
  try {
    const node = Data.castFrom(
      data as Parameters<typeof Data.castFrom>[0],
      SDK.StateQueueNode,
    );
    return node.header.protocolVersion === BigInt(MIDGARD_PROTOCOL_VERSION)
      ? node
      : null;
  } catch {
    return null;
  }
};

/** blake2b-224 of the header's canonical Plutus Data, as the scanner hashes it. */
export const headerHashOf = (header: SDK.Header): string =>
  blake2b224(SDK.encodeHeaderCbor(header)).toString("hex");

/**
 * Reads one output at the state-queue address. Returns null for an output
 * that carries nothing under the queue policy (a third party's payment to
 * the address, never part of the queue). A pure function of the output.
 */
export const decodeQueueOutput = (
  output: OutputSummary,
  stateQueuePolicyId: string,
): DecodedQueueOutput | null => {
  const token = queueToken(output, stateQueuePolicyId);
  if (token.assetName === null) return null;
  if (!token.single) return invalid(token.assetName, "malformed_nft");
  const assetName = token.assetName;
  const datum = output.datum === null ? null : decodeLinkedList(output.datum);
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
      headerHash: root.headerHash,
      daStatus: null,
      endTimeMs: root.endTime,
      problems: [],
    };
  }
  const node = decodeNode(datum.data.Node.data);
  if (node === null) return invalid(assetName, "malformed_datum");
  const view = SDK.linkedListDatumToNodeView(datum, assetName);
  const nodeKey = view.key === "Empty" ? "" : view.key.Key.key.toLowerCase();
  const headerHash = headerHashOf(node.header);
  const problems: QueueOutputProblem[] = [];
  if (nodeKey !== headerHash) problems.push("linked_list_key_mismatch");
  const prefix = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;
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
    daStatus: SDK.daAvailabilityStateQueueStatusIdentity(node.da_attestation),
    endTimeMs: node.header.endTime,
    problems,
  };
};

export type CommitteeQueueParameters = Readonly<{
  /** The state-queue address, as raw address bytes. */
  stateQueueAddress: Buffer;
  /** The state-queue NFT policy id, lowercase hex. */
  stateQueuePolicyId: string;
}>;

/**
 * The S3 derivation that maintains `committee_queue_outputs`: closes the row
 * of every queue output a qualifying tx spends, and inserts a row for every
 * queue output it creates, in block order. It reads nothing but the block.
 */
export const committeeQueueDerivation = (
  parameters: CommitteeQueueParameters,
): DerivationHook => ({
  name: "committee-queue",
  writes: [COMMITTEE_QUEUE_TABLE],
  apply: async ({ tx, block, qualified }) => {
    const slot = block.point.slot;
    for (const entry of qualified) {
      for (const outRef of entry.spent)
        await tx.query(
          `UPDATE ${COMMITTEE_QUEUE_TABLE} SET spent_slot = ? WHERE tx_hash = ? AND output_index = ? AND spent_slot IS NULL`,
          [slot, outRef.txHash, outRef.index],
        );
      for (const { outRef, output } of entry.created) {
        if (!output.address.equals(parameters.stateQueueAddress)) continue;
        const decoded = decodeQueueOutput(
          output,
          parameters.stateQueuePolicyId,
        );
        if (decoded === null) continue;
        await tx.query(
          `INSERT INTO ${COMMITTEE_QUEUE_TABLE} (tx_hash, output_index, kind, asset_name, node_key, next_key, header_hash, da_status, end_time_ms, problems, datum, created_slot, created_height, created_tx_index, spent_slot) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)`,
          [
            outRef.txHash,
            outRef.index,
            decoded.kind,
            decoded.assetName,
            decoded.nodeKey,
            decoded.nextKey,
            decoded.headerHash,
            decoded.daStatus,
            decoded.endTimeMs,
            decoded.problems.join(","),
            output.datum,
            slot,
            block.height,
            entry.tx.index,
          ],
        );
      }
    }
  },
});
