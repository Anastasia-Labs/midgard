import { type MidgardCekDataNode } from "@al-ft/midgard-core";

import { type MidgardCekConstantType } from "./cek-constant.js";
import { type Bytes } from "./cek-executor.build-midgard-cek-execution-graph.js";
import { constantMemoryFromPayloadNode } from "./cek-machine.data-node-topology-matches.js";

/**
 * What revealing a constant payload in full costs the on-chain step
 * verifier, in ExMemory, as measured on the pinned Aiken machine. The
 * verifier decodes the payload and commits to every Data item in it, so the
 * cost grows with the item count and, more slowly, with the bytes.
 */
export const MIDGARD_CEK_REVEALED_ITEM_MEMORY = 520_000n;
export const MIDGARD_CEK_REVEALED_BYTE_MEMORY = 2_100n;

/**
 * The most one payload revealed in full may cost. A Data, list or pair
 * constant that would cost more is committed to through its semantic tree
 * instead. A builtin step reveals at most three payloads, so three at this
 * bound, with the step itself, stay inside the evaluator's default budget.
 */
export const MIDGARD_CEK_MAX_REVEALED_PAYLOAD_MEMORY = 4_500_000n;

/** The authenticated Data material the count walks. */
export type MidgardCekRevealCostMaterial = {
  readonly node: (root: Bytes) => MidgardCekDataNode;
  readonly link: (root: Bytes) => {
    readonly head: Bytes;
    readonly tail: Bytes;
  };
  readonly pairLink: (root: Bytes) => {
    readonly key: Bytes;
    readonly value: Bytes;
    readonly tail: Bytes;
  };
};

/**
 * The Data items under `root`, counted until the count exceeds `limit`. Only
 * whether it does matters, so a node's children stop being queued once the
 * items counted and queued exceed `limit`: every queued item is still
 * counted, so the count still crosses `limit` exactly when the whole tree
 * has more than `limit` items, and a long list is read only up to the limit.
 */
const itemsUpTo = (
  material: MidgardCekRevealCostMaterial,
  root: Bytes,
  limit: bigint,
): bigint => {
  const pending: Bytes[] = [root];
  let count = 0n;
  for (let item = pending.pop(); item !== undefined; item = pending.pop()) {
    count += 1n;
    if (count > limit) return count;
    const node = material.node(item);
    if (node.kind === "map") {
      let cursor = node.entriesRoot;
      // Queued items are all counted, so stopping here keeps the result.
      for (
        let left = node.entriesCount;
        left > 0n && count + BigInt(pending.length) <= limit;
        left -= 1n
      ) {
        const entry = material.pairLink(cursor);
        pending.push(entry.key, entry.value);
        cursor = entry.tail;
      }
      continue;
    }
    const [first, length] =
      node.kind === "list"
        ? [node.itemsRoot, node.itemsCount]
        : node.kind === "constrSmall" || node.kind === "constrLarge"
          ? [node.fieldsRoot, node.fieldsCount]
          : [null, 0n];
    let cursor = first;
    // As for a map: queued items are all counted, so stopping keeps the result.
    for (
      let left = length;
      left > 0n && cursor !== null && count + BigInt(pending.length) <= limit;
      left -= 1n
    ) {
      const entry = material.link(cursor);
      pending.push(entry.head);
      cursor = entry.tail;
    }
  }
  return count;
};

/**
 * Whether revealing the `type` constant whose payload is the node at
 * `dataRoot` would cost the on-chain verifier more than one payload may.
 * Only Data, list and pair constants with a semantic form are weighed: every
 * builtin that reads one takes it through its semantic tree, while a scalar,
 * or a constant whose type has no semantic memory, is read directly.
 */
export const midgardCekRevealIsCostly = (
  material: MidgardCekRevealCostMaterial,
  type: MidgardCekConstantType,
  dataRoot: Bytes,
): boolean => {
  const node = material.node(dataRoot);
  if (
    (type.kind !== "data" && type.kind !== "list" && type.kind !== "pair") ||
    constantMemoryFromPayloadNode(type, node) === null
  ) {
    return false;
  }
  const byteMemory = node.cborLength * MIDGARD_CEK_REVEALED_BYTE_MEMORY;
  if (byteMemory > MIDGARD_CEK_MAX_REVEALED_PAYLOAD_MEMORY) return true;
  const limit =
    (MIDGARD_CEK_MAX_REVEALED_PAYLOAD_MEMORY - byteMemory) /
    MIDGARD_CEK_REVEALED_ITEM_MEMORY;
  return itemsUpTo(material, dataRoot, limit) > limit;
};
