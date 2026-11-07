import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { midgardScriptSourcePhysicalIndex } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import type { DirectScriptMaterial } from "./material.js";
import {
  directResolvedSources,
  type DirectScriptSourceMaterial,
  directWitnessSource,
} from "./sources.js";

const rootHex = (hash: Uint8Array | null | undefined): string =>
  hash === null ||
  hash === undefined ||
  Buffer.from(hash).equals(Buffer.alloc(32))
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(hash).toString("hex");
const descriptorBytes = (source: SDK.DirectScriptSource) =>
  Buffer.from(Data.to(source, SDK.DirectScriptSource), "hex");
const inlineSource = (source: SDK.DirectScriptSource) =>
  source.field_index === 6n;

/** The selected source for a script hash is the first one the T224 lookup
 * visits, named by the shared source-order function. Two same-hash candidates
 * form a two-entry physical frontier: inline witnesses first in item order,
 * then resolved-output reference scripts (spent and reference inputs) in
 * canonical out-ref order. `left` precedes when it sits at the physical slot
 * the lookup visits first. */
export const directSourcePrecedes = (
  left: SDK.DirectScriptSource,
  right: SDK.DirectScriptSource,
): boolean => {
  if (left.script_hash !== right.script_hash)
    throw new Error("direct source priority compares different hashes");
  const leftInline = inlineSource(left);
  const rightInline = inlineSource(right);
  const leftFirstInClass = leftInline
    ? left.item_index < right.item_index
    : Buffer.compare(
        Buffer.from(left.source_key, "hex"),
        Buffer.from(right.source_key, "hex"),
      ) < 0;
  const leftSlot =
    leftInline !== rightInline
      ? leftInline
        ? 0
        : 1
      : leftFirstInClass
        ? 0
        : 1;
  const inlineCount = Number(leftInline) + Number(rightInline);
  return leftSlot === midgardScriptSourcePhysicalIndex(inlineCount, 2, 0);
};

/** Build a single source map from the authenticated source and root-checked
 * event ledger. The physical worker must still authenticate every input item;
 * this producer never upgrades a free descriptor into proof authority. */
export const buildDirectSourceIndexEvidence = async (
  material: DirectScriptMaterial,
) => {
  const resolved = await Promise.all([
    directResolvedSources(material, 0),
    directResolvedSources(material, 1),
  ]);
  const inputs = resolved.flatMap((entries, fieldIndex) =>
    entries.map((entry, itemIndex) => ({
      fieldIndex,
      itemIndex,
      witness: entry.witness,
      source: entry.source,
    })),
  );
  const candidates = [
    ...inputs
      .filter((entry) => entry.source !== null)
      .map((entry) => entry.source!),
    ...material.fields[6]!.map((item, index) =>
      directWitnessSource(item, index),
    ),
  ];
  return { inputs, ...(await buildDirectSourceIndex(candidates)) };
};

/** Admit authenticated candidates, in scan order, into the single source map
 * and record each admission the map workers replay on-chain. */
export const buildDirectSourceIndex = async (
  candidates: readonly DirectScriptSourceMaterial[],
) => {
  const trie = await Trie.fromList([]);
  const selected = new Map<string, DirectScriptSourceMaterial>();
  const steps: {
    candidate: DirectScriptSourceMaterial;
    admission: SDK.DirectSourceAdmission;
    before: SDK.DirectSourceIndex;
    after: SDK.DirectSourceIndex;
  }[] = [];
  for (const candidate of candidates) {
    const source = candidate.descriptor;
    const key = Buffer.from(source.script_hash, "hex");
    const previous = selected.get(source.script_hash);
    const before = { root: rootHex(trie.hash), count: BigInt(selected.size) };
    let admission: SDK.DirectSourceAdmission;
    if (previous === undefined) {
      await trie.insert(key, descriptorBytes(source));
      const proved = await trie.prove(key);
      if (rootHex(proved.verify(false)) !== before.root)
        throw new Error("direct source index insertion predecessor differs");
      admission = {
        First: { proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof) },
      };
      selected.set(source.script_hash, candidate);
    } else {
      const proved = await trie.prove(key);
      if (rootHex(proved.verify(true)) !== before.root)
        throw new Error("direct source index existing root differs");
      admission = {
        Existing: {
          previous: previous.descriptor,
          proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof),
        },
      };
      if (directSourcePrecedes(source, previous.descriptor)) {
        await trie.delete(key);
        await trie.insert(key, descriptorBytes(source));
        if (
          !Buffer.from((await trie.prove(key)).toCBOR()).equals(
            Buffer.from(proved.toCBOR()),
          )
        )
          throw new Error(
            "direct source index value update changed proof path",
          );
        selected.set(source.script_hash, candidate);
      }
    }
    steps.push({
      candidate,
      admission,
      before,
      after: { root: rootHex(trie.hash), count: BigInt(selected.size) },
    });
  }
  const committed = {
    root: rootHex(trie.hash),
    count: BigInt(selected.size),
    entries: [...selected].map(([hash, source]) => ({
      key: Buffer.from(hash, "hex"),
      value: descriptorBytes(source.descriptor),
    })),
  };
  const lookup = async (hash: string) => {
    const source = selected.get(hash) ?? null;
    const proof =
      source === null
        ? await keyValuePhasNonMembershipProof(
            committed,
            Buffer.from(hash, "hex"),
          )
        : await keyValuePhasProof(
            committed,
            Buffer.from(hash, "hex"),
            descriptorBytes(source.descriptor),
          );
    return { selected: source?.descriptor ?? null, proof };
  };
  return {
    lookup,
    steps,
    selected,
    index: { root: rootHex(trie.hash), count: BigInt(selected.size) },
  };
};
