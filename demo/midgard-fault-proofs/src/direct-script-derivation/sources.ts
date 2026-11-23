import {
  buildMidgardBoundedItem,
  computeHash28,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardSpendInputItem,
  decodeMidgardVersionedScriptEnvelope,
  MidgardScriptHashPrefixes,
} from "@al-ft/midgard-core";
import { encodeCborUnsigned } from "@al-ft/midgard-core/codec/cbor";
import type * as SDK from "@al-ft/midgard-sdk/fraud-proof";

import { retainedOutputReferenceScript } from "../evidence/retained-ledger-output.js";
import type { DirectScriptMaterial } from "./material.js";

export type DirectScriptSourceMaterial = Readonly<{
  descriptor: SDK.DirectScriptSource;
  item: Buffer;
}>;

/** Hash the exact versioned outer bytes even when a native payload is the
 * decoding family's fault. Never decode/reencode its native AST to select it. */
export const directWitnessSource = (
  item: Buffer,
  index: number,
): DirectScriptSourceMaterial => {
  const envelope = decodeMidgardVersionedScriptEnvelope(item);
  const tag = MidgardScriptHashPrefixes[envelope.language];
  return {
    item,
    descriptor: {
      field_index: 6n,
      item_index: BigInt(index),
      source_key: encodeCborUnsigned(BigInt(index)).toString("hex"),
      language_tag: BigInt(tag),
      script_hash: computeHash28(
        Buffer.concat([Buffer.from([tag]), envelope.scriptBytes]),
      ).toString("hex"),
      total_length: BigInt(item.length),
      item_commitment: buildMidgardBoundedItem({
        fieldIndex: 6,
        itemIndex: index,
        bytes: item,
      }).commitment.toString("hex"),
    },
  };
};

export const directResolvedSources = async (
  material: DirectScriptMaterial,
  fieldIndex: 0 | 1,
) => {
  const { byKey, proof } = material.resolvedInputs;
  const openings = material.fields[fieldIndex]!.map((key) => {
    const input = byKey.get(key.toString("hex"));
    if (input === undefined)
      throw new Error("direct script derivation: resolved descriptor absent");
    return { key, ...input };
  });
  const proofs = await Promise.all(openings.map(({ key }) => proof(key)));
  return openings.map(({ key, leaf, output }, index) => {
    const witness: SDK.DirectResolvedSourceWitness = {
      descriptor_cbor: leaf.toString("hex"),
      proof: proofs[index]!,
    };
    const descriptor = decodeMidgardLedgerOutputCommitment(leaf);
    const outref = decodeMidgardSpendInputItem(key);
    if (descriptor.outputIndex !== outref.outputIndex)
      throw new Error(
        "direct script derivation: descriptor output index differs",
      );
    if (descriptor.referenceScriptLanguage === -1)
      return { witness, source: null };
    const item = retainedOutputReferenceScript(output);
    if (item === null)
      throw new Error("direct script derivation: retained script absent");
    const envelope = decodeMidgardVersionedScriptEnvelope(item);
    const tag = MidgardScriptHashPrefixes[envelope.language];
    const hash = computeHash28(
      Buffer.concat([Buffer.from([tag]), envelope.scriptBytes]),
    ).toString("hex");
    const commitment = buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: outref.outputIndex,
      bytes: item,
    }).commitment.toString("hex");
    if (
      tag !== descriptor.referenceScriptLanguage ||
      hash !== descriptor.referenceScriptHash.toString("hex") ||
      item.length !== descriptor.referenceScriptTotalLength ||
      commitment !== descriptor.referenceScriptItemCommitment.toString("hex")
    )
      throw new Error(
        "direct script derivation: exact retained script differs from checked descriptor",
      );
    const source: DirectScriptSourceMaterial = {
      item,
      descriptor: {
        field_index: BigInt(fieldIndex),
        item_index: BigInt(index),
        source_key: key.toString("hex"),
        language_tag: BigInt(tag),
        script_hash: hash,
        total_length: BigInt(item.length),
        item_commitment: commitment,
      },
    };
    return { witness, source };
  });
};
