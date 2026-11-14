import {
  computeHash32,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { isMidgardWitnessSetField } from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpening } from "../field-opening.js";
import type { DirectScriptMaterial } from "./material.js";

const u24 = (value: number): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xff_ffff)
    throw new Error(
      "direct field plan: scalar exceeds canonical checkpoint width",
    );
  const bytes = Buffer.alloc(3);
  bytes.writeUIntBE(value, 0, 3);
  return bytes;
};

/** Positions are built once from every actual envelope item. Continuations
 * select an existing position; they never rewalk all earlier output prefixes. */
export const createDirectFieldPlan = (
  material: DirectScriptMaterial,
  fieldIndex: number,
  owner: string,
  options: { readonly publish?: boolean } = {},
) => {
  if (!Number.isInteger(fieldIndex) || fieldIndex < 0 || fieldIndex > 8)
    throw new Error("direct field plan: field index differs");
  const sourceKind = material.bound.subject.source_kind;
  if (sourceKind !== 0n && sourceKind !== 1n)
    throw new Error("direct field plan: source kind differs");
  const witness = deriveMidgardNativeTxWitnessSetCompact(
    material.material.canonical.witnessSet,
  );
  const items = material.fields[fieldIndex]!;
  const preimage = encodeMidgardFieldPreimage(items);
  if (!preimage.equals(material.material.fieldPreimages[fieldIndex]!))
    throw new Error("direct field plan: exact field envelope differs");
  const openingPlan = planFaultProofFieldOpening({
    anchorSourceKind: sourceKind,
    fieldIndex,
    anchorTxId: material.bound.subject.transaction_id,
    nativeTxCompactCbor: material.bound.compact_cbor,
    itemCbors: items,
    owner,
    publish: options.publish ?? false,
    ...(isMidgardWitnessSetField(fieldIndex)
      ? {
          witnessSet: {
            addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
            script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
            redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
          },
          anchorWitnessSetHash:
            material.material.compact.transactionWitnessSetHash.toString("hex"),
        }
      : {}),
    label: "direct field plan",
  });
  const offsets = [encodeMidgardFieldArrayHeader(items.length).length];
  for (const item of items)
    offsets.push(offsets.at(-1)! + encodeMidgardDefiniteBytes(item).length);
  const txId = Buffer.from(material.bound.subject.transaction_id, "hex");
  const commitment = midgardFieldCommitment(preimage);
  const position = (phase: "Certifying" | "Walking", index: number) => {
    if (!Number.isSafeInteger(index) || index < 0 || index > items.length)
      throw new Error("direct field plan: position exceeds actual items");
    const bytes = Buffer.concat([
      Buffer.from(
        phase === "Certifying" ? [0x87, 0x58, 0x20] : [0x86, 0x58, 0x20],
      ),
      txId,
      Buffer.from([0x41, fieldIndex]),
      ...(phase === "Certifying"
        ? [Buffer.from([0x58, 0x20]), commitment]
        : []),
      Buffer.from([0x43]),
      u24(preimage.length),
      Buffer.from([0x43]),
      u24(items.length),
      Buffer.from([0x43]),
      u24(index),
      Buffer.from([0x43]),
      u24(offsets[index]!),
    ]);
    const domain = Buffer.from(
      phase === "Certifying"
        ? "MidgardFieldGrammarCheckpointV1"
        : "MidgardFieldWalkCheckpointV1",
      "ascii",
    );
    const cursor: SDK.DirectFieldCursor = {
      bound: material.bound,
      field_index: BigInt(fieldIndex),
      phase,
      checkpoint_hash: computeHash32(Buffer.concat([domain, bytes])).toString(
        "hex",
      ),
    };
    return {
      cursor,
      checkpointBytes: bytes.toString("hex"),
      index,
      complete: index === items.length,
    };
  };
  return { fieldIndex, items, position, openingPlan };
};
