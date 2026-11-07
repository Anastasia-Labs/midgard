import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
} from "@al-ft/midgard-core";
import {
  readCborBytes,
  readCborMapHeader,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectScriptMaterial } from "./material.js";

/** Same address-only projection as the on-chain receive prefix reader. */
export const directOutputAddress = (item: Uint8Array) => {
  const header = readCborMapHeader(item, 0);
  const key = readCborUnsigned(item, header.nextOffset);
  if (header.length < 2 || header.length > 4 || key.value !== 0n)
    throw new Error("direct script derivation: output address prefix differs");
  const address = readCborBytes(item, key.nextOffset);
  return decodeMidgardAddressBytes(address.value);
};

export type DirectReceiveCredential = Readonly<{
  hash: string;
  firstOutputIndex: number;
}>;

export const directReceiveCredentials = (
  outputs: readonly Uint8Array[],
): readonly DirectReceiveCredential[] => {
  const first = new Map<string, number>();
  for (const [index, item] of outputs.entries()) {
    const address = directOutputAddress(item);
    if (address.protected && address.paymentCredential.kind === "Script") {
      const hash = address.paymentCredential.hash.toString("hex");
      if (!first.has(hash)) first.set(hash, index);
    }
  }
  return [...first]
    .map(([hash, firstOutputIndex]) => ({ hash, firstOutputIndex }))
    .sort((left, right) =>
      Buffer.compare(
        Buffer.from(left.hash, "hex"),
        Buffer.from(right.hash, "hex"),
      ),
    );
};

/** Keep the raw location as evidence while selecting the producer's spend-only
 * rank. Sorting a copy must not rewrite the committed field or its openings. */
export const directSpendInputAt = (
  material: DirectScriptMaterial,
  index: bigint,
): Readonly<{ item: Buffer; rawItemIndex: number }> | null => {
  if (index < 0n)
    throw new Error("direct script derivation: purpose coordinate differs");
  const items = material.fields[0]!;
  if (index >= BigInt(items.length)) return null;
  return items
    .map((item, rawItemIndex) => ({ item, rawItemIndex }))
    .sort((left, right) => Buffer.compare(left.item, right.item))[
    Number(index)
  ]!;
};

/** Position is always the canonical purpose ordinal. First output position
 * stays auxiliary evidence; InputSets traversal position is a separate cursor. */
export const directPurposeAt = (
  material: DirectScriptMaterial,
  coordinate: SDK.DirectPurposeCoordinate,
): SDK.DirectPurpose | null => {
  if (coordinate.kind < 0n || coordinate.kind > 3n || coordinate.index < 0n)
    throw new Error("direct script derivation: purpose coordinate differs");
  if (coordinate.kind === 0n) {
    const selected = directSpendInputAt(material, coordinate.index);
    if (selected === null) return null;
    const { item } = selected;
    const value = material.ledger.root.entries.find((entry) =>
      entry.key.equals(item),
    )?.value;
    if (value === undefined)
      throw new Error("direct script derivation: spent descriptor absent");
    const address = decodeMidgardAddressBytes(
      decodeMidgardLedgerOutputCommitment(value).address,
    );
    return address.paymentCredential.kind === "Script"
      ? {
          coordinate,
          script_hash: address.paymentCredential.hash.toString("hex"),
          subject: item.toString("hex"),
        }
      : null;
  }
  if (coordinate.kind === 1n) {
    if (coordinate.index >= BigInt(material.fields[5]!.length)) return null;
    const item = material.fields[5]![Number(coordinate.index)];
    if (item === undefined) return null;
    const policy = Buffer.from(
      decodeMidgardMintPolicyItem(item).policyId,
    ).toString("hex");
    return { coordinate, script_hash: policy, subject: policy };
  }
  if (coordinate.kind === 2n) {
    if (coordinate.index >= BigInt(material.fields[3]!.length)) return null;
    const item = material.fields[3]![Number(coordinate.index)];
    if (item === undefined) return null;
    if (item.length !== 28)
      throw new Error("direct script derivation: observer hash differs");
    return {
      coordinate,
      script_hash: item.toString("hex"),
      subject: item.toString("hex"),
    };
  }
  const credentials = directReceiveCredentials(material.fields[2]!);
  if (coordinate.index >= BigInt(credentials.length)) return null;
  const credential = credentials[Number(coordinate.index)];
  return credential === undefined
    ? null
    : { coordinate, script_hash: credential.hash, subject: credential.hash };
};
