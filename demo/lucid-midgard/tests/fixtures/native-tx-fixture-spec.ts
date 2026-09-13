import { blake2b } from "@noble/hashes/blake2.js";
import { expect } from "vitest";

import type { NativeTxFixtureFacets } from "./native-tx-fixture-shape.js";

/**
 * An independent reference model for everything a native-transaction
 * conformance fixture advertises.
 *
 * `deriveNativeTxFixtureFacets` computes those facets by calling the very
 * codec the fixtures exist to pin, so a test that compares a fixture against
 * that derivation — or against the file the derivation last wrote — has no
 * oracle at all: it asserts the codec agrees with itself. This module derives
 * the same facets a second time, from the specification rather than from the
 * codec: the §5 field-commitment rule (each commitment is the Blake2b-256 of
 * that field's own preimage bytes), the compact-witness-set and compact-body
 * layouts, and the domain-separated transaction id preimage
 * `"MidgardNativeTxBodyV1" ‖ cbor(version) ‖ compact_body_cbor`.
 *
 * The one primitive it shares with production is Blake2b-256 itself, which is
 * RFC 7693, not Midgard behavior. Everything above it — which bytes are
 * hashed, in what order, under what framing — is written out here by hand and
 * would disagree with the codec if the codec changed.
 */

const hashHex = (bytes: Uint8Array): string =>
  Buffer.from(blake2b(bytes, { dkLen: 32 })).toString("hex");

const bytesOf = (hex: string): Uint8Array => Buffer.from(hex, "hex");

/** CBOR head byte(s) for a definite-length array of `count` items. */
export const cborArrayHeaderHex = (count: number): string => {
  if (count < 24) return (0x80 + count).toString(16).padStart(2, "0");
  if (count < 0x100) return `98${count.toString(16).padStart(2, "0")}`;
  if (count < 0x10000) return `99${count.toString(16).padStart(4, "0")}`;
  throw new Error(`Fixture array too long for this reference model: ${count}`);
};

/** CBOR encoding of a non-negative integer (major type 0). */
export const cborUintHex = (value: bigint): string => {
  if (value < 0n) throw new Error(`Not a CBOR uint: ${value.toString(10)}`);
  if (value < 24n) return Number(value).toString(16).padStart(2, "0");
  if (value < 0x100n) return `18${value.toString(16).padStart(2, "0")}`;
  if (value < 0x10000n) return `19${value.toString(16).padStart(4, "0")}`;
  if (value < 0x100000000n) return `1a${value.toString(16).padStart(8, "0")}`;
  return `1b${value.toString(16).padStart(16, "0")}`;
};

const FIELD_NAMES = [
  "spendInputs",
  "referenceInputs",
  "outputs",
  "requiredObservers",
  "requiredSigners",
  "mint",
  "addrTxWits",
  "scriptTxWits",
  "redeemerTxWits",
] as const;

/**
 * §5.3's fixed-index out-ref item: a 38-byte byte string holding
 * `82 ‖ 5820 tx_id ‖ 19 index_be16`. The list itself is a definite-length
 * array of exactly those items, and no out-ref may repeat.
 *
 * Ascending order is *not* a canonical-decoding requirement — the
 * size-balanced fixture declares its 40 key-witnessed inputs ahead of its 8
 * script-witnessed ones and decodes fine — so it is asserted only for the
 * fixtures built through `LucidMidgard`, whose builder sorts the input list
 * and whose redeemer pointers are indices into that sorted list.
 */
const OUTREF_ITEM = /^5826825820([0-9a-f]{64})19([0-9a-f]{4})$/u;

export const parseOutRefListPreimage = (
  preimageHex: string,
  expectedCount: number,
  label: string,
  requireAscending = false,
): readonly { readonly txHash: string; readonly index: number }[] => {
  const header = cborArrayHeaderHex(expectedCount);
  expect(
    preimageHex.slice(0, header.length),
    `${label}: array header for ${String(expectedCount)} items`,
  ).toBe(header);
  const body = preimageHex.slice(header.length);
  expect(
    body.length,
    `${label}: ${String(expectedCount)} × 40-byte items`,
  ).toBe(expectedCount * 40 * 2);
  const items: string[] = [];
  for (let offset = 0; offset < body.length; offset += 40 * 2) {
    items.push(body.slice(offset, offset + 40 * 2));
  }
  const parsed = items.map((item, index) => {
    const match = OUTREF_ITEM.exec(item);
    expect(
      match,
      `${label}[${String(index)}] is not a §5.3 out-ref item`,
    ).not.toBeNull();
    return { txHash: match![1]!, index: Number.parseInt(match![2]!, 16) };
  });
  if (requireAscending) {
    expect(
      [...items].sort(),
      `${label}: items must be strictly ascending`,
    ).toEqual(items);
  }
  expect(new Set(items).size, `${label}: items must be unique`).toBe(
    items.length,
  );
  return parsed;
};

export type NativeTxFixtureSpecExpectations = {
  readonly label: string;
  readonly version: bigint;
  readonly spendInputs: number;
  readonly referenceInputs: number;
  readonly mintPolicies: number;
  readonly redeemers: number;
  /** Set for fixtures whose inputs are built through the sorting builder. */
  readonly sortedInputs?: boolean;
};

/**
 * Re-derives, from the specification, every facet the fixture advertises about
 * its own canonical bytes, and requires the fixture to agree.
 */
export const expectNativeTxFixtureFacetsSatisfySpec = (
  facets: NativeTxFixtureFacets,
  expectations: NativeTxFixtureSpecExpectations,
): void => {
  const { label } = expectations;

  // Each declared byte count is the length of the bytes it claims to count.
  expect(facets.sizes.fullTxCborBytes, `${label}: fullTxCborBytes`).toBe(
    facets.fullTxCborHex.length / 2,
  );
  expect(facets.sizes.compactTxCborBytes, `${label}: compactTxCborBytes`).toBe(
    facets.compactTxCborHex.length / 2,
  );
  expect(
    facets.sizes.compactBodyCborBytes,
    `${label}: compactBodyCborBytes`,
  ).toBe(facets.compactBodyCborHex.length / 2);

  // §5: every field commitment is Blake2b-256 over that field's own preimage,
  // every preimage appears verbatim in the canonical transaction, and the
  // declared preimage sizes match the preimages.
  for (const field of FIELD_NAMES) {
    const preimageHex = facets.preimages[`${field}CborHex`];
    expect(
      facets.hashes[`${field}HashHex`],
      `${label}: ${field} commitment`,
    ).toBe(hashHex(bytesOf(preimageHex)));
    expect(
      facets.sizes.preimages[field],
      `${label}: ${field} preimage size`,
    ).toBe(preimageHex.length / 2);
    expect(
      facets.fullTxCborHex.includes(preimageHex),
      `${label}: ${field} preimage is not present in the canonical transaction`,
    ).toBe(true);
  }

  // The compact witness set is the three witness commitments in field order;
  // its hash is what the compact transaction carries.
  const witnessSetCbor = `83${(
    ["addrTxWits", "scriptTxWits", "redeemerTxWits"] as const
  )
    .map((field) => `5820${facets.hashes[`${field}HashHex`]}`)
    .join("")}`;
  expect(facets.hashes.witnessSetHashHex, `${label}: witness-set hash`).toBe(
    hashHex(bytesOf(witnessSetCbor)),
  );

  // The compact body is a 12-item array: the three input/output commitments,
  // the three scalars, the three remaining body commitments, the script
  // integrity and auxiliary-data hashes, and the network id.
  const bodyPrefix = `${cborArrayHeaderHex(12)}5820${facets.hashes.spendInputsHashHex}5820${facets.hashes.referenceInputsHashHex}5820${facets.hashes.outputsHashHex}${cborUintHex(BigInt(facets.sizes.fee))}`;
  expect(
    facets.compactBodyCborHex.startsWith(bodyPrefix),
    `${label}: compact body must open with the three field commitments and the fee`,
  ).toBe(true);
  const bodyMiddle = `5820${facets.hashes.requiredObserversHashHex}5820${facets.hashes.requiredSignersHashHex}5820${facets.hashes.mintHashHex}`;
  expect(
    facets.compactBodyCborHex.includes(bodyMiddle),
    `${label}: compact body must carry the observer, signer and mint commitments in field order`,
  ).toBe(true);

  // The transaction id is the domain-separated hash of the compact body.
  expect(facets.txIdHex, `${label}: transaction id`).toBe(
    hashHex(
      Buffer.concat([
        Buffer.from("MidgardNativeTxBodyV1", "ascii"),
        bytesOf(cborUintHex(expectations.version)),
        bytesOf(facets.compactBodyCborHex),
      ]),
    ),
  );

  // The two out-ref fields are canonical §5.3 lists of the declared size.
  parseOutRefListPreimage(
    facets.preimages.spendInputsCborHex,
    expectations.spendInputs,
    `${label}: spend inputs`,
    expectations.sortedInputs ?? false,
  );
  parseOutRefListPreimage(
    facets.preimages.referenceInputsCborHex,
    expectations.referenceInputs,
    `${label}: reference inputs`,
    expectations.sortedInputs ?? false,
  );

  // §5.6 commits mint policies in ascending policy-id order, which is the
  // order a script context observes.
  expect(
    facets.mintPolicyIdsInTxInfoOrder.length,
    `${label}: mint policy count`,
  ).toBe(expectations.mintPolicies);
  for (const policyId of facets.mintPolicyIdsInTxInfoOrder) {
    expect(policyId, `${label}: policy id shape`).toMatch(/^[0-9a-f]{56}$/u);
  }
  expect(
    [...facets.mintPolicyIdsInTxInfoOrder].sort(),
    `${label}: mint policy ids must be ascending`,
  ).toEqual([...facets.mintPolicyIdsInTxInfoOrder]);

  // Field 8 carries one pointer per redeemer, ordered by (purpose tag, index)
  // and free of duplicates.
  expect(facets.redeemerPointers.length, `${label}: redeemer count`).toBe(
    expectations.redeemers,
  );
  const pointerKey = (pointer: string): readonly [number, number] => {
    const [tag, index] = pointer.split(":");
    expect(
      index,
      `${label}: malformed redeemer pointer ${pointer}`,
    ).toBeDefined();
    return [Number(tag), Number(index)];
  };
  const keys = facets.redeemerPointers.map(pointerKey);
  for (let index = 1; index < keys.length; index += 1) {
    const previous = keys[index - 1]!;
    const current = keys[index]!;
    expect(
      previous[0] < current[0] ||
        (previous[0] === current[0] && previous[1] < current[1]),
      `${label}: redeemer pointers must strictly ascend (${facets.redeemerPointers[index - 1]!} then ${facets.redeemerPointers[index]!})`,
    ).toBe(true);
  }
};
