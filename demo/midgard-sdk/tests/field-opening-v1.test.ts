import { readFileSync } from "node:fs";

import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  FieldOpening,
  fieldOpeningForField,
  isMidgardWitnessSetField,
  MIDGARD_FIELD_INDEX,
  MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX,
  MidgardFieldOpeningError,
  NativeTxAnchor,
  nativeTxAnchorForField,
} from "../src/fraud-proof/field-opening-v1.js";

/**
 * The off-chain half of the #575 rebind's two wire shapes. It cannot execute
 * Plutus, so it never claims to prove the door's behaviour — the Aiken family in
 * `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.test.ak` does that,
 * and the emulator flows in `demo/midgard-fault-proofs` prove the end-to-end
 * acceptance. What it proves is that the two sides agree on the wire: the
 * constructor order each sum declares, the record field order each variant
 * carries, and the §2.5 pairing a builder must not get wrong.
 *
 * The constructor orders are read out of the Aiken source rather than restated,
 * because Plutus tags a constructor by its *position*: reordering the
 * declaration silently re-points every anchor and every opening, and no type
 * checker on either side would catch it.
 */

const repositoryRoot = new URL("../../../", import.meta.url);
const AIKEN_MODULE =
  "onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak";

const readRepositoryFile = (relativePath: string): string =>
  readFileSync(new URL(relativePath, repositoryRoot), "utf8");

const aikenTypeBody = (typeName: string): string => {
  const source = readRepositoryFile(AIKEN_MODULE);
  const opening = new RegExp(`^pub type ${typeName} \\{$`, "mu").exec(source);
  if (opening?.index === undefined) {
    throw new Error(`${typeName} is no longer declared in ${AIKEN_MODULE}`);
  }
  const body = source.slice(opening.index + opening[0].length);
  const end = body.indexOf("\n}");
  if (end === -1) {
    throw new Error(`${typeName} has no closing brace`);
  }
  return body.slice(0, end);
};

/** Constructor names of an Aiken sum type, in declaration order. */
const aikenConstructorOrder = (typeName: string): readonly string[] =>
  [
    ...aikenTypeBody(typeName).matchAll(
      /^ {2}([A-Z][A-Za-z0-9]*)(?: \{|\s*$)/gmu,
    ),
  ].map((match) => match[1] as string);

/**
 * Field names of one variant of an Aiken sum, in declaration order — the order
 * PlutusData encodes them in positionally.
 *
 * Aiken writes a record variant on one line or across several depending on
 * width, and both forms occur in this module (`BodyAnchor { tx_id: ByteArray }`
 * against a multi-line `BodyFieldOpening`). The region is therefore taken by
 * matching braces rather than by looking for a line-terminated `{`: a parser
 * that understood only the multi-line form would throw on half the variants
 * here, and one that silently returned `[]` instead would make every assertion
 * below vacuous.
 */
const aikenVariantFieldOrder = (
  typeName: string,
  variant: string,
): readonly string[] => {
  const body = aikenTypeBody(typeName);
  const opening = new RegExp(`^ {2}${variant} \\{`, "mu").exec(body);
  if (opening?.index === undefined) {
    throw new Error(`${typeName}.${variant} is no longer a record variant`);
  }
  let depth = 0;
  let end = -1;
  for (let i = opening.index + opening[0].length - 1; i < body.length; i += 1) {
    const character = body[i];
    if (character === "{") {
      depth += 1;
    } else if (character === "}") {
      depth -= 1;
      if (depth === 0) {
        end = i;
        break;
      }
    }
  }
  if (end === -1) {
    throw new Error(`${typeName}.${variant} has no closing brace`);
  }
  const region = body.slice(opening.index + opening[0].length, end);
  const fields = [...region.matchAll(/([a-z_][a-z0-9_]*): /gu)].map(
    (match) => match[1] as string,
  );
  if (fields.length === 0) {
    throw new Error(`${typeName}.${variant} parsed as having no fields`);
  }
  return fields;
};

/** Plutus constructor tag for index `i` (i < 7): 121 + i, CBOR tag `d879 + i`. */
const constructorTagPrefix = (index: number): string =>
  `d8${(0x79 + index).toString(16)}`;

const h32 = (byte: string): string => byte.repeat(32);

const TX_ID = h32("11");
const WITNESS_SET_HASH = h32("22");
const COMPACT_CBOR = "a1b2c3";
const WITNESS_SET = {
  addr_tx_wits_hash: h32("77"),
  script_tx_wits_hash: h32("66"),
  redeemer_tx_wits_hash: h32("88"),
};

describe("NativeTxAnchorV1 wire contract", () => {
  it("keeps the frozen constructor order the Aiken module declares", () => {
    expect(aikenConstructorOrder("NativeTxAnchorV1")).toEqual([
      "BodyAnchor",
      "WitnessAnchor",
    ]);
  });

  it("keeps each variant's frozen field order", () => {
    expect(aikenVariantFieldOrder("NativeTxAnchorV1", "BodyAnchor")).toEqual([
      "tx_id",
    ]);
    expect(aikenVariantFieldOrder("NativeTxAnchorV1", "WitnessAnchor")).toEqual(
      ["tx_id", "witness_set_hash"],
    );
  });

  it("encodes each arm at its frozen tag with the fields flat", () => {
    // Exact CBOR, not a prefix check. The Aiken variants carry *named* fields,
    // so the payload sits directly in the constructor: `BodyAnchor { tx_id }` is
    // `Constr 0 [B]`, never `Constr 0 [Constr 0 [B]]`. A `Data.Tuple` payload
    // would produce the second shape and this assertion is what catches it.
    const body = Data.to({ BodyAnchor: { tx_id: TX_ID } }, NativeTxAnchor);
    const witness = Data.to(
      { WitnessAnchor: { tx_id: TX_ID, witness_set_hash: WITNESS_SET_HASH } },
      NativeTxAnchor,
    );

    expect(body).toBe(`d8799f5820${TX_ID}ff`);
    expect(witness).toBe(`d87a9f5820${TX_ID}5820${WITNESS_SET_HASH}ff`);
    expect(body.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(witness.startsWith(constructorTagPrefix(1))).toBe(true);

    expect(Data.from(body, NativeTxAnchor)).toEqual({
      BodyAnchor: { tx_id: TX_ID },
    });
    expect(Data.from(witness, NativeTxAnchor)).toEqual({
      WitnessAnchor: { tx_id: TX_ID, witness_set_hash: WITNESS_SET_HASH },
    });
  });
});

describe("FieldOpeningV1 wire contract", () => {
  it("keeps the frozen constructor order the Aiken module declares", () => {
    expect(aikenConstructorOrder("FieldOpeningV1")).toEqual([
      "BodyFieldOpening",
      "WitnessFieldOpening",
    ]);
  });

  it("keeps each variant's frozen field order", () => {
    expect(
      aikenVariantFieldOrder("FieldOpeningV1", "BodyFieldOpening"),
    ).toEqual(["native_tx_compact_cbor", "carriage"]);
    expect(
      aikenVariantFieldOrder("FieldOpeningV1", "WitnessFieldOpening"),
    ).toEqual(["native_tx_compact_cbor", "witness_set", "carriage"]);
  });

  it("encodes a body opening at tag 0 with the carriage nested flat", () => {
    const opening = Data.to(
      {
        BodyFieldOpening: {
          native_tx_compact_cbor: COMPACT_CBOR,
          carriage: { Inline: { preimage: "80" } },
        },
      },
      FieldOpening,
    );
    expect(opening).toBe(`d8799f43${COMPACT_CBOR}d8799f4180ffff`);
    expect(opening.startsWith(constructorTagPrefix(0))).toBe(true);
    expect(Data.from(opening, FieldOpening)).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
      },
    });
  });

  it("encodes a witness opening at tag 1 with the witness set between", () => {
    const opening = Data.to(
      {
        WitnessFieldOpening: {
          native_tx_compact_cbor: COMPACT_CBOR,
          witness_set: WITNESS_SET,
          carriage: { RawUtxo: { ref_input_index: 2n } },
        },
      },
      FieldOpening,
    );
    expect(opening).toBe(
      `d87a9f43${COMPACT_CBOR}d8799f5820${WITNESS_SET.addr_tx_wits_hash}5820${WITNESS_SET.script_tx_wits_hash}5820${WITNESS_SET.redeemer_tx_wits_hash}ffd87a9f02ffff`,
    );
    expect(opening.startsWith(constructorTagPrefix(1))).toBe(true);
    expect(Data.from(opening, FieldOpening)).toEqual({
      WitnessFieldOpening: {
        native_tx_compact_cbor: COMPACT_CBOR,
        witness_set: WITNESS_SET,
        carriage: { RawUtxo: { ref_input_index: 2n } },
      },
    });
  });
});

describe("§2.5 field table", () => {
  it("mirrors the Aiken index constants exactly", () => {
    const source = readRepositoryFile(AIKEN_MODULE);
    const aikenIndex = (name: string): number => {
      const match = new RegExp(
        `^pub const ${name}_field_index: Int = (\\d)$`,
        "mu",
      ).exec(source);
      if (match?.[1] === undefined) {
        throw new Error(`${name}_field_index is no longer declared`);
      }
      return Number(match[1]);
    };
    expect(MIDGARD_FIELD_INDEX).toEqual({
      spendInputs: aikenIndex("spend_inputs"),
      referenceInputs: aikenIndex("reference_inputs"),
      outputs: aikenIndex("outputs"),
      requiredObservers: aikenIndex("required_observers"),
      requiredSigners: aikenIndex("required_signers"),
      mint: aikenIndex("mint"),
      scriptWitnesses: aikenIndex("script_witnesses"),
      addressWitnesses: aikenIndex("address_witnesses"),
      redeemers: aikenIndex("redeemers"),
    });
    // The boundary is named off the table on both sides rather than restated, so
    // this pins that the Aiken module still derives it the same way.
    expect(MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX).toBe(6);
    expect(
      /^pub const first_witness_set_field_index: Int = script_witnesses_field_index$/mu.test(
        source,
      ),
    ).toBe(true);
  });

  it("splits the nine fields at 6", () => {
    expect([0, 1, 2, 3, 4, 5].map(isMidgardWitnessSetField)).toEqual([
      false,
      false,
      false,
      false,
      false,
      false,
    ]);
    expect([6, 7, 8].map(isMidgardWitnessSetField)).toEqual([true, true, true]);
  });
});

describe("the §2.5 pairing is derived, not chosen", () => {
  it("anchors a body field on BodyAnchor and a witness field on WitnessAnchor", () => {
    expect(
      nativeTxAnchorForField({
        fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
        txId: TX_ID,
      }),
    ).toEqual({ BodyAnchor: { tx_id: TX_ID } });
    expect(
      nativeTxAnchorForField({
        fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
        txId: TX_ID,
        witnessSetHash: WITNESS_SET_HASH,
      }),
    ).toEqual({
      WitnessAnchor: { tx_id: TX_ID, witness_set_hash: WITNESS_SET_HASH },
    });
  });

  it("refuses a witness anchor with no witness_set_hash to anchor", () => {
    expect(() =>
      nativeTxAnchorForField({
        fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
        txId: TX_ID,
      }),
    ).toThrow(MidgardFieldOpeningError);
  });

  it("refuses a body anchor handed a witness_set_hash it cannot carry", () => {
    expect(() =>
      nativeTxAnchorForField({
        fieldIndex: MIDGARD_FIELD_INDEX.outputs,
        txId: TX_ID,
        witnessSetHash: WITNESS_SET_HASH,
      }),
    ).toThrow(MidgardFieldOpeningError);
  });

  it("opens a body field with no witness set and a witness field with one", () => {
    expect(
      fieldOpeningForField({
        fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
        nativeTxCompactCbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
      }),
    ).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
      },
    });
    expect(
      fieldOpeningForField({
        fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
        nativeTxCompactCbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
        witnessSet: WITNESS_SET,
      }),
    ).toEqual({
      WitnessFieldOpening: {
        native_tx_compact_cbor: COMPACT_CBOR,
        witness_set: WITNESS_SET,
        carriage: { Inline: { preimage: "80" } },
      },
    });
  });

  it("refuses a witness opening with no witness set, and a body one carrying one", () => {
    expect(() =>
      fieldOpeningForField({
        fieldIndex: MIDGARD_FIELD_INDEX.redeemers,
        nativeTxCompactCbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
      }),
    ).toThrow(MidgardFieldOpeningError);
    expect(() =>
      fieldOpeningForField({
        fieldIndex: MIDGARD_FIELD_INDEX.mint,
        nativeTxCompactCbor: COMPACT_CBOR,
        carriage: { Inline: { preimage: "80" } },
        witnessSet: WITNESS_SET,
      }),
    ).toThrow(MidgardFieldOpeningError);
  });

  it("refuses a field index §2.5 does not name", () => {
    for (const fieldIndex of [-1, 9, 1.5]) {
      expect(() => nativeTxAnchorForField({ fieldIndex, txId: TX_ID })).toThrow(
        MidgardFieldOpeningError,
      );
    }
  });
});

describe("#606 — tier 3 is admissible at every field (E2 limit 3 resolved)", () => {
  const certified = {
    Certified: { cert_ref_input_index: 0n, chunk_ref_input_indices: [1n, 2n] },
  };

  /**
   * The former builder-side refusal (the #604 hardening that duplicated
   * `carriage_reaches_the_anchor`) lifted with #606's welded-hash repair, and
   * door parity is the property that has to survive: this module emits
   * exactly what the on-chain door accepts, which now includes tier-3
   * carriage of a witness-set field.
   */
  it("emits a certified carriage on every witness-set field", () => {
    for (const fieldIndex of [6, 7, 8]) {
      expect(
        fieldOpeningForField({
          fieldIndex,
          nativeTxCompactCbor: COMPACT_CBOR,
          carriage: certified,
          witnessSet: WITNESS_SET,
        }),
      ).toEqual({
        WitnessFieldOpening: {
          native_tx_compact_cbor: COMPACT_CBOR,
          witness_set: WITNESS_SET,
          carriage: certified,
        },
      });
    }
  });

  /** The body direction, pinned from the other side exactly as before. */
  it("still admits a certified carriage on every body field", () => {
    for (const fieldIndex of [0, 1, 2, 3, 4, 5]) {
      expect(
        fieldOpeningForField({
          fieldIndex,
          nativeTxCompactCbor: COMPACT_CBOR,
          carriage: certified,
        }),
      ).toEqual({
        BodyFieldOpening: {
          native_tx_compact_cbor: COMPACT_CBOR,
          carriage: certified,
        },
      });
    }
  });

  /**
   * Door parity's other half, asserted rather than only commented: what
   * actually stands between an adversary and a forged witness-set fault is
   * the on-chain door's welded-hash equality (#606), and lifting the
   * builder-side refusal is only sound while that equality is there. If this
   * ever stops matching, the door has lost the E2 closure — which is the
   * state E2's disposition says must not be reached silently.
   */
  it("mirrors the on-chain welded-hash equality that closed E2", () => {
    const accessModule = readRepositoryFile(
      "onchain/aiken/lib/midgard/native-tx-field-access-v1.ak",
    );
    expect(accessModule).toMatch(
      /expect certificate\.field_hash == expected_hash/u,
    );
    expect(accessModule).toContain("#606");
    // And the old refusal is gone rather than duplicated somewhere new.
    const opening = readRepositoryFile(AIKEN_MODULE);
    expect(opening).not.toMatch(
      /Certified \{ \.\. \} -> field_index < first_witness_set_field_index/u,
    );
  });
});
