import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cardanoTxBytesToMidgardNativeTxFull,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardFieldPreimageForField,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
  midgardNativeTxFullToCardanoTxEncoding,
  type MidgardRedeemerPurpose,
} from "../src/index.js";

/**
 * The field-8 preimage the bridge normalises two Cardano spellings onto, under
 * §5.1's envelope: `82` array(2), then `4a` bytes(10) wrapping
 * `84 00 00 43 d87980 82 05 07` and `49` bytes(9) wrapping
 * `84 03 01 42 182a 82 0b 0d`.
 *
 * The retired counted scheme concatenated the two `enc_8` arrays with no per-item
 * envelope (`8284000043d879808205078403…`); §5.1 prohibits that form for all nine
 * fields, so this value is the same two items two bytes wider.
 */
const NORMALIZED_REDEEMERS_CBOR_HEX =
  "824a84000043d879808205074984030142182a820b0d";

/**
 * A field-8 preimage from `enc_8` parts, under §5.1's envelope. The retired
 * counted scheme spelled these as a bare CBOR array of four-element arrays;
 * §5.1 wraps each item in a definite byte string.
 */
const redeemerFieldPreimage = (
  items: readonly {
    readonly purpose: MidgardRedeemerPurpose;
    readonly index: bigint;
    readonly redeemerCborHex: string;
    readonly memory: bigint;
    readonly steps: bigint;
  }[],
): Buffer =>
  encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: items.map((item) => ({
      purpose: item.purpose,
      index: item.index,
      redeemerCbor: Buffer.from(item.redeemerCborHex, "hex"),
      executionUnits: { memory: item.memory, steps: item.steps },
    })),
  });

const NESTED_REDEEMER_DATA_CBOR_HEX = [
  "a2019fd87980ff",
  "5f5840",
  "71".repeat(64),
  "4171ff",
  "d8668218809f00ff",
].join("");

const makeCanonical = (
  redeemerTxWitsPreimageCbor: Buffer,
): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: EMPTY_CBOR_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor,
  },
});

const unitRedeemer = (): CML.LegacyRedeemer =>
  CML.LegacyRedeemer.new(
    CML.RedeemerTag.Spend,
    0n,
    CML.PlutusData.from_cbor_hex("d87980"),
    CML.ExUnits.new(5n, 7n),
  );

const integerRedeemer = (): CML.LegacyRedeemer =>
  CML.LegacyRedeemer.new(
    CML.RedeemerTag.Reward,
    1n,
    CML.PlutusData.from_cbor_hex("182a"),
    CML.ExUnits.new(11n, 13n),
  );

const nestedRedeemer = (dataCborHex: string): CML.LegacyRedeemer =>
  CML.LegacyRedeemer.new(
    CML.RedeemerTag.Spend,
    2n,
    CML.PlutusData.from_cbor_hex(dataCborHex),
    CML.ExUnits.new(17n, 19n),
  );

const cardanoTxWithRedeemers = (redeemers: CML.Redeemers): Buffer => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    0n,
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_redeemers(redeemers);
  return Buffer.from(
    CML.Transaction.new(body, witnessSet, true, undefined).to_cbor_bytes(),
  );
};

const cardanoLegacyRedeemers = (
  redeemers: readonly CML.LegacyRedeemer[],
): CML.Redeemers => {
  const list = CML.LegacyRedeemerList.new();
  for (const redeemer of redeemers) {
    list.add(redeemer);
  }
  return CML.Redeemers.new_arr_legacy_redeemer(list);
};

const cardanoMapRedeemers = (
  redeemers: readonly CML.LegacyRedeemer[],
): CML.Redeemers => {
  const map = CML.MapRedeemerKeyToRedeemerVal.new();
  for (const redeemer of redeemers) {
    map.insert(
      CML.RedeemerKey.new(redeemer.tag(), redeemer.index()),
      CML.RedeemerVal.new(redeemer.data(), redeemer.ex_units()),
    );
  }
  return CML.Redeemers.new_map_redeemer_key_to_redeemer_val(map);
};

describe("canonical V1 Cardano redeemer bridge", () => {
  it("normalizes Cardano map and legacy redeemers to the same Midgard golden bytes", () => {
    const reverseOrder = [integerRedeemer(), unitRedeemer()];

    for (const cardanoRedeemers of [
      cardanoLegacyRedeemers(reverseOrder),
      cardanoMapRedeemers(reverseOrder),
    ]) {
      const native = cardanoTxBytesToMidgardNativeTxFull(
        cardanoTxWithRedeemers(cardanoRedeemers),
      );
      expect(native.witnessSet.redeemerTxWitsPreimageCbor.toString("hex")).toBe(
        NORMALIZED_REDEEMERS_CBOR_HEX,
      );
    }
  });

  it("reconstructs a canonical Cardano map with exact Data and execution units", () => {
    const preimageCbor = Buffer.from(NORMALIZED_REDEEMERS_CBOR_HEX, "hex");
    const native = materializeMidgardNativeTxFromCanonical(
      makeCanonical(preimageCbor),
    );
    const cardanoCbor = midgardNativeTxFullToCardanoTxEncoding(native);
    const cardanoRedeemers = CML.Transaction.from_cbor_bytes(cardanoCbor)
      .witness_set()
      .redeemers();
    expect(cardanoRedeemers).toBeDefined();
    expect(
      cardanoRedeemers?.as_map_redeemer_key_to_redeemer_val(),
    ).toBeDefined();

    const flat = cardanoRedeemers!.to_flat_format();
    expect(flat.len()).toBe(2);
    expect(
      Array.from({ length: flat.len() }, (_, index) => {
        const redeemer = flat.get(index);
        return {
          tag: redeemer.tag(),
          index: redeemer.index(),
          dataCborHex: redeemer.data().to_canonical_cbor_hex(),
          memory: redeemer.ex_units().mem(),
          steps: redeemer.ex_units().steps(),
        };
      }),
    ).toEqual([
      {
        tag: CML.RedeemerTag.Spend,
        index: 0n,
        dataCborHex: "d87980",
        memory: 5n,
        steps: 7n,
      },
      {
        tag: CML.RedeemerTag.Reward,
        index: 1n,
        dataCborHex: "182a",
        memory: 11n,
        steps: 13n,
      },
    ]);

    const roundTrip = cardanoTxBytesToMidgardNativeTxFull(cardanoCbor);
    expect(roundTrip.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      preimageCbor,
    );
  });

  it("normalizes nested Cardano Data to exact Aiken serialiseData bytes in both directions", () => {
    const aikenData = CML.PlutusData.from_cbor_hex(
      NESTED_REDEEMER_DATA_CBOR_HEX,
    );
    const cmlCanonicalDataCborHex = aikenData.to_canonical_cbor_hex();
    expect(cmlCanonicalDataCborHex).not.toBe(NESTED_REDEEMER_DATA_CBOR_HEX);

    const fromAiken = cardanoTxBytesToMidgardNativeTxFull(
      cardanoTxWithRedeemers(
        cardanoMapRedeemers([nestedRedeemer(NESTED_REDEEMER_DATA_CBOR_HEX)]),
      ),
    );
    const fromCmlCanonical = cardanoTxBytesToMidgardNativeTxFull(
      cardanoTxWithRedeemers(
        cardanoMapRedeemers([nestedRedeemer(cmlCanonicalDataCborHex)]),
      ),
    );
    const expectedPreimage = redeemerFieldPreimage([
      {
        purpose: "Spend",
        index: 2n,
        redeemerCborHex: NESTED_REDEEMER_DATA_CBOR_HEX,
        memory: 17n,
        steps: 19n,
      },
    ]);
    expect(fromAiken.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedPreimage,
    );
    expect(fromCmlCanonical.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedPreimage,
    );

    const reconstructed = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonical(
          makeCanonical(expectedPreimage),
        ),
      ),
    )
      .witness_set()
      .redeemers()!
      .to_flat_format()
      .get(0);
    expect({
      tag: reconstructed.tag(),
      index: reconstructed.index(),
      dataCborHex: reconstructed.data().to_cbor_hex(),
      memory: reconstructed.ex_units().mem(),
      steps: reconstructed.ex_units().steps(),
    }).toEqual({
      tag: CML.RedeemerTag.Spend,
      index: 2n,
      dataCborHex: NESTED_REDEEMER_DATA_CBOR_HEX,
      memory: 17n,
      steps: 19n,
    });

    const roundTrip = cardanoTxBytesToMidgardNativeTxFull(
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonical(
          makeCanonical(expectedPreimage),
        ),
      ),
    );
    expect(roundTrip.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedPreimage,
    );
  });

  it("rejects duplicate pointers and purposes that cannot cross the bridge", () => {
    const duplicateCardano = cardanoLegacyRedeemers([
      unitRedeemer(),
      unitRedeemer(),
    ]);
    expect(() =>
      cardanoTxBytesToMidgardNativeTxFull(
        cardanoTxWithRedeemers(duplicateCardano),
      ),
    ).toThrow(/duplicate redeemer pointer/u);

    const unsupportedCardano = cardanoLegacyRedeemers([
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Cert,
        0n,
        CML.PlutusData.from_cbor_hex("d87980"),
        CML.ExUnits.new(5n, 7n),
      ),
    ]);
    expect(() =>
      cardanoTxBytesToMidgardNativeTxFull(
        cardanoTxWithRedeemers(unsupportedCardano),
      ),
    ).toThrow(/cannot be converted without loss/u);

    const receivingPreimage = redeemerFieldPreimage([
      {
        purpose: "Receive",
        index: 0n,
        redeemerCborHex: "d87980",
        memory: 5n,
        steps: 7n,
      },
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonical(
          makeCanonical(receivingPreimage),
        ),
      ),
    ).toThrow(/cannot be converted without loss/u);

    const duplicateMidgardPreimage = redeemerFieldPreimage([
      {
        purpose: "Spend",
        index: 0n,
        redeemerCborHex: "d87980",
        memory: 5n,
        steps: 7n,
      },
      {
        purpose: "Spend",
        index: 0n,
        redeemerCborHex: "d87980",
        memory: 5n,
        steps: 7n,
      },
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonical(
          makeCanonical(duplicateMidgardPreimage),
        ),
      ),
    ).toThrow(/duplicate redeemer pointer/u);

    const nonCanonicalDataPreimage = redeemerFieldPreimage([
      {
        purpose: "Spend",
        index: 0n,
        redeemerCborHex: "1800",
        memory: 5n,
        steps: 7n,
      },
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonical(
          makeCanonical(nonCanonicalDataPreimage),
        ),
      ),
    ).toThrow(/canonical Plutus Data CBOR/u);
  });
});
