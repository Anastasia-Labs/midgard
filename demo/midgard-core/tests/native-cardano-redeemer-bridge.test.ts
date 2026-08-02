import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cardanoTxBytesToMidgardNativeTxFullV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
  midgardNativeTxFullToCardanoTxEncoding,
} from "../src/index.js";

const NORMALIZED_REDEEMERS_CBOR_HEX =
  "8284000043d8798082050784030142182a820b0d";

const NESTED_REDEEMER_DATA_CBOR_HEX = [
  "a2019fd87980ff",
  "5f5840",
  "71".repeat(64),
  "4171ff",
  "d8668218809f00ff",
].join("");

const makeCanonical = (
  redeemerTxWitsPreimageCbor: Buffer,
): MidgardNativeTxCanonicalV1 => ({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
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
      const native = cardanoTxBytesToMidgardNativeTxFullV1(
        cardanoTxWithRedeemers(cardanoRedeemers),
      );
      expect(native.witnessSet.redeemerTxWitsPreimageCbor.toString("hex")).toBe(
        NORMALIZED_REDEEMERS_CBOR_HEX,
      );
    }
  });

  it("reconstructs a canonical Cardano map with exact Data and execution units", () => {
    const preimageCbor = Buffer.from(NORMALIZED_REDEEMERS_CBOR_HEX, "hex");
    const native = materializeMidgardNativeTxFromCanonicalV1(
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

    const roundTrip = cardanoTxBytesToMidgardNativeTxFullV1(cardanoCbor);
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

    const fromAiken = cardanoTxBytesToMidgardNativeTxFullV1(
      cardanoTxWithRedeemers(
        cardanoMapRedeemers([nestedRedeemer(NESTED_REDEEMER_DATA_CBOR_HEX)]),
      ),
    );
    const fromCmlCanonical = cardanoTxBytesToMidgardNativeTxFullV1(
      cardanoTxWithRedeemers(
        cardanoMapRedeemers([nestedRedeemer(cmlCanonicalDataCborHex)]),
      ),
    );
    const expectedPreimage = encodeCbor([
      [
        CML.RedeemerTag.Spend,
        2n,
        Buffer.from(NESTED_REDEEMER_DATA_CBOR_HEX, "hex"),
        [17n, 19n],
      ],
    ]);
    expect(fromAiken.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedPreimage,
    );
    expect(fromCmlCanonical.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedPreimage,
    );

    const reconstructed = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonicalV1(
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

    const roundTrip = cardanoTxBytesToMidgardNativeTxFullV1(
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonicalV1(
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
      cardanoTxBytesToMidgardNativeTxFullV1(
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
      cardanoTxBytesToMidgardNativeTxFullV1(
        cardanoTxWithRedeemers(unsupportedCardano),
      ),
    ).toThrow(/cannot be converted without loss/u);

    const receivingPreimage = encodeCbor([
      [6, 0, Buffer.from("d87980", "hex"), [5, 7]],
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonicalV1(
          makeCanonical(receivingPreimage),
        ),
      ),
    ).toThrow(/cannot be converted without loss/u);

    const duplicateMidgardPreimage = encodeCbor([
      [0, 0, Buffer.from("d87980", "hex"), [5, 7]],
      [0, 0, Buffer.from("d87980", "hex"), [5, 7]],
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonicalV1(
          makeCanonical(duplicateMidgardPreimage),
        ),
      ),
    ).toThrow(/duplicate redeemer pointer/u);

    const nonCanonicalDataPreimage = encodeCbor([
      [0, 0, Buffer.from("1800", "hex"), [5, 7]],
    ]);
    expect(() =>
      midgardNativeTxFullToCardanoTxEncoding(
        materializeMidgardNativeTxFromCanonicalV1(
          makeCanonical(nonCanonicalDataPreimage),
        ),
      ),
    ).toThrow(/canonical Plutus Data CBOR/u);
  });
});
