import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeHash32,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardFieldPreimageForField,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCommitment,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  midgardNativeTxFullToCardanoTxEncoding,
  type MidgardNativeTxWitnessSetCanonical,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";
import {
  deriveMidgardTxFieldPreimages,
  verifyMidgardTxFieldPreimage,
} from "@al-ft/midgard-core/consensus-validation-v1";
import { CML } from "@lucid-evolution/lucid";
import { encode } from "cborg";
import { describe, expect, it } from "vitest";

import { makeConvertibleCardanoTxBytes } from "./helpers/cardano-native-fixtures.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

const mkHash = (tag: string): Buffer => computeHash32(Buffer.from(tag, "utf8"));

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const makePlutusIntegerData = (value: bigint): CML.PlutusData =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

const mkBody = (): MidgardNativeTxBodyCanonical => {
  const spendInputsPreimageCbor = encodeByteList([
    Buffer.from([1]),
    Buffer.from([2]),
  ]);
  const referenceInputsPreimageCbor = encodeByteList([Buffer.from([3])]);
  const outputsPreimageCbor = encodeByteList([
    Buffer.from([4]),
    Buffer.from([5]),
    Buffer.from([6]),
  ]);
  const requiredObserversPreimageCbor = Buffer.from("80", "hex");
  const requiredSignersPreimageCbor = encodeByteList([
    Buffer.from([7]),
    Buffer.from([8]),
  ]);
  const mintPreimageCbor = Buffer.from("80", "hex");

  return {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 42n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: 1_735_000_000_000n,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: mkHash("script-integrity"),
    auxiliaryDataHash: mkHash("aux-data"),
    networkId: 1n,
  };
};

const mkWitnessSet = (): MidgardNativeTxWitnessSetCanonical => {
  const addrTxWitsPreimageCbor = Buffer.from("81420102", "hex");
  // §5.1: fields 6 and 8 wrap each item in a definite byte string like the other
  // seven. The retired counted scheme concatenated raw item CBOR here — an
  // array-of-arrays for the scripts, a bare uint list for the redeemers — and
  // both forms are now refused by the §5.1 gate, so the two fields are built
  // with the production encoders.
  const scriptTxWitsPreimageCbor = encodeMidgardVersionedScriptListPreimage([
    { language: "PlutusV3", scriptBytes: Buffer.from("deadbeef", "hex") },
  ]);
  const redeemerTxWitsPreimageCbor = encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: [
      {
        purpose: "Spend",
        index: 3n,
        redeemerCbor: Buffer.from("00", "hex"),
        executionUnits: { memory: 0n, steps: 0n },
      },
    ],
  });

  return {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
};

const mkFull = (): MidgardNativeTxFull => {
  const body = mkBody();
  const witnessSet = mkWitnessSet();
  const compact = deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid");
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    compact,
    body,
    witnessSet,
  };
};

describe("midgard native tx codec - strict roundtrip", () => {
  it("roundtrips compact tx/body/witness and full tx", () => {
    const full = mkFull();

    const bodyCompact = deriveMidgardNativeTxBodyCompact(full.body);
    const witnessCompact = deriveMidgardNativeTxWitnessSetCompact(
      full.witnessSet,
    );

    expect(
      decodeMidgardNativeTxBodyCompact(
        encodeMidgardNativeTxBodyCompact(bodyCompact),
      ),
    ).toEqual(bodyCompact);
    expect(
      decodeMidgardNativeTxWitnessSetCompact(
        encodeMidgardNativeTxWitnessSetCompact(witnessCompact),
      ),
    ).toEqual(witnessCompact);
    expect(
      decodeMidgardNativeTxCompact(encodeMidgardNativeTxCompact(full.compact)),
    ).toEqual(full.compact);

    const encodedCanonical = encodeMidgardNativeTxCanonical(full);
    const decodedFull =
      decodeMidgardNativeTxFullFromCanonicalCbor(encodedCanonical);
    expect(decodedFull).toEqual(full);
  });

  it("uses the canonical V1 compact-body domain for the transaction id", () => {
    const full = mkFull();

    expect(computeMidgardNativeTxId(full)).toEqual(
      computeHash32(
        Buffer.concat([
          Buffer.from("MidgardNativeTxBodyV1", "ascii"),
          Buffer.from([1]),
          encodeMidgardNativeTxBodyCompact(full.compact.transactionBody),
        ]),
      ),
    );
  });

  it("rejects witness-set compact encodings with an extra datum witness bucket", () => {
    const witnessCompact =
      deriveMidgardNativeTxWitnessSetCompact(mkWitnessSet());
    const unsupportedShape = Buffer.from(
      encode([
        Buffer.from(witnessCompact.addrTxWitsHash),
        Buffer.from(witnessCompact.scriptTxWitsHash),
        Buffer.from(witnessCompact.redeemerTxWitsHash),
        Buffer.from(mkHash("extra-datum-wits")),
      ]),
    );

    expect(() =>
      decodeMidgardNativeTxWitnessSetCompact(unsupportedShape),
    ).toThrow(/exactly 3 elements/i);
  });

  it("keeps the witness tuple ABI while projecting scripts to field 6 and vkeys to field 7", () => {
    const full = mkFull();
    const canonicalCbor = encodeMidgardNativeTxCanonical(full);
    const fields = deriveMidgardTxFieldPreimages(canonicalCbor);
    const source =
      deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalCbor);
    const transactionCommitment = computeMidgardNativeTxProofCommitment(source);

    expect(fields[6]).toMatchObject({
      fieldIndex: 6,
      fieldName: "script_witnesses",
      preimageCbor: full.witnessSet.scriptTxWitsPreimageCbor,
    });
    expect(fields[7]).toMatchObject({
      fieldIndex: 7,
      fieldName: "address_witnesses",
      preimageCbor: full.witnessSet.addrTxWitsPreimageCbor,
    });

    const compactWitnessSet = deriveMidgardNativeTxWitnessSetCompact(
      full.witnessSet,
    );
    const encodedTuple = Buffer.from(
      encode([
        Buffer.from(compactWitnessSet.addrTxWitsHash),
        Buffer.from(compactWitnessSet.scriptTxWitsHash),
        Buffer.from(compactWitnessSet.redeemerTxWitsHash),
      ]),
    );
    expect(encodeMidgardNativeTxWitnessSetCompact(compactWitnessSet)).toEqual(
      encodedTuple,
    );

    for (const [fieldIndex, substitutedPreimage] of [
      [6, fields[7]!.preimageCbor],
      [7, fields[6]!.preimageCbor],
    ] as const) {
      expect(() =>
        verifyMidgardTxFieldPreimage({
          transactionId: computeMidgardNativeTxId(full),
          transactionCommitment,
          source,
          fieldIndex,
          preimageCbor: substitutedPreimage,
        }),
      ).toThrow(
        /(preimage (length does not match|hash mismatch)|must be a CBOR byte string)/u,
      );
    }
  });
});

describe("midgard native tx codec - consistency checks", () => {
  it("rejects inconsistent compact hash commitments", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      compact: {
        ...full.compact,
        transactionBody: {
          ...full.compact.transactionBody,
          outputsHash: Buffer.from(full.compact.transactionBody.outputsHash),
        },
      },
    };

    tampered.compact.transactionBody.outputsHash[0] ^= 0xff;

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow();
  });

  it("rejects inconsistent body hash/preimage pairs", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      body: {
        ...full.body,
        outputsPreimageCbor: Buffer.from(full.body.outputsPreimageCbor),
      },
    };

    tampered.body.outputsPreimageCbor[0] ^= 0xff;

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow();
  });

  it("accepts when explicit consistency verification passes", () => {
    const full = mkFull();
    expect(() => verifyMidgardNativeTxFullConsistency(full)).not.toThrow();
  });

  it("rejects mismatched outer and compact versions", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      compact: {
        ...full.compact,
        version: 23n,
      },
    };

    expect(() => encodeMidgardNativeTxCanonical(tampered)).toThrow(
      /transaction_full.version must match transaction_compact.version/i,
    );
  });
});

describe("midgard native tx codec - cardano compatibility bridge", () => {
  const sampleTxBytes = [makeConvertibleCardanoTxBytes()];

  it("converts Cardano tx fixtures into Midgard native full tx bytes", () => {
    for (const cardanoTx of sampleTxBytes) {
      const nativeCanonicalCbor =
        cardanoTxBytesToMidgardNativeTxCanonicalCbor(cardanoTx);
      const decoded =
        decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);
      const cardanoDecoded = CML.Transaction.from_cbor_bytes(cardanoTx);
      const networkId = cardanoDecoded.body().network_id();
      const expectedNetworkId =
        networkId !== undefined
          ? BigInt(networkId.network())
          : MIDGARD_NATIVE_NETWORK_ID_NONE;

      expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
      expect(computeMidgardNativeTxId(decoded).length).toBe(32);
      expect(decoded.compact.transactionWitnessSetHash.length).toBe(32);
      expect(decoded.body.networkId).toBe(expectedNetworkId);
    }
  });

  it("normalizes an empty Cardano mint map to the native empty-list mint preimage", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    body.set_mint(CML.Mint.new());
    const mutated = CML.Transaction.new(
      body,
      parsed.witness_set(),
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded =
      decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);
    const emptyList = Buffer.from("80", "hex");

    expect(decoded.body.mintPreimageCbor).toEqual(emptyList);
    expect(
      decoded.compact.transactionBody.mintHash.equals(
        midgardFieldCommitment(emptyList),
      ),
    ).toBe(true);
  });

  it("preserves non-empty Cardano mint fields", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(CML.AssetName.from_raw_bytes(Buffer.from([])), 1n);
    const mint = CML.Mint.new();
    const policyId = Buffer.from("11".repeat(28), "hex");
    mint.insert_assets(CML.ScriptHash.from_raw_bytes(policyId), mintAssets);
    body.set_mint(mint);
    const mutated = CML.Transaction.new(
      body,
      parsed.witness_set(),
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded =
      decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);
    // §5.6: the mint field is the §5.1 envelope over one enveloped item per
    // policy, not the retired raw `a1 …` map.
    const expectedMintPreimage = encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: [
        {
          policyId,
          assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
        },
      ],
    });

    expect(decoded.body.mintPreimageCbor).toEqual(expectedMintPreimage);
    expect(
      decoded.compact.transactionBody.mintHash.equals(
        midgardFieldCommitment(expectedMintPreimage),
      ),
    ).toBe(true);
  });

  it("maps zero-ADA script withdrawals into required observers", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const scriptHash = CML.ScriptHash.from_hex("44".repeat(28));
    const withdrawals = CML.MapRewardAccountToCoin.new();
    withdrawals.insert(
      CML.RewardAddress.new(0, CML.Credential.new_script(scriptHash)),
      0n,
    );
    body.set_withdrawals(withdrawals);
    const mutated = CML.Transaction.new(
      body,
      parsed.witness_set(),
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded =
      decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);

    expect(decoded.body.requiredObserversPreimageCbor).toEqual(
      encodeByteList([Buffer.from(scriptHash.to_raw_bytes())]),
    );
  });

  it("preserves script data hash, auxiliary data hash, redeemers, and Plutus scripts", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const witnessSet = CML.TransactionWitnessSet.from_cbor_bytes(
      parsed.witness_set().to_cbor_bytes(),
    );
    const scriptDataHash = Buffer.from("55".repeat(32), "hex");
    const auxiliaryDataHash = Buffer.from("66".repeat(32), "hex");
    const redeemerBytes = Buffer.from(
      encode([[0, 0, Buffer.alloc(0), [0, 0]]]),
    );
    const redeemers = CML.Redeemers.from_cbor_bytes(redeemerBytes);
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(
      Buffer.from("deadbeef", "hex"),
    );
    const plutusScripts = CML.PlutusV3ScriptList.new();
    plutusScripts.add(plutusScript);

    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(scriptDataHash),
    );
    body.set_auxiliary_data_hash(
      CML.AuxiliaryDataHash.from_raw_bytes(auxiliaryDataHash),
    );
    witnessSet.set_redeemers(redeemers);
    witnessSet.set_plutus_v3_scripts(plutusScripts);
    const mutated = CML.Transaction.new(
      body,
      witnessSet,
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    const nativeCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded =
      decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonicalCbor);
    // §5.3 field 8 inside §5.1's envelope: the Cardano redeemer's empty-bytes
    // Plutus Data payload survives as the `40` it serialises to.
    const expectedNativeRedeemers = encodeMidgardFieldPreimageForField({
      fieldIndex: 8,
      items: [
        {
          purpose: "Spend",
          index: 0n,
          redeemerCbor: Buffer.from([0x40]),
          executionUnits: { memory: 0n, steps: 0n },
        },
      ],
    });

    expect(decoded.body.scriptIntegrityHash).toEqual(scriptDataHash);
    expect(decoded.body.auxiliaryDataHash).toEqual(auxiliaryDataHash);
    expect(decoded.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedNativeRedeemers,
    );
    expect(decoded.witnessSet.scriptTxWitsPreimageCbor).toEqual(
      encodeMidgardVersionedScriptListPreimage([
        {
          language: "PlutusV3",
          scriptBytes: Buffer.from(plutusScript.to_raw_bytes()),
        },
      ]),
    );
    const roundtrip = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(decoded),
    );
    const nativeRoundtrip = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(roundtrip.to_cbor_bytes()),
      ),
    );
    expect(nativeRoundtrip.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      expectedNativeRedeemers,
    );
  });

  it("rejects Cardano transactions with Plutus datum witnesses", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const witnessSet = CML.TransactionWitnessSet.from_cbor_bytes(
      parsed.witness_set().to_cbor_bytes(),
    );
    const datumWitnesses = CML.PlutusDataList.new();
    datumWitnesses.add(makePlutusIntegerData(42n));
    witnessSet.set_plutus_datums(datumWitnesses);
    const mutated = CML.Transaction.new(
      parsed.body(),
      witnessSet,
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    expect(() =>
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(mutated.to_cbor_bytes()),
      ),
    ).toThrow(/Plutus datum witnesses/i);
  });

  it("fails fast on Cardano fields that the native format cannot represent", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const witnessSet = CML.TransactionWitnessSet.from_cbor_bytes(
      parsed.witness_set().to_cbor_bytes(),
    );
    body.set_collateral_return(parsed.body().outputs().get(0));
    const mutated = CML.Transaction.new(
      body,
      witnessSet,
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    expect(() =>
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(mutated.to_cbor_bytes()),
      ),
    ).toThrow();
  });

  it("re-encodes supported Midgard-native transactions into Cardano tx CBOR", () => {
    const cardanoTx = sampleTxBytes[0];
    const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(cardanoTx),
    );

    const reEncoded = midgardNativeTxFullToCardanoTxEncoding(nativeTx);

    expect(Buffer.from(reEncoded)).toEqual(cardanoTx);
  });

  it("can omit vkey witnesses for eval-only Cardano export", () => {
    const full = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(sampleTxBytes[0]),
    );
    const signer = CML.PrivateKey.generate_ed25519();
    const vkeyWitness = Buffer.from(
      CML.make_vkey_witness(
        CML.TransactionHash.from_raw_bytes(computeMidgardNativeTxId(full)),
        signer,
      ).to_cbor_bytes(),
    );
    const addrTxWitsPreimageCbor = encodeByteList([vkeyWitness]);
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      ...full.witnessSet,
      addrTxWitsPreimageCbor,
    };
    const tx: MidgardNativeTxFull = {
      ...full,
      witnessSet,
      compact: deriveMidgardNativeTxCompact(full.body, witnessSet, "TxIsValid"),
    };

    const defaultExport = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(tx),
    );
    const evalOnlyExport = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(tx, { omitVkeyWitnesses: true }),
    );

    expect(defaultExport.witness_set().vkeywitnesses()?.len()).toBe(1);
    expect(evalOnlyExport.witness_set().vkeywitnesses()).toBeUndefined();
  });

  it("maps Midgard observers into zero-lovelace Cardano withdrawals", () => {
    const input = makeOutRefCbor("11".repeat(32), 0);
    const output = Buffer.from(
      makeMidgardTxOutput(
        CML.Address.from_bech32(
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
        ),
        CML.Value.from_coin(2_000_000n),
      ).to_cbor_bytes(),
    );
    const observerCredential = CML.Credential.new_script(
      CML.ScriptHash.from_hex("22".repeat(28)),
    );
    const observerBytes = Buffer.from(observerCredential.to_cbor_bytes());
    const emptyList = Buffer.from("80", "hex");
    const emptyNull = Buffer.from("f6", "hex");

    const body: MidgardNativeTxBodyCanonical = {
      spendInputsPreimageCbor: encodeByteList([input]),
      referenceInputsPreimageCbor: emptyList,
      outputsPreimageCbor: encodeByteList([output]),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: encodeByteList([observerBytes]),
      requiredSignersPreimageCbor: emptyList,
      mintPreimageCbor: emptyList,
      scriptIntegrityHash: computeHash32(emptyNull),
      auxiliaryDataHash: computeHash32(emptyNull),
      networkId: 0n,
    };
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      addrTxWitsPreimageCbor: emptyList,
      scriptTxWitsPreimageCbor: emptyList,
      redeemerTxWitsPreimageCbor: emptyList,
    };
    const tx: MidgardNativeTxFull = {
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
      body,
      witnessSet,
    };

    const cardanoTx = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(tx),
    );
    const withdrawals = cardanoTx.body().withdrawals();

    expect(withdrawals).toBeDefined();
    expect(withdrawals?.len()).toBe(1);
    const rewardAccount = withdrawals!.keys().get(0);
    expect(rewardAccount.network_id()).toBe(0);
    expect(rewardAccount.payment().kind()).toBe(CML.CredentialKind.Script);
    expect(rewardAccount.payment().as_script()?.to_hex()).toBe("22".repeat(28));
    expect(withdrawals!.get(rewardAccount)).toBe(0n);
  });

  it("maps the invalid Midgard validity state to a Cardano script-invalid tx", () => {
    const full = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(sampleTxBytes[0]),
    );
    const invalid: MidgardNativeTxFull = {
      ...full,
      validity: "TxIsInvalid",
      compact: deriveMidgardNativeTxCompact(
        full.body,
        full.witnessSet,
        "TxIsInvalid",
      ),
    };

    const cardanoTx = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(invalid),
    );

    expect(cardanoTx.is_valid()).toBe(false);
  });

  it("preserves mint, script integrity hash, auxiliary data hash, and redeemers", () => {
    const full = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(sampleTxBytes[0]),
    );
    const policyId = Buffer.from("33".repeat(28), "hex");
    const assetName = Buffer.from("aa", "hex");
    // §5.6 orders asset names length-first, then byte-lexicographically, so the
    // empty name precedes `aa`; the production encoder enforces that rather than
    // trusting the caller.
    const mintPreimageCbor = encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: [
        {
          policyId,
          assets: [
            { assetName: Buffer.alloc(0), quantity: -2n },
            { assetName, quantity: 5n },
          ],
        },
      ],
    });
    const redeemerTxWitsPreimageCbor = encodeMidgardFieldPreimageForField({
      fieldIndex: 8,
      items: [
        {
          purpose: "Spend",
          index: 0n,
          redeemerCbor: Buffer.from([0x00]),
          executionUnits: { memory: 0n, steps: 0n },
        },
      ],
    });
    const scriptIntegrityHash = mkHash("script-data-hash");
    const auxiliaryDataHash = mkHash("auxiliary-data-hash");

    const body: MidgardNativeTxBodyCanonical = {
      ...full.body,
      mintPreimageCbor,
      scriptIntegrityHash,
      auxiliaryDataHash,
    };
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      ...full.witnessSet,
      redeemerTxWitsPreimageCbor,
    };
    const tx: MidgardNativeTxFull = {
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
      body,
      witnessSet,
    };

    const cardanoTx = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(tx),
    );
    const mint = cardanoTx.body().mint();
    const assets = mint?.get_assets(CML.ScriptHash.from_raw_bytes(policyId));
    const redeemers = cardanoTx.witness_set().redeemers();

    expect(mint).toBeDefined();
    expect(mint?.policy_count()).toBe(1);
    expect(assets?.get(CML.AssetName.from_raw_bytes(assetName))).toBe(5n);
    expect(assets?.get(CML.AssetName.from_raw_bytes(Buffer.alloc(0)))).toBe(
      -2n,
    );
    expect(
      Buffer.from(cardanoTx.body().script_data_hash()!.to_raw_bytes()),
    ).toEqual(scriptIntegrityHash);
    expect(
      Buffer.from(cardanoTx.body().auxiliary_data_hash()!.to_raw_bytes()),
    ).toEqual(auxiliaryDataHash);
    expect(redeemers).toBeDefined();
    expect(Buffer.from(redeemers!.to_cbor_bytes())).toEqual(
      Buffer.from(
        encode(
          new Map([
            [
              [0, 0],
              [0, [0, 0]],
            ],
          ]),
        ),
      ),
    );
    const nativeRoundtrip = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(cardanoTx.to_cbor_bytes()),
      ),
    );
    expect(nativeRoundtrip.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      redeemerTxWitsPreimageCbor,
    );
    expect(cardanoTx.witness_set().plutus_datums()).toBeUndefined();
  });
});
