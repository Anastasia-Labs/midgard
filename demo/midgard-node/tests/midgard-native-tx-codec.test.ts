import { describe, expect, it } from "vitest";
import fs from "node:fs";
import path from "node:path";
import { encode } from "cborg";
import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFull,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardVersionedScriptListPreimage,
  midgardNativeTxFullToCardanoTxEncoding,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  verifyMidgardNativeTxFullConsistency,
} from "@/midgard-tx-codec/index.js";
import {
  makeCardanoTxOutput,
  makeMidgardTxOutput,
} from "./midgard-output-helpers.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

const mkHash = (tag: string): Buffer => computeHash32(Buffer.from(tag, "utf8"));

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const makePlutusIntegerData = (
  value: bigint,
): InstanceType<typeof CML.PlutusData> =>
  CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString(10)));

const makeConvertibleCardanoTxBytes = (): Buffer => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    makeCardanoTxOutput(
      CML.Address.from_bech32(
        "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
      ),
      CML.Value.from_coin(2_000_000n),
    ),
  );
  return Buffer.from(
    CML.Transaction.new(
      CML.TransactionBody.new(inputs, outputs, 0n),
      CML.TransactionWitnessSet.new(),
      true,
      undefined,
    ).to_cbor_bytes(),
  );
};

const mkBody = (): MidgardNativeTxBodyCanonical => {
  const spendInputsPreimageCbor = Buffer.from("820102", "hex");
  const referenceInputsPreimageCbor = Buffer.from("8103", "hex");
  const outputsPreimageCbor = Buffer.from("83040506", "hex");
  const requiredObserversPreimageCbor = Buffer.from("80", "hex");
  const requiredSignersPreimageCbor = Buffer.from("820708", "hex");
  const mintPreimageCbor = Buffer.from("a0", "hex");

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
    networkId: 11n,
  };
};

const mkWitnessSet = (): MidgardNativeTxWitnessSetCanonical => {
  const addrTxWitsPreimageCbor = Buffer.from("8101", "hex");
  const scriptTxWitsPreimageCbor = Buffer.from("8102", "hex");
  const redeemerTxWitsPreimageCbor = Buffer.from("8103", "hex");

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

    const encodedFull = encodeMidgardNativeTxFull(full);
    const decodedFull = decodeMidgardNativeTxFull(encodedFull);
    expect(decodedFull).toEqual(full);
  });

  it("uses the body hash as the canonical tx id", () => {
    const full = mkFull();

    expect(computeMidgardNativeTxIdFromFull(full)).toEqual(
      computeHash32(encodeMidgardNativeTxBodyCompact(full.compact.transactionBody)),
    );
  });

  it("rejects legacy witness-set compact encodings with a datum witness bucket", () => {
    const witnessCompact =
      deriveMidgardNativeTxWitnessSetCompact(mkWitnessSet());
    const legacyShape = Buffer.from(
      encode([
        Buffer.from(witnessCompact.addrTxWitsHash),
        Buffer.from(witnessCompact.scriptTxWitsHash),
        Buffer.from(witnessCompact.redeemerTxWitsHash),
        Buffer.from(mkHash("legacy-datum-wits")),
      ]),
    );

    expect(() => decodeMidgardNativeTxWitnessSetCompact(legacyShape)).toThrow(
      /exactly 3 elements/i,
    );
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

    expect(() => encodeMidgardNativeTxFull(tampered)).toThrow();
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

    expect(() => encodeMidgardNativeTxFull(tampered)).toThrow();
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
        version: 2n,
      },
    };

    expect(() => encodeMidgardNativeTxFull(tampered)).toThrow(
      /transaction_full.version must match transaction_compact.version/i,
    );
  });
});

describe("midgard native tx codec - cardano compatibility bridge", () => {
  const sampleTxBytes = [makeConvertibleCardanoTxBytes()];

  it("converts Cardano tx fixtures into Midgard native full tx bytes", () => {
    for (const cardanoTx of sampleTxBytes) {
      const nativeFullBytes =
        cardanoTxBytesToMidgardNativeTxFullBytes(cardanoTx);
      const decoded = decodeMidgardNativeTxFull(nativeFullBytes);
      const cardanoDecoded = CML.Transaction.from_cbor_bytes(cardanoTx);
      const networkId = cardanoDecoded.body().network_id();
      const expectedNetworkId =
        networkId !== undefined
          ? BigInt(networkId.network())
          : MIDGARD_NATIVE_NETWORK_ID_NONE;

      expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
      expect(computeMidgardNativeTxIdFromFull(decoded).length).toBe(32);
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

    const nativeFullBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFull(nativeFullBytes);
    const emptyList = Buffer.from("80", "hex");

    expect(decoded.body.mintPreimageCbor).toEqual(emptyList);
    expect(
      decoded.compact.transactionBody.mintHash.equals(computeHash32(emptyList)),
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

    const nativeFullBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFull(nativeFullBytes);
    const expectedMintPreimage = Buffer.from(
      encode(
        new Map([
          [policyId, new Map<Uint8Array, bigint>([[Buffer.alloc(0), 1n]])],
        ]),
      ),
    );

    expect(decoded.body.mintPreimageCbor).toEqual(expectedMintPreimage);
    expect(
      decoded.compact.transactionBody.mintHash.equals(
        computeHash32(expectedMintPreimage),
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

    const nativeFullBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFull(nativeFullBytes);

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

    const nativeFullBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFull(nativeFullBytes);

    expect(decoded.body.scriptIntegrityHash).toEqual(scriptDataHash);
    expect(decoded.body.auxiliaryDataHash).toEqual(auxiliaryDataHash);
    expect(decoded.witnessSet.redeemerTxWitsPreimageCbor).toEqual(
      redeemerBytes,
    );
    expect(decoded.witnessSet.scriptTxWitsPreimageCbor).toEqual(
      encodeMidgardVersionedScriptListPreimage([
        {
          language: "PlutusV3",
          scriptBytes: Buffer.from(plutusScript.to_raw_bytes()),
        },
      ]),
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
      cardanoTxBytesToMidgardNativeTxFullBytes(
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
      cardanoTxBytesToMidgardNativeTxFullBytes(
        Buffer.from(mutated.to_cbor_bytes()),
      ),
    ).toThrow();
  });

  it("re-encodes supported Midgard-native transactions into Cardano tx CBOR", () => {
    const cardanoTx = sampleTxBytes[0];
    const nativeTx = decodeMidgardNativeTxFull(
      cardanoTxBytesToMidgardNativeTxFullBytes(cardanoTx),
    );

    const reEncoded = midgardNativeTxFullToCardanoTxEncoding(nativeTx);

    expect(Buffer.from(reEncoded)).toEqual(cardanoTx);
  });

  it("can omit vkey witnesses for eval-only Cardano export", () => {
    const full = decodeMidgardNativeTxFull(
      cardanoTxBytesToMidgardNativeTxFullBytes(sampleTxBytes[0]),
    );
    const signer = CML.PrivateKey.generate_ed25519();
    const vkeyWitness = Buffer.from(
      CML.make_vkey_witness(
        CML.TransactionHash.from_raw_bytes(computeMidgardNativeTxIdFromFull(full)),
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
    const input = Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("11".repeat(32)),
        0n,
      ).to_cbor_bytes(),
    );
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

  it("maps non-success Midgard validity states to Cardano script-invalid txs", () => {
    const full = decodeMidgardNativeTxFull(
      cardanoTxBytesToMidgardNativeTxFullBytes(sampleTxBytes[0]),
    );
    const invalid: MidgardNativeTxFull = {
      ...full,
      validity: "NonExistentInputUtxo",
      compact: deriveMidgardNativeTxCompact(
        full.body,
        full.witnessSet,
        "NonExistentInputUtxo",
      ),
    };

    const cardanoTx = CML.Transaction.from_cbor_bytes(
      midgardNativeTxFullToCardanoTxEncoding(invalid),
    );

    expect(cardanoTx.is_valid()).toBe(false);
  });

  it("preserves mint, script integrity hash, auxiliary data hash, and redeemers", () => {
    const full = decodeMidgardNativeTxFull(
      cardanoTxBytesToMidgardNativeTxFullBytes(sampleTxBytes[0]),
    );
    const policyId = Buffer.from("33".repeat(28), "hex");
    const assetName = Buffer.from("aa", "hex");
    const mintPreimageCbor = Buffer.from(
      encode(
        new Map([
          [
            policyId,
            new Map<Uint8Array, bigint>([
              [assetName, 5n],
              [Buffer.alloc(0), -2n],
            ]),
          ],
        ]),
      ),
    );
    const redeemerTxWitsPreimageCbor = Buffer.from(
      encode([[0, 0, Buffer.alloc(0), [0, 0]]]),
    );
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
      redeemerTxWitsPreimageCbor,
    );
    expect(cardanoTx.witness_set().plutus_datums()).toBeUndefined();
  });
});
