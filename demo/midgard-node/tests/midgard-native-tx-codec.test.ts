import {
  cardanoTxBytesToMidgardNativeTxCanonicalBinary,
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalBinary,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardMint,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  midgardNativeTxFullToCardanoTxEncoding,
  type MidgardNativeTxWitnessSetCanonical,
  type OutputReference,
  type VKeyWitness,
  verifyMidgardNativeTxFullConsistency,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { encode } from "cborg";
import { describe, expect, it } from "vitest";

import {
  makeCardanoTxOutput,
  makeMidgardTxOutput,
} from "./midgard-output-helpers.js";

const mkHash = (tag: string): Buffer => computeHash32(Buffer.from(tag, "utf8"));

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

const sampleSpendInput: OutputReference = {
  txId: Buffer.from("aa".repeat(32), "hex"),
  index: 2,
};

const sampleRefInput: OutputReference = {
  txId: Buffer.from("bb".repeat(32), "hex"),
  index: 0,
};

const sampleSignerHash = Buffer.from("cc".repeat(28), "hex");

const sampleMint = (): MidgardMint =>
  new Map([
    [
      "dd".repeat(28),
      new Map([
        ["", 1n],
        ["616263", 2n],
      ]),
    ],
  ]);

const sampleVKeyWitness: VKeyWitness = {
  vkey: Buffer.alloc(32, 0xee),
  signature: Buffer.alloc(64, 0xff),
};

const mkBody = (): MidgardNativeTxBodyCanonical => ({
  spendInputs: [sampleSpendInput],
  referenceInputs: [sampleRefInput],
  outputs: [
    (() => {
      const { to_cbor_bytes: _toCbor, ...output } = makeMidgardTxOutput(
        CML.Address.from_bech32(
          "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
        ),
        CML.Value.from_coin(2_000_000n),
      );
      return output;
    })(),
  ],
  fee: 42n,
  validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd: 1_735_000_000_000n,
  requiredObservers: [],
  requiredSigners: [sampleSignerHash],
  mint: sampleMint(),
  scriptIntegrityHash: mkHash("script-integrity"),
  auxiliaryDataHash: mkHash("aux-data"),
  networkId: 11n,
});

const mkWitnessSet = (): MidgardNativeTxWitnessSetCanonical => ({
  addrTxWits: [sampleVKeyWitness],
  scriptTxWits: [],
  redeemerTxWits: Buffer.alloc(0),
});

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

describe("midgard native tx codec - binary roundtrip", () => {
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
      decodeMidgardNativeTxFullFromCanonicalBinary(encodedCanonical);
    expect(decodedFull).toEqual(full);
  });

  it("uses the body hash as the canonical tx id", () => {
    const full = mkFull();

    expect(computeMidgardNativeTxId(full)).toEqual(
      computeHash32(
        encodeMidgardNativeTxBodyCompact(full.compact.transactionBody),
      ),
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
        version: 2n,
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
      const nativeCanonicalBytes =
        cardanoTxBytesToMidgardNativeTxCanonicalBinary(cardanoTx);
      const decoded =
        decodeMidgardNativeTxFullFromCanonicalBinary(nativeCanonicalBytes);
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

  it("represents an empty Cardano mint map as an empty typed mint", () => {
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

    const nativeBytes = cardanoTxBytesToMidgardNativeTxCanonicalBinary(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalBinary(nativeBytes);

    expect(decoded.body.mint.size).toBe(0);
  });

  it("preserves non-empty Cardano mint fields", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(CML.AssetName.from_raw_bytes(Buffer.from([])), 1n);
    const mint = CML.Mint.new();
    const policyIdHex = "11".repeat(28);
    const policyId = Buffer.from(policyIdHex, "hex");
    mint.insert_assets(CML.ScriptHash.from_raw_bytes(policyId), mintAssets);
    body.set_mint(mint);
    const mutated = CML.Transaction.new(
      body,
      parsed.witness_set(),
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    const nativeBytes = cardanoTxBytesToMidgardNativeTxCanonicalBinary(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalBinary(nativeBytes);

    expect(decoded.body.mint.get(policyIdHex)?.get("")).toBe(1n);
  });

  it("maps zero-ADA script withdrawals into required observers", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const scriptHashHex = "44".repeat(28);
    const scriptHash = CML.ScriptHash.from_hex(scriptHashHex);
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

    const nativeBytes = cardanoTxBytesToMidgardNativeTxCanonicalBinary(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalBinary(nativeBytes);

    expect(decoded.body.requiredObservers).toHaveLength(1);
    expect(decoded.body.requiredObservers[0].toString("hex")).toBe(
      scriptHashHex,
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
    const plutusBytes = Buffer.from("deadbeef", "hex");
    const plutusScript = CML.PlutusV3Script.from_raw_bytes(plutusBytes);
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

    const nativeBytes = cardanoTxBytesToMidgardNativeTxCanonicalBinary(
      Buffer.from(mutated.to_cbor_bytes()),
    );
    const decoded = decodeMidgardNativeTxFullFromCanonicalBinary(nativeBytes);

    expect(decoded.body.scriptIntegrityHash).toEqual(scriptDataHash);
    expect(decoded.body.auxiliaryDataHash).toEqual(auxiliaryDataHash);
    expect(decoded.witnessSet.redeemerTxWits).toEqual(redeemerBytes);
    expect(decoded.witnessSet.scriptTxWits).toEqual([
      {
        language: "PlutusV3",
        scriptBytes: plutusBytes,
      },
    ]);
  });

  it("re-encodes supported Midgard-native transactions into Cardano tx CBOR", () => {
    const cardanoTx = sampleTxBytes[0];
    const nativeTx = decodeMidgardNativeTxFullFromCanonicalBinary(
      cardanoTxBytesToMidgardNativeTxCanonicalBinary(cardanoTx),
    );

    const reEncoded = midgardNativeTxFullToCardanoTxEncoding(nativeTx);

    expect(Buffer.from(reEncoded)).toEqual(cardanoTx);
  });

  it("can omit vkey witnesses for eval-only Cardano export", () => {
    const full = decodeMidgardNativeTxFullFromCanonicalBinary(
      cardanoTxBytesToMidgardNativeTxCanonicalBinary(sampleTxBytes[0]),
    );
    const signer = CML.PrivateKey.generate_ed25519();
    const cmlWitness = CML.make_vkey_witness(
      CML.TransactionHash.from_raw_bytes(computeMidgardNativeTxId(full)),
      signer,
    );
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      ...full.witnessSet,
      addrTxWits: [
        {
          vkey: Buffer.from(cmlWitness.vkey().to_raw_bytes()),
          signature: Buffer.from(cmlWitness.ed25519_signature().to_raw_bytes()),
        },
      ],
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
    const spendInput: OutputReference = {
      txId: Buffer.from("11".repeat(32), "hex"),
      index: 0,
    };
    const output = makeMidgardTxOutput(
      CML.Address.from_bech32(
        "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58",
      ),
      CML.Value.from_coin(2_000_000n),
    );
    const observerCredential = CML.Credential.new_script(
      CML.ScriptHash.from_hex("22".repeat(28)),
    );
    const observerBytes = Buffer.from(observerCredential.to_cbor_bytes());

    const body: MidgardNativeTxBodyCanonical = {
      spendInputs: [spendInput],
      referenceInputs: [],
      outputs: [output],
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObservers: [observerBytes],
      requiredSigners: [],
      mint: new Map(),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    };
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      addrTxWits: [],
      scriptTxWits: [],
      redeemerTxWits: Buffer.alloc(0),
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
    const full = decodeMidgardNativeTxFullFromCanonicalBinary(
      cardanoTxBytesToMidgardNativeTxCanonicalBinary(sampleTxBytes[0]),
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
    const full = decodeMidgardNativeTxFullFromCanonicalBinary(
      cardanoTxBytesToMidgardNativeTxCanonicalBinary(sampleTxBytes[0]),
    );
    const policyIdHex = "33".repeat(28);
    const mint: MidgardMint = new Map([
      [
        policyIdHex,
        new Map([
          ["aa", 5n],
          ["", -2n],
        ]),
      ],
    ]);
    const redeemerTxWits = Buffer.from(encode([[0, 0, Buffer.alloc(0), [0, 0]]]));
    const scriptIntegrityHash = mkHash("script-data-hash");
    const auxiliaryDataHash = mkHash("auxiliary-data-hash");

    const body: MidgardNativeTxBodyCanonical = {
      ...full.body,
      mint,
      scriptIntegrityHash,
      auxiliaryDataHash,
    };
    const witnessSet: MidgardNativeTxWitnessSetCanonical = {
      ...full.witnessSet,
      redeemerTxWits,
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
    const cmlMint = cardanoTx.body().mint();
    const assets = cmlMint?.get_assets(
      CML.ScriptHash.from_raw_bytes(Buffer.from(policyIdHex, "hex")),
    );
    const redeemers = cardanoTx.witness_set().redeemers();

    expect(cmlMint).toBeDefined();
    expect(cmlMint?.policy_count()).toBe(1);
    expect(
      assets?.get(CML.AssetName.from_raw_bytes(Buffer.from("aa", "hex"))),
    ).toBe(5n);
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
    expect(Buffer.from(redeemers!.to_cbor_bytes())).toEqual(redeemerTxWits);
    expect(cardanoTx.witness_set().plutus_datums()).toBeUndefined();
  });
});
