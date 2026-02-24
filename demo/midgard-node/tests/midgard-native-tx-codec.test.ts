import { describe, expect, it } from "vitest";
import fs from "node:fs";
import path from "node:path";
import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeHash32,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFull,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompactFromFull,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompactFromFull,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardNativeTxBodyFull,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetFull,
  verifyMidgardNativeTxFullConsistency,
} from "@/midgard-tx-codec/index.js";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

const mkHash = (tag: string): Buffer => computeHash32(Buffer.from(tag, "utf8"));

const mkBody = (): MidgardNativeTxBodyFull => {
  const spendInputsPreimageCbor = Buffer.from("820102", "hex");
  const referenceInputsPreimageCbor = Buffer.from("8103", "hex");
  const outputsPreimageCbor = Buffer.from("83040506", "hex");
  const requiredObserversPreimageCbor = Buffer.from("80", "hex");
  const requiredSignersPreimageCbor = Buffer.from("820708", "hex");
  const mintPreimageCbor = Buffer.from("a0", "hex");

  return {
    spendInputsRoot: computeHash32(spendInputsPreimageCbor),
    spendInputsPreimageCbor,
    referenceInputsRoot: computeHash32(referenceInputsPreimageCbor),
    referenceInputsPreimageCbor,
    outputsRoot: computeHash32(outputsPreimageCbor),
    outputsPreimageCbor,
    fee: 42n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: 1_735_000_000_000n,
    requiredObserversRoot: computeHash32(requiredObserversPreimageCbor),
    requiredObserversPreimageCbor,
    requiredSignersRoot: computeHash32(requiredSignersPreimageCbor),
    requiredSignersPreimageCbor,
    mintRoot: computeHash32(mintPreimageCbor),
    mintPreimageCbor,
    scriptIntegrityHash: mkHash("script-integrity"),
    auxiliaryDataHash: mkHash("aux-data"),
    networkId: 11n,
  };
};

const mkWitnessSet = (): MidgardNativeTxWitnessSetFull => {
  const addrTxWitsPreimageCbor = Buffer.from("8101", "hex");
  const scriptTxWitsPreimageCbor = Buffer.from("8102", "hex");
  const redeemerTxWitsPreimageCbor = Buffer.from("8103", "hex");

  return {
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
    scriptTxWitsRoot: computeHash32(scriptTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor,
    redeemerTxWitsRoot: computeHash32(redeemerTxWitsPreimageCbor),
    redeemerTxWitsPreimageCbor,
  };
};

const mkFull = (): MidgardNativeTxFull => {
  const body = mkBody();
  const witnessSet = mkWitnessSet();
  const compact = deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid");
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    compact,
    body,
    witnessSet,
  };
};

describe("midgard native tx codec - strict roundtrip", () => {
  it("roundtrips compact tx/body/witness and full tx", () => {
    const full = mkFull();

    const bodyCompact = deriveMidgardNativeTxBodyCompactFromFull(full.body);
    const witnessCompact =
      deriveMidgardNativeTxWitnessSetCompactFromFull(full.witnessSet);

    expect(
      decodeMidgardNativeTxBodyCompact(encodeMidgardNativeTxBodyCompact(bodyCompact)),
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
});

describe("midgard native tx codec - consistency checks", () => {
  it("rejects inconsistent compact hash commitments", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      compact: {
        ...full.compact,
        transactionBodyHash: Buffer.from(full.compact.transactionBodyHash),
      },
    };

    tampered.compact.transactionBodyHash[0] ^= 0xff;

    const bytes = encodeMidgardNativeTxFull(tampered, {
      enforceConsistency: false,
    });

    expect(() => decodeMidgardNativeTxFull(bytes)).toThrow();
  });

  it("rejects inconsistent body root/preimage pairs", () => {
    const full = mkFull();
    const tampered: MidgardNativeTxFull = {
      ...full,
      body: {
        ...full.body,
        outputsPreimageCbor: Buffer.from(full.body.outputsPreimageCbor),
      },
    };

    tampered.body.outputsPreimageCbor[0] ^= 0xff;

    const bytes = encodeMidgardNativeTxFull(tampered, {
      enforceConsistency: false,
    });

    expect(() => decodeMidgardNativeTxFull(bytes)).toThrow();
  });

  it("accepts when explicit consistency verification passes", () => {
    const full = mkFull();
    expect(() => verifyMidgardNativeTxFullConsistency(full)).not.toThrow();
  });
});

describe("midgard native tx codec - cardano compatibility bridge", () => {
  const sampleTxBytes = txFixtures
    .slice(0, 8)
    .map((tx) => Buffer.from(tx.cborHex, "hex"));

  it("converts Cardano tx fixtures into Midgard native full tx bytes", () => {
    for (const cardanoTx of sampleTxBytes) {
      const nativeFullBytes = cardanoTxBytesToMidgardNativeTxFullBytes(
        cardanoTx,
      );
      const decoded = decodeMidgardNativeTxFull(nativeFullBytes);
      const cardanoDecoded = CML.Transaction.from_cbor_bytes(cardanoTx);
      const networkId = cardanoDecoded.body().network_id();
      const expectedNetworkId =
        networkId !== undefined
          ? BigInt(networkId.network())
          : MIDGARD_NATIVE_NETWORK_ID_NONE;

      expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
      expect(decoded.compact.transactionBodyHash.length).toBe(32);
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
    expect(decoded.body.mintRoot.equals(computeHash32(emptyList))).toBe(true);
  });

  it("fails fast on non-empty Cardano mint fields", () => {
    const parsed = CML.Transaction.from_cbor_bytes(sampleTxBytes[0]);
    const body = CML.TransactionBody.from_cbor_bytes(
      parsed.body().to_cbor_bytes(),
    );
    const mintAssets = CML.MapAssetNameToNonZeroInt64.new();
    mintAssets.insert(CML.AssetName.from_raw_bytes(Buffer.from([])), 1n);
    const mint = CML.Mint.new();
    mint.insert_assets(CML.ScriptHash.from_hex("11".repeat(28)), mintAssets);
    body.set_mint(mint);
    const mutated = CML.Transaction.new(
      body,
      parsed.witness_set(),
      parsed.is_valid(),
      parsed.auxiliary_data(),
    );

    expect(() =>
      cardanoTxBytesToMidgardNativeTxFullBytes(
        Buffer.from(mutated.to_cbor_bytes()),
      ),
    ).toThrow();
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
});
