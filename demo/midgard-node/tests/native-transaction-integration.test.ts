import { describe, expect, it } from "vitest";
import { encode } from "cborg";
import fs from "node:fs";
import path from "node:path";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  deriveMidgardNativeTxBodyCompactFromFull,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxFull,
  type MidgardNativeTxBodyFull,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetFull,
} from "@/midgard-tx-codec/index.js";
import { findSpentAndProducedUTxOs, breakDownTx } from "@/utils.js";
import { RejectCodes, runPhaseAValidation, type QueuedTx } from "@/validation/index.js";

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const TEST_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const txFixtures = JSON.parse(
  fs.readFileSync(fixturePath, "utf8"),
) as readonly TxFixture[];

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const buildNativeTx = (opts?: {
  readonly redeemerItems?: readonly Uint8Array[];
  readonly requiredObserverItems?: readonly Uint8Array[];
  readonly witnessMode?: "none" | "valid" | "invalid";
  readonly networkId?: bigint;
}): { tx: MidgardNativeTxFull; txId: Buffer; txCbor: Buffer; inputOutRef: Buffer; outputCbor: Buffer } => {
  const spendInput = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex("11".repeat(32)),
      0n,
    ).to_cbor_bytes(),
  );
  const referenceInput = Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex("22".repeat(32)),
      1n,
    ).to_cbor_bytes(),
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(TEST_ADDRESS),
    CML.Value.from_coin(3_000_000n),
  );
  const outputCbor = Buffer.from(output.to_cbor_bytes());

  const spendInputsPreimageCbor = encodeByteList([spendInput]);
  const referenceInputsPreimageCbor = encodeByteList([referenceInput]);
  const outputsPreimageCbor = encodeByteList([outputCbor]);
  const requiredObserversPreimageCbor = encodeByteList(
    opts?.requiredObserverItems ?? [],
  );
  const witnessMode = opts?.witnessMode ?? "none";
  const witnessSignerPrivateKey =
    witnessMode === "none" ? undefined : CML.PrivateKey.generate_ed25519();
  const requiredSignersPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(witnessSignerPrivateKey.to_public().hash().to_raw_bytes()),
        ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = encodeByteList(opts?.redeemerItems ?? []);

  const body: MidgardNativeTxBodyFull = {
    spendInputsRoot: computeHash32(spendInputsPreimageCbor),
    spendInputsPreimageCbor,
    referenceInputsRoot: computeHash32(referenceInputsPreimageCbor),
    referenceInputsPreimageCbor,
    outputsRoot: computeHash32(outputsPreimageCbor),
    outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversRoot: computeHash32(requiredObserversPreimageCbor),
    requiredObserversPreimageCbor,
    requiredSignersRoot: computeHash32(requiredSignersPreimageCbor),
    requiredSignersPreimageCbor,
    mintRoot: computeHash32(mintPreimageCbor),
    mintPreimageCbor,
    scriptIntegrityHash: computeHash32(EMPTY_CBOR_NULL),
    auxiliaryDataHash: computeHash32(EMPTY_CBOR_NULL),
    networkId: opts?.networkId ?? 0n,
  };

  const bodyHash = computeHash32(
    encodeMidgardNativeTxBodyCompact(
      deriveMidgardNativeTxBodyCompactFromFull(body),
    ),
  );
  const signedBodyHash =
    witnessMode === "invalid" ? Buffer.alloc(32, 0x7f) : bodyHash;
  const addrTxWitsPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              witnessSignerPrivateKey,
            ).to_cbor_bytes(),
          ),
        ]);

  const witnessSet: MidgardNativeTxWitnessSetFull = {
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
    scriptTxWitsRoot: computeHash32(scriptTxWitsPreimageCbor),
    scriptTxWitsPreimageCbor,
    redeemerTxWitsRoot: computeHash32(redeemerTxWitsPreimageCbor),
    redeemerTxWitsPreimageCbor,
  };

  const tx: MidgardNativeTxFull = {
    version: MIDGARD_NATIVE_TX_VERSION,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
    body,
    witnessSet,
  };

  const txCbor = encodeMidgardNativeTxFull(tx);
  const txId = computeMidgardNativeTxIdFromFull(tx);

  return {
    tx,
    txId,
    txCbor,
    inputOutRef: spendInput,
    outputCbor,
  };
};

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

const mkQueued = (txId: Buffer, txCbor: Buffer): QueuedTx => ({
  txId,
  txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

describe("native transaction integration", () => {
  it("accepts a canonical native tx in phase A", async () => {
    const { txId, txCbor, inputOutRef } = buildNativeTx();
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    const accepted = result.accepted[0];
    expect(accepted.txId.equals(txId)).toBe(true);
    expect(accepted.processedTx.spent).toHaveLength(1);
    expect(accepted.processedTx.spent[0].equals(inputOutRef)).toBe(true);
    expect(accepted.processedTx.produced).toHaveLength(1);
    expect(accepted.witnessKeyHashes).toStrictEqual([]);
    expect(accepted.nativeScriptHashes).toStrictEqual([]);
    expect(accepted.outputSum.coin()).toBe(3_000_000n);
  });

  it("accepts required signer when native witness signature is valid", async () => {
    const { txId, txCbor } = buildNativeTx({ witnessMode: "valid" });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
    expect(result.accepted[0].witnessKeyHashes.length).toBe(1);
  });

  it("accepts native txs when network id is omitted via sentinel", async () => {
    const { txId, txCbor } = buildNativeTx({
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });

  it("rejects native txs with invalid vkey witness signatures", async () => {
    const { txId, txCbor } = buildNativeTx({ witnessMode: "invalid" });
    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidSignature);
  });

  it("rejects native txs with non-empty redeemer witnesses", async () => {
    const { txId, txCbor } = buildNativeTx({
      redeemerItems: [Buffer.from("00", "hex")],
    });

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.UnsupportedFieldNonEmpty);
    expect(result.rejected[0].detail).toBe("redeemer_tx_wits");
  });

  it("rejects native txs with non-empty required observers", async () => {
    const { txId, txCbor } = buildNativeTx({
      requiredObserverItems: [Buffer.from("01", "hex")],
    });

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.UnsupportedFieldNonEmpty);
    expect(result.rejected[0].detail).toBe("required_observers");
  });

  it("rejects malformed native required signer preimages", async () => {
    const base = buildNativeTx();
    const malformedRequiredSignersPreimageCbor = encodeByteList([
      Buffer.alloc(27, 0x11),
    ]);
    const tx: MidgardNativeTxFull = {
      ...base.tx,
      body: {
        ...base.tx.body,
        requiredSignersRoot: computeHash32(malformedRequiredSignersPreimageCbor),
        requiredSignersPreimageCbor: malformedRequiredSignersPreimageCbor,
      },
    };
    tx.compact = deriveMidgardNativeTxCompact(tx.body, tx.witnessSet, "TxIsValid");
    const txId = computeMidgardNativeTxIdFromFull(tx);
    const txCbor = encodeMidgardNativeTxFull(tx);

    const result = await Effect.runPromise(
      runPhaseAValidation([mkQueued(txId, txCbor)], phaseAConfig),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InvalidFieldType);
  });

  it("rejects legacy Cardano payloads in native-only phase A", async () => {
    const cardanoTx = txFixtures[0];
    const result = await Effect.runPromise(
      runPhaseAValidation(
        [
          mkQueued(
            Buffer.from(cardanoTx.txId, "hex"),
            Buffer.from(cardanoTx.cborHex, "hex"),
          ),
        ],
        phaseAConfig,
      ),
    );

    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.CborDeserialization);
  });

  it("rejects legacy Cardano payloads in native-only utility decoders", async () => {
    const cardanoTxBytes = Buffer.from(txFixtures[0].cborHex, "hex");
    const cardanoTxId = Buffer.from(txFixtures[0].txId, "hex");

    await expect(Effect.runPromise(breakDownTx(cardanoTxBytes))).rejects.toThrow();
    await expect(
      Effect.runPromise(findSpentAndProducedUTxOs(cardanoTxBytes, cardanoTxId)),
    ).rejects.toThrow();
  });

  it("breaks down native txs into spent/produced outputs", async () => {
    const { txId, txCbor, inputOutRef, outputCbor } = buildNativeTx();
    const result = await Effect.runPromise(breakDownTx(txCbor));

    expect(result.txId.equals(txId)).toBe(true);
    expect(result.spent).toHaveLength(1);
    expect(result.spent[0].equals(inputOutRef)).toBe(true);
    expect(result.produced).toHaveLength(1);
    expect(result.produced[0].output.equals(outputCbor)).toBe(true);
    expect(result.produced[0].address).toBe(TEST_ADDRESS);
  });

  it("finds spent/produced sets for native tx payloads", async () => {
    const { txId, txCbor, inputOutRef, outputCbor } = buildNativeTx();
    const result = await Effect.runPromise(findSpentAndProducedUTxOs(txCbor, txId));

    expect(result.spent).toHaveLength(1);
    expect(result.spent[0].equals(inputOutRef)).toBe(true);
    expect(result.produced).toHaveLength(1);
    expect(result.produced[0].outref.equals(txId)).toBe(true);
    expect(result.produced[0].output.equals(outputCbor)).toBe(true);
  });
});
