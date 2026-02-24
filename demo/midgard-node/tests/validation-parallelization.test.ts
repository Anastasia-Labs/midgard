import { readFileSync } from "node:fs";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";
import {
  PhaseAAccepted,
  QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidation,
} from "@/validation/index.js";

type TxFixture = {
  readonly txId: string;
  readonly cborHex: string;
};

type TxFixtureFile = {
  readonly transactions: readonly TxFixture[];
};

const fixturePath = new URL(
  "./benchmarks/fixtures/tx-sequence.json",
  import.meta.url,
);

const loadTxFixtures = (): readonly TxFixture[] => {
  const raw = readFileSync(fixturePath, "utf8");
  const parsed = JSON.parse(raw) as TxFixtureFile;
  return parsed.transactions;
};

const hex32 = (byte: number) => byte.toString(16).padStart(2, "0").repeat(32);

const outRefFromHash = (txHashHex: string, index: bigint): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHashHex),
      index,
    ).to_cbor_bytes(),
  );

const makeOutput = (address: string, lovelace: bigint): Buffer =>
  Buffer.from(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

const testAddress = walletFromSeed(
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  { network: "Preprod" },
).address;

const signerHash = (() => {
  const paymentCred = CML.Address.from_bech32(testAddress).payment_cred();
  const signer = paymentCred?.as_pub_key()?.to_hex();
  if (signer === undefined) {
    throw new Error("failed to derive pubkey hash from test address");
  }
  return signer;
})();

const makeCandidate = ({
  txByte,
  arrivalSeq,
  spent,
  referenceInputs = [],
  inputLovelace = 10n,
}: {
  readonly txByte: number;
  readonly arrivalSeq: bigint;
  readonly spent: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly inputLovelace?: bigint;
}): PhaseAAccepted => {
  const txId = Buffer.from(hex32(txByte), "hex");
  const producedOutRef = outRefFromHash(txId.toString("hex"), 0n);
  const output = makeOutput(testAddress, inputLovelace);
  return {
    txId,
    txCbor: Buffer.alloc(0),
    arrivalSeq,
    fee: 0n,
    validityIntervalStart: undefined,
    validityIntervalEnd: undefined,
    referenceInputs,
    outputSum: CML.Value.from_coin(inputLovelace),
    witnessKeyHashes: [signerHash],
    nativeScriptHashes: [],
    processedTx: {
      txId,
      txCbor: Buffer.alloc(0),
      spent: [...spent],
      produced: [
        {
          tx_id: txId,
          outref: producedOutRef,
          output,
          address: testAddress,
        },
      ],
    },
  };
};

describe("validation parallelization", () => {
  it("keeps phase-A verdicts and order stable across concurrency levels", async () => {
    const fixtures = loadTxFixtures().slice(0, 64);
    const queued: QueuedTx[] = fixtures.map((tx, index) => ({
      txId: Buffer.from(tx.txId, "hex"),
      txCbor: Buffer.from(tx.cborHex, "hex"),
      arrivalSeq: BigInt(index),
      createdAt: new Date(0),
    }));
    queued.push({
      txId: Buffer.alloc(32, 0xff),
      txCbor: Buffer.from("80", "hex"),
      arrivalSeq: BigInt(queued.length),
      createdAt: new Date(0),
    });

    const baseConfig = {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      strictnessProfile: "phase1_midgard",
    };

    const serial = await Effect.runPromise(
      runPhaseAValidation(queued, {
        ...baseConfig,
        concurrency: 1,
      }),
    );
    const parallel = await Effect.runPromise(
      runPhaseAValidation(queued, {
        ...baseConfig,
        concurrency: 32,
      }),
    );

    expect(
      parallel.accepted.map((tx) => tx.txId.toString("hex")),
    ).toStrictEqual(serial.accepted.map((tx) => tx.txId.toString("hex")));
    expect(parallel.rejected.map((tx) => tx.code)).toStrictEqual(
      serial.rejected.map((tx) => tx.code),
    );
  });

  it("does not accept conflicting txs across parallel phase-B buckets", async () => {
    const spentX = outRefFromHash(hex32(0x01), 0n);
    const spentZ = outRefFromHash(hex32(0x02), 0n);
    const preState = new Map<string, Buffer>([
      [spentX.toString("hex"), makeOutput(testAddress, 10n)],
      [spentZ.toString("hex"), makeOutput(testAddress, 10n)],
    ]);

    const txA = makeCandidate({
      txByte: 0xa1,
      arrivalSeq: 0n,
      spent: [spentX],
    });
    const txB = makeCandidate({
      txByte: 0xb1,
      arrivalSeq: 1n,
      spent: [spentX],
      referenceInputs: [spentZ],
    });
    const txC = makeCandidate({
      txByte: 0xc1,
      arrivalSeq: 2n,
      spent: [spentZ],
    });

    const phaseB = await Effect.runPromise(
      runPhaseBValidation([txA, txB, txC], preState, {
        nowMillis: 0n,
        bucketConcurrency: 8,
      }),
    );

    expect(phaseB.accepted.map((tx) => tx.txId.toString("hex"))).toStrictEqual([
      txA.txId.toString("hex"),
      txC.txId.toString("hex"),
    ]);
    expect(phaseB.rejected).toHaveLength(1);
    expect(phaseB.rejected[0].txId.toString("hex")).toBe(
      txB.txId.toString("hex"),
    );
    expect([RejectCodes.DoubleSpend, RejectCodes.InputNotFound]).toContain(
      phaseB.rejected[0].code,
    );
  });
});
