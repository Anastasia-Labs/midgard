import { readFileSync } from "node:fs";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";
import {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";
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

/**
 * Builds a fixed-width 32-byte hex string for validation tests.
 */
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

/**
 * Builds a signer hash fixture for validation tests.
 */
const signerHash = (() => {
  const paymentCred = CML.Address.from_bech32(testAddress).payment_cred();
  const signer = paymentCred?.as_pub_key()?.to_hex();
  if (signer === undefined) {
    throw new Error("failed to derive pubkey hash from test address");
  }
  return signer;
})();
const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_LIST_ROOT = computeHash32(EMPTY_CBOR_LIST);

const makeCandidate = ({
  txByte,
  arrivalSeq,
  spent,
  referenceInputs = [],
  inputLovelace = 10n,
  validityIntervalStart,
  validityIntervalEnd,
}: {
  readonly txByte: number;
  readonly arrivalSeq: bigint;
  readonly spent: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly inputLovelace?: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): PhaseAAccepted => {
  const txId = Buffer.from(hex32(txByte), "hex");
  const producedOutRef = outRefFromHash(txId.toString("hex"), 0n);
  const output = makeOutput(testAddress, inputLovelace);
  return {
    txId,
    txCbor: Buffer.alloc(0),
    arrivalSeq,
    fee: 0n,
    validityIntervalStart,
    validityIntervalEnd,
    referenceInputs,
    outputSum: CML.Value.from_coin(inputLovelace),
    witnessKeyHashes: [signerHash],
    requiredObserverHashes: [],
    mintPolicyHashes: [],
    mintedValue: CML.Value.zero(),
    burnedValue: CML.Value.zero(),
    nativeScriptHashes: [],
    plutusScriptHashes: [],
    requiresPlutusEvaluation: false,
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
    const queued: QueuedTx[] = fixtures.map((tx, index) => {
      const converted = decodeMidgardNativeTxFull(
        cardanoTxBytesToMidgardNativeTxFullBytes(
          Buffer.from(tx.cborHex, "hex"),
        ),
      );
      const normalized = {
        version: converted.version,
        body: {
          ...converted.body,
          requiredSignersRoot: EMPTY_LIST_ROOT,
          requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        },
        witnessSet: {
          ...converted.witnessSet,
          addrTxWitsRoot: EMPTY_LIST_ROOT,
          addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          scriptTxWitsRoot: EMPTY_LIST_ROOT,
          scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          redeemerTxWitsRoot: EMPTY_LIST_ROOT,
          redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          datumTxWitsRoot: EMPTY_LIST_ROOT,
          datumTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        },
      };
      const txForQueue = {
        ...normalized,
        compact: deriveMidgardNativeTxCompact(
          normalized.body,
          normalized.witnessSet,
          "TxIsValid",
        ),
      };
      const nativeTxBytes = encodeMidgardNativeTxFull(txForQueue);
      return {
        txId: computeMidgardNativeTxIdFromFull(txForQueue),
        txCbor: nativeTxBytes,
        arrivalSeq: BigInt(index),
        createdAt: new Date(0),
      };
    });
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
        nowCardanoSlotNo: 0n,
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

  it("evaluates validity intervals against the current Cardano slot number", async () => {
    const spent = outRefFromHash(hex32(0x03), 0n);
    const preState = new Map<string, Buffer>([
      [spent.toString("hex"), makeOutput(testAddress, 10n)],
    ]);

    const expired = makeCandidate({
      txByte: 0xd1,
      arrivalSeq: 0n,
      spent: [spent],
      validityIntervalEnd: 119n,
    });
    const active = makeCandidate({
      txByte: 0xd2,
      arrivalSeq: 1n,
      spent: [spent],
      validityIntervalStart: 120n,
      validityIntervalEnd: 121n,
    });

    const expiredPhaseB = await Effect.runPromise(
      runPhaseBValidation([expired], preState, {
        nowCardanoSlotNo: 120n,
        bucketConcurrency: 1,
      }),
    );
    expect(expiredPhaseB.accepted).toHaveLength(0);
    expect(expiredPhaseB.rejected).toHaveLength(1);
    expect(expiredPhaseB.rejected[0].code).toBe(
      RejectCodes.ValidityIntervalMismatch,
    );
    expect(expiredPhaseB.rejected[0].detail).toBe("120 > 119");

    const activePhaseB = await Effect.runPromise(
      runPhaseBValidation([active], preState, {
        nowCardanoSlotNo: 120n,
        bucketConcurrency: 1,
      }),
    );
    expect(activePhaseB.accepted.map((tx) => tx.txId.toString("hex"))).toStrictEqual([
      active.txId.toString("hex"),
    ]);
    expect(activePhaseB.rejected).toHaveLength(0);
  });
});
