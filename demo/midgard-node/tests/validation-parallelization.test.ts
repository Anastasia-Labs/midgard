import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";
import {
  decodeTransactionOutput,
  encodeTransaction,
  encodeTransactionOutput,
  transactionId,
  type OutputReference,
  type Transaction,
  type TransactionOutput,
} from "@/midgard-tx-codec/index.js";
import {
  PhaseAAccepted,
  QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidation,
} from "@/validation/index.js";
import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

/**
 * Builds a fixed-width 32-byte hex string for validation tests.
 */
const hex32 = (byte: number) => byte.toString(16).padStart(2, "0").repeat(32);

const outputRef = (txHashHex: string, index: number): OutputReference => ({
  tx_id: Buffer.from(txHashHex, "hex"),
  index,
});

const outRefCbor = (ref: OutputReference): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.from(ref.tx_id)),
      BigInt(ref.index),
    ).to_cbor_bytes(),
  );

const makeOutput = (
  address: string,
  lovelace: bigint,
): TransactionOutput =>
  // `makeMidgardTxOutput` exposes a `to_cbor_bytes()` accessor that returns
  // midgard-ts wire-format bytes (back-compat name). Decode straight back to
  // the structured midgard-ts `TransactionOutput`.
  decodeTransactionOutput(
    makeMidgardTxOutput(
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

const makeNativeTx = ({
  spent,
  referenceInputs = [],
  outputs,
  validityIntervalStart,
  validityIntervalEnd,
}: {
  readonly spent: readonly OutputReference[];
  readonly referenceInputs?: readonly OutputReference[];
  readonly outputs: readonly TransactionOutput[];
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): Transaction => ({
  body: {
    inputs: [...spent],
    outputs: [...outputs],
    fee: 0n,
    ttl:
      validityIntervalEnd !== undefined
        ? Number(validityIntervalEnd)
        : undefined,
    auxiliary_data_hash: undefined,
    validity_interval_start:
      validityIntervalStart !== undefined
        ? Number(validityIntervalStart)
        : undefined,
    mint: undefined,
    script_data_hash: undefined,
    required_signers: undefined,
    network_id: 0,
    reference_inputs:
      referenceInputs.length > 0 ? [...referenceInputs] : undefined,
    required_observers: undefined,
  },
  witness_set: {
    vkey_witnesses: undefined,
    scripts: undefined,
    redeemers: undefined,
  },
  is_valid: true,
});

const makeCandidate = ({
  arrivalSeq,
  spent,
  referenceInputs = [],
  inputLovelace = 10n,
  validityIntervalStart,
  validityIntervalEnd,
}: {
  readonly arrivalSeq: bigint;
  readonly spent: readonly OutputReference[];
  readonly referenceInputs?: readonly OutputReference[];
  readonly inputLovelace?: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): PhaseAAccepted => {
  const output = makeOutput(testAddress, inputLovelace);
  const tx = makeNativeTx({
    spent,
    referenceInputs,
    outputs: [output],
    validityIntervalStart,
    validityIntervalEnd,
  });
  const txId = Buffer.from(transactionId(tx));
  const txCbor = Buffer.from(encodeTransaction(tx));
  const producedOutRef = outRefCbor({ tx_id: txId, index: 0 });
  const outputBytes = Buffer.from(encodeTransactionOutput(output));
  const spentCbors = spent.map(outRefCbor);
  const referenceInputCbors = referenceInputs.map(outRefCbor);
  return {
    txId,
    txCbor,
    arrivalSeq,
    fee: 0n,
    validityIntervalStart,
    validityIntervalEnd,
    referenceInputs: referenceInputCbors,
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
      txCbor,
      spent: spentCbors,
      produced: [
        {
          tx_id: txId,
          outref: producedOutRef,
          output: outputBytes,
          address: testAddress,
        },
      ],
    },
  };
};

describe("validation parallelization", () => {
  it("keeps phase-A verdicts and order stable across concurrency levels", async () => {
    const queued: QueuedTx[] = Array.from({ length: 64 }, (_, index) => {
      const spent = outputRef(hex32(index + 1), 0);
      const nativeTx = makeNativeTx({
        spent: [spent],
        outputs: [makeOutput(testAddress, 10n)],
      });
      const nativeTxBytes = Buffer.from(encodeTransaction(nativeTx));
      return {
        txId: Buffer.from(transactionId(nativeTx)),
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
    const spentX = outputRef(hex32(0x01), 0);
    const spentZ = outputRef(hex32(0x02), 0);
    const preStateOutput = Buffer.from(
      encodeTransactionOutput(makeOutput(testAddress, 10n)),
    );
    const preState = new Map<string, Buffer>([
      [outRefCbor(spentX).toString("hex"), preStateOutput],
      [outRefCbor(spentZ).toString("hex"), preStateOutput],
    ]);

    const txA = makeCandidate({
      arrivalSeq: 0n,
      spent: [spentX],
    });
    const txB = makeCandidate({
      arrivalSeq: 1n,
      spent: [spentX],
      referenceInputs: [spentZ],
    });
    const txC = makeCandidate({
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
    const spent = outputRef(hex32(0x03), 0);
    const preStateOutput = Buffer.from(
      encodeTransactionOutput(makeOutput(testAddress, 10n)),
    );
    const preState = new Map<string, Buffer>([
      [outRefCbor(spent).toString("hex"), preStateOutput],
    ]);

    const expired = makeCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      validityIntervalEnd: 119n,
    });
    const active = makeCandidate({
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
    expect(
      activePhaseB.accepted.map((tx) => tx.txId.toString("hex")),
    ).toStrictEqual([active.txId.toString("hex")]);
    expect(activePhaseB.rejected).toHaveLength(0);
  });
});
