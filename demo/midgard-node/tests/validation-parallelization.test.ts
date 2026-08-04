import {
  computeHash32,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
} from "@al-ft/midgard-core/codec";
import {
  buildPhaseAValidatedTx,
  decodeMidgardSubmittedTxFromCanonicalCbor,
  PhaseAValidatedTx,
  QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { encode } from "cborg";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  hasUnseenTxQueueWake,
  sampleValidationQueueWaits,
} from "@/fibers/tx-queue-processor.js";
import { resolveValidationWorkerPoolSize } from "@/services/config.js";
import { packPhaseAJob } from "@/workers/utils/validation-pool.js";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

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
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(lovelace),
    ).to_cbor_bytes(),
  );

const testPrivateKey = CML.PrivateKey.generate_ed25519();
const testPublicKeyHash = testPrivateKey.to_public().hash();
const testAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(testPublicKeyHash),
)
  .to_address()
  .to_bech32();

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

describe("validation queue-wait metric sampling", () => {
  it("keeps a bounded uniform sample with endpoints and the exact max", () => {
    const waits = Array.from({ length: 2_048 }, (_, index) => index);
    waits[123] = 99_999;
    const sampled = sampleValidationQueueWaits(waits);
    expect(sampled).toHaveLength(64);
    expect(sampled).toContain(waits[0]);
    expect(sampled).toContain(waits.at(-1));
    expect(sampled).toContain(99_999);
    expect(sampleValidationQueueWaits([3, 1, 2])).toEqual([3, 1, 2]);
  });
});

const encodeByteList = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(encode(items.map((item) => Buffer.from(item))));

const makeNativeTx = ({
  spent,
  referenceInputs = [],
  outputs,
  validityIntervalStart,
  validityIntervalEnd,
}: {
  readonly spent: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly outputs: readonly Buffer[];
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFull => {
  const spendInputsPreimageCbor = encodeByteList(spent);
  const referenceInputsPreimageCbor = encodeByteList(referenceInputs);
  const outputsPreimageCbor = encodeByteList(outputs);
  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: validityIntervalStart ?? MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: validityIntervalEnd ?? MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: 0n,
  };
  const bodyHash = computeHash32(
    encodeMidgardNativeTxBodyCompact(deriveMidgardNativeTxBodyCompact(body)),
  );
  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor: encodeByteList([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(bodyHash),
          testPrivateKey,
        ).to_cbor_bytes(),
      ),
    ]),
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  return {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
  };
};

const makeCandidate = ({
  arrivalSeq,
  spent,
  referenceInputs = [],
  inputLovelace = 10n,
  validityIntervalStart,
  validityIntervalEnd,
}: {
  readonly arrivalSeq: bigint;
  readonly spent: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
  readonly inputLovelace?: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): PhaseAValidatedTx => {
  const output = makeOutput(testAddress, inputLovelace);
  const tx = makeNativeTx({
    spent,
    referenceInputs,
    outputs: [output],
    validityIntervalStart,
    validityIntervalEnd,
  });
  const txCbor = encodeMidgardNativeTxCanonical(tx);
  const submittedTx = decodeMidgardSubmittedTxFromCanonicalCbor(txCbor);
  return buildPhaseAValidatedTx({
    ledgerTx: submittedTx.ledgerTx,
    txCbor: submittedTx.txCbor,
    arrivalSeq,
    createdAt: new Date(0),
    redeemerWitnessHash: submittedTx.commitments.redeemerWitnessHash,
  });
};

describe("validation parallelization", () => {
  it("distinguishes auto pool sizing from the explicit inline rollback", () => {
    expect(resolveValidationWorkerPoolSize(undefined, 8)).toBe(6);
    expect(resolveValidationWorkerPoolSize(undefined, 1)).toBe(1);
    expect(resolveValidationWorkerPoolSize(0, 8)).toBe(0);
    expect(resolveValidationWorkerPoolSize(3, 8)).toBe(3);
    expect(() => resolveValidationWorkerPoolSize(-1, 8)).toThrow(
      "non-negative safe integer",
    );
  });

  it("packs an isolated transferable arena and rejects corrupt tx ids", () => {
    const tx = {
      txId: Buffer.alloc(32, 0x11),
      txCbor: Buffer.from("010203", "hex"),
      arrivalSeq: 9n,
      createdAt: new Date(1234),
    };
    const request = packPhaseAJob(7, [tx]);
    expect(request.txs[0]).toStrictEqual({
      txIdOffset: 0,
      cborOffset: 32,
      cborLength: 3,
      arrivalSeq: 9n,
      createdAtMs: 1234,
    });
    expect(Buffer.from(request.arena, 0, 32)).toStrictEqual(tx.txId);
    expect(Buffer.from(request.arena, 32, 3)).toStrictEqual(tx.txCbor);
    expect(() => packPhaseAJob(8, [{ ...tx, txId: Buffer.alloc(31) }])).toThrow(
      "must be 32 bytes",
    );
  });

  it("cannot lose a coalesced wake while two drain loops finalize", () => {
    const loopAHandled = 41n;
    const loopBHandled = 41n;
    const generationAfterWake = 42n;
    expect(hasUnseenTxQueueWake(loopAHandled, generationAfterWake)).toBe(true);
    expect(hasUnseenTxQueueWake(loopBHandled, generationAfterWake)).toBe(true);
    // Whichever finalizer reserves the replacement loop handles generation 42;
    // the other may coalesce without stranding the durable row.
    expect(hasUnseenTxQueueWake(generationAfterWake, generationAfterWake)).toBe(
      false,
    );
  });
  it("keeps phase-A verdicts and order stable across concurrency levels", async () => {
    const queued: QueuedTx[] = Array.from({ length: 64 }, (_, index) => {
      const spent = outRefFromHash(hex32(index + 1), 0n);
      const nativeTx = makeNativeTx({
        spent: [spent],
        outputs: [makeOutput(testAddress, 10n)],
      });
      const nativeTxBytes = encodeMidgardNativeTxCanonical(nativeTx);
      return {
        txId: computeMidgardNativeTxId(nativeTx),
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
      parallel.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual(
      serial.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    );
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

    const { accepted, rejected } = await Effect.runPromise(
      runPhaseBValidationWithPatch([txA, txB, txC], preState, {
        nowCardanoSlotNo: 0n,
        bucketConcurrency: 8,
      }),
    );

    expect(
      accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([
      txA.ledgerTx.txId.toString("hex"),
      txC.ledgerTx.txId.toString("hex"),
    ]);
    expect(rejected).toHaveLength(1);
    expect(rejected[0].txId.toString("hex")).toBe(
      txB.ledgerTx.txId.toString("hex"),
    );
    expect([RejectCodes.DoubleSpend, RejectCodes.InputNotFound]).toContain(
      rejected[0].code,
    );
  });

  it("evaluates validity intervals against the current Cardano slot number", async () => {
    const spent = outRefFromHash(hex32(0x03), 0n);
    const preState = new Map<string, Buffer>([
      [spent.toString("hex"), makeOutput(testAddress, 10n)],
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

    const { accepted: expiredAccepted, rejected: expiredRejected } =
      await Effect.runPromise(
        runPhaseBValidationWithPatch([expired], preState, {
          nowCardanoSlotNo: 120n,
          bucketConcurrency: 1,
        }),
      );
    expect(expiredAccepted).toHaveLength(0);
    expect(expiredRejected).toHaveLength(1);
    expect(expiredRejected[0].code).toBe(RejectCodes.ValidityIntervalMismatch);
    expect(expiredRejected[0].detail).toBe("120 > 119");

    const { accepted: activeAccepted, rejected: activeRejected } =
      await Effect.runPromise(
        runPhaseBValidationWithPatch([active], preState, {
          nowCardanoSlotNo: 120n,
          bucketConcurrency: 1,
        }),
      );
    expect(
      activeAccepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([active.ledgerTx.txId.toString("hex")]);
    expect(activeRejected).toHaveLength(0);
  });
});
