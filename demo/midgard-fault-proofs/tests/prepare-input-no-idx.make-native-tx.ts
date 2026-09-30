import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { Effect } from "effect";

import {
  buildTrieView,
  decodeTransactionMaterial,
  type NodeTransactionPayload,
  transactionSourceTrieItem,
} from "../src/prepare-double-spend.js";
import { InputNoIdxRejection } from "../src/prepare-input-no-idx.js";

const EMPTY_CBOR_LIST = encodeCbor([]);

export const EMPTY_NULL_ROOT = computeHash32(encodeCbor(null));

export const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

/**
 * One canonical native output: an enterprise (no stake credential) pubkey
 * address on network 0 holding only lovelace, exactly as
 * `encode_midgard_tx_output` frames it.
 */
const nativeOutputCbor = (paymentByte: number, lovelace: bigint): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, paymentByte),
    Buffer.from([0x01, 0x82]),
    encodeCbor(lovelace),
    Buffer.from([0xa0]),
  ]);

const makeNativeTx = ({
  spendInputCbors,
  outputCbors,
  fee,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly outputCbors: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([...spendInputCbors]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([...outputCbors]),
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const payloadFromTx = (tx: MidgardNativeTxFull): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
});

/** A producer committing `outputCount` canonical outputs. */
export const producerTx = (
  outputCount: number,
  fee: bigint,
): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({
      spendInputCbors: [inputCbor(h32(0x99), fee)],
      outputCbors: Array.from({ length: outputCount }, (_, index) =>
        nativeOutputCbor(0x40 + index, 5_000_000n + BigInt(index)),
      ),
      fee,
    }),
  );

/** A transaction spending `(producerTxId, outputIndex)`. */
export const spenderTx = (
  producerTxId: string,
  outputIndex: bigint,
  fee: bigint,
  spendInputCount = 1,
): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({
      spendInputCbors: Array.from({ length: spendInputCount }, (_, index) =>
        index === spendInputCount - 1
          ? inputCbor(producerTxId, outputIndex)
          : inputCbor(index.toString(16).padStart(64, "0"), BigInt(index)),
      ),
      outputCbors: [nativeOutputCbor(0x11, 1_000_000n)],
      fee,
    }),
  );

export const committedTransactionsRoot = async (
  transactions: readonly NodeTransactionPayload[],
): Promise<string> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  return await Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain: SDK.ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
};

/** Producer with one output; spender challenging index 7 (out of range). */
export const violatingBlock = async (
  spendInputCount = 1,
): Promise<{
  readonly transactions: readonly NodeTransactionPayload[];
  readonly producer: NodeTransactionPayload;
  readonly spender: NodeTransactionPayload;
  readonly expectedTransactionsRoot: string;
}> => {
  const producer = producerTx(1, 1n);
  const spender = spenderTx(producer.nodeTxId, 7n, 2n, spendInputCount);
  const transactions = [producer, spender];
  return {
    transactions,
    producer,
    spender,
    expectedTransactionsRoot: await committedTransactionsRoot(transactions),
  };
};

export const rejectionCode = async (
  run: () => Promise<unknown>,
): Promise<string> => {
  try {
    await run();
  } catch (error) {
    if (
      error instanceof InputNoIdxRejection ||
      error instanceof SDK.CanonicalEvidenceRejection
    ) {
      return error.code;
    }
    return `unexpected:${error instanceof Error ? error.message : String(error)}`;
  }
  return "no_rejection";
};

export const withTempDir = async <A>(
  run: (dir: string) => Promise<A>,
): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-input-no-idx-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};
