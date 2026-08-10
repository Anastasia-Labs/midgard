import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  EMPTY_SPEND_INPUTS_HASH,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildTrieView,
  decodeTransactionMaterial,
  nativeTrieItem,
  type NodeTransactionPayload,
  prepareZeroInputFromTransactions,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const makeNativeTx = ({
  spendInputCbors,
  fee,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
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

const payloadFromTx = (tx: MidgardNativeTxFullV1): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdV1(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
});

const spendingTx = (fee: bigint): NodeTransactionPayload =>
  payloadFromTx(
    makeNativeTx({ spendInputCbors: [inputCbor(h32("11"), fee)], fee }),
  );

const zeroInputTx = (fee: bigint): NodeTransactionPayload =>
  payloadFromTx(makeNativeTx({ spendInputCbors: [], fee }));

const transactionRoots = async (
  transactions: readonly NodeTransactionPayload[],
): Promise<{
  readonly transactionsPhasRoot: string;
  readonly committedTransactionsRoot: string;
}> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(nativeTrieItem));
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  return {
    transactionsPhasRoot: trie.root,
    committedTransactionsRoot,
  };
};

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-zero-input-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-zero-input", () => {
  it("selects the transaction that spends no inputs", async () => {
    const validTx = spendingTx(1n);
    const badTx = zeroInputTx(2n);
    const transactions = [validTx, badTx];
    const roots = await transactionRoots(transactions);

    const output = await prepareZeroInputFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(2);
    expect(output.tx.nodeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.spendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
    expect(output.tx.nativeTx.body.spend_inputs_hash).toBe(
      EMPTY_SPEND_INPUTS_HASH,
    );
    expect(output.tx.txInclusion.nativeTxId).toBe(badTx.nodeTxId);
    expect(output.tx.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(
      0,
    );
    const expectedCommittedRoot = await Effect.runPromise(
      commitCountedRootProgram({
        domain: ROOT_DOMAINS.transactionsV1,
        phasRoot: output.transactionsPhasRoot,
        count: BigInt(output.txCount),
      }),
    );
    expect(output.committedTransactionsRoot).toBe(expectedCommittedRoot);
    expect(output.transactionsPhasRoot).not.toBe(
      output.committedTransactionsRoot,
    );
    expect(output.expectedTransactionsRoot).toEqual({
      value: roots.committedTransactionsRoot,
      matches: true,
    });
  });

  it("accepts the counted transactions root committed by the block header", async () => {
    const transactions = [spendingTx(1n), zeroInputTx(2n)];
    const roots = await transactionRoots(transactions);

    const verified = await prepareZeroInputFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
    });

    expect(verified.expectedTransactionsRoot).toEqual({
      value: roots.committedTransactionsRoot,
      matches: true,
    });
    expect(verified.transactionsPhasRoot).toBe(roots.transactionsPhasRoot);
    expect(verified.transactionsPhasRoot).not.toBe(
      verified.committedTransactionsRoot,
    );
  });

  it("rejects the raw PHAS root where the counted header root is required", async () => {
    const transactions = [spendingTx(1n), zeroInputTx(2n)];
    const roots = await transactionRoots(transactions);

    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.transactionsPhasRoot,
      }),
    ).rejects.toThrow("does not match --expected-transactions-root");
  });

  it("rejects a transactions root that is not committed by the block header", async () => {
    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions: [spendingTx(1n), zeroInputTx(2n)],
        expectedTransactionsRoot: h32("ff"),
      }),
    ).rejects.toThrow("The prepared proof would not verify against this block");
  });

  it("throws when every transaction in the block spends at least one input", async () => {
    const transactions = [spendingTx(1n), spendingTx(2n)];
    const roots = await transactionRoots(transactions);
    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
      }),
    ).rejects.toThrow("No zero-input transaction found in the selected block.");
  });

  it("rejects an explicitly requested tx that does spend inputs", async () => {
    const validTx = spendingTx(1n);
    const transactions = [validTx, zeroInputTx(2n)];
    const roots = await transactionRoots(transactions);
    await expect(
      prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
        txId: validTx.nodeTxId,
      }),
    ).rejects.toThrow("spends at least one input");
  });

  it("writes tx-inclusion and plan artifacts the submit steps consume", async () => {
    const badTx = zeroInputTx(2n);
    const transactions = [spendingTx(1n), badTx];
    const roots = await transactionRoots(transactions);
    await withTempDir(async (dir) => {
      const output = await prepareZeroInputFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
        outputDir: dir,
      });

      expect(output.files?.txInclusionPath).toBe(
        join(dir, "tx-inclusion.json"),
      );
      const txInclusion = JSON.parse(
        await readFile(output.files!.txInclusionPath, "utf8"),
      ) as { readonly nativeTxId: string };
      expect(txInclusion.nativeTxId).toBe(badTx.nodeTxId);

      const plan = JSON.parse(
        await readFile(output.files!.planPath, "utf8"),
      ) as {
        readonly spendInputsHash: string;
        readonly transactionsPhasRoot: string;
        readonly committedTransactionsRoot: string;
      };
      expect(plan.spendInputsHash).toBe(EMPTY_SPEND_INPUTS_HASH);
      expect(plan.transactionsPhasRoot).toBe(output.transactionsPhasRoot);
      expect(plan.committedTransactionsRoot).toBe(
        output.committedTransactionsRoot,
      );
    });
  });
});
