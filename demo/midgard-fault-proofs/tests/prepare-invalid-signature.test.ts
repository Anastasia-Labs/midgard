import { mkdtemp, readFile, rm } from "node:fs/promises";
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
import {
  commitCountedRootProgram,
  invalidSignatureAddressWitnessesCommitment,
  invalidSignatureWitnessSetCommitment,
  type MidgardAddressWitness,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildTrieView,
  decodeTransactionMaterial,
  type NodeTransactionPayload,
  prepareInvalidSignatureFromTransactions,
  transactionSourceTrieItem,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const EMPTY_CBOR_LIST = encodeCbor([]);
const EMPTY_CBOR_NULL = encodeCbor(null);
const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);

const inputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const witnessCbor = (witness: MidgardAddressWitness): Buffer =>
  encodeCbor([
    Buffer.from(witness.verification_key, "hex"),
    Buffer.from(witness.signature, "hex"),
  ]);

const makeNativeTx = ({
  fee,
  addrTxWits,
}: {
  readonly fee: bigint;
  readonly addrTxWits: readonly MidgardAddressWitness[];
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([inputCbor(h32("11"), fee)]),
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
      addrTxWitsPreimageCbor: encodeCbor(addrTxWits.map(witnessCbor)),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

const payloadFromTx = (tx: MidgardNativeTxFull): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
});

const corrupt = (signature: string): string =>
  (signature.startsWith("00") ? "11" : "00") + signature.slice(2);

/**
 * Build a transaction whose address witnesses are genuinely signed over its own
 * transaction id, optionally corrupting some of them.
 *
 * The id is `blake2b_256(compact_body_cbor)` and the compact body does not cover
 * the witness set, so the body can be fixed first and the witnesses signed
 * afterwards without moving the id they commit to.
 */
const signedTx = ({
  fee,
  witnessCount,
  corruptIndices,
}: {
  readonly fee: bigint;
  readonly witnessCount: number;
  readonly corruptIndices: readonly number[];
}): {
  readonly payload: NodeTransactionPayload;
  readonly nodeTxId: string;
  readonly addrTxWits: readonly MidgardAddressWitness[];
} => {
  const placeholder: MidgardAddressWitness = {
    verification_key: h32("aa"),
    signature: "bb".repeat(64),
  };
  const nodeTxId = computeMidgardNativeTxId(
    makeNativeTx({
      fee,
      addrTxWits: Array.from({ length: witnessCount }, () => placeholder),
    }),
  ).toString("hex");

  const addrTxWits = Array.from({ length: witnessCount }, (_unused, index) => {
    const key = CML.PrivateKey.generate_ed25519();
    const signature = Buffer.from(
      key.sign(Buffer.from(nodeTxId, "hex")).to_raw_bytes(),
    ).toString("hex");
    return {
      verification_key: Buffer.from(key.to_public().to_raw_bytes()).toString(
        "hex",
      ),
      signature: corruptIndices.includes(index)
        ? corrupt(signature)
        : signature,
    };
  });

  const tx = makeNativeTx({ fee, addrTxWits });
  const payload = payloadFromTx(tx);
  // Signing must not have perturbed the id the signatures commit to.
  expect(payload.nodeTxId).toBe(nodeTxId);
  return { payload, nodeTxId, addrTxWits };
};

const transactionRoots = async (
  transactions: readonly NodeTransactionPayload[],
): Promise<{
  readonly transactionsPhasRoot: string;
  readonly committedTransactionsRoot: string;
}> => {
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  return { transactionsPhasRoot: trie.root, committedTransactionsRoot };
};

const withTempDir = async <A>(run: (dir: string) => Promise<A>): Promise<A> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-invalid-signature-"));
  try {
    return await run(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
};

describe("prepare-invalid-signature", () => {
  it("selects the transaction carrying a signature that does not verify", async () => {
    const valid = signedTx({ fee: 1n, witnessCount: 2, corruptIndices: [] });
    const bad = signedTx({ fee: 2n, witnessCount: 3, corruptIndices: [2] });
    const transactions = [valid.payload, bad.payload];
    const roots = await transactionRoots(transactions);

    const output = await prepareInvalidSignatureFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
    });

    expect(output.headerHash).toBe(h28("aa"));
    expect(output.txCount).toBe(2);
    expect(output.tx.nodeTxId).toBe(bad.payload.nodeTxId);
    expect(output.tx.badAddrTxWitIndex).toBe(2);
    expect(output.tx.badAddrTxWitVerificationKey).toBe(
      bad.addrTxWits[2]!.verification_key,
    );
    expect(output.tx.addrTxWitsPreimage).toEqual(bad.addrTxWits);
    expect(output.transactionsPhasRoot).toBe(roots.transactionsPhasRoot);
    expect(output.expectedTransactionsRoot.matches).toBe(true);
  });

  it("derives preimages that reproduce both committed openings", async () => {
    const bad = signedTx({ fee: 5n, witnessCount: 2, corruptIndices: [0] });
    const transactions = [bad.payload];
    const roots = await transactionRoots(transactions);

    const output = await prepareInvalidSignatureFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
    });

    // This is exactly the chain the two steps walk on-chain: the witness-set
    // compact opens `witness_set_hash`, then the address-witness list opens the
    // `addr_tx_wits_hash` that compact exposes.
    expect(
      invalidSignatureWitnessSetCommitment(output.tx.badTxWitnessSetCompact),
    ).toBe(output.tx.badTxWitnessSetHash);
    expect(output.tx.badTxWitnessSetHash).toBe(
      output.tx.nativeTx.witness_set_hash,
    );
    expect(
      invalidSignatureAddressWitnessesCommitment(output.tx.addrTxWitsPreimage),
    ).toBe(output.tx.badAddrTxWitsHash);
    expect(output.tx.badAddrTxWitsHash).toBe(
      output.tx.badTxWitnessSetCompact.addr_tx_wits_hash,
    );
    expect(output.tx.badAddrTxWitIndex).toBe(0);
  });

  it("reports the first failing witness when several are corrupt", async () => {
    const bad = signedTx({ fee: 3n, witnessCount: 4, corruptIndices: [1, 3] });
    const transactions = [bad.payload];
    const roots = await transactionRoots(transactions);

    const output = await prepareInvalidSignatureFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
    });

    expect(output.tx.badAddrTxWitIndex).toBe(1);
  });

  it("pins an explicit transaction with --tx-id", async () => {
    const first = signedTx({ fee: 1n, witnessCount: 1, corruptIndices: [0] });
    const second = signedTx({ fee: 2n, witnessCount: 2, corruptIndices: [1] });
    const transactions = [first.payload, second.payload];
    const roots = await transactionRoots(transactions);

    const output = await prepareInvalidSignatureFromTransactions({
      headerHash: h28("aa"),
      transactions,
      expectedTransactionsRoot: roots.committedTransactionsRoot,
      txId: second.payload.nodeTxId,
    });

    expect(output.tx.nodeTxId).toBe(second.payload.nodeTxId);
    expect(output.tx.badAddrTxWitIndex).toBe(1);
  });

  it("rejects a block whose signatures all verify", async () => {
    const transactions = [
      signedTx({ fee: 1n, witnessCount: 2, corruptIndices: [] }).payload,
      signedTx({ fee: 2n, witnessCount: 1, corruptIndices: [] }).payload,
    ];
    const roots = await transactionRoots(transactions);

    await expect(
      prepareInvalidSignatureFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
      }),
    ).rejects.toThrow(
      "No transaction with an invalid address-witness signature found",
    );
  });

  it("rejects a pinned transaction whose witnesses all verify", async () => {
    const good = signedTx({ fee: 1n, witnessCount: 2, corruptIndices: [] });
    const bad = signedTx({ fee: 2n, witnessCount: 1, corruptIndices: [0] });
    const transactions = [good.payload, bad.payload];
    const roots = await transactionRoots(transactions);

    await expect(
      prepareInvalidSignatureFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
        txId: good.payload.nodeTxId,
      }),
    ).rejects.toThrow("has a valid signature for every address witness");
  });

  it("rejects a --tx-id that is not in the block", async () => {
    const bad = signedTx({ fee: 1n, witnessCount: 1, corruptIndices: [0] });
    const transactions = [bad.payload];
    const roots = await transactionRoots(transactions);

    await expect(
      prepareInvalidSignatureFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
        txId: h32("de"),
      }),
    ).rejects.toThrow("was not found in the block");
  });

  it("aborts before writing files when the expected root does not match", async () => {
    const bad = signedTx({ fee: 1n, witnessCount: 1, corruptIndices: [0] });
    const transactions = [bad.payload];

    await withTempDir(async (dir) => {
      await expect(
        prepareInvalidSignatureFromTransactions({
          headerHash: h28("aa"),
          transactions,
          expectedTransactionsRoot: h32("de"),
          outputDir: dir,
        }),
      ).rejects.toThrow("does not match --expected-transactions-root");
      await expect(
        readFile(join(dir, "invalid-signature-tx-inclusion.json"), "utf8"),
      ).rejects.toThrow();
    });
  });

  it("writes the artifacts the two submit steps consume", async () => {
    const bad = signedTx({ fee: 4n, witnessCount: 2, corruptIndices: [1] });
    const transactions = [bad.payload];
    const roots = await transactionRoots(transactions);

    await withTempDir(async (dir) => {
      const output = await prepareInvalidSignatureFromTransactions({
        headerHash: h28("aa"),
        transactions,
        expectedTransactionsRoot: roots.committedTransactionsRoot,
        outputDir: dir,
      });

      expect(output.files?.txInclusionPath).toBe(
        join(dir, "invalid-signature-tx-inclusion.json"),
      );
      const txInclusion = JSON.parse(
        await readFile(output.files!.txInclusionPath, "utf8"),
      );
      expect(txInclusion.nativeTxId).toBe(bad.payload.nodeTxId);

      const witnessSet = JSON.parse(
        await readFile(output.files!.witnessSetCompactPath, "utf8"),
      );
      expect(witnessSet.addrTxWitsHash).toBe(
        output.tx.badTxWitnessSetCompact.addr_tx_wits_hash,
      );

      const addrTxWits = JSON.parse(
        await readFile(output.files!.addrTxWitsPreimagePath, "utf8"),
      );
      expect(addrTxWits).toEqual(
        bad.addrTxWits.map((witness) => ({
          verificationKey: witness.verification_key,
          signature: witness.signature,
        })),
      );

      const plan = JSON.parse(await readFile(output.files!.planPath, "utf8"));
      expect(plan.badAddrTxWitIndex).toBe(1);
      expect(plan.addrTxWitsCount).toBe(2);
      expect(plan.expectedTransactionsRoot.matches).toBe(true);
    });
  });
});
