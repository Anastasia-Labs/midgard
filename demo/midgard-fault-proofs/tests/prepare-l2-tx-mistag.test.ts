import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { commitCountedRootProgram, ROOT_DOMAINS } from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { prepareL2TxMistagFromTransactions } from "../src/index.js";
import {
  deriveL2TransactionSourceCbor,
  type NodeTransactionPayload,
} from "../src/prepare-double-spend.js";

const payload = (
  validity: "TxIsValid" | "TxIsInvalid",
  fee: bigint,
): NodeTransactionPayload => {
  const tx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  return {
    nodeTxId: computeMidgardNativeTxId(tx).toString("hex"),
    txCbor: encodeMidgardNativeTxCanonical(tx).toString("hex"),
  };
};

/**
 * Independent commitment model for the block's `transactions_root`.
 *
 * The builder under test walks its own `decodeTransactionMaterial` /
 * `buildTrieView` / `transactionSourceTrieItem` chain, inserting leaves one at
 * a time into a `Store`-backed trie. This reference model instead feeds the
 * leaf set to the merkle-patricia-forestry library's own `Trie.fromList`
 * (a different construction path in a third-party package) and commits the
 * result with the SDK's normative counted-root program. Only the leaf encoding
 * itself -- key = native tx id, value = the exact L2 transaction-source value
 * -- is shared, because that encoding is the specification of the leaf rather
 * than of the root or of the openings being checked here.
 */
const referenceCommitment = async (
  transactions: readonly NodeTransactionPayload[],
) => {
  const leaves = transactions.map((transaction) => ({
    key: Buffer.from(transaction.nodeTxId, "hex"),
    value: Buffer.from(
      deriveL2TransactionSourceCbor(Buffer.from(transaction.txCbor, "hex")),
      "hex",
    ),
  }));
  const trie = await Trie.fromList(leaves);
  const phasRoot = Buffer.from(trie.hash as Uint8Array).toString("hex");
  const committedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot,
      count: BigInt(leaves.length),
    }),
  );
  return { trie, phasRoot, committedRoot };
};

const headerHash = "aa".repeat(28);

describe("prepare l2-tx-mistag", () => {
  it("selects the single committed code-1 leaf out of a mixed block", async () => {
    // Two honestly-accepted leaves surround the mistagged one, so "picked the
    // first leaf" and "echoed the only input" cannot pass.
    const honestBefore = payload("TxIsValid", 5n);
    const mistagged = payload("TxIsInvalid", 7n);
    const honestAfter = payload("TxIsValid", 9n);
    const transactions = [honestBefore, mistagged, honestAfter];
    const reference = await referenceCommitment(transactions);

    const prepared = await prepareL2TxMistagFromTransactions({
      headerHash,
      transactions,
      expectedTransactionsRoot: reference.committedRoot,
    });

    expect(prepared.headerHash).toBe(headerHash);
    expect(prepared.txCount).toBe(3);
    expect(prepared.tx.nodeTxId).toBe(mistagged.nodeTxId);
    expect(prepared.tx.committedValidityCode).toBe(1n);
    expect(prepared.tx.txInclusion.nativeTxId).toBe(mistagged.nodeTxId);
    expect(prepared.transactionsPhasRoot).toBe(reference.phasRoot);
    expect(prepared.committedTransactionsRoot).toBe(reference.committedRoot);
    // The carried opening must be the opening of the selected leaf under the
    // committed root, not merely some present proof.
    const referenceProof = await reference.trie.prove(
      Buffer.from(mistagged.nodeTxId, "hex"),
    );
    expect(prepared.tx.txInclusion.txMembershipProofCbor).toBe(
      referenceProof.toCBOR().toString("hex"),
    );
  });

  it("refuses a block whose leaves are all code 0", async () => {
    const honest = payload("TxIsValid", 5n);
    const reference = await referenceCommitment([honest]);
    await expect(
      prepareL2TxMistagFromTransactions({
        headerHash,
        transactions: [honest],
        expectedTransactionsRoot: reference.committedRoot,
      }),
    ).rejects.toThrow(/No code-1 normal transaction leaf/u);
  });

  it("refuses an explicitly requested code-0 leaf even when a code-1 leaf exists", async () => {
    const honest = payload("TxIsValid", 5n);
    const mistagged = payload("TxIsInvalid", 7n);
    const transactions = [honest, mistagged];
    const reference = await referenceCommitment(transactions);
    await expect(
      prepareL2TxMistagFromTransactions({
        headerHash,
        transactions,
        expectedTransactionsRoot: reference.committedRoot,
        txId: honest.nodeTxId,
      }),
    ).rejects.toThrow(
      new RegExp(
        `Requested --tx-id ${honest.nodeTxId} carries validity code 0`,
        "u",
      ),
    );
  });

  it("refuses evidence whose reconstructed root does not match the committed one", async () => {
    const mistagged = payload("TxIsInvalid", 7n);
    const reference = await referenceCommitment([mistagged]);
    const foreignRoot = await referenceCommitment([
      payload("TxIsInvalid", 11n),
    ]);
    expect(foreignRoot.committedRoot).not.toBe(reference.committedRoot);
    await expect(
      prepareL2TxMistagFromTransactions({
        headerHash,
        transactions: [mistagged],
        expectedTransactionsRoot: foreignRoot.committedRoot,
      }),
    ).rejects.toThrow(
      /Reconstructed counted transactions root .* does not match/u,
    );
  });
});
