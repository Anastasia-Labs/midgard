/**
 * `non-existent-input` evidence builder.
 *
 * This module emits *evidence* — canonical item lists, compact structures and
 * inclusion proofs — and constructs no datum or redeemer. The submitters that
 * consume it also take the disputed transaction's compact CBOR, which this
 * module emits as part of its inclusion argument.
 */

import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
  formatUnknownError,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  EMPTY_MERKLE_TREE_ROOT,
  type MidgardTxInput,
  type NativeTxCompact as NativeTxCompactData,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import type { Ledger } from "./ledger-store/index.js";
import {
  buildMembershipProof,
  buildNonMembershipProof,
  computeTrieRoot,
  type TrieEntry,
} from "./ne-proofs.js";
export type NeInputPreimageEntry = {
  readonly txId: string;
  readonly index: bigint;
};
import { predecessorNonMembershipProof } from "./predecessor-non-membership.js";
import {
  deriveL2TransactionSourceCbor,
  type NodeTransactionPayload,
} from "./prepare-double-spend.encode-l2-transaction-source-value.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import { ledgerKeyBytesHex, nativeTxFromCoreCompact } from "./step-support.js";

/** step-01 material: the bad tx and its membership proof in the txs trie. */
export type PreparedNeTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  // Raw transactions MPF root the membership proof opens; authenticated on-chain
  // against the header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export type PreparedNonExistentInputOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root used by the transaction membership/non-membership proofs. */
  readonly transactionsRoot: string;
  /** Counted root committed in the state-queue block header. */
  readonly committedTransactionsRoot: string;
  readonly prevUtxosRoot: string;
  readonly badTxId: string;
  readonly badInputIndex: number;
  readonly missingInput: MidgardTxInput;
  readonly expectedTransactionsRoot?: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly txInclusion: PreparedNeTxInclusionJson;
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly inputsPreimagePath: string;
    readonly ledgerNonMembershipPath: string;
    readonly txsNonMembershipPath: string;
    readonly planPath: string;
  };
};

type DecodedTx = {
  readonly nodeTxId: string;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly nativeCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly inputs: readonly MidgardTxInput[];
};

const decodeTx = (payload: NodeTransactionPayload): DecodedTx => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFull;
  try {
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(txCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${nodeTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const computed = computeMidgardNativeTxId(nativeTx).toString("hex");
  if (computed !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computed}.`,
    );
  }
  const spendInputCbors = decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    `tx ${nodeTxId} spend_inputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  return {
    nodeTxId,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    nativeCompactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    ),
    l2TransactionSourceCbor: deriveL2TransactionSourceCbor(
      Buffer.from(txCbor, "hex"),
    ),
    inputs: spendInputsWitnessFromCbors(spendInputCbors, "spend_inputs").inputs,
  };
};

const parseBadInputIndex = (value: string | number | undefined): number => {
  if (value === undefined) {
    return 0;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(
      `--bad-input-index must be a non-negative integer, got "${String(value)}".`,
    );
  }
  return parsed;
};

/**
 * Builds the four non-existent-input submit-step artifacts from a block the node
 * actually committed. The transactions trie is reconstructed with the node's
 * exact canonical `Data(L2TransactionSourceV1)` values keyed by raw tx id, so
 * its root matches the committed `transactions_root` by construction.
 */
export const prepareNonExistentInputFromTransactions = async ({
  headerHash,
  transactions,
  badTxId,
  badInputIndex,
  prevUtxosRoot = EMPTY_MERKLE_TREE_ROOT,
  prevLedger,
  expectedTransactionsRoot,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly badTxId?: string;
  readonly badInputIndex?: string | number;
  /** The challenged header's `prev_utxos_root`. */
  readonly prevUtxosRoot?: string;
  /** The ledger the block starts from; omitted over the empty genesis ledger. */
  readonly prevLedger?: Ledger;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
}): Promise<PreparedNonExistentInputOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedPrevUtxosRoot = parseHex(
    prevUtxosRoot,
    "--prev-utxos-root",
    32,
  );
  const decoded = transactions.map(decodeTx);
  if (decoded.length === 0) {
    throw new Error("The selected block contains no transactions.");
  }

  const bad =
    badTxId === undefined
      ? decoded.length === 1
        ? decoded[0]!
        : (() => {
            throw new Error(
              "The block has multiple transactions; specify --bad-tx-id.",
            );
          })()
      : (() => {
          const normalized = parseHex(badTxId, "--bad-tx-id", 32);
          const found = decoded.find((tx) => tx.nodeTxId === normalized);
          if (found === undefined) {
            throw new Error(
              `--bad-tx-id ${normalized} not found in the block.`,
            );
          }
          return found;
        })();

  const resolvedBadInputIndex = parseBadInputIndex(badInputIndex);
  if (resolvedBadInputIndex >= bad.inputs.length) {
    throw new Error(
      `--bad-input-index ${resolvedBadInputIndex.toString()} is out of bounds for ${bad.inputs.length.toString()} inputs.`,
    );
  }
  const missingInput = bad.inputs[resolvedBadInputIndex]!;

  // --- Transactions trie (exact source values, matches the node) ------------
  const txsEntries: TrieEntry[] = decoded.map((tx) => ({
    key: Buffer.from(tx.nodeTxId, "hex"),
    value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
  }));
  const transactionsRoot = await computeTrieRoot(txsEntries);
  const txMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(bad.nodeTxId, "hex"),
  );
  // step-04 txs non-membership: the missing input's producing tx id is absent
  // from the block's transactions trie (keyed by the raw 32-byte tx id).
  const txsNonMembershipProofCbor = await buildNonMembershipProof(
    txsEntries,
    Buffer.from(missingInput.tx_id, "hex"),
  );

  // step-03 ledger non-membership: the missing input is absent from the block's
  // prev-utxos ledger trie (keyed by the Cardano `TransactionInput` CBOR).
  const ledgerNonMembershipProofCbor = await predecessorNonMembershipProof({
    ledger: prevLedger,
    utxosRoot: normalizedPrevUtxosRoot,
    key: Buffer.from(ledgerKeyBytesHex(missingInput), "hex"),
    label: "non-existent-input predecessor ledger",
  });

  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: transactionsRoot,
      count: BigInt(decoded.length),
    }),
  );

  const expectedCheck =
    expectedTransactionsRoot === undefined
      ? undefined
      : (() => {
          const value = parseHex(
            expectedTransactionsRoot,
            "--expected-transactions-root",
            32,
          );
          return { value, matches: value === committedTransactionsRoot };
        })();
  if (expectedCheck !== undefined && !expectedCheck.matches) {
    throw new Error(
      `Reconstructed raw transactions root ${transactionsRoot} commits to counted root ${committedTransactionsRoot}, which does not match --expected-transactions-root ${expectedCheck.value}. The prepared proofs would not verify against this block.`,
    );
  }

  const inputsPreimage: readonly NeInputPreimageEntry[] = bad.inputs.map(
    (input) => ({ txId: input.tx_id, index: input.output_index }),
  );

  const base: PreparedNonExistentInputOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsRoot,
    committedTransactionsRoot,
    prevUtxosRoot: normalizedPrevUtxosRoot,
    badTxId: bad.nodeTxId,
    badInputIndex: resolvedBadInputIndex,
    missingInput,
    ...(expectedCheck === undefined
      ? {}
      : { expectedTransactionsRoot: expectedCheck }),
    txInclusion: {
      nativeTxId: bad.nodeTxId,
      nativeTx: bad.nativeTxCompact,
      nativeTxCompactCbor: bad.nativeCompactCbor,
      l2TransactionSourceCbor: bad.l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor,
    },
    inputsPreimage,
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
  };
  if (outputDir === undefined) {
    return base;
  }
  await mkdir(outputDir, { recursive: true });
  const files = {
    txInclusionPath: join(outputDir, "ne-tx-inclusion.json"),
    inputsPreimagePath: join(outputDir, "ne-inputs-preimage.json"),
    ledgerNonMembershipPath: join(outputDir, "ne-ledger-non-membership.json"),
    txsNonMembershipPath: join(outputDir, "ne-txs-non-membership.json"),
    planPath: join(outputDir, "ne-plan.json"),
  };
  await Promise.all([
    writeFile(files.txInclusionPath, stringifyJson(base.txInclusion)),
    writeFile(files.inputsPreimagePath, stringifyJson(base.inputsPreimage)),
    writeFile(
      files.ledgerNonMembershipPath,
      stringifyJson(base.ledgerNonMembershipProofCbor),
    ),
    writeFile(
      files.txsNonMembershipPath,
      stringifyJson(base.txsNonMembershipProofCbor),
    ),
    writeFile(
      files.planPath,
      stringifyJson({
        headerHash: base.headerHash,
        badTxId: base.badTxId,
        badInputIndex: base.badInputIndex,
        missingInput: base.missingInput,
        transactionsRoot: base.transactionsRoot,
        committedTransactionsRoot: base.committedTransactionsRoot,
        prevUtxosRoot: base.prevUtxosRoot,
        expectedTransactionsRoot: base.expectedTransactionsRoot,
      }),
    ),
  ]);
  return { ...base, files };
};
