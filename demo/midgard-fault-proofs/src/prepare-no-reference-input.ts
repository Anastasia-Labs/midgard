/**
 * `no-reference-input` evidence builder.
 *
 * **Re-derived-adjacent, and checked to be unaffected by #604.** This module
 * emits *evidence* — canonical item lists, compact structures and inclusion
 * proofs — and constructs no datum or redeemer, so the #575 rebind left its
 * output shape alone. What changed is downstream: the submitters that consume
 * this evidence now also take the disputed transaction's compact CBOR, which
 * this module already emits as part of its inclusion argument. The banner it
 * used to carry is gone because the family is re-derived, not because the
 * module was skipped.
 */

import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeMidgardNativeTxCompactV1,
  formatUnknownError,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  EMPTY_MERKLE_TREE_ROOT,
  type MidgardTxInput,
  type NativeTxCompact as NativeTxCompactData,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildMembershipProof,
  buildNonMembershipProof,
  computeTrieRoot,
  type TrieEntry,
} from "./ne-proofs.js";
import { ledgerKeyBytesHex } from "./ne-submit-step-03.js";
import {
  deriveL2TransactionSourceCborV1,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  readNodeTransactionPayloadsFile,
} from "./prepare-double-spend.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import { nativeTxFromCoreCompact } from "./submit-step-01.js";
import { keyValuePhasNonMembershipProof } from "./transition-trace/phas.js";
import { reconstructDaPayloadV1 } from "./transition-trace/reconstruct.js";

type LucidDataSchema = Parameters<typeof Data.to>[1];

/**
 * One reference input of the bad transaction, as committed by its native
 * `reference_inputs_hash`. Mirrors `NeInputPreimageEntry` on the spend side and
 * is consumed by `submit-no-reference-input-step-02.ts`, which re-commits the
 * whole list against the hash the on-chain step-02 datum carries.
 */
export type NoReferenceInputPreimageEntry = {
  readonly txId: string;
  readonly index: number | bigint;
};

export type PrepareNoReferenceInputCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly prevUtxosRoot?: string;
  readonly prevBlockPayloadPath?: string;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareNoReferenceInputFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly prevUtxosRoot?: string;
  readonly prevBlockPayloadPath?: string;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
};

/** step-01 material: the bad tx and its membership proof in the txs trie. */
export type PreparedNriTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  // Raw transactions MPF root the membership proof opens; authenticated on-chain
  // against the header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export type PreparedNoReferenceInputOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  /** Raw MPF root used by the transaction membership/non-membership proofs. */
  readonly transactionsRoot: string;
  /** Counted root committed in the state-queue block header. */
  readonly committedTransactionsRoot: string;
  readonly prevUtxosRoot: string;
  readonly badTxId: string;
  readonly badReferenceInputIndex: number;
  readonly missingReferenceInput: MidgardTxInput;
  readonly expectedTransactionsRoot?: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly txInclusion: PreparedNriTxInclusionJson;
  readonly referenceInputsPreimage: readonly NoReferenceInputPreimageEntry[];
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly referenceInputsPreimagePath: string;
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
  readonly referenceInputs: readonly MidgardTxInput[];
};

const decodeTx = (payload: NodeTransactionPayload): DecodedTx => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFullV1;
  try {
    nativeTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(txCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `Failed to decode native Midgard tx ${nodeTxId}: ${formatUnknownError(cause)}`,
    );
  }
  const computed = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  if (computed !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computed}.`,
    );
  }
  const referenceInputCbors = decodeMidgardNativeByteListPreimage(
    nativeTx.body.referenceInputsPreimageCbor,
    `tx ${nodeTxId} reference_inputs`,
  ).map((bytes) => Buffer.from(bytes).toString("hex"));
  return {
    nodeTxId,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    nativeCompactCbor: encodeMidgardNativeTxCompactV1(
      nativeTx.compact,
    ).toString("hex"),
    l2TransactionSourceCbor: deriveL2TransactionSourceCborV1(
      Buffer.from(txCbor, "hex"),
    ),
    referenceInputs: spendInputsWitnessFromCbors(
      referenceInputCbors,
      "reference_inputs",
    ).inputs,
  };
};

const parseBadReferenceInputIndex = (
  value: string | number | undefined,
): number => {
  if (value === undefined) {
    return 0;
  }
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(
      `--bad-reference-input-index must be a non-negative integer, got "${String(value)}".`,
    );
  }
  return parsed;
};

/**
 * Ledger non-membership proof CBOR for the missing reference input against the
 * block's `prevUtxosRoot`.
 *
 * - Empty genesis ledger: built locally against an empty trie.
 * - Non-empty ledger: reconstructed from the **previous** block's DA payload,
 *   whose `utxos_root` is exactly this block's `prev_utxos_root`. We verify the
 *   reconstruction opens `prevUtxosRoot` before proving exclusion.
 *
 * The `utxos_root` is a raw PHAS/MPF root (unlike the counted `transactions_root`),
 * so this exclusion proof verifies directly against `prev_utxos_root` on-chain via
 * `plutarch_pexcludes_raw`.
 */
const resolveLedgerNonMembershipProof = async ({
  prevUtxosRoot,
  missingReferenceInput,
  prevBlockPayloadEnvelopeCbor,
}: {
  readonly prevUtxosRoot: string;
  readonly missingReferenceInput: MidgardTxInput;
  readonly prevBlockPayloadEnvelopeCbor?: Uint8Array;
}): Promise<string> => {
  const ledgerKey = Buffer.from(
    ledgerKeyBytesHex(missingReferenceInput),
    "hex",
  );
  if (prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT) {
    return await buildNonMembershipProof([], ledgerKey);
  }
  if (prevBlockPayloadEnvelopeCbor === undefined) {
    throw new Error(
      `The block sits over a non-empty prev-utxos ledger (root ${prevUtxosRoot}). ` +
        "Supply the previous block's DA payload via --prev-block-payload-file so " +
        "the ledger trie at that root can be reconstructed and the non-membership " +
        "proof built. (Only the empty-genesis first block needs no payload.)",
    );
  }
  const reconstruction = await reconstructDaPayloadV1({
    payloadEnvelopeCbor: prevBlockPayloadEnvelopeCbor,
  });
  if (reconstruction.roots.utxosRoot !== prevUtxosRoot) {
    throw new Error(
      `--prev-block-payload-file reconstructs utxos_root ${reconstruction.roots.utxosRoot}, ` +
        `which does not match --prev-utxos-root ${prevUtxosRoot}. The supplied payload is ` +
        "not the block immediately preceding the fraudulent one.",
    );
  }
  // Throws if the reference input is actually present in the ledger (then it is
  // not a non-existent reference input) and verifies the proof opens `prevUtxosRoot`.
  const proof = await keyValuePhasNonMembershipProof(
    reconstruction.rootData.utxos,
    ledgerKey,
  );
  return Data.to(proof, Proof as unknown as LucidDataSchema);
};

/**
 * Builds the four no-reference-input submit-step artifacts from a block the node
 * actually committed. The transactions trie is reconstructed with the node's
 * exact canonical `Data(L2TransactionSourceV1)` values keyed by raw tx id, so
 * its root matches the committed `transactions_root` by construction.
 */
export const prepareNoReferenceInputFromTransactions = async ({
  headerHash,
  transactions,
  badTxId,
  badReferenceInputIndex,
  prevUtxosRoot = EMPTY_MERKLE_TREE_ROOT,
  prevBlockPayloadEnvelopeCbor,
  expectedTransactionsRoot,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly prevUtxosRoot?: string;
  readonly prevBlockPayloadEnvelopeCbor?: Uint8Array;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
}): Promise<PreparedNoReferenceInputOutput> => {
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

  const resolvedBadReferenceInputIndex = parseBadReferenceInputIndex(
    badReferenceInputIndex,
  );
  if (resolvedBadReferenceInputIndex >= bad.referenceInputs.length) {
    throw new Error(
      `--bad-reference-input-index ${resolvedBadReferenceInputIndex.toString()} is out of bounds for ${bad.referenceInputs.length.toString()} reference inputs.`,
    );
  }
  const missingReferenceInput =
    bad.referenceInputs[resolvedBadReferenceInputIndex]!;

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
  // step-04 txs non-membership: the missing reference input's producing tx id is
  // absent from the block's transactions trie (keyed by the raw 32-byte tx id).
  const txsNonMembershipProofCbor = await buildNonMembershipProof(
    txsEntries,
    Buffer.from(missingReferenceInput.tx_id, "hex"),
  );

  // step-03 ledger non-membership: the missing reference input is absent from
  // the block's prev-utxos ledger trie (keyed by the Cardano `TransactionInput`
  // CBOR).
  const ledgerNonMembershipProofCbor = await resolveLedgerNonMembershipProof({
    prevUtxosRoot: normalizedPrevUtxosRoot,
    missingReferenceInput,
    prevBlockPayloadEnvelopeCbor,
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

  const referenceInputsPreimage: readonly NoReferenceInputPreimageEntry[] =
    bad.referenceInputs.map((input) => ({
      txId: input.tx_id,
      index: input.output_index,
    }));

  const base: PreparedNoReferenceInputOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsRoot,
    committedTransactionsRoot,
    prevUtxosRoot: normalizedPrevUtxosRoot,
    badTxId: bad.nodeTxId,
    badReferenceInputIndex: resolvedBadReferenceInputIndex,
    missingReferenceInput,
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
    referenceInputsPreimage,
    ledgerNonMembershipProofCbor,
    txsNonMembershipProofCbor,
  };
  if (outputDir === undefined) {
    return base;
  }
  await mkdir(outputDir, { recursive: true });
  const files = {
    txInclusionPath: join(outputDir, "nri-tx-inclusion.json"),
    referenceInputsPreimagePath: join(
      outputDir,
      "nri-reference-inputs-preimage.json",
    ),
    ledgerNonMembershipPath: join(outputDir, "nri-ledger-non-membership.json"),
    txsNonMembershipPath: join(outputDir, "nri-txs-non-membership.json"),
    planPath: join(outputDir, "nri-plan.json"),
  };
  await Promise.all([
    writeFile(files.txInclusionPath, stringifyJson(base.txInclusion)),
    writeFile(
      files.referenceInputsPreimagePath,
      stringifyJson(base.referenceInputsPreimage),
    ),
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
        badReferenceInputIndex: base.badReferenceInputIndex,
        missingReferenceInput: base.missingReferenceInput,
        transactionsRoot: base.transactionsRoot,
        committedTransactionsRoot: base.committedTransactionsRoot,
        prevUtxosRoot: base.prevUtxosRoot,
        expectedTransactionsRoot: base.expectedTransactionsRoot,
      }),
    ),
  ]);
  return { ...base, files };
};

/**
 * Reads a previous-block DA payload file: the `DaPayloadEnvelopeV1` canonical CBOR as a
 * hex string (whitespace tolerated). Returns `undefined` when no path is given.
 */
const readPrevBlockPayloadEnvelopeCbor = async (
  path: string | undefined,
): Promise<Uint8Array | undefined> => {
  if (path === undefined) {
    return undefined;
  }
  const raw = (await readFile(path, "utf8")).trim();
  return Buffer.from(parseHex(raw, "--prev-block-payload-file"), "hex");
};

export const prepareNoReferenceInputFromNode = async (
  config: PrepareNoReferenceInputCliConfig,
): Promise<PreparedNoReferenceInputOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const [transactions, prevBlockPayloadEnvelopeCbor] = await Promise.all([
    fetchNodeBlockTransactions({
      midgardNodeUrl: config.midgardNodeUrl,
      headerHash,
      fetchImpl: config.fetchImpl,
    }),
    readPrevBlockPayloadEnvelopeCbor(config.prevBlockPayloadPath),
  ]);
  return await prepareNoReferenceInputFromTransactions({
    headerHash,
    transactions,
    badTxId: config.badTxId,
    badReferenceInputIndex: config.badReferenceInputIndex,
    prevUtxosRoot: config.prevUtxosRoot,
    prevBlockPayloadEnvelopeCbor,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    outputDir: config.outputDir,
  });
};

export const prepareNoReferenceInputFromFile = async (
  config: PrepareNoReferenceInputFromFileConfig,
): Promise<PreparedNoReferenceInputOutput> => {
  const [transactions, prevBlockPayloadEnvelopeCbor] = await Promise.all([
    readNodeTransactionPayloadsFile(config.transactionsPath),
    readPrevBlockPayloadEnvelopeCbor(config.prevBlockPayloadPath),
  ]);
  return await prepareNoReferenceInputFromTransactions({
    headerHash: config.headerHash,
    transactions,
    badTxId: config.badTxId,
    badReferenceInputIndex: config.badReferenceInputIndex,
    prevUtxosRoot: config.prevUtxosRoot,
    prevBlockPayloadEnvelopeCbor,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    outputDir: config.outputDir,
  });
};
