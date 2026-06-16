import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
  formatUnknownError,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  type MidgardTxInput,
  type NativeTxCompact as NativeTxCompactData,
} from "@al-ft/midgard-sdk";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
} from "./prepare-double-spend.js";
import { spendInputsWitnessFromCbors } from "./spend-input-witness.js";
import { nativeTxFromCoreCompact } from "./submit-step-01.js";

export type PrepareNonExistentInputConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly badTxId?: string;
  readonly badInputIndex?: number;
  readonly prevUtxosRoot?: string;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
};

export type PreparedNeTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly txMembershipProofCbor: string;
};

export type PreparedNonExistentInputOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly transactionsRoot: string;
  readonly prevUtxosRoot: string;
  readonly badTxId: string;
  readonly badInputIndex: number;
  readonly missingInput: MidgardTxInput;
  readonly expectedTransactionsRoot?: {
    readonly value: string;
    readonly matches: boolean;
  };
  readonly txInclusion: PreparedNeTxInclusionJson;
  readonly spendInputCbors: readonly string[];
  readonly ledgerNonMembershipProofCbor: string;
  readonly txsNonMembershipProofCbor: string;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly inputsPath: string;
    readonly ledgerNonMembershipPath: string;
    readonly txsNonMembershipPath: string;
    readonly planPath: string;
  };
};

type DecodedTx = {
  readonly nodeTxId: string;
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly nativeCompactCbor: string;
  readonly spendInputCbors: readonly string[];
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
    nativeTx,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    nativeCompactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    ),
    spendInputCbors,
  };
};

const trieItem = (tx: DecodedTx) => ({
  key: Buffer.from(tx.nodeTxId, "hex"),
  value: Buffer.from(tx.nativeCompactCbor, "hex"),
});

const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

/**
 * Builds the four non-existent-input submit-step artifacts from a block the node
 * actually committed. The transactions trie is reconstructed with the node's
 * native encoding (`encodeMidgardNativeTxCompact` keyed by the raw 32-byte tx
 * id), so its root matches the committed `transactions_root` by construction.
 */
export const prepareNonExistentInputFromTransactions = async ({
  headerHash,
  transactions,
  badTxId,
  badInputIndex = 0,
  prevUtxosRoot = EMPTY_MERKLE_TREE_ROOT,
  expectedTransactionsRoot,
  outputDir,
  midgardNodeUrl,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly badTxId?: string;
  readonly badInputIndex?: number;
  readonly prevUtxosRoot?: string;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
  // Needed only when `prevUtxosRoot` is non-empty: the ledger non-membership
  // proof is then fetched from the node, which alone can reconstruct the
  // historical prev-utxos ledger trie at that root.
  readonly midgardNodeUrl?: string;
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

  if (badInputIndex < 0 || badInputIndex >= bad.spendInputCbors.length) {
    throw new Error(
      `--bad-input-index ${badInputIndex.toString()} is out of bounds for ${bad.spendInputCbors.length.toString()} inputs.`,
    );
  }
  const missingInput = spendInputsWitnessFromCbors(
    bad.spendInputCbors,
    "spend_inputs",
  ).inputs[badInputIndex]!;

  // --- Transactions trie (native encoding, matches the node) ----------------
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const tx of decoded) {
    const item = trieItem(tx);
    await trie.insert(item.key, item.value);
  }
  const transactionsRoot = trieRootHex(trie);
  const txMembershipProof = await trie.prove(Buffer.from(bad.nodeTxId, "hex"));
  const txMembershipProofCbor = txMembershipProof.toCBOR().toString("hex");

  // step-04 txs non-membership: the missing input's producing tx id is absent
  // from the block's transactions trie (keyed by the raw 32-byte tx id).
  const txsNonMembershipProof = await buildExclusionProof(
    trie,
    Buffer.from(missingInput.tx_id, "hex"),
  );

  // step-03 ledger non-membership: the missing input is absent from the block's
  // prev-utxos ledger trie (keyed by the Cardano TransactionInput CBOR). Over an
  // empty genesis ledger the proof is built locally against an empty trie; over
  // a ledger that already has blocks it is fetched from the node, the only party
  // that can reconstruct the historical prev-utxos trie at `prevUtxosRoot`.
  const ledgerNonMembershipProof = await resolveLedgerNonMembershipProof({
    prevUtxosRoot: normalizedPrevUtxosRoot,
    inputCbor: bad.spendInputCbors[badInputIndex]!,
    midgardNodeUrl,
  });

  const expectedCheck =
    expectedTransactionsRoot === undefined
      ? undefined
      : (() => {
          const value = parseHex(
            expectedTransactionsRoot,
            "--expected-transactions-root",
            32,
          );
          return { value, matches: value === transactionsRoot };
        })();
  if (expectedCheck !== undefined && !expectedCheck.matches) {
    throw new Error(
      `Reconstructed transactions root ${transactionsRoot} does not match the committed --expected-transactions-root ${expectedCheck.value}. The prepared proofs would not verify against this block.`,
    );
  }

  const base: PreparedNonExistentInputOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsRoot,
    prevUtxosRoot: normalizedPrevUtxosRoot,
    badTxId: bad.nodeTxId,
    badInputIndex,
    missingInput,
    ...(expectedCheck === undefined
      ? {}
      : { expectedTransactionsRoot: expectedCheck }),
    txInclusion: {
      nativeTxId: bad.nodeTxId,
      nativeTx: bad.nativeTxCompact,
      nativeTxCompactCbor: bad.nativeCompactCbor,
      txMembershipProofCbor,
    },
    spendInputCbors: bad.spendInputCbors,
    ledgerNonMembershipProofCbor: ledgerNonMembershipProof,
    txsNonMembershipProofCbor: txsNonMembershipProof,
  };
  if (outputDir === undefined) {
    return base;
  }
  await mkdir(outputDir, { recursive: true });
  const files = {
    txInclusionPath: join(outputDir, "ne-tx-inclusion.json"),
    inputsPath: join(outputDir, "ne-inputs.json"),
    ledgerNonMembershipPath: join(outputDir, "ne-ledger-non-membership.json"),
    txsNonMembershipPath: join(outputDir, "ne-txs-non-membership.json"),
    planPath: join(outputDir, "ne-plan.json"),
  };
  await Promise.all([
    writeFile(files.txInclusionPath, stringifyJson(base.txInclusion)),
    writeFile(files.inputsPath, stringifyJson(base.spendInputCbors)),
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
        prevUtxosRoot: base.prevUtxosRoot,
        expectedTransactionsRoot: base.expectedTransactionsRoot,
      }),
    ),
  ]);
  return { ...base, files };
};

/**
 * Non-membership (exclusion) proof CBOR for a key absent from `trie`: insert the
 * key with an empty value, prove it, and return that proof — verifying it in
 * exclusion reconstructs the original (key-absent) root, exactly what the
 * on-chain `pexcludes` validator checks.
 */
const buildExclusionProof = async (
  trie: Trie,
  absentKey: Buffer,
): Promise<string> => {
  if (trie.hash != null && (await trie.get(absentKey)) !== undefined) {
    throw new Error(
      `Cannot build a non-membership proof for key ${absentKey.toString("hex")}, which is present in the trie.`,
    );
  }
  await trie.insert(absentKey, Buffer.from(""));
  const proof = await trie.prove(absentKey);
  return proof.toCBOR().toString("hex");
};

/**
 * Returns the ledger non-membership proof CBOR for the missing input against the
 * block's `prevUtxosRoot`. The empty root needs no ledger state, so it is built
 * locally against an empty trie (works offline / for empty-genesis drills); any
 * other root must be reconstructed from the node's persisted prev-utxos ledger,
 * so it is fetched from the node's `GET /ledger-non-membership` endpoint.
 */
const resolveLedgerNonMembershipProof = async ({
  prevUtxosRoot,
  inputCbor,
  midgardNodeUrl,
}: {
  readonly prevUtxosRoot: string;
  readonly inputCbor: string;
  readonly midgardNodeUrl?: string;
}): Promise<string> => {
  if (prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT) {
    const emptyStore = new Store(undefined);
    await emptyStore.ready();
    return await buildExclusionProof(
      new Trie(emptyStore),
      Buffer.from(inputCbor, "hex"),
    );
  }
  if (midgardNodeUrl === undefined) {
    throw new Error(
      `The block sits over a non-empty prev-utxos ledger (root ${prevUtxosRoot}); pass --midgard-node-url so the ledger non-membership proof can be fetched from the node, which alone can reconstruct that historical ledger trie.`,
    );
  }
  const nodeUrl = midgardNodeUrl.trim().replace(/\/+$/, "");
  const url = `${nodeUrl}/ledger-non-membership?prev_utxos_root=${encodeURIComponent(
    prevUtxosRoot,
  )}&input=${encodeURIComponent(inputCbor)}`;
  const response = await fetch(url);
  const text = await response.text();
  if (!response.ok) {
    throw new Error(
      `GET /ledger-non-membership failed with HTTP ${response.status.toString()}: ${text}`,
    );
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(text);
  } catch (cause) {
    throw new Error(
      `GET /ledger-non-membership did not return valid JSON: ${formatUnknownError(cause)}`,
    );
  }
  const proofCbor =
    parsed !== null &&
    typeof parsed === "object" &&
    "proofCbor" in parsed &&
    typeof (parsed as { proofCbor: unknown }).proofCbor === "string"
      ? (parsed as { proofCbor: string }).proofCbor
      : undefined;
  if (proofCbor === undefined) {
    throw new Error(
      `GET /ledger-non-membership response is missing a string "proofCbor" field: ${text}`,
    );
  }
  return parseHex(proofCbor, "ledger-non-membership proofCbor");
};

export const prepareNonExistentInputFromNode = async (
  config: PrepareNonExistentInputConfig,
): Promise<PreparedNonExistentInputOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
  });
  return await prepareNonExistentInputFromTransactions({
    headerHash,
    transactions,
    badTxId: config.badTxId,
    badInputIndex: config.badInputIndex,
    prevUtxosRoot: config.prevUtxosRoot,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    outputDir: config.outputDir,
    midgardNodeUrl: config.midgardNodeUrl,
  });
};
