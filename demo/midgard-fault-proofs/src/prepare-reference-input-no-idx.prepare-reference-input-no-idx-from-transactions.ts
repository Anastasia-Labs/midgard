import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  commitCountedRootProgram,
  type MidgardTxInput,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { parseHex, stringifyJson } from "./json-file.js";
import {
  buildMembershipProof,
  computeTrieRoot,
  type TrieEntry,
} from "./ne-proofs.js";
import { type NodeTransactionPayload } from "./prepare-double-spend.js";
import {
  type DecodedTx,
  decodeTx,
  parseBadReferenceInputIndex,
  type PreparedReferenceInputNoIdxOutput,
  type ReferenceInputNoIdxPreimageEntry,
} from "./prepare-reference-input-no-idx.decode-tx.js";

/**
 * Locates an out-of-range reference input: an input whose producing transaction
 * IS present in the block, but whose `output_index` is >= that producing
 * transaction's output count. Honours an explicit
 * `--bad-tx-id`/`--bad-reference-input-index` selection when supplied, otherwise
 * scans for the first offending pair.
 */
const selectOffendingReferenceInput = ({
  decoded,
  byId,
  badTxId,
  badReferenceInputIndex,
}: {
  readonly decoded: readonly DecodedTx[];
  readonly byId: ReadonlyMap<string, DecodedTx>;
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: number;
}): {
  readonly badTx: DecodedTx;
  readonly badReferenceInputIndex: number;
  readonly badReferenceInput: MidgardTxInput;
  readonly producingTx: DecodedTx;
} => {
  const isOffending = (
    tx: DecodedTx,
    index: number,
  ):
    | { readonly input: MidgardTxInput; readonly producing: DecodedTx }
    | undefined => {
    const input = tx.referenceInputs[index];
    if (input === undefined) {
      return undefined;
    }
    const producing = byId.get(input.tx_id);
    if (producing === undefined) {
      return undefined;
    }
    if (input.output_index < producing.outputsPreimageCbor.length) {
      return undefined;
    }
    return { input, producing };
  };

  if (badTxId !== undefined) {
    const normalized = parseHex(badTxId, "--bad-tx-id", 32);
    const badTx = decoded.find((tx) => tx.nodeTxId === normalized);
    if (badTx === undefined) {
      throw new Error(`--bad-tx-id ${normalized} not found in the block.`);
    }
    if (badReferenceInputIndex !== undefined) {
      if (badReferenceInputIndex >= badTx.referenceInputs.length) {
        throw new Error(
          `--bad-reference-input-index ${badReferenceInputIndex.toString()} is out of bounds for ${badTx.referenceInputs.length.toString()} reference inputs.`,
        );
      }
      const found = isOffending(badTx, badReferenceInputIndex);
      if (found === undefined) {
        const input = badTx.referenceInputs[badReferenceInputIndex]!;
        const producing = byId.get(input.tx_id);
        throw new Error(
          producing === undefined
            ? `Reference input ${badReferenceInputIndex.toString()} of ${normalized} references producing tx ${input.tx_id}, which is not in the block — that is a no-reference-input fault, not reference-input-no-idx.`
            : `Reference input ${badReferenceInputIndex.toString()} of ${normalized} has output index ${input.output_index.toString()}, which is within range of its producing tx's ${producing.outputsPreimageCbor.length.toString()} outputs — not a fault.`,
        );
      }
      return {
        badTx,
        badReferenceInputIndex,
        badReferenceInput: found.input,
        producingTx: found.producing,
      };
    }
    for (let i = 0; i < badTx.referenceInputs.length; i++) {
      const found = isOffending(badTx, i);
      if (found !== undefined) {
        return {
          badTx,
          badReferenceInputIndex: i,
          badReferenceInput: found.input,
          producingTx: found.producing,
        };
      }
    }
    throw new Error(
      `--bad-tx-id ${normalized} has no out-of-range reference input; specify a different tx.`,
    );
  }

  for (const badTx of decoded) {
    for (let i = 0; i < badTx.referenceInputs.length; i++) {
      const found = isOffending(badTx, i);
      if (found !== undefined) {
        return {
          badTx,
          badReferenceInputIndex: i,
          badReferenceInput: found.input,
          producingTx: found.producing,
        };
      }
    }
  }
  throw new Error(
    "No out-of-range reference input found in the block. Every referenced input " +
      "either resolves within its in-block producing transaction's outputs or " +
      "references a producing tx absent from the block (a no-reference-input fault).",
  );
};

/**
 * Builds the four reference-input-no-idx submit-step artifacts from a block the
 * node actually committed. The transactions trie is reconstructed with the
 * exact canonical `Data(L2TransactionSourceV1)` values keyed by raw tx id, so
 * its root matches the committed `transactions_root` by construction.
 */
export const prepareReferenceInputNoIdxFromTransactions = async ({
  headerHash,
  transactions,
  badTxId,
  badReferenceInputIndex,
  expectedTransactionsRoot,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly badTxId?: string;
  readonly badReferenceInputIndex?: string | number;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
}): Promise<PreparedReferenceInputNoIdxOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const decoded = transactions.map(decodeTx);
  if (decoded.length === 0) {
    throw new Error("The selected block contains no transactions.");
  }
  const byId = new Map(decoded.map((tx) => [tx.nodeTxId, tx] as const));

  const {
    badTx,
    badReferenceInputIndex: resolvedBadReferenceInputIndex,
    badReferenceInput,
    producingTx,
  } = selectOffendingReferenceInput({
    decoded,
    byId,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badReferenceInputIndex === undefined
      ? {}
      : {
          badReferenceInputIndex: parseBadReferenceInputIndex(
            badReferenceInputIndex,
          ),
        }),
  });

  // --- Transactions trie (native encoding, matches the node) ----------------
  const txsEntries: TrieEntry[] = decoded.map((tx) => ({
    key: Buffer.from(tx.nodeTxId, "hex"),
    value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
  }));
  const transactionsRoot = await computeTrieRoot(txsEntries);
  const badTxMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(badTx.nodeTxId, "hex"),
  );
  const producingTxMembershipProofCbor = await buildMembershipProof(
    txsEntries,
    Buffer.from(producingTx.nodeTxId, "hex"),
  );

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

  const referenceInputsPreimage: readonly ReferenceInputNoIdxPreimageEntry[] =
    badTx.referenceInputs.map((input) => ({
      txId: input.tx_id,
      index: input.output_index,
    }));

  const base: PreparedReferenceInputNoIdxOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    transactionsRoot,
    committedTransactionsRoot,
    badTxId: badTx.nodeTxId,
    badReferenceInputIndex: resolvedBadReferenceInputIndex,
    badReferenceInput,
    producingTxId: producingTx.nodeTxId,
    producingTxOutputCount: producingTx.outputsPreimageCbor.length,
    ...(expectedCheck === undefined
      ? {}
      : { expectedTransactionsRoot: expectedCheck }),
    badTxInclusion: {
      nativeTxId: badTx.nodeTxId,
      nativeTx: badTx.nativeTxCompact,
      nativeTxCompactCbor: badTx.nativeCompactCbor,
      l2TransactionSourceCbor: badTx.l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: badTxMembershipProofCbor,
    },
    referenceInputsPreimage,
    producingTxInclusion: {
      nativeTxId: producingTx.nodeTxId,
      nativeTx: producingTx.nativeTxCompact,
      nativeTxCompactCbor: producingTx.nativeCompactCbor,
      l2TransactionSourceCbor: producingTx.l2TransactionSourceCbor,
      transactionsPhasRoot: transactionsRoot,
      txMembershipProofCbor: producingTxMembershipProofCbor,
    },
    outputsPreimageCbor: producingTx.outputsPreimageCbor,
  };
  if (outputDir === undefined) {
    return base;
  }
  await mkdir(outputDir, { recursive: true });
  const files = {
    badTxInclusionPath: join(
      outputDir,
      "reference-input-no-idx-bad-tx-inclusion.json",
    ),
    referenceInputsPreimagePath: join(
      outputDir,
      "reference-input-no-idx-reference-inputs-preimage.json",
    ),
    producingTxInclusionPath: join(
      outputDir,
      "reference-input-no-idx-producing-tx-inclusion.json",
    ),
    outputsPreimagePath: join(
      outputDir,
      "reference-input-no-idx-outputs-preimage.json",
    ),
    planPath: join(outputDir, "reference-input-no-idx-plan.json"),
  };
  await Promise.all([
    writeFile(files.badTxInclusionPath, stringifyJson(base.badTxInclusion)),
    writeFile(
      files.referenceInputsPreimagePath,
      stringifyJson(base.referenceInputsPreimage),
    ),
    writeFile(
      files.producingTxInclusionPath,
      stringifyJson(base.producingTxInclusion),
    ),
    writeFile(
      files.outputsPreimagePath,
      stringifyJson(base.outputsPreimageCbor),
    ),
    writeFile(
      files.planPath,
      stringifyJson({
        headerHash: base.headerHash,
        badTxId: base.badTxId,
        badReferenceInputIndex: base.badReferenceInputIndex,
        badReferenceInput: base.badReferenceInput,
        producingTxId: base.producingTxId,
        producingTxOutputCount: base.producingTxOutputCount,
        transactionsRoot: base.transactionsRoot,
        committedTransactionsRoot: base.committedTransactionsRoot,
        expectedTransactionsRoot: base.expectedTransactionsRoot,
      }),
    ),
  ]);
  return { ...base, files };
};
