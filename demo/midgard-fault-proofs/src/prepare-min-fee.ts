/** Security-grade-capable evidence builder for the standalone min-fee family. */
import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  decodeMidgardNativeByteListPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  minFeeThreadTokenAssetName,
  minimumFeeFromProofSource,
  type NativeTxCompact,
  type NativeTxWitnessSetCompact,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { parseHex, parseSignedInteger, stringifyJson } from "./json-file.js";
import {
  buildTrieView,
  type DecodedTransactionMaterial,
  decodeTransactionMaterial,
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  type PreparedTxInclusionJson,
  readNodeTransactionPayloadsFile,
  requireProof,
  transactionSourceTrieItem,
} from "./prepare-double-spend.js";

export type PreparedMinFeeTx = {
  readonly nodeTxId: string;
  readonly nativeTx: NativeTxCompact;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly witnessSet: NativeTxWitnessSetCompact;
  /** Exactly nine arrays in §2.5 wire order; each member is canonical item CBOR. */
  readonly fieldItemCbors: readonly (readonly string[])[];
  readonly fee: bigint;
  readonly canonicalTxSize: bigint;
  readonly minimumFee: bigint;
  readonly shortfall: bigint;
};

export type PreparedMinFeeOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly transactionsPhasRoot: string;
  readonly committedTransactionsRoot: string;
  readonly expectedTransactionsRoot: string;
  readonly threadTokenAssetName?: string;
  readonly tx: PreparedMinFeeTx;
  readonly files?: {
    readonly txInclusionPath: string;
    readonly step02EvidencePath: string;
    readonly planPath: string;
  };
};

export type PrepareMinFeeCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot: string;
  readonly minFeeA: string | number | bigint;
  readonly minFeeB: string | number | bigint;
  readonly txId?: string;
  readonly categoryId?: string;
  readonly outputDir?: string;
  readonly fetchImpl?: FetchLike;
};

export type PrepareMinFeeFromFileConfig = Omit<
  PrepareMinFeeCliConfig,
  "midgardNodeUrl" | "fetchImpl"
> & { readonly transactionsPath: string };

const nonNegative = (
  value: string | number | bigint,
  label: string,
): bigint => {
  const parsed = parseSignedInteger(value, label);
  if (parsed < 0n) throw new Error(`${label} must be non-negative.`);
  return parsed;
};

const witnessSetFromMaterial = (
  material: DecodedTransactionMaterial,
): NativeTxWitnessSetCompact => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    material.nativeTx.witnessSet,
  );
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

const fieldPreimages = (material: DecodedTransactionMaterial) => [
  material.nativeTx.body.spendInputsPreimageCbor,
  material.nativeTx.body.referenceInputsPreimageCbor,
  material.nativeTx.body.outputsPreimageCbor,
  material.nativeTx.body.requiredObserversPreimageCbor,
  material.nativeTx.body.requiredSignersPreimageCbor,
  material.nativeTx.body.mintPreimageCbor,
  material.nativeTx.witnessSet.scriptTxWitsPreimageCbor,
  material.nativeTx.witnessSet.addrTxWitsPreimageCbor,
  material.nativeTx.witnessSet.redeemerTxWitsPreimageCbor,
];

const fieldItemCbors = (
  material: DecodedTransactionMaterial,
): readonly (readonly string[])[] =>
  fieldPreimages(material).map((preimage, fieldIndex) =>
    decodeMidgardNativeByteListPreimage(
      preimage,
      `min-fee field ${fieldIndex.toString()}`,
    ).map((item) => item.toString("hex")),
  );

const writePreparedFiles = async ({
  output,
  outputDir,
}: {
  readonly output: PreparedMinFeeOutput;
  readonly outputDir: string;
}): Promise<NonNullable<PreparedMinFeeOutput["files"]>> => {
  await mkdir(outputDir, { recursive: true });
  const paths = {
    txInclusionPath: join(outputDir, "min-fee-tx-inclusion.json"),
    step02EvidencePath: join(outputDir, "min-fee-step-02-evidence.json"),
    planPath: join(outputDir, "min-fee-plan.json"),
  };
  await Promise.all([
    writeFile(paths.txInclusionPath, stringifyJson(output.tx.txInclusion)),
    writeFile(
      paths.step02EvidencePath,
      stringifyJson({
        nativeTxCompactCbor: output.tx.nativeTxCompactCbor,
        witnessSet: output.tx.witnessSet,
        fieldItemCbors: output.tx.fieldItemCbors,
      }),
    ),
    writeFile(
      paths.planPath,
      stringifyJson({
        headerHash: output.headerHash,
        txId: output.tx.nodeTxId,
        fee: output.tx.fee,
        canonicalTxSize: output.tx.canonicalTxSize,
        minFeeA: output.minFeeA,
        minFeeB: output.minFeeB,
        minimumFee: output.tx.minimumFee,
        shortfall: output.tx.shortfall,
        transactionsPhasRoot: output.transactionsPhasRoot,
        committedTransactionsRoot: output.committedTransactionsRoot,
        expectedTransactionsRoot: output.expectedTransactionsRoot,
        threadTokenAssetName: output.threadTokenAssetName,
      }),
    ),
  ]);
  return paths;
};

export const prepareMinFeeFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  minFeeA,
  minFeeB,
  txId,
  categoryId,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot: string;
  readonly minFeeA: string | number | bigint;
  readonly minFeeB: string | number | bigint;
  readonly txId?: string;
  readonly categoryId?: string;
  readonly outputDir?: string;
}): Promise<PreparedMinFeeOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedRoot = parseHex(
    expectedTransactionsRoot,
    "--expected-transactions-root",
    32,
  );
  const normalizedTxId =
    txId === undefined ? undefined : parseHex(txId, "--tx-id", 32);
  const feeA = nonNegative(minFeeA, "--min-fee-a");
  const feeB = nonNegative(minFeeB, "--min-fee-b");
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const priced = decoded.map((material) => {
    const boundary = minimumFeeFromProofSource({
      source: deriveMidgardNativeTxProofSource(material.nativeTx),
      minFeeA: feeA,
      minFeeB: feeB,
    });
    return { material, boundary };
  });
  const candidates = priced.filter(
    ({ material, boundary }) =>
      material.nativeTx.body.fee < boundary.minimumFee,
  );
  const selected =
    normalizedTxId === undefined
      ? candidates[0]
      : candidates.find(({ material }) => material.nodeTxId === normalizedTxId);
  if (selected === undefined) {
    if (normalizedTxId !== undefined) {
      const exact = priced.find(
        ({ material }) => material.nodeTxId === normalizedTxId,
      );
      if (exact !== undefined) {
        throw new Error(
          `Requested --tx-id ${normalizedTxId} pays ${exact.material.nativeTx.body.fee.toString()}, satisfying exact minimum ${exact.boundary.minimumFee.toString()}.`,
        );
      }
      throw new Error(`Requested --tx-id ${normalizedTxId} was not found.`);
    }
    throw new Error("No min-fee violation found in the selected block.");
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  const proofCbor = requireProof(
    trie,
    transactionSourceTrieItem(selected.material).key,
    "min-fee tx",
  );
  const committedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: trie.root,
      count: BigInt(decoded.length),
    }),
  );
  if (committedTransactionsRoot !== normalizedRoot) {
    throw new Error(
      `Min-fee transactions commit to ${committedTransactionsRoot}, not authenticated header root ${normalizedRoot}.`,
    );
  }
  const material = selected.material;
  const base: PreparedMinFeeOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    minFeeA: feeA,
    minFeeB: feeB,
    transactionsPhasRoot: trie.root,
    committedTransactionsRoot,
    expectedTransactionsRoot: normalizedRoot,
    ...(categoryId === undefined
      ? {}
      : {
          threadTokenAssetName: minFeeThreadTokenAssetName(
            categoryId,
            normalizedHeaderHash,
          ),
        }),
    tx: {
      nodeTxId: material.nodeTxId,
      nativeTx: material.nativeTxCompact,
      nativeTxCompactCbor: material.nativeCompactCbor,
      txInclusion: {
        nativeTxId: material.nodeTxId,
        nativeTx: material.nativeTxCompact,
        nativeTxCompactCbor: material.nativeCompactCbor,
        l2TransactionSourceCbor: material.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: proofCbor,
      },
      witnessSet: witnessSetFromMaterial(material),
      fieldItemCbors: fieldItemCbors(material),
      fee: material.nativeTx.body.fee,
      canonicalTxSize: selected.boundary.canonicalTxSize,
      minimumFee: selected.boundary.minimumFee,
      shortfall: selected.boundary.minimumFee - material.nativeTx.body.fee,
    },
  };
  if (outputDir === undefined) return base;
  return {
    ...base,
    files: await writePreparedFiles({ output: base, outputDir }),
  };
};

export const prepareMinFeeFromNode = async (
  config: PrepareMinFeeCliConfig,
): Promise<PreparedMinFeeOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
    fetchImpl: config.fetchImpl,
  });
  return await prepareMinFeeFromTransactions({
    ...config,
    headerHash,
    transactions,
  });
};

export const prepareMinFeeFromFile = async (
  config: PrepareMinFeeFromFileConfig,
): Promise<PreparedMinFeeOutput> =>
  await prepareMinFeeFromTransactions({
    ...config,
    transactions: await readNodeTransactionPayloadsFile(
      config.transactionsPath,
    ),
  });
