/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 */

import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardSpendInputItemV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardSpendInputItemV1,
  formatUnknownError,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFullV1,
  type MidgardTxInputV1,
} from "@al-ft/midgard-core";
import {
  commitCountedRootProgram,
  EMPTY_MERKLE_TREE_ROOT,
  type NativeTxCompact as NativeTxCompactData,
  type OutputReference as OutputReferenceData,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  parseHex,
  parseJsonText,
  requireRecord,
  stringifyJson,
} from "./json-file.js";
import { nativeTxFromCoreCompact } from "./submit-step-01.js";

export type FetchLike = (
  input: string | URL,
  init?: RequestInit,
) => Promise<Response>;

export type PrepareDoubleSpendCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
};

export type NodeTransactionPayload = {
  readonly nodeTxId: string;
  readonly txCbor: string;
};

export type PreparedTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  // Raw transactions MPF root the membership proof opens; authenticated on-chain
  // against the header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProofCbor: string;
};

export type PreparedDoubleSpendTx = {
  readonly nodeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  readonly txInclusion: PreparedTxInclusionJson;
  readonly inputs: readonly OutputReferenceData[];
  readonly spendInputCbors: readonly string[];
  readonly doubleSpentInputIndex: number;
};

export type PreparedDoubleSpendOutput = {
  readonly headerHash: string;
  readonly txCount: number;
  readonly doubleSpentInput: OutputReferenceData;
  readonly commitmentEncodings: {
    readonly nativeNode: {
      readonly transactionsRoot: string;
    };
    readonly expectedTransactionsRoot?: {
      readonly value: string;
    };
  };
  readonly tx1: PreparedDoubleSpendTx;
  readonly tx2: PreparedDoubleSpendTx;
  readonly files?: {
    readonly blockTransactionsPath?: string;
    readonly tx1InclusionPath: string;
    readonly tx2InclusionPath: string;
    readonly tx1InputsPath: string;
    readonly tx2InputsPath: string;
    readonly planPath: string;
  };
};

export type PrepareDoubleSpendFromFileConfig = {
  readonly transactionsPath: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
};

export type PrepareSampleDoubleSpendConfig = {
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly outputDir?: string;
};

export type DecodedTransactionMaterial = {
  readonly nodeTxId: string;
  readonly txCbor: string;
  readonly nativeTx: MidgardNativeTxFullV1;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly inputs: readonly OutputReferenceData[];
  readonly spendInputCbors: readonly string[];
  readonly nativeCompactCbor: string;
};

export type TrieView = {
  readonly root: string;
  readonly proofCborByKeyHex: ReadonlyMap<string, string>;
};

const normalizeNodeUrl = (url: string): string => {
  const trimmed = url.trim();
  if (trimmed.length === 0) {
    throw new Error("--midgard-node-url must not be empty.");
  }
  return trimmed.replace(/\/+$/, "");
};

const readJson = async (
  response: Response,
  label: string,
): Promise<unknown> => {
  const text = await response.text();
  return parseJsonText(
    text,
    (cause) =>
      `${label} did not return valid JSON: ${formatUnknownError(cause)}`,
  );
};

const fetchJson = async (
  fetchImpl: FetchLike,
  url: string,
  label: string,
): Promise<unknown> => {
  const response = await fetchImpl(url);
  if (!response.ok) {
    throw new Error(`${label} failed with HTTP ${response.status.toString()}.`);
  }
  return await readJson(response, label);
};

const fetchBlockTxIds = async ({
  fetchImpl,
  nodeUrl,
  headerHash,
}: {
  readonly fetchImpl: FetchLike;
  readonly nodeUrl: string;
  readonly headerHash: string;
}): Promise<readonly string[]> => {
  const json = requireRecord(
    await fetchJson(
      fetchImpl,
      `${nodeUrl}/block?header_hash=${encodeURIComponent(headerHash)}`,
      "GET /block",
    ),
    "GET /block response",
  );
  if (!Array.isArray(json.hashes)) {
    throw new Error("GET /block response.hashes must be an array.");
  }
  return json.hashes.map((value, index) =>
    parseHex(value, `GET /block.hashes[${index.toString()}]`, 32),
  );
};

const fetchTxCbor = async ({
  fetchImpl,
  nodeUrl,
  txId,
}: {
  readonly fetchImpl: FetchLike;
  readonly nodeUrl: string;
  readonly txId: string;
}): Promise<string> => {
  const json = requireRecord(
    await fetchJson(
      fetchImpl,
      `${nodeUrl}/tx?tx_hash=${encodeURIComponent(txId)}`,
      `GET /tx ${txId}`,
    ),
    "GET /tx response",
  );
  return parseHex(json.tx, "GET /tx.tx");
};

const parseNodeTransactionPayloads = (
  value: unknown,
  label: string,
): readonly NodeTransactionPayload[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be an array of transaction payloads.`);
  }
  return value.map((entry, index) => {
    const candidate = requireRecord(entry, `${label}[${index.toString()}]`);
    return {
      nodeTxId: parseHex(
        candidate.nodeTxId,
        `${label}[${index.toString()}].nodeTxId`,
        32,
      ),
      txCbor: parseHex(
        candidate.txCbor,
        `${label}[${index.toString()}].txCbor`,
      ),
    };
  });
};

export const readNodeTransactionPayloadsFile = async (
  path: string,
): Promise<readonly NodeTransactionPayload[]> => {
  const text = await readFile(path, "utf8");
  const json = parseJsonText(
    text,
    (cause) => `Failed to parse ${path} as JSON: ${formatUnknownError(cause)}`,
  );
  return parseNodeTransactionPayloads(json, path);
};

export const fetchNodeBlockTransactions = async ({
  midgardNodeUrl,
  headerHash,
  fetchImpl = globalThis.fetch as FetchLike,
}: {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly fetchImpl?: FetchLike;
}): Promise<readonly NodeTransactionPayload[]> => {
  if (typeof fetchImpl !== "function") {
    throw new Error("No fetch implementation is available.");
  }
  const nodeUrl = normalizeNodeUrl(midgardNodeUrl);
  const txIds = await fetchBlockTxIds({ fetchImpl, nodeUrl, headerHash });
  return await Promise.all(
    txIds.map(async (nodeTxId) => ({
      nodeTxId,
      txCbor: await fetchTxCbor({ fetchImpl, nodeUrl, txId: nodeTxId }),
    })),
  );
};

const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

/**
 * The §5.3 field-0/1 item encoding — `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`,
 * a fixed 38 bytes — matching on-chain `ledger_outref_key` /
 * `encode_midgard_tx_input`, not CML's minimal-index `TransactionInput` CBOR.
 */
const transactionInputCbor = (txHash: string, outputIndex: bigint): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });

const sampleNativeTx = (
  inputs: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFullV1 => {
  const body = {
    spendInputsPreimageCbor: encodeCbor(inputs),
    referenceInputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    outputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    requiredSignersPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    mintPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    scriptIntegrityHash: Buffer.from(EMPTY_NULL_ROOT),
    auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
    networkId: 0n,
  };
  const witnessSet = {
    addrTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    scriptTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    redeemerTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
  };
  return materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet,
  });
};

const payloadFromNativeTx = (
  tx: MidgardNativeTxFullV1,
): NodeTransactionPayload => ({
  nodeTxId: computeMidgardNativeTxIdV1(tx).toString("hex"),
  txCbor: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
});

export const makeSampleDoubleSpendTransactions =
  (): readonly NodeTransactionPayload[] => {
    const sharedInput = transactionInputCbor("11".repeat(32), 7n);
    const tx1 = sampleNativeTx(
      [sharedInput, transactionInputCbor("22".repeat(32), 0n)],
      1n,
    );
    const tx2 = sampleNativeTx(
      [transactionInputCbor("33".repeat(32), 0n), sharedInput],
      2n,
    );
    const tx3 = sampleNativeTx([transactionInputCbor("44".repeat(32), 0n)], 3n);
    return [
      payloadFromNativeTx(tx1),
      payloadFromNativeTx(tx2),
      payloadFromNativeTx(tx3),
    ];
  };

/**
 * Decodes the §5.3 field-0/1 item form — `82 ‖ 58 20 tx_id(32) ‖ 19
 * index_be16`, a fixed 38 bytes — the same bytes on-chain
 * `decode_midgard_tx_input_cbor` accepts. CML's minimal-index
 * `TransactionInput` CBOR is deliberately rejected.
 */
const outputReferenceFromNativeInput = (
  bytes: Uint8Array,
  label: string,
): OutputReferenceData => {
  let input: MidgardTxInputV1;
  try {
    input = decodeMidgardSpendInputItemV1(bytes);
  } catch (cause) {
    throw new Error(
      `${label} is not a valid Midgard §5.3 TxOutRef CBOR: ${formatUnknownError(cause)}`,
    );
  }
  return {
    transactionId: Buffer.from(input.txId).toString("hex"),
    outputIndex: BigInt(input.outputIndex),
  };
};

const decodeNativeInputPreimage = (
  preimageCbor: Uint8Array,
  label: string,
): readonly OutputReferenceData[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(
    (bytes: Buffer, index: number) =>
      outputReferenceFromNativeInput(bytes, `${label}[${index.toString()}]`),
  );

const decodeNativeInputCbors = (
  preimageCbor: Uint8Array,
  label: string,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(preimageCbor, label).map(bytesHex);

export const decodeTransactionMaterial = async (
  payload: NodeTransactionPayload,
): Promise<DecodedTransactionMaterial> => {
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
  const computedNodeTxId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  if (computedNodeTxId !== nodeTxId) {
    throw new Error(
      `Node tx id mismatch: listed=${nodeTxId}, computed=${computedNodeTxId}.`,
    );
  }
  const inputs = decodeNativeInputPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    `tx ${nodeTxId} spend_inputs`,
  );
  const spendInputCbors = decodeNativeInputCbors(
    nativeTx.body.spendInputsPreimageCbor,
    `tx ${nodeTxId} spend_inputs`,
  );
  return {
    nodeTxId,
    txCbor,
    nativeTx,
    nativeTxCompact: nativeTxFromCoreCompact(nativeTx.compact),
    inputs,
    spendInputCbors,
    nativeCompactCbor: encodeMidgardNativeTxCompactV1(
      nativeTx.compact,
    ).toString("hex"),
  };
};

const outputReferenceKey = (outRef: OutputReferenceData): string =>
  `${outRef.transactionId}#${outRef.outputIndex.toString()}`;

const resolveDoubleSpendPair = ({
  transactions,
  tx1Id,
  tx2Id,
}: {
  readonly transactions: readonly DecodedTransactionMaterial[];
  readonly tx1Id?: string;
  readonly tx2Id?: string;
}): {
  readonly tx1: DecodedTransactionMaterial;
  readonly tx2: DecodedTransactionMaterial;
  readonly doubleSpentInput: OutputReferenceData;
  readonly tx1DoubleSpentInputIndex: number;
  readonly tx2DoubleSpentInputIndex: number;
} => {
  if ((tx1Id === undefined) !== (tx2Id === undefined)) {
    throw new Error("--tx1-id and --tx2-id must be provided together.");
  }

  if (tx1Id !== undefined && tx2Id !== undefined) {
    const normalizedTx1 = parseHex(tx1Id, "--tx1-id", 32);
    const normalizedTx2 = parseHex(tx2Id, "--tx2-id", 32);
    if (normalizedTx1 === normalizedTx2) {
      throw new Error(
        "--tx1-id and --tx2-id must identify distinct transactions.",
      );
    }
    const tx1 = transactions.find((tx) => tx.nodeTxId === normalizedTx1);
    const tx2 = transactions.find((tx) => tx.nodeTxId === normalizedTx2);
    if (tx1 === undefined || tx2 === undefined) {
      throw new Error(
        "Requested --tx1-id/--tx2-id was not found in the block.",
      );
    }
    for (const [tx1InputIndex, input] of tx1.inputs.entries()) {
      const tx2InputIndex = tx2.inputs.findIndex(
        (candidate) =>
          outputReferenceKey(candidate) === outputReferenceKey(input),
      );
      if (tx2InputIndex >= 0) {
        return {
          tx1,
          tx2,
          doubleSpentInput: input,
          tx1DoubleSpentInputIndex: tx1InputIndex,
          tx2DoubleSpentInputIndex: tx2InputIndex,
        };
      }
    }
    throw new Error("Requested transactions do not spend the same input.");
  }

  const firstSpendByInput = new Map<
    string,
    {
      readonly tx: DecodedTransactionMaterial;
      readonly input: OutputReferenceData;
      readonly inputIndex: number;
    }
  >();
  for (const tx of transactions) {
    for (const [inputIndex, input] of tx.inputs.entries()) {
      const key = outputReferenceKey(input);
      const first = firstSpendByInput.get(key);
      if (first === undefined) {
        firstSpendByInput.set(key, { tx, input, inputIndex });
        continue;
      }
      if (first.tx.nodeTxId === tx.nodeTxId) {
        continue;
      }
      return {
        tx1: first.tx,
        tx2: tx,
        doubleSpentInput: first.input,
        tx1DoubleSpentInputIndex: first.inputIndex,
        tx2DoubleSpentInputIndex: inputIndex,
      };
    }
  }
  throw new Error("No double spend found in the selected block.");
};

export const buildTrieView = async (
  items: readonly {
    readonly key: Buffer;
    readonly value: Buffer;
  }[],
): Promise<TrieView> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const item of items) {
    await trie.insert(item.key, item.value);
  }
  const proofEntries = await Promise.all(
    items.map(async (item) => {
      const proof = await trie.prove(item.key);
      return [
        item.key.toString("hex"),
        proof.toCBOR().toString("hex"),
      ] as const;
    }),
  );
  return {
    root:
      trie.hash == null
        ? EMPTY_MERKLE_TREE_ROOT
        : Buffer.from(trie.hash).toString("hex"),
    proofCborByKeyHex: new Map(proofEntries),
  };
};

export const nativeTrieItem = (tx: DecodedTransactionMaterial) => ({
  key: Buffer.from(tx.nodeTxId, "hex"),
  value: Buffer.from(tx.nativeCompactCbor, "hex"),
});

export const requireProof = (
  trie: TrieView,
  key: Buffer,
  label: string,
): string => {
  const proof = trie.proofCborByKeyHex.get(key.toString("hex"));
  if (proof === undefined) {
    throw new Error(`Internal error: missing ${label} membership proof.`);
  }
  return proof;
};

const prepareTx = ({
  tx,
  doubleSpentInputIndex,
  proofCbor,
  transactionsPhasRoot,
}: {
  readonly tx: DecodedTransactionMaterial;
  readonly doubleSpentInputIndex: number;
  readonly proofCbor: string;
  readonly transactionsPhasRoot: string;
}): PreparedDoubleSpendTx => ({
  nodeTxId: tx.nodeTxId,
  nativeTx: tx.nativeTxCompact,
  nativeTxCompactCbor: tx.nativeCompactCbor,
  txInclusion: {
    nativeTxId: tx.nodeTxId,
    nativeTx: tx.nativeTxCompact,
    nativeTxCompactCbor: tx.nativeCompactCbor,
    transactionsPhasRoot,
    txMembershipProofCbor: proofCbor,
  },
  inputs: tx.inputs,
  spendInputCbors: tx.spendInputCbors,
  doubleSpentInputIndex,
});

export const requireTransactionsRootMatchV1 = async ({
  nativeRoot,
  expectedTransactionsRoot,
  count,
}: {
  readonly nativeRoot: string;
  readonly expectedTransactionsRoot?: string;
  readonly count: bigint;
}): Promise<void> => {
  if (expectedTransactionsRoot === undefined) {
    return;
  }
  const committedRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: nativeRoot,
      count,
    }),
  );
  if (expectedTransactionsRoot !== committedRoot) {
    throw new Error(
      `Expected V1 transactions root ${expectedTransactionsRoot} does not match the counted native node transaction root ${committedRoot} derived from raw PHAS root ${nativeRoot} at count ${count.toString()}.`,
    );
  }
};

const writePreparedFiles = async ({
  output,
  outputDir,
}: {
  readonly output: PreparedDoubleSpendOutput;
  readonly outputDir: string;
}): Promise<PreparedDoubleSpendOutput["files"]> => {
  await mkdir(outputDir, { recursive: true });
  const paths = {
    tx1InclusionPath: join(outputDir, "tx1-inclusion.json"),
    tx2InclusionPath: join(outputDir, "tx2-inclusion.json"),
    tx1InputsPath: join(outputDir, "tx1-inputs.json"),
    tx2InputsPath: join(outputDir, "tx2-inputs.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.tx1InclusionPath, stringifyJson(output.tx1.txInclusion)),
    writeFile(paths.tx2InclusionPath, stringifyJson(output.tx2.txInclusion)),
    writeFile(paths.tx1InputsPath, stringifyJson(output.tx1.spendInputCbors)),
    writeFile(paths.tx2InputsPath, stringifyJson(output.tx2.spendInputCbors)),
    writeFile(
      paths.planPath,
      stringifyJson({
        headerHash: output.headerHash,
        doubleSpentInput: output.doubleSpentInput,
        tx1NodeTxId: output.tx1.nodeTxId,
        tx2NodeTxId: output.tx2.nodeTxId,
        tx1DoubleSpentInputIndex: output.tx1.doubleSpentInputIndex,
        tx2DoubleSpentInputIndex: output.tx2.doubleSpentInputIndex,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

const writeBlockTransactionsFile = async ({
  outputDir,
  transactions,
}: {
  readonly outputDir: string;
  readonly transactions: readonly NodeTransactionPayload[];
}): Promise<string> => {
  await mkdir(outputDir, { recursive: true });
  const path = join(outputDir, "block-transactions.json");
  await writeFile(path, stringifyJson(transactions));
  return path;
};

export const prepareDoubleSpendFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  tx1Id,
  tx2Id,
  outputDir,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
}): Promise<PreparedDoubleSpendOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot =
    expectedTransactionsRoot === undefined
      ? undefined
      : parseHex(expectedTransactionsRoot, "--expected-transactions-root", 32);
  const decoded = await Promise.all(
    transactions.map(decodeTransactionMaterial),
  );
  const pair = resolveDoubleSpendPair({ transactions: decoded, tx1Id, tx2Id });
  const nativeTrie = await buildTrieView(decoded.map(nativeTrieItem));
  const tx1Proof = requireProof(
    nativeTrie,
    nativeTrieItem(pair.tx1).key,
    "tx1",
  );
  const tx2Proof = requireProof(
    nativeTrie,
    nativeTrieItem(pair.tx2).key,
    "tx2",
  );
  await requireTransactionsRootMatchV1({
    nativeRoot: nativeTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
    count: BigInt(decoded.length),
  });
  const baseOutput: PreparedDoubleSpendOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    doubleSpentInput: pair.doubleSpentInput,
    commitmentEncodings: {
      nativeNode: {
        transactionsRoot: nativeTrie.root,
      },
      ...(normalizedExpectedRoot === undefined
        ? {}
        : {
            expectedTransactionsRoot: {
              value: normalizedExpectedRoot,
            },
          }),
    },
    tx1: prepareTx({
      tx: pair.tx1,
      doubleSpentInputIndex: pair.tx1DoubleSpentInputIndex,
      proofCbor: tx1Proof,
      transactionsPhasRoot: nativeTrie.root,
    }),
    tx2: prepareTx({
      tx: pair.tx2,
      doubleSpentInputIndex: pair.tx2DoubleSpentInputIndex,
      proofCbor: tx2Proof,
      transactionsPhasRoot: nativeTrie.root,
    }),
  };
  if (outputDir === undefined) {
    return baseOutput;
  }
  const files = await writePreparedFiles({
    output: baseOutput,
    outputDir,
  });
  return { ...baseOutput, files };
};

export const prepareDoubleSpendFromNode = async (
  config: PrepareDoubleSpendCliConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const headerHash = parseHex(config.headerHash, "--header-hash", 28);
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash,
  });
  return await prepareDoubleSpendFromTransactions({
    headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    tx1Id: config.tx1Id,
    tx2Id: config.tx2Id,
    outputDir: config.outputDir,
  });
};

export const prepareDoubleSpendFromFile = async (
  config: PrepareDoubleSpendFromFileConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const transactions = await readNodeTransactionPayloadsFile(
    config.transactionsPath,
  );
  return await prepareDoubleSpendFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    tx1Id: config.tx1Id,
    tx2Id: config.tx2Id,
    outputDir: config.outputDir,
  });
};

export const prepareSampleDoubleSpend = async (
  config: PrepareSampleDoubleSpendConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const transactions = makeSampleDoubleSpendTransactions();
  const output = await prepareDoubleSpendFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    outputDir: config.outputDir,
  });
  if (config.outputDir === undefined) {
    return output;
  }
  const blockTransactionsPath = await writeBlockTransactionsFile({
    outputDir: config.outputDir,
    transactions,
  });
  return {
    ...output,
    files:
      output.files === undefined
        ? undefined
        : {
            blockTransactionsPath,
            ...output.files,
          },
  };
};
