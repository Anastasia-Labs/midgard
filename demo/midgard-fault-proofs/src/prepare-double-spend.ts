import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { CML } from "@lucid-evolution/lucid";
import {
  EMPTY_MERKLE_TREE_ROOT,
  type NativeTxCompact as NativeTxCompactData,
  type OutputReference as OutputReferenceData,
} from "@al-ft/midgard-sdk";
import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { nativeTxFromCoreCompact, parseHex } from "./submit-step-01.js";

type FetchLike = (
  input: string | URL,
  init?: RequestInit,
) => Promise<Response>;

type JsonObject = Record<string, unknown>;

export type PrepareDoubleSpendCliConfig = {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
};

export type NodeTransactionPayload = {
  readonly nodeTxId: string;
  readonly txCbor: string;
};

export type PreparedTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
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
  readonly compatibility: {
    readonly canUseSubmitStepCommands: boolean;
    readonly reasons: readonly string[];
  };
  readonly commitmentEncodings: {
    readonly nativeNode: {
      readonly transactionsRoot: string;
    };
    readonly expectedTransactionsRoot?: {
      readonly value: string;
      readonly matchesNativeNodeRoot: boolean;
    };
  };
  readonly tx1: PreparedDoubleSpendTx;
  readonly tx2: PreparedDoubleSpendTx;
  readonly files?: {
    readonly tx1InclusionPath: string;
    readonly tx2InclusionPath: string;
    readonly tx1InputsPath: string;
    readonly tx2InputsPath: string;
    readonly planPath: string;
  };
};

type DecodedTransactionMaterial = {
  readonly nodeTxId: string;
  readonly txCbor: string;
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly inputs: readonly OutputReferenceData[];
  readonly spendInputCbors: readonly string[];
  readonly nativeCompactCbor: string;
};

type TrieView = {
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

const isJsonObject = (value: unknown): value is JsonObject =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const readJson = async (response: Response, label: string): Promise<unknown> => {
  const text = await response.text();
  try {
    return JSON.parse(text) as unknown;
  } catch (cause) {
    throw new Error(`${label} did not return valid JSON: ${String(cause)}`);
  }
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
  const json = await fetchJson(
    fetchImpl,
    `${nodeUrl}/block?header_hash=${encodeURIComponent(headerHash)}`,
    "GET /block",
  );
  if (!isJsonObject(json) || !Array.isArray(json.hashes)) {
    throw new Error('GET /block response must be an object with "hashes".');
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
  const json = await fetchJson(
    fetchImpl,
    `${nodeUrl}/tx?tx_hash=${encodeURIComponent(txId)}`,
    `GET /tx ${txId}`,
  );
  if (!isJsonObject(json)) {
    throw new Error("GET /tx response must be a JSON object.");
  }
  return parseHex(json.tx, "GET /tx.tx");
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

const bytesHex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const outputReferenceFromNativeInput = (
  bytes: Uint8Array,
  label: string,
): OutputReferenceData => {
  let input: InstanceType<typeof CML.TransactionInput>;
  try {
    input = CML.TransactionInput.from_cbor_bytes(bytes);
  } catch (cause) {
    throw new Error(`${label} is not a valid Cardano TxOutRef CBOR: ${String(cause)}`);
  }
  const outputIndex = input.index();
  if (outputIndex < 0n) {
    throw new Error(`${label}.outputIndex must be non-negative.`);
  }
  return {
    transactionId: input.transaction_id().to_hex(),
    outputIndex,
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

const decodeTransactionMaterial = async (
  payload: NodeTransactionPayload,
): Promise<DecodedTransactionMaterial> => {
  const nodeTxId = parseHex(payload.nodeTxId, "nodeTxId", 32);
  const txCbor = parseHex(payload.txCbor, `tx ${nodeTxId} CBOR`);
  let nativeTx: MidgardNativeTxFull;
  try {
    nativeTx = decodeMidgardNativeTxFull(Buffer.from(txCbor, "hex"));
  } catch (cause) {
    throw new Error(`Failed to decode native Midgard tx ${nodeTxId}: ${String(cause)}`);
  }
  const computedNodeTxId = computeMidgardNativeTxIdFromFull(nativeTx).toString("hex");
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
    nativeCompactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString(
      "hex",
    ),
  };
};

const outputReferenceKey = (outRef: OutputReferenceData): string =>
  `${outRef.transactionId}#${outRef.outputIndex.toString()}`;

const sameNodeTx = (
  left: DecodedTransactionMaterial,
  right: DecodedTransactionMaterial,
): boolean => left.nodeTxId === right.nodeTxId;

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
      throw new Error("--tx1-id and --tx2-id must identify distinct transactions.");
    }
    const tx1 = transactions.find((tx) => tx.nodeTxId === normalizedTx1);
    const tx2 = transactions.find((tx) => tx.nodeTxId === normalizedTx2);
    if (tx1 === undefined || tx2 === undefined) {
      throw new Error("Requested --tx1-id/--tx2-id was not found in the block.");
    }
    for (const [tx1InputIndex, input] of tx1.inputs.entries()) {
      const tx2InputIndex = tx2.inputs.findIndex(
        (candidate) => outputReferenceKey(candidate) === outputReferenceKey(input),
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
      if (sameNodeTx(first.tx, tx)) {
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

const buildTrieView = async (
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
      return [item.key.toString("hex"), proof.toCBOR().toString("hex")] as const;
    }),
  );
  return {
    root:
      trie.hash === null || trie.hash === undefined
        ? EMPTY_MERKLE_TREE_ROOT
        : Buffer.from(trie.hash).toString("hex"),
    proofCborByKeyHex: new Map(proofEntries),
  };
};

const nativeTrieItem = (tx: DecodedTransactionMaterial) => ({
  key: Buffer.from(tx.nodeTxId, "hex"),
  value: Buffer.from(tx.nativeCompactCbor, "hex"),
});

const requireProof = (
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
}: {
  readonly tx: DecodedTransactionMaterial;
  readonly doubleSpentInputIndex: number;
  readonly proofCbor: string;
}): PreparedDoubleSpendTx => {
  return {
    nodeTxId: tx.nodeTxId,
    nativeTx: tx.nativeTxCompact,
    nativeTxCompactCbor: tx.nativeCompactCbor,
    txInclusion: {
      nativeTxId: tx.nodeTxId,
      nativeTx: tx.nativeTxCompact,
      nativeTxCompactCbor: tx.nativeCompactCbor,
      txMembershipProofCbor: proofCbor,
    },
    inputs: tx.inputs,
    spendInputCbors: tx.spendInputCbors,
    doubleSpentInputIndex,
  };
};

const compatibilityReasons = ({
  nativeRoot,
  expectedTransactionsRoot,
}: {
  readonly nativeRoot: string;
  readonly expectedTransactionsRoot?: string;
}): readonly string[] => {
  const reasons: string[] = [];
  if (
    expectedTransactionsRoot !== undefined &&
    expectedTransactionsRoot !== nativeRoot
  ) {
    reasons.push(
      `Expected transactions root ${expectedTransactionsRoot} does not match the native node transaction root ${nativeRoot}.`,
    );
  }
  return reasons;
};

const jsonStringify = (value: unknown): string =>
  `${JSON.stringify(
    value,
    (_key, item) => (typeof item === "bigint" ? item.toString() : item),
    2,
  )}\n`;

const writePreparedFiles = async ({
  output,
  outputDir,
  allowIncompatibleOutput,
}: {
  readonly output: PreparedDoubleSpendOutput;
  readonly outputDir: string;
  readonly allowIncompatibleOutput: boolean;
}): Promise<PreparedDoubleSpendOutput["files"]> => {
  if (!output.compatibility.canUseSubmitStepCommands && !allowIncompatibleOutput) {
    throw new Error(
      "Refusing to write submit-step material because the selected block is not compatible with the current fault-proof ABI. Pass --allow-incompatible-output to write diagnostic files anyway.",
    );
  }
  await mkdir(outputDir, { recursive: true });
  const paths = {
    tx1InclusionPath: join(outputDir, "tx1-inclusion.json"),
    tx2InclusionPath: join(outputDir, "tx2-inclusion.json"),
    tx1InputsPath: join(outputDir, "tx1-inputs.json"),
    tx2InputsPath: join(outputDir, "tx2-inputs.json"),
    planPath: join(outputDir, "plan.json"),
  };
  await Promise.all([
    writeFile(paths.tx1InclusionPath, jsonStringify(output.tx1.txInclusion)),
    writeFile(paths.tx2InclusionPath, jsonStringify(output.tx2.txInclusion)),
    writeFile(paths.tx1InputsPath, jsonStringify(output.tx1.spendInputCbors)),
    writeFile(paths.tx2InputsPath, jsonStringify(output.tx2.spendInputCbors)),
    writeFile(
      paths.planPath,
      jsonStringify({
        headerHash: output.headerHash,
        doubleSpentInput: output.doubleSpentInput,
        tx1NodeTxId: output.tx1.nodeTxId,
        tx2NodeTxId: output.tx2.nodeTxId,
        tx1DoubleSpentInputIndex: output.tx1.doubleSpentInputIndex,
        tx2DoubleSpentInputIndex: output.tx2.doubleSpentInputIndex,
        compatibility: output.compatibility,
        commitmentEncodings: output.commitmentEncodings,
      }),
    ),
  ]);
  return paths;
};

export const prepareDoubleSpendFromTransactions = async ({
  headerHash,
  transactions,
  expectedTransactionsRoot,
  tx1Id,
  tx2Id,
  outputDir,
  allowIncompatibleOutput = false,
}: {
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
  readonly expectedTransactionsRoot?: string;
  readonly tx1Id?: string;
  readonly tx2Id?: string;
  readonly outputDir?: string;
  readonly allowIncompatibleOutput?: boolean;
}): Promise<PreparedDoubleSpendOutput> => {
  const normalizedHeaderHash = parseHex(headerHash, "--header-hash", 28);
  const normalizedExpectedRoot =
    expectedTransactionsRoot === undefined
      ? undefined
      : parseHex(expectedTransactionsRoot, "--expected-transactions-root", 32);
  const decoded = await Promise.all(transactions.map(decodeTransactionMaterial));
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
  const reasons = compatibilityReasons({
    nativeRoot: nativeTrie.root,
    expectedTransactionsRoot: normalizedExpectedRoot,
  });
  const baseOutput: PreparedDoubleSpendOutput = {
    headerHash: normalizedHeaderHash,
    txCount: decoded.length,
    doubleSpentInput: pair.doubleSpentInput,
    compatibility: {
      canUseSubmitStepCommands: reasons.length === 0,
      reasons,
    },
    commitmentEncodings: {
      nativeNode: {
        transactionsRoot: nativeTrie.root,
      },
      ...(normalizedExpectedRoot === undefined
        ? {}
        : {
            expectedTransactionsRoot: {
              value: normalizedExpectedRoot,
              matchesNativeNodeRoot: normalizedExpectedRoot === nativeTrie.root,
            },
          }),
    },
    tx1: prepareTx({
      tx: pair.tx1,
      doubleSpentInputIndex: pair.tx1DoubleSpentInputIndex,
      proofCbor: tx1Proof,
    }),
    tx2: prepareTx({
      tx: pair.tx2,
      doubleSpentInputIndex: pair.tx2DoubleSpentInputIndex,
      proofCbor: tx2Proof,
    }),
  };
  if (outputDir === undefined) {
    return baseOutput;
  }
  const files = await writePreparedFiles({
    output: baseOutput,
    outputDir,
    allowIncompatibleOutput,
  });
  return { ...baseOutput, files };
};

export const prepareDoubleSpendFromNode = async (
  config: PrepareDoubleSpendCliConfig,
): Promise<PreparedDoubleSpendOutput> => {
  const transactions = await fetchNodeBlockTransactions({
    midgardNodeUrl: config.midgardNodeUrl,
    headerHash: parseHex(config.headerHash, "--header-hash", 28),
  });
  return await prepareDoubleSpendFromTransactions({
    headerHash: config.headerHash,
    transactions,
    expectedTransactionsRoot: config.expectedTransactionsRoot,
    tx1Id: config.tx1Id,
    tx2Id: config.tx2Id,
    outputDir: config.outputDir,
    allowIncompatibleOutput: config.allowIncompatibleOutput,
  });
};
