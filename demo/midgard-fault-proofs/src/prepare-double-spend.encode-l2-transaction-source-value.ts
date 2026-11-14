import { readFile } from "node:fs/promises";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeMidgardSpendInputItem,
  formatUnknownError,
  type MidgardNativeTxFull,
  type MidgardNativeTxProofSource,
  verifyMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  type L2TransactionSource,
  L2TransactionSourceSchema,
  type NativeTxCompact as NativeTxCompactData,
  type OutputReference as OutputReferenceData,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { parseHex, parseJsonText, requireRecord } from "./json-file.js";

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
  /** Exact source leaf when supplied by retained DA; derivation must agree. */
  readonly l2TransactionSourceCbor?: string;
};

export const deriveL2TransactionSourceCbor = (
  canonicalTxCbor: Uint8Array,
): string => {
  const proofSource =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(canonicalTxCbor);
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(canonicalTxCbor),
  ).toString("hex");
  return encodeL2TransactionSourceValue({ txId, proofSource });
};

export const encodeL2TransactionSourceValue = ({
  txId,
  proofSource,
}: {
  readonly txId: string;
  readonly proofSource: MidgardNativeTxProofSource;
}): string => {
  const normalizedTxId = parseHex(txId, "transaction source tx id", 32);
  verifyMidgardNativeTxProofSource({
    transactionId: Buffer.from(normalizedTxId, "hex"),
    source: proofSource,
  });
  const sourceValue: L2TransactionSource = {
    tx_id: normalizedTxId,
    source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return Data.to(sourceValue as never, L2TransactionSourceSchema as never);
};

export type PreparedTxInclusionJson = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  /** Canonical Data(L2TransactionSource) bytes opened by membership. */
  readonly l2TransactionSourceCbor: string;
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
  readonly nativeTx: MidgardNativeTxFull;
  readonly nativeTxCompact: NativeTxCompactData;
  readonly inputs: readonly OutputReferenceData[];
  readonly referenceInputs: readonly OutputReferenceData[];
  readonly spendInputCbors: readonly string[];
  readonly nativeCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
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

export const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

/**
 * The §5.3 field-0/1 item encoding — `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`,
 * a fixed 38 bytes — matching on-chain `ledger_outref_key` /
 * `encode_midgard_tx_input`, not CML's minimal-index `TransactionInput` CBOR.
 */
export const transactionInputCbor = (
  txHash: string,
  outputIndex: bigint,
): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txHash, "hex"),
    outputIndex: Number(outputIndex),
  });
