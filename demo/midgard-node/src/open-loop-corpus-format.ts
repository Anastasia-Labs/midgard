/**
 * Open-loop transaction corpus wire format (NDJSON rows of canonical Midgard
 * native V1 transactions) and the corpus funding UTxO shape.
 *
 * The corpus itself is produced and consumed by the stress tooling in
 * midgard-node-tools, but the row format is also read on the node side — the
 * mpf-engine-probe worker replays corpora and the stage-B validation benchmark
 * feeds them to the validation pool — so the format lives here, where both can
 * reach it without the operator package depending on its own test tooling.
 */
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeMidgardSpendInputItemV1,
} from "@al-ft/midgard-core/codec";
import { hexToBytes } from "@al-ft/midgard-core/hex";

import { nonEmptyString } from "./artifact-schema.js";
import { decodeNodeUtxo, type NodeUtxo } from "./commands/command-utils.js";
import { sha256Hex } from "./sha256.js";

export type OpenLoopCorpusShape = "fanout" | "chain" | "mixed";

export type OpenLoopCorpusRow = {
  readonly txHash: string;
  readonly canonicalCborHex: string;
  readonly canonicalCborSha256: string;
  readonly canonicalCborByteLength: number;
  readonly senderWalletId: string;
  readonly selectedInputOutref: string;
  readonly outputOutrefs: readonly string[];
  readonly planShape: OpenLoopCorpusShape;
  readonly parentTxHash: string | null;
  readonly corpusSliceId: string;
};

const TX_HASH_PATTERN = /^[0-9a-f]{64}$/;
const SHA256_PATTERN = /^[0-9a-f]{64}$/;
const OUTREF_PATTERN = /^[0-9a-f]{64}#(0|[1-9][0-9]*)$/;
const CORPUS_ROW_KEYS = [
  "txHash",
  "canonicalCborHex",
  "canonicalCborSha256",
  "canonicalCborByteLength",
  "senderWalletId",
  "selectedInputOutref",
  "outputOutrefs",
  "planShape",
  "parentTxHash",
  "corpusSliceId",
] as const;

const normalizeHex = (value: string): string => value.trim().toLowerCase();

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const parseStringField = (
  input: Record<string, unknown>,
  name: string,
): string => {
  const value = nonEmptyString(input[name], `corpus row ${name}`);
  if (value !== value.trim()) {
    throw new Error(`corpus row ${name} must be an exact non-empty string`);
  }
  return value;
};

const parseOutputOutrefs = (
  input: Record<string, unknown>,
): readonly string[] => {
  const value = input.outputOutrefs;
  if (
    !Array.isArray(value) ||
    value.some(
      (entry) =>
        typeof entry !== "string" ||
        entry.length === 0 ||
        entry !== entry.trim(),
    )
  ) {
    throw new Error(
      "corpus row outputOutrefs must be an array of exact non-empty strings",
    );
  }
  return value;
};

const parsePlanShape = (value: string): OpenLoopCorpusShape => {
  if (value === "fanout" || value === "chain" || value === "mixed") {
    return value;
  }
  throw new Error(`unsupported corpus planShape ${value}`);
};

export const parseOpenLoopCorpusLine = (
  line: string,
  index: number,
): OpenLoopCorpusRow => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(line) as unknown;
  } catch (error) {
    throw new Error(
      `invalid JSON in corpus row ${index.toString()}: ${errorMessage(error)}`,
    );
  }
  if (typeof parsed !== "object" || parsed === null) {
    throw new Error(`corpus row ${index.toString()} must be an object`);
  }
  const input = parsed as Record<string, unknown>;
  const keys = Object.keys(input);
  const extra = keys.filter(
    (key) => !CORPUS_ROW_KEYS.includes(key as (typeof CORPUS_ROW_KEYS)[number]),
  );
  const missing = CORPUS_ROW_KEYS.filter((key) => !Object.hasOwn(input, key));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `corpus row ${index.toString()} keys must be exact; missing=[${missing.join(",")}], extra=[${extra.join(",")}]`,
    );
  }
  for (const field of [
    "txHash",
    "canonicalCborHex",
    "canonicalCborSha256",
  ] as const) {
    const value = input[field];
    if (typeof value !== "string" || value !== value.trim().toLowerCase()) {
      throw new Error(
        `corpus row ${index.toString()} ${field} must use exact lowercase encoding`,
      );
    }
  }
  const txHash = normalizeHex(parseStringField(input, "txHash"));
  const canonicalCborHex = normalizeHex(
    parseStringField(input, "canonicalCborHex"),
  );
  const canonicalCborSha256 = normalizeHex(
    parseStringField(input, "canonicalCborSha256"),
  );
  const canonicalCborBytes = Buffer.from(canonicalCborHex, "hex");
  const byteLength = input.canonicalCborByteLength;
  if (!TX_HASH_PATTERN.test(txHash)) {
    throw new Error(
      `corpus row ${index.toString()} txHash must be 32-byte hex`,
    );
  }
  if (
    canonicalCborHex.length === 0 ||
    canonicalCborHex.length % 2 !== 0 ||
    canonicalCborBytes.toString("hex") !== canonicalCborHex
  ) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborHex must be hex`,
    );
  }
  if (!SHA256_PATTERN.test(canonicalCborSha256)) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborSha256 must be 32-byte hex`,
    );
  }
  if (sha256Hex(canonicalCborBytes) !== canonicalCborSha256) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborSha256 does not match canonicalCborHex`,
    );
  }
  if (
    typeof byteLength !== "number" ||
    !Number.isSafeInteger(byteLength) ||
    byteLength !== canonicalCborBytes.length
  ) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborByteLength does not match canonicalCborHex`,
    );
  }
  let outputCount: number;
  let computedTxHash: string;
  try {
    const nativeTx =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCborBytes);
    computedTxHash = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
    outputCount = decodeMidgardNativeByteListPreimage(
      nativeTx.body.outputsPreimageCbor,
      `corpus row ${index.toString()} outputs`,
    ).length;
  } catch (cause) {
    throw new Error(
      `corpus row ${index.toString()} canonicalCborHex must be canonical Midgard native V1 transaction CBOR: ${errorMessage(cause)}`,
    );
  }
  if (computedTxHash !== txHash) {
    throw new Error(
      `corpus row ${index.toString()} txHash does not bind canonicalCborHex`,
    );
  }

  const rawParentTxHash = input.parentTxHash;
  if (rawParentTxHash !== null && typeof rawParentTxHash !== "string") {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must be null or 32-byte hex`,
    );
  }
  const parentTxHash =
    rawParentTxHash === null ? null : normalizeHex(rawParentTxHash);
  if (
    typeof rawParentTxHash === "string" &&
    rawParentTxHash !== rawParentTxHash.trim().toLowerCase()
  ) {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must use exact lowercase encoding`,
    );
  }
  if (parentTxHash !== null && !TX_HASH_PATTERN.test(parentTxHash)) {
    throw new Error(
      `corpus row ${index.toString()} parentTxHash must be null or 32-byte hex`,
    );
  }

  const senderWalletId = parseStringField(input, "senderWalletId");
  const selectedInputOutref = parseStringField(input, "selectedInputOutref");
  if (!OUTREF_PATTERN.test(selectedInputOutref)) {
    throw new Error(
      `corpus row ${index.toString()} selectedInputOutref must be canonical <64hex>#<index>`,
    );
  }
  const outputOutrefs = parseOutputOutrefs(input);
  if (
    outputOutrefs.length !== outputCount ||
    outputOutrefs.some(
      (outref, outputIndex) => outref !== `${txHash}#${outputIndex.toString()}`,
    )
  ) {
    throw new Error(
      `corpus row ${index.toString()} outputOutrefs must exactly enumerate canonicalCborHex outputs`,
    );
  }

  return {
    txHash,
    canonicalCborHex,
    canonicalCborSha256,
    canonicalCborByteLength: byteLength,
    senderWalletId,
    selectedInputOutref,
    outputOutrefs,
    planShape: parsePlanShape(parseStringField(input, "planShape")),
    parentTxHash,
    corpusSliceId: parseStringField(input, "corpusSliceId"),
  };
};

export const parseOpenLoopCorpusNdjson = (
  raw: string,
): readonly OpenLoopCorpusRow[] =>
  raw
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => parseOpenLoopCorpusLine(line, index + 1));

export type CorpusFundingUtxo = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outputCborHex: string;
};

// The §5.3 field-0/1 item encoding — `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`,
// fixed 38 bytes — which is what on-chain `ledger_outref_key` derives through
// `encode_midgard_tx_input`, not CML's minimal-index `TransactionInput` CBOR.
export const outRefCborHex = (txHash: string, outputIndex: number): string =>
  encodeMidgardSpendInputItemV1({
    txId: hexToBytes(txHash, { fieldName: "outRef.txHash" }),
    outputIndex,
  }).toString("hex");

export const nodeUtxoFromCorpusFunding = (
  funding: CorpusFundingUtxo,
): NodeUtxo =>
  decodeNodeUtxo({
    outref: outRefCborHex(funding.txHash, funding.outputIndex),
    outputCbor: funding.outputCborHex,
  });
