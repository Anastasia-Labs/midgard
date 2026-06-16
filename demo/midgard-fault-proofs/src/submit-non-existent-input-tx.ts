import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  formatUnknownError,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import { parseHex } from "./json-file.js";

type FetchLike = (input: string | URL, init?: RequestInit) => Promise<Response>;

export type SubmitNonExistentInputTxConfig = {
  readonly midgardNodeUrl: string;
  /** Fabricated input tx id (32-byte hex). Defaults to a 0xde…×32 phantom. */
  readonly phantomTxId?: string;
  /** Fabricated input output index. Defaults to 0. */
  readonly phantomIndex?: number;
  readonly fetchImpl?: FetchLike;
};

export type SubmitNonExistentInputTxResult = {
  readonly nativeTxId: string;
  readonly phantomInput: { readonly txId: string; readonly index: number };
  readonly status: number;
  readonly responseText: string;
};

const normalizeNodeUrl = (url: string): string => {
  const trimmed = url.trim();
  if (trimmed.length === 0) {
    throw new Error("--midgard-node-url must not be empty.");
  }
  return trimmed.replace(/\/+$/, "");
};

const phantomInputCbor = (txId: string, index: number): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txId),
      BigInt(index),
    ).to_cbor_bytes(),
  );

/**
 * Crafts a Midgard-native transaction whose single spend input is a fabricated
 * (non-existent) output reference and POSTs its canonical CBOR to the node's
 * `/submit` endpoint. With `SKIP_TX_VALIDATION=true` the node admits it without
 * validation and the block builder commits it, producing the faulty block the
 * non-existent-input fault proof disputes.
 */
export const submitNonExistentInputTx = async ({
  midgardNodeUrl,
  phantomTxId = "de".repeat(32),
  phantomIndex = 0,
  fetchImpl = globalThis.fetch as FetchLike,
}: SubmitNonExistentInputTxConfig): Promise<SubmitNonExistentInputTxResult> => {
  if (typeof fetchImpl !== "function") {
    throw new Error("No fetch implementation is available.");
  }
  const nodeUrl = normalizeNodeUrl(midgardNodeUrl);
  const normalizedTxId = parseHex(phantomTxId, "--phantom-input-tx-id", 32);
  if (!Number.isInteger(phantomIndex) || phantomIndex < 0) {
    throw new Error("--phantom-input-index must be a non-negative integer.");
  }
  const phantom = phantomInputCbor(normalizedTxId, phantomIndex);

  const tx = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([phantom]),
      referenceInputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      outputsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      requiredSignersPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      mintPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      scriptIntegrityHash: Buffer.from(EMPTY_NULL_ROOT),
      auxiliaryDataHash: Buffer.from(EMPTY_NULL_ROOT),
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      scriptTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
      redeemerTxWitsPreimageCbor: Buffer.from(EMPTY_CBOR_LIST),
    },
  });

  const nativeTxId = computeMidgardNativeTxId(tx).toString("hex");
  const body = encodeMidgardNativeTxCanonical(tx);

  let response: Response;
  try {
    response = await fetchImpl(`${nodeUrl}/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/cbor" },
      body,
    });
  } catch (cause) {
    throw new Error(
      `POST ${nodeUrl}/submit failed: ${formatUnknownError(cause)}`,
    );
  }
  const responseText = await response.text();
  return {
    nativeTxId,
    phantomInput: { txId: normalizedTxId, index: phantomIndex },
    status: response.status,
    responseText,
  };
};
