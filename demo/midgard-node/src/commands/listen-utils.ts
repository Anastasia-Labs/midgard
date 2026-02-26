import { isHexString } from "@/utils.js";
import { CML, fromHex } from "@lucid-evolution/lucid";
import {
  cardanoTxBytesToMidgardNativeTxFull,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";

export const ADMIN_ROUTE_PATHS: ReadonlySet<string> = new Set([
  "/init",
  "/commit",
  "/merge",
  "/reset",
  "/stateQueue",
  "/logBlocksDB",
  "/logGlobals",
]);

const normalizePath = (path: string): string =>
  path.startsWith("/") ? path.replace(/\/+$/, "") || "/" : `/${path}`;

export const isAdminRoutePath = (path: string): boolean =>
  ADMIN_ROUTE_PATHS.has(normalizePath(path));

export type AdminRouteAuthorization =
  | { readonly authorized: true }
  | {
      readonly authorized: false;
      readonly status: 401 | 403;
      readonly error: string;
    };

export const authorizeAdminRoute = (
  configuredAdminKey: string,
  providedAdminKey: string | undefined,
): AdminRouteAuthorization => {
  const expected = configuredAdminKey.trim();
  if (expected.length === 0) {
    return {
      authorized: false,
      status: 403,
      error: "Admin endpoints are disabled",
    };
  }
  if (providedAdminKey !== expected) {
    return {
      authorized: false,
      status: 401,
      error: "Missing or invalid admin credentials",
    };
  }
  return { authorized: true };
};

const asRecord = (value: unknown): Record<string, unknown> | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  return value as Record<string, unknown>;
};

export const extractSubmitTxHex = (payload: unknown): string | undefined => {
  const body = asRecord(payload);
  if (body === undefined) {
    return undefined;
  }
  const canonical = body.tx_cbor;
  if (typeof canonical === "string") {
    return canonical;
  }
  const camel = body.txCbor;
  if (typeof camel === "string") {
    return camel;
  }
  return undefined;
};

export const extractSubmitTxHexFromQueryParams = (
  params: Record<string, string | readonly string[] | undefined>,
): string | undefined => {
  const canonical = params.tx_cbor;
  if (typeof canonical === "string") {
    return canonical;
  }
  const camel = params.txCbor;
  if (typeof camel === "string") {
    return camel;
  }
  return undefined;
};

export type SubmitTxValidation =
  | {
      readonly ok: true;
      readonly txHex: string;
      readonly byteLength: number;
    }
  | {
      readonly ok: false;
      readonly status: 400 | 413;
      readonly error: string;
    };

export const validateSubmitTxHex = (
  txHex: string,
  maxTxBytes: number,
): SubmitTxValidation => {
  if (txHex.length === 0 || txHex.length % 2 !== 0 || !isHexString(txHex)) {
    return {
      ok: false,
      status: 400,
      error: "Invalid transaction CBOR payload",
    };
  }

  const byteLength = txHex.length / 2;
  const maxAllowed = Math.max(1, maxTxBytes);
  if (byteLength > maxAllowed) {
    return {
      ok: false,
      status: 413,
      error: `Transaction CBOR exceeds max size (${byteLength} > ${maxAllowed})`,
    };
  }

  return {
    ok: true,
    txHex,
    byteLength,
  };
};

export type NormalizedSubmitTx =
  | {
      readonly ok: true;
      readonly txId: Buffer;
      readonly txIdHex: string;
      readonly txCbor: Buffer;
      readonly txBodyHashForWitnesses?: Buffer;
      readonly source: "native" | "cardano-converted";
    }
  | {
      readonly ok: false;
      readonly error: string;
      readonly detail: string;
    };

export const normalizeSubmitTxHexToNative = (
  txHex: string,
): NormalizedSubmitTx => {
  const submittedTxCbor = Buffer.from(fromHex(txHex));
  try {
    const nativeTx = decodeMidgardNativeTxFull(submittedTxCbor);
    const txId = computeMidgardNativeTxIdFromFull(nativeTx);
    return {
      ok: true,
      txId,
      txIdHex: txId.toString("hex"),
      txCbor: submittedTxCbor,
      source: "native",
    };
  } catch (nativeDecodeError) {
    try {
      const nativeTx = cardanoTxBytesToMidgardNativeTxFull(submittedTxCbor);
      const normalizedTxCbor = encodeMidgardNativeTxFull(nativeTx);
      const txId = computeMidgardNativeTxIdFromFull(nativeTx);
      const cardanoTx = CML.Transaction.from_cbor_bytes(submittedTxCbor);
      const txBodyHashForWitnesses = Buffer.from(
        CML.hash_transaction(cardanoTx.body()).to_raw_bytes(),
      );
      return {
        ok: true,
        txId,
        txIdHex: txId.toString("hex"),
        txCbor: Buffer.from(normalizedTxCbor),
        txBodyHashForWitnesses,
        source: "cardano-converted",
      };
    } catch (conversionError) {
      return {
        ok: false,
        error: "Invalid transaction CBOR payload",
        detail: `native decode failed: ${String(nativeDecodeError)}; cardano conversion failed: ${String(conversionError)}`,
      };
    }
  }
};
