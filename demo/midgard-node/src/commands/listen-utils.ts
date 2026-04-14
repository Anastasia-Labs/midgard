import { isHexString } from "@/utils.js";
import { fromHex } from "@lucid-evolution/lucid";
import {
  cardanoTxBytesToMidgardNativeTxFull,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";

/**
 * Paths that require explicit admin-key authorization.
 */
export const ADMIN_ROUTE_PATHS: ReadonlySet<string> = new Set([
  "/init",
  "/commit",
  "/merge",
  "/reset",
  "/stateQueue",
  "/logBlocksDB",
  "/logGlobals",
]);

/**
 * Normalizes an HTTP path into the canonical route-path form used for access
 * checks.
 */
const normalizePath = (path: string): string =>
  path.startsWith("/") ? path.replace(/\/+$/, "") || "/" : `/${path}`;

/**
 * Returns whether a path belongs to the admin-only route set.
 */
export const isAdminRoutePath = (path: string): boolean =>
  ADMIN_ROUTE_PATHS.has(normalizePath(path));

/**
 * Result of evaluating admin-route authorization.
 */
export type AdminRouteAuthorization =
  | { readonly authorized: true }
  | {
      readonly authorized: false;
      readonly status: 401 | 403;
      readonly error: string;
    };

/**
 * Validates the provided admin key against the configured one.
 */
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

/**
 * Narrows an unknown JSON payload into a generic record when possible.
 */
const asRecord = (value: unknown): Record<string, unknown> | undefined => {
  if (typeof value !== "object" || value === null) {
    return undefined;
  }
  return value as Record<string, unknown>;
};

/**
 * Extracts transaction CBOR from the canonical or camel-case request-body field
 * names.
 */
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

/**
 * Extracts transaction CBOR from canonical or camel-case query parameters.
 */
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

/**
 * Validation result for a submitted transaction CBOR payload.
 */
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

/**
 * Validates hex formatting and maximum size for a submitted tx payload.
 */
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

/**
 * Normalized result of accepting either native Midgard CBOR or Cardano tx CBOR
 * converted into Midgard-native form.
 */
export type NormalizedSubmitTx =
  | {
      readonly ok: true;
      readonly txId: Buffer;
      readonly txIdHex: string;
      readonly txCbor: Buffer;
      readonly source: "native" | "cardano-converted";
    }
  | {
      readonly ok: false;
      readonly error: string;
      readonly detail: string;
    };

/**
 * Normalizes a submitted tx payload into Midgard-native bytes and derives the
 * canonical tx id.
 */
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
      return {
        ok: true,
        txId,
        txIdHex: txId.toString("hex"),
        txCbor: Buffer.from(normalizedTxCbor),
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
