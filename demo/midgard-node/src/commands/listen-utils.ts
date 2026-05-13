import {
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
} from "@/midgard-tx-codec/index.js";

/**
 * Paths that require explicit admin-key authorization.
 */
export const ADMIN_ROUTE_PATHS: ReadonlySet<string> = new Set([
  "/init",
  "/commit",
  "/merge",
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
 * Validation result for a submitted transaction envelope CBOR payload.
 */
export type SubmitTxValidation =
  | {
      readonly ok: true;
      readonly txEnvelopeCbor: Buffer;
      readonly byteLength: number;
    }
  | {
      readonly ok: false;
      readonly status: 400 | 413;
      readonly error: string;
    };

/**
 * Validates maximum size and non-emptiness for a submitted tx envelope.
 */
export const validateSubmitTxEnvelopeCbor = (
  txEnvelopeCbor: Uint8Array,
  maxTxBytes: number,
): SubmitTxValidation => {
  if (txEnvelopeCbor.length === 0) {
    return {
      ok: false,
      status: 400,
      error: "Invalid transaction envelope CBOR payload",
    };
  }

  const byteLength = txEnvelopeCbor.length;
  const maxAllowed = Math.max(1, maxTxBytes);
  if (byteLength > maxAllowed) {
    return {
      ok: false,
      status: 413,
      error: `Transaction envelope CBOR exceeds max size (${byteLength} > ${maxAllowed})`,
    };
  }

  return {
    ok: true,
    txEnvelopeCbor: Buffer.from(txEnvelopeCbor),
    byteLength,
  };
};

/**
 * Normalized result of accepting Midgard-native transaction-envelope CBOR.
 */
export type NormalizedSubmitTx =
  | {
      readonly ok: true;
      readonly txId: Buffer;
      readonly txIdHex: string;
      readonly txEnvelopeCbor: Buffer;
      readonly source: "native";
    }
  | {
      readonly ok: false;
      readonly error: string;
      readonly detail: string;
    };

/**
 * Normalizes a submitted tx payload into Midgard-native bytes, rejecting
 * non-envelope payloads and deriving the canonical tx id.
 */
export const normalizeSubmitTxEnvelopeCborToNative = (
  txEnvelopeCbor: Uint8Array,
): NormalizedSubmitTx => {
  const submittedTxEnvelopeCbor = Buffer.from(txEnvelopeCbor);
  try {
    const nativeTx = decodeMidgardNativeTxFull(submittedTxEnvelopeCbor);
    const txId = computeMidgardNativeTxIdFromFull(nativeTx);
    return {
      ok: true,
      txId,
      txIdHex: txId.toString("hex"),
      txEnvelopeCbor: submittedTxEnvelopeCbor,
      source: "native",
    };
  } catch (nativeDecodeError) {
    return {
      ok: false,
      error: "Invalid transaction envelope CBOR payload",
      detail: `native envelope decode failed: ${String(nativeDecodeError)}`,
    };
  }
};
