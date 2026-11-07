import { timingSafeEqual } from "node:crypto";

import type { MidgardNativeTxFull } from "@al-ft/midgard-core/codec";
import {
  decodeMidgardSubmittedNativeTxFromCanonicalCbor,
  type MidgardSubmittedTx,
} from "@al-ft/midgard-validation";

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
  "/operator/status",
  "/admin/session",
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
  if (
    providedAdminKey === undefined ||
    Buffer.byteLength(providedAdminKey) !== Buffer.byteLength(expected) ||
    !timingSafeEqual(Buffer.from(providedAdminKey), Buffer.from(expected))
  ) {
    return {
      authorized: false,
      status: 401,
      error: "Missing or invalid admin credentials",
    };
  }
  return { authorized: true };
};

/**
 * Validation result for a submitted canonical transaction CBOR payload.
 */
export type SubmitTxValidation =
  | {
      readonly ok: true;
      readonly txCanonicalCbor: Buffer;
      readonly byteLength: number;
    }
  | {
      readonly ok: false;
      readonly status: 400 | 413;
      readonly error: string;
    };

/**
 * Validates maximum size and non-emptiness for a submitted canonical tx.
 */
export const validateSubmitTxCanonicalCbor = (
  txCanonicalCbor: Uint8Array,
  maxTxBytes: number,
): SubmitTxValidation => {
  if (txCanonicalCbor.length === 0) {
    return {
      ok: false,
      status: 400,
      error: "Invalid canonical transaction CBOR payload",
    };
  }

  const byteLength = txCanonicalCbor.length;
  if (byteLength > maxTxBytes) {
    return {
      ok: false,
      status: 413,
      error: `Canonical transaction CBOR exceeds max size (${byteLength} > ${maxTxBytes})`,
    };
  }

  return {
    ok: true,
    txCanonicalCbor: Buffer.from(txCanonicalCbor),
    byteLength,
  };
};

/**
 * Normalized result of accepting Midgard-native canonical transaction CBOR.
 */
export type NormalizedSubmitTx =
  | {
      readonly ok: true;
      readonly txId: Buffer;
      readonly txIdHex: string;
      readonly txCanonicalCbor: Buffer;
      readonly source: "native";
    }
  | {
      readonly ok: false;
      readonly error: string;
      readonly detail: string;
    };

/**
 * A submitted tx payload decoded once: the core full decode and the
 * submitted-tx envelope built from it.
 */
export type DecodedSubmitTx =
  | {
      readonly ok: true;
      readonly nativeTx: MidgardNativeTxFull;
      readonly submittedTx: MidgardSubmittedTx;
    }
  | {
      readonly ok: false;
      readonly error: string;
      readonly detail: string;
    };

/**
 * Decodes submitted canonical Midgard-native bytes once, keeping the native
 * decode for the consensus check. Any decode failure, at the canonical or
 * the ledger stage, is the same invalid-payload result.
 */
export const decodeSubmitTxCanonicalCbor = (
  txCanonicalCbor: Uint8Array,
): DecodedSubmitTx => {
  try {
    return {
      ok: true,
      ...decodeMidgardSubmittedNativeTxFromCanonicalCbor(txCanonicalCbor),
    };
  } catch {
    return {
      ok: false,
      error: "Invalid canonical transaction CBOR payload",
      detail: "canonical native transaction decode failed",
    };
  }
};

/**
 * Normalizes a submitted tx payload into canonical Midgard-native bytes and
 * derives the canonical tx id from the materialized compact form.
 */
export const normalizeSubmitTxCanonicalCborToNative = (
  txCanonicalCbor: Uint8Array,
): NormalizedSubmitTx => {
  const decoded = decodeSubmitTxCanonicalCbor(txCanonicalCbor);
  if (!decoded.ok) return decoded;
  const txId = decoded.submittedTx.ledgerTx.txId;
  return {
    ok: true,
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor: decoded.submittedTx.txCbor,
    source: "native",
  };
};
