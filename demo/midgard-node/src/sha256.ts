import { createHash } from "node:crypto";

/**
 * SHA-256 primitives shared across the node.
 *
 * These exist so the ~dozen modules that need a digest stop redeclaring the
 * same `createHash("sha256")` one-liner. Strings are hashed as UTF-8, matching
 * Node's default `Hash.update` encoding.
 */
export const sha256 = (value: string | Uint8Array): Buffer =>
  createHash("sha256").update(value).digest();

export const sha256Hex = (value: string | Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");
