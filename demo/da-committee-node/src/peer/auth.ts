import { createHash, randomBytes } from "node:crypto";

import type { DaCommitteeValidation, DaSigner } from "../signer.js";
import { verifyEd25519Signature } from "../signer.js";
import { normalizeHex, timingSafeHexEqual } from "../utils/hex.js";

export const PEER_HTTP_AUTH_PREFIX = "MidgardDAPeerHttpV1";

export const PEER_AUTH_HEADERS = {
  signerIndex: "x-midgard-da-signer-index",
  timestampMs: "x-midgard-da-timestamp-ms",
  nonce: "x-midgard-da-nonce",
  bodySha256: "x-midgard-da-body-sha256",
  signature: "x-midgard-da-signature",
} as const;

export type PeerHttpAuthFields = {
  readonly signerIndex: number;
  readonly timestampMs: number;
  readonly nonce: string;
  readonly bodySha256: string;
  readonly signature: string;
};

export const signPeerRequest = ({
  signer,
  signerIndex,
  deploymentFingerprint,
  method,
  pathAndSearch,
  body,
  timestampMs = Date.now(),
  nonce = randomBytes(16).toString("hex"),
}: {
  readonly signer: DaSigner;
  readonly signerIndex: number;
  readonly deploymentFingerprint: string;
  readonly method: string;
  readonly pathAndSearch: string;
  readonly body: Buffer;
  readonly timestampMs?: number;
  readonly nonce?: string;
}): Record<string, string> => {
  const bodySha256 = sha256Hex(body);
  const message = peerHttpAuthMessage({
    deploymentFingerprint,
    method,
    pathAndSearch,
    bodySha256,
    timestampMs,
    nonce,
    signerIndex,
  });
  return {
    [PEER_AUTH_HEADERS.signerIndex]: signerIndex.toString(),
    [PEER_AUTH_HEADERS.timestampMs]: timestampMs.toString(),
    [PEER_AUTH_HEADERS.nonce]: nonce,
    [PEER_AUTH_HEADERS.bodySha256]: bodySha256,
    [PEER_AUTH_HEADERS.signature]: signer.sign(message).toString("hex"),
  };
};

export const verifyPeerRequestAuth = ({
  headers,
  signerValidation,
  deploymentFingerprint,
  method,
  pathAndSearch,
  body,
  nowMs = Date.now(),
  replayWindowMs,
}: {
  readonly headers: HeaderLookup;
  readonly signerValidation: DaCommitteeValidation;
  readonly deploymentFingerprint: string;
  readonly method: string;
  readonly pathAndSearch: string;
  readonly body: Buffer;
  readonly nowMs?: number;
  readonly replayWindowMs: number;
}):
  | { readonly ok: true; readonly fields: PeerHttpAuthFields }
  | { readonly ok: false; readonly error: string } => {
  const signerIndexHeader = getHeader(headers, PEER_AUTH_HEADERS.signerIndex);
  const timestampHeader = getHeader(headers, PEER_AUTH_HEADERS.timestampMs);
  const nonce = getHeader(headers, PEER_AUTH_HEADERS.nonce);
  const bodySha256Header = getHeader(headers, PEER_AUTH_HEADERS.bodySha256);
  const signatureHeader = getHeader(headers, PEER_AUTH_HEADERS.signature);
  if (
    signerIndexHeader === undefined ||
    timestampHeader === undefined ||
    nonce === undefined ||
    bodySha256Header === undefined ||
    signatureHeader === undefined
  ) {
    return { ok: false, error: "missing peer authentication headers" };
  }
  const signerIndex = Number(signerIndexHeader);
  if (
    !Number.isSafeInteger(signerIndex) ||
    signerIndex < 0 ||
    signerIndex > 255
  ) {
    return { ok: false, error: "peer signer index must fit in one byte" };
  }
  if (signerIndex >= signerValidation.committeeKeys.length) {
    return {
      ok: false,
      error: "peer signer index is outside the DA committee",
    };
  }
  const timestampMs = Number(timestampHeader);
  if (!Number.isSafeInteger(timestampMs) || timestampMs < 0) {
    return {
      ok: false,
      error: "peer timestamp must be a non-negative integer",
    };
  }
  if (Math.abs(nowMs - timestampMs) > replayWindowMs) {
    return {
      ok: false,
      error: "peer request timestamp is outside replay window",
    };
  }
  if (!/^[0-9a-fA-F]{16,128}$/.test(nonce)) {
    return { ok: false, error: "peer nonce must be 8-64 bytes of hex" };
  }
  let bodySha256: string;
  let signature: string;
  try {
    bodySha256 = normalizeHex(bodySha256Header, {
      fieldName: "peer body sha256",
      byteLength: 32,
    });
    signature = normalizeHex(signatureHeader, {
      fieldName: "peer request signature",
      byteLength: 64,
    });
  } catch (error) {
    return {
      ok: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
  if (!timingSafeHexEqual(bodySha256, sha256Hex(body))) {
    return { ok: false, error: "peer body hash does not match request body" };
  }
  const message = peerHttpAuthMessage({
    deploymentFingerprint,
    method,
    pathAndSearch,
    bodySha256,
    timestampMs,
    nonce,
    signerIndex,
  });
  const verified = verifyEd25519Signature({
    publicKeyHex: signerValidation.committeeKeys[signerIndex]!,
    message,
    signatureHex: signature,
  });
  return verified
    ? {
        ok: true,
        fields: {
          signerIndex,
          timestampMs,
          nonce,
          bodySha256,
          signature,
        },
      }
    : { ok: false, error: "peer request signature verification failed" };
};

export const peerHttpAuthMessage = ({
  deploymentFingerprint,
  method,
  pathAndSearch,
  bodySha256,
  timestampMs,
  nonce,
  signerIndex,
}: {
  readonly deploymentFingerprint: string;
  readonly method: string;
  readonly pathAndSearch: string;
  readonly bodySha256: string;
  readonly timestampMs: number;
  readonly nonce: string;
  readonly signerIndex: number;
}): Buffer =>
  Buffer.from(
    [
      PEER_HTTP_AUTH_PREFIX,
      deploymentFingerprint,
      method.toUpperCase(),
      pathAndSearch,
      bodySha256,
      timestampMs.toString(),
      nonce,
      signerIndex.toString(),
    ].join("\n"),
    "utf8",
  );

export const sha256Hex = (body: Buffer): string =>
  createHash("sha256").update(body).digest("hex");

type HeaderLookup =
  | Headers
  | Record<string, string | string[] | undefined>
  | NodeJS.Dict<string | string[]>;

const getHeader = (headers: HeaderLookup, name: string): string | undefined => {
  if (headers instanceof Headers) {
    return headers.get(name) ?? undefined;
  }
  const value = headers[name] ?? headers[name.toLowerCase()];
  if (Array.isArray(value)) {
    return value[0];
  }
  return value;
};
