/**
 * The wire format operators use to share pending L2 transactions.
 *
 * Both peer routes take and return a canonical CBOR array of byte strings:
 * transaction bytes for `/peer/txs`, 32-byte transaction ids for
 * `/peer/inventory`. Each request carries the sender's operator verification
 * key and its Ed25519 signature over {@link peerRequestDigest}, which binds
 * the route, the deployment and the exact body.
 */
import {
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";

import { sha256 } from "../sha256.js";

export const PEER_MEDIA_TYPE = "application/cbor";
export const PEER_KEY_HEADER = "x-midgard-operator-key";
export const PEER_SIGNATURE_HEADER = "x-midgard-operator-signature";
export const PEER_TXS_PATH = "/peer/txs";
export const PEER_INVENTORY_PATH = "/peer/inventory";

/**
 * Most ids one inventory request may carry. The answer lists at most as many
 * 34-byte CBOR items, which stays under the 1 MiB HTTP response cap.
 */
export const PEER_INVENTORY_MAX_IDS = 20_000;

/** Largest inventory request body: the array header plus every id item. */
export const PEER_INVENTORY_MAX_BODY_BYTES = 5 + PEER_INVENTORY_MAX_IDS * 34;

/** CBOR head bytes counted per list and per item: the largest head used. */
export const PEER_CBOR_HEAD_BYTES = 5;

/**
 * Largest `/peer/txs` body: one largest transaction with its list and item
 * heads. A sender packs several smaller ones up to the same size.
 */
export const PEER_TXS_MAX_BODY_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes + 2 * PEER_CBOR_HEAD_BYTES;

/** Largest body either peer route takes. */
export const PEER_MAX_BODY_BYTES = Math.max(
  PEER_TXS_MAX_BODY_BYTES,
  PEER_INVENTORY_MAX_BODY_BYTES,
);

const DOMAIN = Buffer.from("midgard/l2-peer/v1\0", "utf8");

/**
 * What the operator key signs: SHA-256 over the domain tag, the route path,
 * the deployment's hub-oracle policy id and the body. A signature for one
 * route, deployment or body is worthless for any other.
 */
export const peerRequestDigest = (
  path: string,
  deploymentPolicyIdHex: string,
  body: Uint8Array,
): Buffer =>
  sha256(
    Buffer.concat([
      DOMAIN,
      Buffer.from(`${path}\0`, "utf8"),
      Buffer.from(deploymentPolicyIdHex, "hex"),
      body,
    ]),
  );

export type PeerSigner = {
  readonly verificationKeyHex: string;
  readonly sign: (message: Uint8Array) => string;
};

/** The two authentication headers for a request body sent to `path`. */
export const peerAuthHeaders = (
  signer: PeerSigner,
  path: string,
  deploymentPolicyIdHex: string,
  body: Uint8Array,
): Record<string, string> => ({
  [PEER_KEY_HEADER]: signer.verificationKeyHex,
  [PEER_SIGNATURE_HEADER]: signer.sign(
    peerRequestDigest(path, deploymentPolicyIdHex, body),
  ),
});

/**
 * The key hash of a correctly signed request, or null when either header is
 * missing or malformed or the signature does not verify.
 */
export const verifyPeerRequest = (
  headers: Readonly<Record<string, string | undefined>>,
  path: string,
  deploymentPolicyIdHex: string,
  body: Uint8Array,
): string | null => {
  const keyHex = headers[PEER_KEY_HEADER] ?? "";
  const signatureHex = headers[PEER_SIGNATURE_HEADER] ?? "";
  if (!/^[0-9a-f]{64}$/.test(keyHex) || !/^[0-9a-f]{128}$/.test(signatureHex))
    return null;
  try {
    const key = CML.PublicKey.from_bytes(Buffer.from(keyHex, "hex"));
    const signature = CML.Ed25519Signature.from_raw_bytes(
      Buffer.from(signatureHex, "hex"),
    );
    return key.verify(
      peerRequestDigest(path, deploymentPolicyIdHex, body),
      signature,
    )
      ? key.hash().to_hex()
      : null;
  } catch {
    return null;
  }
};

export const encodePeerList = (items: readonly Uint8Array[]): Buffer =>
  encodeCbor(items.map((item) => Buffer.from(item)));

/**
 * Decodes a canonical CBOR array of at most `maxItems` byte strings. Throws
 * on anything else, including a non-canonical encoding.
 */
export const decodePeerList = (
  bytes: Uint8Array,
  maxItems: number,
): readonly Buffer[] => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "peer_list");
  if (header.length > maxItems)
    throw new Error(`peer list holds more than ${maxItems.toString()} items`);
  const items: Buffer[] = [];
  let offset = header.nextOffset;
  for (let index = 0; index < header.length; index++) {
    const item = readCborBytes(source, offset, "peer_list.item");
    items.push(item.value);
    offset = item.nextOffset;
  }
  if (offset !== source.length) throw new Error("peer list has trailing bytes");
  if (!encodePeerList(items).equals(source))
    throw new Error("peer list CBOR is not canonical");
  return items;
};
