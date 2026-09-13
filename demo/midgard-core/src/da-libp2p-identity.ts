import { readFile } from "node:fs/promises";

import {
  generateKeyPairFromSeed,
  privateKeyFromProtobuf,
  privateKeyToProtobuf,
} from "@libp2p/crypto/keys";
import type { PrivateKey } from "@libp2p/interface";
import { peerIdFromPrivateKey } from "@libp2p/peer-id";

import { hexToBytes } from "./hex.js";

/**
 * The DA libp2p identity loader shared by every process that speaks on the
 * DA network: the producer inside `midgard-node`, the committee node, and the
 * public retained-DA listener.
 *
 * `midgard-node` and `da-committee-node` each carried their own copy of this
 * parser. They had already drifted — one exposed the protobuf re-encoding the
 * committee node persists and the other did not, and their hex validation
 * disagreed on error wording — while both parsed the same
 * `DA_LIBP2P_PRIVATE_KEY_SOURCE` grammar. A peer id derived by one side must be
 * the peer id the other side expects, so the derivation lives once, here.
 *
 * The source grammar is `<scheme>:<payload>` with three schemes:
 *
 * - `seed:<32-byte hex>` — an Ed25519 seed; deterministic, used by tests and
 *   local devnets.
 * - `hex:<hex>` — a libp2p protobuf-encoded private key.
 * - `file:<path>` — a file holding the `hex:` payload without its prefix.
 */
export type DaLibp2pIdentity = {
  readonly privateKey: PrivateKey;
  readonly peerId: string;
  /** The key re-encoded as libp2p protobuf hex — the `hex:` payload form. */
  readonly privateKeyProtobufHex: string;
};

const SOURCE_VARIABLE = "DA_LIBP2P_PRIVATE_KEY_SOURCE";

export const loadDaLibp2pIdentity = async (
  source: string,
): Promise<DaLibp2pIdentity> =>
  identityFromPrivateKey(await privateKeyFromSource(source));

export const identityFromSeedHex = async (
  seedHex: string,
): Promise<DaLibp2pIdentity> =>
  identityFromPrivateKey(
    await generateKeyPairFromSeed(
      "Ed25519",
      hexToBytes(seedHex, {
        fieldName: `${SOURCE_VARIABLE} seed`,
        byteLength: 32,
      }),
    ),
  );

const identityFromPrivateKey = (privateKey: PrivateKey): DaLibp2pIdentity => ({
  privateKey,
  peerId: peerIdFromPrivateKey(privateKey).toString(),
  privateKeyProtobufHex: Buffer.from(privateKeyToProtobuf(privateKey)).toString(
    "hex",
  ),
});

const privateKeyFromSource = async (source: string): Promise<PrivateKey> => {
  const trimmed = source.trim();
  if (trimmed.startsWith("seed:")) {
    return (await identityFromSeedHex(trimmed.slice("seed:".length)))
      .privateKey;
  }
  if (trimmed.startsWith("hex:")) {
    return privateKeyFromProtobuf(
      hexToBytes(trimmed.slice("hex:".length), {
        fieldName: `${SOURCE_VARIABLE} protobuf key`,
      }),
    );
  }
  if (trimmed.startsWith("file:")) {
    const path = trimmed.slice("file:".length);
    if (path.trim().length === 0) {
      throw new Error(`${SOURCE_VARIABLE} file path is empty`);
    }
    return privateKeyFromProtobuf(
      hexToBytes((await readFile(path, "utf8")).trim(), {
        fieldName: `${SOURCE_VARIABLE} file`,
      }),
    );
  }
  throw new Error(`${SOURCE_VARIABLE} must use seed:, hex:, or file:`);
};
