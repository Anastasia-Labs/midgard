import { readFile } from "node:fs/promises";

import {
  generateKeyPairFromSeed,
  privateKeyFromProtobuf,
  privateKeyToProtobuf,
} from "@libp2p/crypto/keys";
import type { PrivateKey } from "@libp2p/interface";
import { peerIdFromPrivateKey } from "@libp2p/peer-id";

import { bytesToHex, hexToBytes } from "../../utils/hex.js";

export type DaLibp2pIdentity = {
  readonly privateKey: PrivateKey;
  readonly peerId: string;
  readonly privateKeyProtobufHex: string;
};

export const loadDaLibp2pIdentity = async (
  source: string,
): Promise<DaLibp2pIdentity> => {
  const privateKey = await privateKeyFromSource(source);
  return identityFromPrivateKey(privateKey);
};

export const identityFromSeedHex = async (
  seedHex: string,
): Promise<DaLibp2pIdentity> => {
  const seed = hexToBytes(seedHex, "DA_LIBP2P_PRIVATE_KEY_SOURCE seed", 32);
  const privateKey = await generateKeyPairFromSeed("Ed25519", seed);
  return identityFromPrivateKey(privateKey);
};

const identityFromPrivateKey = (privateKey: PrivateKey): DaLibp2pIdentity => ({
  privateKey,
  peerId: peerIdFromPrivateKey(privateKey).toString(),
  privateKeyProtobufHex: bytesToHex(privateKeyToProtobuf(privateKey)),
});

const privateKeyFromSource = async (source: string): Promise<PrivateKey> => {
  const trimmed = source.trim();
  if (trimmed.startsWith("seed:")) {
    return (await identityFromSeedHex(trimmed.slice("seed:".length)))
      .privateKey;
  }
  if (trimmed.startsWith("hex:")) {
    return privateKeyFromProtobuf(
      hexToBytes(
        trimmed.slice("hex:".length),
        "DA_LIBP2P_PRIVATE_KEY_SOURCE protobuf key",
      ),
    );
  }
  if (trimmed.startsWith("file:")) {
    const fileText = await readFile(trimmed.slice("file:".length), "utf8");
    return privateKeyFromProtobuf(
      hexToBytes(fileText.trim(), "DA_LIBP2P_PRIVATE_KEY_SOURCE file"),
    );
  }
  throw new Error(
    "DA_LIBP2P_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
  );
};
