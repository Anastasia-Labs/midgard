import { readFile } from "node:fs/promises";

import {
  generateKeyPairFromSeed,
  privateKeyFromProtobuf,
} from "@libp2p/crypto/keys";
import type { PrivateKey } from "@libp2p/interface";
import { peerIdFromPrivateKey } from "@libp2p/peer-id";

export type DaLibp2pIdentity = {
  readonly privateKey: PrivateKey;
  readonly peerId: string;
};

export const loadDaLibp2pIdentity = async (
  source: string,
): Promise<DaLibp2pIdentity> => {
  const privateKey = await privateKeyFromSource(source);
  return {
    privateKey,
    peerId: peerIdFromPrivateKey(privateKey).toString(),
  };
};

const privateKeyFromSource = async (source: string): Promise<PrivateKey> => {
  const trimmed = source.trim();
  if (trimmed.startsWith("seed:")) {
    return generateKeyPairFromSeed(
      "Ed25519",
      Buffer.from(
        normalizeHexBytes(
          trimmed.slice("seed:".length),
          "DA_LIBP2P_PRIVATE_KEY_SOURCE seed",
          32,
        ),
        "hex",
      ),
    );
  }
  if (trimmed.startsWith("hex:")) {
    return privateKeyFromProtobuf(
      Buffer.from(
        normalizeHexBytes(
          trimmed.slice("hex:".length),
          "DA_LIBP2P_PRIVATE_KEY_SOURCE protobuf key",
        ),
        "hex",
      ),
    );
  }
  if (trimmed.startsWith("file:")) {
    const path = trimmed.slice("file:".length);
    if (path.trim().length === 0) {
      throw new Error("DA_LIBP2P_PRIVATE_KEY_SOURCE file path is empty");
    }
    return privateKeyFromProtobuf(
      Buffer.from(
        normalizeHexBytes(
          (await readFile(path, "utf8")).trim(),
          "DA_LIBP2P_PRIVATE_KEY_SOURCE file",
        ),
        "hex",
      ),
    );
  }
  throw new Error(
    "DA_LIBP2P_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
  );
};

const normalizeHexBytes = (
  value: string,
  fieldName: string,
  byteLength?: number,
): string => {
  const normalized = value.trim().toLowerCase();
  if (!/^[0-9a-f]*$/.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(`${fieldName} must be even-length hex`);
  }
  if (byteLength !== undefined && normalized.length !== byteLength * 2) {
    throw new Error(`${fieldName} must be ${byteLength.toString()}-byte hex`);
  }
  return normalized;
};
