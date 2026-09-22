import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { peerIdFromString } from "@libp2p/peer-id";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  identityFromSeedHex,
  loadDaLibp2pIdentity,
} from "../src/da-libp2p-identity.js";

const SEED_HEX = "5a".repeat(32);

describe("DA libp2p identity", () => {
  let directory: string;

  beforeAll(async () => {
    directory = await mkdtemp(join(tmpdir(), "midgard-da-identity-"));
  });

  afterAll(async () => {
    await rm(directory, { force: true, recursive: true });
  });

  it("derives one peer id from a seed however the seed is spelled", async () => {
    const fromSeed = await identityFromSeedHex(SEED_HEX);
    const fromSource = await loadDaLibp2pIdentity(`seed:${SEED_HEX}`);
    const fromPaddedUpper = await loadDaLibp2pIdentity(
      `  seed:${SEED_HEX.toUpperCase()}  `,
    );
    expect(fromSource.peerId).toBe(fromSeed.peerId);
    expect(fromPaddedUpper.peerId).toBe(fromSeed.peerId);
    expect(fromSeed.privateKey.type).toBe("Ed25519");
    expect(peerIdFromString(fromSeed.peerId).toString()).toBe(fromSeed.peerId);
  });

  it("round-trips the protobuf hex through the hex: and file: schemes", async () => {
    const fromSeed = await identityFromSeedHex(SEED_HEX);
    const fromHex = await loadDaLibp2pIdentity(
      `hex:${fromSeed.privateKeyProtobufHex}`,
    );
    const path = join(directory, "identity.hex");
    await writeFile(path, `${fromSeed.privateKeyProtobufHex}\n`, "utf8");
    const fromFile = await loadDaLibp2pIdentity(`file:${path}`);
    expect(fromHex.peerId).toBe(fromSeed.peerId);
    expect(fromFile.peerId).toBe(fromSeed.peerId);
    expect(fromHex.privateKeyProtobufHex).toBe(fromSeed.privateKeyProtobufHex);
    expect(fromFile.privateKeyProtobufHex).toBe(fromSeed.privateKeyProtobufHex);
  });

  it("rejects an unknown scheme, a short seed, odd hex, and an empty path", async () => {
    await expect(loadDaLibp2pIdentity(`raw:${SEED_HEX}`)).rejects.toThrow(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE must use seed:, hex:, or file:",
    );
    await expect(loadDaLibp2pIdentity("seed:5a5a")).rejects.toThrow(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE seed must be a 32-byte hex string",
    );
    await expect(loadDaLibp2pIdentity("hex:abc")).rejects.toThrow(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE protobuf key must be an even-length hex string",
    );
    await expect(loadDaLibp2pIdentity("file:   ")).rejects.toThrow(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE file path is empty",
    );
  });
});
