/**
 * The node identity a watcher admits before it follows the chain: the node
 * config must name the configured Shelley genesis, whose network magic,
 * Custom slot clock and digest match the watcher configuration. Each
 * refusal is exercised over in-memory files.
 */
import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  deriveWatcherNativeGenesisIdentity,
  type WatcherNativeNodeConfig,
} from "../../src/l1/native-chain-sync.derive-watcher-native-genesis-identity.js";

const NODE_CONFIG = "/etc/cardano/config.json";
const GENESIS = "/etc/cardano/shelley-genesis.json";
const ZERO_TIME = Date.parse("2026-01-01T00:00:00Z");

type Files = { nodeConfig: unknown; genesis: unknown };
type Network = "Preprod" | "Custom";

const encode = (value: unknown): Uint8Array =>
  typeof value === "string" || value instanceof Uint8Array
    ? Buffer.from(value as string)
    : Buffer.from(JSON.stringify(value));

const valid = (network: Network): Files => ({
  nodeConfig: {
    ShelleyGenesisFile: "shelley-genesis.json",
    ...(network === "Custom"
      ? { TestShelleyHardForkAtEpoch: 0, TestConwayHardForkAtEpoch: 0 }
      : {}),
  },
  genesis: {
    networkMagic: network === "Custom" ? 42 : 1,
    ...(network === "Custom"
      ? {
          systemStart: new Date(ZERO_TIME).toISOString(),
          slotLength: 1,
          networkId: "Testnet",
        }
      : {}),
  },
});

const derive = async (network: Network, files: Files, digest?: string) => {
  const genesisBytes = encode(files.genesis);
  const watcherConfig = {
    targetNetwork: network,
    ...(network === "Custom"
      ? {
          customNetwork: {
            networkMagic: 42,
            slotConfig: { zeroTime: ZERO_TIME, zeroSlot: 0, slotLength: 1000 },
          },
        }
      : {}),
    l1: {
      source: {
        chainSync: {
          nodeConfigPath: NODE_CONFIG,
          genesisConfigPath: GENESIS,
          genesisIdentitySha256:
            digest ?? createHash("sha256").update(genesisBytes).digest("hex"),
        },
      },
    },
  } as unknown as WatcherNativeNodeConfig;
  return await deriveWatcherNativeGenesisIdentity({
    watcherConfig,
    unsafeReadIdentityFileForTest: (path) =>
      Promise.resolve(
        path === NODE_CONFIG ? encode(files.nodeConfig) : genesisBytes,
      ),
  });
};

const edit = (
  network: Network,
  change: (files: {
    nodeConfig: Record<string, unknown>;
    genesis: Record<string, unknown>;
  }) => void,
): Files => {
  const files = valid(network) as {
    nodeConfig: Record<string, unknown>;
    genesis: Record<string, unknown>;
  };
  change(files);
  return files;
};

describe("deriveWatcherNativeGenesisIdentity", () => {
  it.each(["Preprod", "Custom"] as const)(
    "admits a %s node whose genesis matches the configuration",
    async (network) => {
      const files = valid(network);
      expect(await derive(network, files)).toEqual({
        genesisIdentitySha256: createHash("sha256")
          .update(encode(files.genesis))
          .digest("hex"),
        networkMagic: network === "Custom" ? 42 : 1,
      });
    },
  );

  it.each([
    [
      "an empty identity file",
      () => ({ nodeConfig: "", genesis: valid("Preprod").genesis }),
      "identity file size is invalid",
    ],
    [
      "an identity file that is not an object",
      () => ({ nodeConfig: [], genesis: valid("Preprod").genesis }),
      "identity file is not an object",
    ],
    [
      "a node config without ShelleyGenesisFile",
      () => edit("Preprod", (f) => delete f.nodeConfig.ShelleyGenesisFile),
      "does not declare ShelleyGenesisFile",
    ],
    [
      "a node config naming another genesis",
      () =>
        edit("Preprod", (f) => {
          f.nodeConfig.ShelleyGenesisFile = "other-genesis.json";
        }),
      "genesis path differs from watcher authority",
    ],
    [
      "a genesis of another network",
      () =>
        edit("Preprod", (f) => {
          f.genesis.networkMagic = 2;
        }),
      "network magic differs from watcher network",
    ],
  ])("refuses %s", async (_label, files, message) => {
    await expect(derive("Preprod", files())).rejects.toThrow(message);
  });

  it.each([
    [
      "another system start",
      (f: Files) =>
        ((f.genesis as Record<string, unknown>).systemStart =
          "2026-01-02T00:00:00Z"),
    ],
    [
      "another slot length",
      (f: Files) => ((f.genesis as Record<string, unknown>).slotLength = 2),
    ],
    [
      "a mainnet genesis",
      (f: Files) =>
        ((f.genesis as Record<string, unknown>).networkId = "Mainnet"),
    ],
    [
      "a later Shelley fork",
      (f: Files) =>
        ((f.nodeConfig as Record<string, unknown>).TestShelleyHardForkAtEpoch =
          1),
    ],
    [
      "a later Conway fork",
      (f: Files) =>
        ((f.nodeConfig as Record<string, unknown>).TestConwayHardForkAtEpoch =
          1),
    ],
  ])("refuses a Custom genesis with %s", async (_label, change) => {
    const files = valid("Custom");
    change(files);
    await expect(derive("Custom", files)).rejects.toThrow(
      "Custom network slot clock differs from epoch-zero testnet genesis",
    );
  });

  it("refuses a genesis whose digest differs from the configured identity", async () => {
    await expect(
      derive("Preprod", valid("Preprod"), "00".repeat(32)),
    ).rejects.toThrow(
      "node genesis identity differs from watcher configuration",
    );
  });
});
