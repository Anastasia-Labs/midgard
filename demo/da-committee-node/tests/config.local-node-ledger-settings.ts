import { describe, expect, it } from "vitest";

import {
  loadCommitteeConfig,
  parseNativeLedgerConfig,
  rejectRetiredWatcherEnvNames,
} from "../src/config.js";
import { tempDir } from "./helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "./helpers/committee-config-files.js";

describe("rejectRetiredWatcherEnvNames", () => {
  it("refuses the pre-split WATCHER_* names and points at the DA_COMMITTEE_* replacements", () => {
    expect(() =>
      rejectRetiredWatcherEnvNames({
        WATCHER_DATABASE_URL: "postgres://x",
        WATCHER_API_PORT: "8787",
        DA_COMMITTEE_API_HOST: "127.0.0.1",
      }),
    ).toThrow(
      /retired environment variable\(s\) WATCHER_API_PORT, WATCHER_DATABASE_URL: .*use DA_COMMITTEE_API_PORT, DA_COMMITTEE_DATABASE_URL/u,
    );
  });

  it("accepts an environment that uses only the current names", () => {
    expect(() =>
      rejectRetiredWatcherEnvNames({
        DA_COMMITTEE_DATABASE_URL: "postgres://committee",
        DA_COMMITTEE_API_PORT: "8787",
        WATCHER_UNRELATED_PREFIX_ELSEWHERE: "ignored",
      }),
    ).not.toThrow();
  });
});

describe("local node ledger settings", () => {
  const nativeLedgerEnv = {
    CARDANO_LOCAL_NODE_SOCKET_PATH: "/run/cardano/node.socket",
    CARDANO_LOCAL_NODE_CONFIG_PATH: "/etc/cardano/config.json",
    CARDANO_L1_NODE_TRANSPORT_BINARY_PATH:
      "/opt/midgard/midgard-l1-node-transport",
  } as const;

  it("is absent when none of the settings is present", () => {
    expect(parseNativeLedgerConfig({})).toBeUndefined();
    expect(
      parseNativeLedgerConfig({
        CARDANO_LOCAL_NODE_SOCKET_PATH: " ",
        CARDANO_LOCAL_NODE_CONFIG_PATH: "",
      }),
    ).toBeUndefined();
  });

  it("is all-or-none and names every missing setting", () => {
    expect(() =>
      parseNativeLedgerConfig({
        CARDANO_LOCAL_NODE_SOCKET_PATH:
          nativeLedgerEnv.CARDANO_LOCAL_NODE_SOCKET_PATH,
      }),
    ).toThrow(
      /all-or-none; missing CARDANO_LOCAL_NODE_CONFIG_PATH, CARDANO_L1_NODE_TRANSPORT_BINARY_PATH$/u,
    );
    expect(() =>
      parseNativeLedgerConfig({
        ...nativeLedgerEnv,
        CARDANO_LOCAL_NODE_CONFIG_PATH: undefined,
      }),
    ).toThrow(/all-or-none; missing CARDANO_LOCAL_NODE_CONFIG_PATH$/u);
  });

  it("requires absolute canonical paths", () => {
    for (const [name, value] of [
      ["CARDANO_LOCAL_NODE_SOCKET_PATH", "node.socket"],
      ["CARDANO_LOCAL_NODE_SOCKET_PATH", "./run/node.socket"],
      ["CARDANO_LOCAL_NODE_CONFIG_PATH", "/etc/cardano/../cardano/config.json"],
      ["CARDANO_LOCAL_NODE_CONFIG_PATH", "/etc//cardano/config.json"],
      ["CARDANO_L1_NODE_TRANSPORT_BINARY_PATH", "/opt/midgard/./chain-sync"],
      ["CARDANO_L1_NODE_TRANSPORT_BINARY_PATH", "/opt/midgard/"],
    ] as const) {
      expect(() =>
        parseNativeLedgerConfig({ ...nativeLedgerEnv, [name]: value }),
      ).toThrow(
        new RegExp(`^${name} must be an absolute canonical path$`, "u"),
      );
    }
  });

  it("binds the configured local-node authority id, defaulting when none is set", () => {
    expect(parseNativeLedgerConfig(nativeLedgerEnv)).toEqual({
      authorityNodeId: "local-cardano-node",
      socketPath: "/run/cardano/node.socket",
      nodeConfigPath: "/etc/cardano/config.json",
      binaryPath: "/opt/midgard/midgard-l1-node-transport",
    });
    expect(
      parseNativeLedgerConfig({
        ...nativeLedgerEnv,
        CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-a",
      }),
    ).toMatchObject({ authorityNodeId: "preview-node-a" });
    expect(() =>
      parseNativeLedgerConfig({
        ...nativeLedgerEnv,
        CARDANO_LOCAL_NODE_AUTHORITY_ID: "preview-node-",
      }),
    ).toThrow(
      /CARDANO_LOCAL_NODE_AUTHORITY_ID must be a native ledger authority id/u,
    );
  });

  it("is loaded all-or-none", async () => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const baseEnv = libp2pConfigEnv(manifestPath, deploymentInfoPath);

    expect((await loadCommitteeConfig(baseEnv)).nativeLedger).toBeUndefined();
    await expect(
      loadCommitteeConfig({ ...baseEnv, ...nativeLedgerEnv }),
    ).resolves.toMatchObject({
      nativeLedger: {
        authorityNodeId: "local-cardano-node",
        socketPath: nativeLedgerEnv.CARDANO_LOCAL_NODE_SOCKET_PATH,
      },
    });
    await expect(
      loadCommitteeConfig({
        ...baseEnv,
        CARDANO_L1_NODE_TRANSPORT_BINARY_PATH:
          nativeLedgerEnv.CARDANO_L1_NODE_TRANSPORT_BINARY_PATH,
      }),
    ).rejects.toThrow(/all-or-none/u);
  });
});
