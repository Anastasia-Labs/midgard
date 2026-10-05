import { createHash } from "node:crypto";
import { createServer } from "node:http";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  createWatcherTrustedHeadAuthorityClient,
  makeWatcherFinalityPolicy,
  parseWatcherConfig,
  WATCHER_CONFIG_SCHEMA_VERSION,
} from "midgard-watcher";
import {
  authenticationKey,
  head,
  recordAuthenticationKey,
} from "midgard-watcher/tests/runtime/trusted-head-authority.policy";
import { afterEach, expect, it, vi } from "vitest";

import { authorityReadinessProbe } from "../src/devnet-stack/authority-readiness.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";

let port = 0;
let missingConfig = false;
let releaseMismatch = false;
let policyMismatch = false;
let deploymentMismatch = false;
const fixtureHex = (byte: string) => byte.repeat(32);
const admittedPolicy = makeWatcherFinalityPolicy(
  parseWatcherConfig({
    schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
    mode: "acceptance",
    targetNetwork: "Custom",
    customNetwork: {
      networkMagic: 42,
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
    },
    l1: {
      source: {
        sourceMode: "local_node",
        authorityNodeId: "synthetic-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/var/lib/synthetic/node.socket",
          nodeConfigPath: "/var/lib/synthetic/config.json",
          genesisConfigPath: "/var/lib/synthetic/genesis.json",
          genesisIdentitySha256: fixtureHex("11"),
        },
        queryServices: [
          {
            kind: "kupo",
            identity: "synthetic-kupo",
            endpoint: "http://127.0.0.1:2442",
          },
          {
            kind: "ogmios",
            identity: "synthetic-query",
            endpoint: "http://127.0.0.1:2337",
          },
        ],
      },
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
      finality: {
        depth: 10,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: 10,
        },
      },
    },
    da: {
      peers: [
        {
          identity: "da-peer-a",
          multiaddr:
            "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ],
      requestTimeoutMs: 10_000,
      maxConcurrency: 4,
    },
    storage: {
      driver: "sqlite",
      path: "/var/lib/midgard-watcher/watcher.sqlite",
      rollbackAuthorityKeySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
      },
    },
    proverWallet: {
      keySource: {
        kind: "environment",
        variable: "MIDGARD_WATCHER_PROVER_KEY",
      },
    },
    deadlines: {
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    },
  }),
  {
    manifestId: fixtureHex("33"),
    network: "Custom",
    trustRootId: fixtureHex("44"),
    fundingProfileBundleDigest: "ab".repeat(32),
    blueprintHash: fixtureHex("55"),
    ruleBundleCommitment: fixtureHex("66"),
    programCommitments: { validation: fixtureHex("77") },
    durableMarker: makeDeploymentMarker(fixtureHex("33")),
  },
);
if (admittedPolicy === null) throw new Error("synthetic policy rejected");
vi.mock("../src/devnet-stack/layout.js", async (original) => ({
  ...(await original<typeof import("../src/devnet-stack/layout.js")>()),
  servicePorts: () => ({ watcherAuthority: port }),
}));
vi.mock("../src/devnet-stack/watcher-release.js", () => ({
  readFinalizedManifest: () => ({
    manifestId: (deploymentMismatch ? "44" : "33").repeat(32),
  }),
  loadWatcherModule: async () => ({
    loadWatcherProcessConfigFile: async () => {
      if (missingConfig) throw new Error("missing recorded config");
      return {
        deploymentAuthorityPath: "/synthetic/release",
        ruleBundlePath: "/synthetic/rules",
        watcherConfig: { storage: { rollbackAuthorityKeySource: "rollback" } },
        trustedHeadAuthorityEndpoint: `http://127.0.0.1:${port}`,
        httpBearerSecretSource: "bearer",
      };
    },
    loadWatcherTrustedHeadAuthorityProcessConfigFile: async () => ({
      policy: policyMismatch
        ? { ...admittedPolicy, policyDigest: "00".repeat(32) }
        : admittedPolicy,
      endpoint: `http://127.0.0.1:${port}`,
      recordAuthenticationKeySource: "record",
    }),
    loadWatcherVerifiedDeploymentAuthority: async () => {
      if (releaseMismatch) throw new Error("signed release mismatch");
      return { deploymentIdentity: {} };
    },
    makeWatcherFinalityPolicy: () => admittedPolicy,
    watcherDeploymentReleaseFinalityAuthority: () => ({
      verifyForWorkflow: async () => ({ policy: { confirmationDepth: 10 } }),
    }),
    loadWatcherSecretText: async (source: string) =>
      source === "record"
        ? Buffer.from(recordAuthenticationKey).toString("hex")
        : source === "rollback"
          ? Buffer.from(authenticationKey).toString("hex")
          : "synthetic-bearer-for-isolated-tests-only",
    decodeWatcherAuthenticationKey32: (text: string) =>
      Uint8Array.from(Buffer.from(text, "hex")),
    decodeWatcherHttpBearerSecret: (text: string) => text,
    createWatcherTrustedHeadAuthorityClient,
  }),
}));
const run: RunEnv = {
  runId: "synthetic",
  composeProject: "synthetic",
  networkMagic: 42,
  ogmiosPort: 2337,
  kupoPort: 2442,
  postgresPort: 5432,
  postgresUser: "synthetic",
  postgresPassword: "synthetic",
  postgresDatabase: "synthetic",
  cardanoImage: "synthetic",
  postgresImage: "synthetic",
  portOffset: 0,
};
afterEach(() => {
  missingConfig = false;
  releaseMismatch = false;
  policyMismatch = false;
  deploymentMismatch = false;
});

it.each([
  "null",
  "valid",
  "wrong-key",
  "poisoned",
  "auth-failure",
  "missing-config",
  "release-mismatch",
  "wrong-policy",
  "wrong-deployment",
])(
  "admits only authenticated exact recorded authority identity and valid current head: %s",
  async (mode) => {
    const requests: string[] = [];
    const server = createServer((req, res) => {
      requests.push(`${req.method} ${req.url}`);
      expect(req.headers.authorization).toBe(
        "Bearer synthetic-bearer-for-isolated-tests-only",
      );
      res.setHeader("content-type", "application/json");
      if (mode === "auth-failure") {
        res.writeHead(401);
        res.end("{}");
        return;
      }
      if (req.url === "/v1/identity") {
        res.end(
          JSON.stringify({
            recordAuthenticationKeyId:
              mode === "wrong-key"
                ? "ef".repeat(32)
                : createHash("sha256")
                    .update(recordAuthenticationKey)
                    .digest("hex"),
          }),
        );
      } else {
        const trusted = head(admittedPolicy, 1, "77");
        res.end(
          JSON.stringify({
            head:
              mode === "null"
                ? null
                : mode === "poisoned"
                  ? { ...trusted, headMac: "00".repeat(32) }
                  : trusted,
          }),
        );
      }
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("missing synthetic port");
    port = address.port;
    missingConfig = mode === "missing-config";
    releaseMismatch = mode === "release-mismatch";
    policyMismatch = mode === "wrong-policy";
    deploymentMismatch = mode === "wrong-deployment";
    try {
      const result = await authorityReadinessProbe(
        makeLayout("/tmp/synthetic-no-files"),
        run,
      )
        .check(1000)
        .catch((error) => {
          if (mode === "null" || mode === "valid" || mode === "wrong-key")
            throw error;
          return false;
        });
      expect(result).toBe(mode === "null" || mode === "valid");
      expect(requests.every((request) => request.startsWith("GET "))).toBe(
        true,
      );
      if (mode === "wrong-key") expect(requests).toEqual(["GET /v1/identity"]);
      if (
        missingConfig ||
        releaseMismatch ||
        policyMismatch ||
        deploymentMismatch
      )
        expect(requests).toEqual([]);
    } finally {
      await new Promise<void>((resolve) => server.close(() => resolve()));
    }
  },
);
