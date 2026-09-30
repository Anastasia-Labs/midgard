import { createHash, createHmac } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterEach } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";

export const hex32 = (byte: string): string => byte.repeat(32);

export const authenticationKey = Uint8Array.from(
  { length: 32 },
  (_, index) => index,
);

export const recordAuthenticationKey = Uint8Array.from(
  { length: 32 },
  (_, index) => 255 - index,
);

export const policy = (): WatcherFinalityPolicy => {
  const value = makeWatcherFinalityPolicy(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "external_providers",
          providers: [
            {
              identity: "provider-a",
              operatorIdentitySha256: hex32("11"),
              endpoint: "https://provider-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: hex32("22"),
              endpoint: "https://provider-b.example",
            },
          ],
        },
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: 30,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 30,
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
    },
    {
      manifestId: hex32("33"),
      network: "Preprod",
      trustRootId: hex32("44"),
      fundingProfileBundleDigest: "ab".repeat(32),
      blueprintHash: hex32("55"),
      ruleBundleCommitment: hex32("66"),
      programCommitments: { validation: hex32("77") },
      durableMarker: makeDeploymentMarker(hex32("33")),
    },
  );
  if (value === null) throw new Error("test finality policy was rejected");
  return value;
};

export const head = (
  finalityPolicy: WatcherFinalityPolicy,
  revision: number,
  byte: string,
): WatcherRollbackDurableTrustedHead => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: finalityPolicy.policyDigest,
    deploymentMarker: finalityPolicy.deploymentMarker,
    authenticationKeyId: createHash("sha256")
      .update(authenticationKey)
      .digest("hex"),
    revision: revision.toString(),
    snapshotSha256: hex32(byte),
    authorityDigest: hex32(
      (Number.parseInt(byte, 16) + 1).toString(16).padStart(2, "0"),
    ),
  };
  return Object.freeze({
    ...canonical,
    headMac: createHmac("sha256", authenticationKey)
      .update(
        `${WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION}:${watcherCanonicalJson(canonical)}`,
        "utf8",
      )
      .digest("hex"),
  });
};

const directories: string[] = [];

export const directory = async (): Promise<string> => {
  const value = await mkdtemp("/var/tmp/midgard-trusted-head-");
  directories.push(value);
  return value;
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { recursive: true })),
  );
});
