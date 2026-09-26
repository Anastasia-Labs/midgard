import { createHash } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it, vi } from "vitest";

import { makeWatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import {
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/process-config.js";
import type { WatcherTrustedHeadClientRuntime } from "../../src/runtime/trusted-head-runtime.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  type WatcherProtectedUserEventCheckpointRead,
} from "../../src/storage/durable-runtime.js";
import {
  openWatcherSqliteDurableBackend,
  type WatcherSqliteDurableBackend,
} from "../../src/storage/sqlite-durable-backend.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  type WatcherUserEventArchive,
} from "../../src/storage/user-event-checkpoint.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "../support/deployment-authority-fixture.js";

const boundary = vi.hoisted(() => ({
  capture: false,
  trusted: undefined as WatcherTrustedHeadClientRuntime | undefined,
  opened: undefined as WatcherSqliteDurableBackend | undefined,
  forwardedArchive: undefined as WatcherUserEventArchive | undefined,
  admitted: undefined as WatcherProtectedUserEventCheckpointRead | undefined,
  closed: vi.fn(),
  acquireL1: vi.fn(() => {
    throw new Error("test boundary after durable checkpoint admission");
  }),
}));

// These unrelated infrastructure doubles allow the real launcher to reach its
// storage boundary. They establish no release readiness or live L1 authority.
vi.mock("../../src/runtime/trusted-head-runtime.js", () => ({
  createWatcherTrustedHeadClientRuntime: async () => {
    if (boundary.trusted === undefined) throw new Error("missing test sidecar");
    return boundary.trusted;
  },
}));
vi.mock(
  "../../src/funding/workflow-funding-profile-overlay.js",
  async (load) => ({
    ...(await load<
      typeof import("../../src/funding/workflow-funding-profile-overlay.js")
    >()),
    loadWatcherWorkflowFundingProfileOverlay: async () => Object.freeze({}),
  }),
);
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-application.js")
    >();
  return {
    ...actual,
    createWatcherFaultProofApplication: () => ({
      installedCategories: actual.WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      assertStartupReady: async () => ({ ready: true }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    }),
  };
});
vi.mock("../../src/runtime/user-event-runtime.js", async (load) => ({
  ...(await load<typeof import("../../src/runtime/user-event-runtime.js")>()),
  createWatcherUserEventRuntime: boundary.acquireL1,
}));

// Both storage wrappers call the actual implementations, including SQLite,
// snapshot authentication, archive hashing and protected checkpoint admission.
vi.mock("../../src/storage/sqlite-durable-backend.js", async (load) => {
  const actual =
    await load<typeof import("../../src/storage/sqlite-durable-backend.js")>();
  return {
    ...actual,
    openWatcherSqliteDurableBackend: async (
      ...args: Parameters<typeof actual.openWatcherSqliteDurableBackend>
    ) => {
      const opened = await actual.openWatcherSqliteDurableBackend(...args);
      if (!boundary.capture) return opened;
      boundary.opened = opened;
      return Object.freeze({
        ...opened,
        close: () => {
          boundary.closed();
          opened.close();
        },
      });
    },
  };
});
vi.mock("../../src/storage/durable-runtime.js", async (load) => {
  const actual =
    await load<typeof import("../../src/storage/durable-runtime.js")>();
  return {
    ...actual,
    createWatcherDurableRuntime: async (
      ...args: Parameters<typeof actual.createWatcherDurableRuntime>
    ) => {
      if (boundary.capture)
        boundary.forwardedArchive = args[0].userEventArchive;
      const runtime = await actual.createWatcherDurableRuntime(...args);
      if (boundary.capture) {
        boundary.admitted =
          actual.readWatcherProtectedUserEventCheckpointReceipt(
            await actual.readWatcherProtectedUserEventCheckpoint(runtime),
          );
      }
      return runtime;
    },
  };
});

const directories: string[] = [];
const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);
const sha256 = (bytes: Uint8Array) =>
  createHash("sha256").update(bytes).digest("hex");

const fixture = async () => {
  const directory = await mkdtemp("/var/tmp/midgard-watcher-runtime-archive-");
  directories.push(directory);
  // Reuse the existing signed unit-authority fixture and actual file loader.
  // These bytes are unit release material, not a genuine deployment receipt.
  const construction = makeWatcherDeploymentAuthorityFixture();
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: construction.result.manifestId,
      network: construction.result.network,
      blueprintHash: construction.result.blueprintHash,
      programCommitments: construction.result.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  const deployment = makeWatcherDeploymentAuthorityFixture({
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
  });
  const configInput = {
    schemaVersion: WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "local_node",
          authorityNodeId: "watcher-node",
          chainSync: {
            kind: "cardano_node_socket",
            socketPath: "/run/cardano/node.socket",
            nodeConfigPath: "/etc/cardano/node-config.json",
            genesisConfigPath: "/etc/cardano/shelley-genesis.json",
            genesisIdentitySha256: h32(0x66),
          },
          queryServices: [
            {
              kind: "ogmios",
              identity: "local-ogmios",
              endpoint: "ws://127.0.0.1:1337",
            },
            {
              kind: "kupo",
              identity: "local-kupo",
              endpoint: "http://127.0.0.1:1442",
            },
          ],
        },
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
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
        path: join(directory, "watcher.sqlite"),
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
    watcherRuntimeConfigPath: join(directory, "watcher.json"),
    deploymentAuthorityPath: join(directory, "deployment-authority.json"),
    ruleBundlePath: join(directory, "rule-bundle.json"),
    fundingProfileBundlePath: join(directory, "funding-profiles.json"),
    nativeChainSyncBinaryPath: "/usr/local/bin/midgard-chain-sync",
    trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123",
    operationsEndpoint: "http://127.0.0.1:43124",
    httpBearerSecretSource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
    },
    workflowJournalDirectory: join(directory, "workflows"),
    availability: {
      keySource: { kind: "environment", variable: "WATCHER_AVAILABILITY_KEY" },
      journalPath: join(directory, "availability.sqlite"),
      minimumFundingLovelace: "100000000",
    },
    faultProofInfrastructure: {
      manifestPath: join(directory, "deployment-manifest.json"),
      blueprintPath: join(directory, "plutus.json"),
      deploymentInfoPath: join(directory, "contract-deployment-info.json"),
      historicalNativeScriptHistory: {
        sourceMode: "external_provider_quorum",
        consistencyPolicy: "exact_bytes_all_providers_v1",
        providers: [
          {
            sourceId: "history-a",
            operatorIdentitySha256: h32(0xa1),
            authorityEndpoint: "https://history-a.example.test",
          },
          {
            sourceId: "history-b",
            operatorIdentitySha256: h32(0xb2),
            authorityEndpoint: "https://history-b.example.test",
          },
        ],
      },
    },
  };
  const config = parseWatcherProcessConfig(configInput);
  await Promise.all([
    writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify(configInput.watcherConfig),
    ),
    writeFile(
      config.deploymentAuthorityPath,
      JSON.stringify({
        signedIdentity: deployment.signedIdentity,
        policy: deployment.policy,
        trustRoots: deployment.trustRoots,
        durableMarker: deployment.marker,
      }),
    ),
    writeFile(config.ruleBundlePath, JSON.stringify(ruleBundle)),
    // The mocked L1 acquisition stops before these unit bytes are interpreted.
    writeFile(config.faultProofInfrastructure.blueprintPath, "{}"),
  ]);
  const admittedDeployment = await loadWatcherVerifiedDeploymentAuthority({
    path: config.deploymentAuthorityPath,
    ruleBundlePath: config.ruleBundlePath,
  });
  const policy = makeWatcherFinalityPolicy(
    config.watcherConfig,
    admittedDeployment.deploymentIdentity,
  );
  if (policy === null) throw new Error("invalid unit finality policy");
  let head: WatcherRollbackDurableTrustedHead | null = null;
  boundary.trusted = {
    rollbackAuthenticationKey: key,
    rollbackAuthenticationKeyId: sha256(key),
    recordAuthenticationKeyId: h32(0x99),
    client: {
      readRecordAuthenticationKeyId: async () => h32(0x99),
      readCurrent: async () => head,
      compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
        if (JSON.stringify(expectedTrustedHead) !== JSON.stringify(head))
          return false;
        head = nextTrustedHead;
        return true;
      },
    },
  };
  const sqlite = await openWatcherSqliteDurableBackend({
    path: config.watcherConfig.storage.path,
  });
  try {
    const durable = await createWatcherDurableRuntime({
      backend: sqlite.backend,
      userEventArchive: sqlite.userEventArchive,
      policy,
      authenticationKey: key,
      client: boundary.trusted.client,
    });
    const payload = new TextEncoder().encode('{"cursor":null}');
    const payloadDigest = await sqlite.userEventArchive.put(payload);
    const frame = makeWatcherUserEventCheckpoint({
      schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
      deploymentMarker: policy.deploymentMarker,
      network: policy.network,
      blueprintHash: policy.blueprintHash,
      finalityPolicyDigest: policy.policyDigest,
      userEventPolicyDigest: h32(0x88),
      checkpointSequence: "0",
      predecessorCheckpointDigest: null,
      rollbackGeneration: "0",
      payloadDigest,
      requiredArchiveDigests: [payloadDigest],
    });
    await persistWatcherUserEventCheckpoint(durable, {
      expectedCheckpointDigest: null,
      expectedCheckpointSequence: null,
      nextCheckpoint: frame,
    });
    return { config, frame, payload, head: () => head };
  } finally {
    sqlite.close();
  }
};

afterEach(async () => {
  boundary.capture = false;
  boundary.trusted = undefined;
  boundary.opened = undefined;
  boundary.forwardedArchive = undefined;
  boundary.admitted = undefined;
  boundary.closed.mockClear();
  boundary.acquireL1.mockClear();
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

describe("launcher checkpoint admission with signed unit release fixtures", () => {
  it("resumes the actual archived checkpoint before deliberate L1 acquisition stop and closes SQLite", async () => {
    const seeded = await fixture();
    const head = seeded.head();
    boundary.capture = true;
    await expect(
      createWatcherRuntime({ config: seeded.config }),
    ).rejects.toThrow("test boundary after durable checkpoint admission");
    expect(boundary.forwardedArchive).toBe(boundary.opened?.userEventArchive);
    expect(boundary.admitted).toMatchObject({
      checkpoint: seeded.frame,
      payload: seeded.payload,
      trustedHead: head,
    });
    expect(seeded.head()).toEqual(head);
    expect(boundary.acquireL1).toHaveBeenCalledOnce();
    expect(boundary.closed).toHaveBeenCalledOnce();
    await expect(
      boundary.opened!.userEventArchive.read(seeded.frame.payloadDigest),
    ).rejects.toThrow();
  });

  it.each(["missing", "corrupt"] as const)(
    "refuses %s archive bytes before L1 acquisition and closes SQLite",
    async (damage) => {
      const seeded = await fixture();
      const connection = new DatabaseSync(
        seeded.config.watcherConfig.storage.path,
      );
      try {
        if (damage === "missing") {
          connection
            .prepare(
              "DELETE FROM watcher_user_event_archive_v1 WHERE digest = ?",
            )
            .run(seeded.frame.payloadDigest);
        } else {
          connection
            .prepare(
              "UPDATE watcher_user_event_archive_v1 SET bytes = ? WHERE digest = ?",
            )
            .run(
              new TextEncoder().encode("damaged archive bytes"),
              seeded.frame.payloadDigest,
            );
        }
      } finally {
        connection.close();
      }
      boundary.capture = true;
      await expect(
        createWatcherRuntime({ config: seeded.config }),
      ).rejects.toThrow(
        damage === "missing"
          ? "required archive record is missing"
          : "digest mismatch",
      );
      expect(boundary.forwardedArchive).toBe(boundary.opened?.userEventArchive);
      expect(boundary.admitted).toBeUndefined();
      expect(boundary.acquireL1).not.toHaveBeenCalled();
      expect(boundary.closed).toHaveBeenCalledOnce();
      // A valid backend read must fail after closure, independent of Node's
      // SQLite error wording.
      await expect(boundary.opened!.backend.read()).rejects.toThrow();
    },
  );
});
