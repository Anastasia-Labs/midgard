import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-test-support/hex";
import "vitest";
import "../../src/l1/finality-engine.js";
import "../../src/runtime/config.js";
import "../../src/runtime/process-config.js";
import "../../src/runtime/trusted-head-runtime.js";
import "../../src/runtime/watcher-runtime.js";
import "../../src/storage/durable-store.js";
import "./process-config.watcher-config-value.js";

import { randomUUID } from "node:crypto";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it } from "vitest";

import {
  loadWatcherProcessConfigFile,
  loadWatcherTrustedHeadAuthorityProcessConfigFile,
  parseWatcherProcessConfig,
  parseWatcherTrustedHeadAuthorityProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
  type WatcherTrustedHeadAuthorityProcessConfig,
} from "../../src/runtime/process-config.js";
import { initializeSelectedAuthorityStore } from "../../src/runtime/trusted-head-authority.js";
import {
  createWatcherTrustedHeadClientRuntime,
  startWatcherTrustedHeadAuthorityProcess,
} from "../../src/runtime/trusted-head-runtime.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import {
  directories,
  policy,
  productionConfig,
  RELEASE_DEPTH,
  shippedTemplate,
  watcherConfigValue,
} from "./process-config.watcher-config-value.js";

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (directory) =>
        rm(directory, { force: true, recursive: true }),
      ),
  );
});

describe("production process authority separation", () => {
  it("loads the strict JSON process configuration with its required release artifact path", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-config-");
    directories.push(directory);
    const path = join(directory, "process.json");
    const config = productionConfig();
    await writeFile(
      path,
      JSON.stringify({ ...config, watcherConfig: watcherConfigValue() }),
    );
    await expect(loadWatcherProcessConfigFile(path)).resolves.toEqual(config);
  });

  it("admits only acceptance Preprod or Custom local-node watcher topology", () => {
    expect(productionConfig().watcherConfig.l1.source.sourceMode).toBe(
      "local_node",
    );
    const base = productionConfig();
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherConfig: { ...watcherConfigValue(), mode: "development" },
      }),
    ).toThrow("requires acceptance Preprod or Custom local_node authority");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherRollbackKeySource:
          base.watcherConfig.storage.rollbackAuthorityKeySource,
      }),
    ).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        readinessHeaderHash: h28(0x77),
      }),
    ).toThrow("unknown or missing fields");
    expect(() => {
      const { ruleBundlePath: _omitted, ...withoutBundle } = base;
      return parseWatcherProcessConfig(withoutBundle);
    }).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        ruleBundlePath: "etc/midgard/rule-bundle.json",
      }),
    ).toThrow("watcher release rule bundle is not a canonical production path");
    expect(() => {
      const { fundingProfileBundlePath: _omitted, ...withoutBundle } = base;
      return parseWatcherProcessConfig(withoutBundle);
    }).toThrow("unknown or missing fields");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        fundingProfileBundlePath: "etc/midgard/funding-profiles.json",
      }),
    ).toThrow(
      "watcher funding profile bundle is not a canonical production path",
    );
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        faultProofInfrastructure: {
          ...base.faultProofInfrastructure,
          historicalNativeScriptHistory: {
            ...base.faultProofInfrastructure.historicalNativeScriptHistory,
            providers: [
              base.faultProofInfrastructure.historicalNativeScriptHistory
                .providers[0]!,
              {
                ...base.faultProofInfrastructure.historicalNativeScriptHistory
                  .providers[1],
                operatorIdentitySha256:
                  base.faultProofInfrastructure.historicalNativeScriptHistory
                    .providers[0]!.operatorIdentitySha256,
              },
            ],
          },
        },
      }),
    ).toThrow("not independent");
  });

  it("rejects the deleted Midgard node lease coordination keys as unknown fields", () => {
    const base = productionConfig();
    for (const deleted of [
      { midgardNodeUrl: "http://127.0.0.1:3000" },
      {
        midgardNodeAdminKeySource: {
          kind: "environment",
          variable: "MIDGARD_NODE_ADMIN_KEY",
        },
      },
      { stateQueueLeaseTtlMs: 30_000 },
    ]) {
      expect(() =>
        parseWatcherProcessConfig({
          ...base,
          faultProofInfrastructure: {
            ...base.faultProofInfrastructure,
            ...deleted,
          },
        }),
      ).toThrow(
        "watcher fault-proof infrastructure has unknown or missing fields",
      );
    }
  });

  it("parses the shipped watcher-process.example.json template", async () => {
    // Parsed from its bytes rather than loaded by path: the loader refuses a
    // checkout under /tmp, and the template's location is not what is tested.
    const config = parseWatcherProcessConfig(
      await shippedTemplate("watcher-process.example.json"),
    );
    expect(config.schemaVersion).toBe(WATCHER_PROCESS_CONFIG_SCHEMA_VERSION);
    expect(Object.keys(config.faultProofInfrastructure).sort()).toEqual([
      "blueprintPath",
      "deploymentInfoPath",
      "historicalNativeScriptHistory",
      "manifestPath",
    ]);
  });

  it("parses the shipped authority.example.json template as the sidecar of the start template", async () => {
    const template = shippedTemplate;
    const authority = parseWatcherTrustedHeadAuthorityProcessConfig(
      await template("authority.example.json"),
    );
    const start = parseWatcherProcessConfig(
      await template("watcher-process.example.json"),
    );
    expect(authority.schemaVersion).toBe(
      WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    );
    expect(authority.endpoint).toBe(start.trustedHeadAuthorityEndpoint);
    expect(authority.httpBearerSecretSource).toEqual(
      start.httpBearerSecretSource,
    );
    const source = start.watcherConfig.l1.source;
    if (source.sourceMode !== "local_node") {
      throw new Error("start template is not a local_node watcher");
    }
    expect(authority.policy.network).toBe(start.watcherConfig.targetNetwork);
    expect(authority.policy.authorityNodeId).toBe(source.authorityNodeId);
    expect(authority.policy.authorityChainSyncSocketPath).toBe(
      source.chainSync.socketPath,
    );
    expect(authority.policy.authorityGenesisIdentitySha256).toBe(
      source.chainSync.genesisIdentitySha256,
    );
    expect(
      authority.policy.localQueryServices
        .map((service) => `${service.providerId} ${service.endpoint}`)
        .sort(),
    ).toEqual(
      source.queryServices
        .map((service) => `${service.identity} ${service.endpoint}`)
        .sort(),
    );
  });

  it("requires a durable availability journal, positive capital, and a distinct challenger key", () => {
    const base = productionConfig();
    const { availability: _omitted, ...withoutAvailability } = base;
    expect(() => parseWatcherProcessConfig(withoutAvailability)).toThrow(
      "unknown or missing fields",
    );
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        availability: {
          ...base.availability,
          keySource: base.watcherConfig.proverWallet.keySource,
        },
      }),
    ).toThrow("pairwise distinct");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        availability: {
          ...base.availability,
          journalPath: "/tmp/availability.sqlite",
        },
      }),
    ).toThrow("canonical production path");
    for (const minimumFundingLovelace of ["0", "-1", "01", "1.5"]) {
      expect(() =>
        parseWatcherProcessConfig({
          ...base,
          availability: {
            ...base.availability,
            minimumFundingLovelace,
          },
        }),
      ).toThrow("positive lovelace");
    }
  });

  it("binds finality and pre-finality rollback depth to the compiled profile depth", () => {
    // Acceptance is tied to the compiled selection: only the selected profile's
    // depth parses, and the other profile family's depth is always refused.
    const base = productionConfig();
    expect(base.watcherConfig.l1.finality.depth).toBe(RELEASE_DEPTH);
    expect(base.watcherConfig.l1.finality.rollback.maxDepth).toBe(
      RELEASE_DEPTH,
    );
    const otherDepth = RELEASE_DEPTH === 3 ? 30 : 3;
    const withDepths = (depth: number, maxDepth: number) => {
      const value = watcherConfigValue();
      return {
        ...base,
        watcherConfig: {
          ...value,
          l1: {
            ...value.l1,
            finality: {
              ...value.l1.finality,
              depth,
              rollback: { ...value.l1.finality.rollback, maxDepth },
            },
          },
        },
      };
    };
    expect(
      parseWatcherProcessConfig(withDepths(RELEASE_DEPTH, RELEASE_DEPTH))
        .watcherConfig.l1.finality.depth,
    ).toBe(RELEASE_DEPTH);
    const refusal = `requires finality depth and pre-finality rollback depth ${RELEASE_DEPTH} from the deployment profile`;
    expect(() =>
      parseWatcherProcessConfig(withDepths(otherDepth, otherDepth)),
    ).toThrow(refusal);
    expect(() =>
      parseWatcherProcessConfig(withDepths(RELEASE_DEPTH, RELEASE_DEPTH - 1)),
    ).toThrow(refusal);
  });

  it("refuses startup without the signed release rule-bundle artifact", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-release-rule-bundle-");
    directories.push(directory);
    const config = parseWatcherProcessConfig({
      ...productionConfig(),
      watcherRuntimeConfigPath: join(directory, "watcher.json"),
      deploymentAuthorityPath: join(directory, "deployment-authority.json"),
      ruleBundlePath: join(directory, "rule-bundle.json"),
      workflowJournalDirectory: join(directory, "workflows"),
    });
    await writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify(watcherConfigValue()),
    );
    await writeFile(config.deploymentAuthorityPath, "{}");
    await expect(createWatcherRuntime({ config })).rejects.toMatchObject({
      code: "ENOENT",
      path: config.ruleBundlePath,
    });
  });

  it("loads authority config from JSON and keeps signer sources separate", async () => {
    const input = {
      schemaVersion:
        WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
      policy: policy(),
      directory: "/var/lib/midgard-trusted-head",
      liveRecordLimit: 8,
      endpoint: "http://127.0.0.1:43123",
      recordAuthenticationKeySource: {
        kind: "environment",
        variable: "MIDGARD_SIDECAR_RECORD_KEY",
      },
      httpBearerSecretSource: {
        kind: "environment",
        variable: "MIDGARD_SIDECAR_BEARER",
      },
    };
    expect(parseWatcherTrustedHeadAuthorityProcessConfig(input)).toEqual(input);
    const directory = await mkdtemp("/var/tmp/midgard-authority-config-");
    directories.push(directory);
    const path = join(directory, "authority.json");
    await writeFile(path, JSON.stringify(input));
    expect(
      await loadWatcherTrustedHeadAuthorityProcessConfigFile(path),
    ).toEqual(input);
    expect(() =>
      parseWatcherTrustedHeadAuthorityProcessConfig({
        ...input,
        proofSignerKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      }),
    ).toThrow("unknown or missing fields");
  });

  it("rejects equal authority record and HTTP bearer values before opening the server", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-secrets-");
    directories.push(directory);
    const authorityConfig: WatcherTrustedHeadAuthorityProcessConfig = {
      schemaVersion:
        WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
      policy: policy(),
      directory,
      liveRecordLimit: 8,
      endpoint: "http://127.0.0.1:0",
      recordAuthenticationKeySource: {
        kind: "environment",
        variable: "RECORD_KEY",
      },
      httpBearerSecretSource: {
        kind: "environment",
        variable: "BEARER",
      },
    };
    await expect(
      startWatcherTrustedHeadAuthorityProcess({
        config: authorityConfig,
        unsafeEnvironmentForTest: {
          RECORD_KEY: "11".repeat(32),
          BEARER: "11".repeat(32),
        },
        unsafeAllowEphemeralPortForTest: true,
      }),
    ).rejects.toThrow("pairwise distinct");
  });

  it("rejects a sidecar record-key identity collision from the watcher without receiving the record key", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-identity-");
    directories.push(directory);
    const rollbackAndRecordKey = "12".repeat(32);
    const bearer = "watcher-sidecar-bearer-secret-0001";
    await initializeSelectedAuthorityStore({
      directory,
      policy: policy(),
      recordAuthenticationKey: Uint8Array.from(
        Buffer.from(rollbackAndRecordKey, "hex"),
      ),
      liveRecordLimit: 8,
      generation: `generation-${randomUUID()}`,
    });
    const authority = await startWatcherTrustedHeadAuthorityProcess({
      config: {
        schemaVersion:
          WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
        policy: policy(),
        directory,
        liveRecordLimit: 8,
        endpoint: "http://127.0.0.1:0",
        recordAuthenticationKeySource: {
          kind: "environment",
          variable: "RECORD_KEY",
        },
        httpBearerSecretSource: {
          kind: "environment",
          variable: "BEARER",
        },
      },
      unsafeEnvironmentForTest: {
        RECORD_KEY: rollbackAndRecordKey,
        BEARER: bearer,
      },
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const base = productionConfig();
      await expect(
        createWatcherTrustedHeadClientRuntime({
          config: {
            ...base,
            trustedHeadAuthorityEndpoint: authority.server.endpoint,
          },
          policy: policy(),
          unsafeEnvironmentForTest: {
            MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY: rollbackAndRecordKey,
            MIDGARD_WATCHER_PROVER_KEY:
              "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
            MIDGARD_WATCHER_TRUSTED_HEAD_BEARER: bearer,
          },
        }),
      ).rejects.toThrow("pairwise distinct");
    } finally {
      await authority.close();
    }
  });
});
