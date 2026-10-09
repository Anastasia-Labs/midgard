import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-test-support/hex";
import "vitest";
import "../../src/runtime/config.js";
import "../../src/runtime/process-config.js";
import "../../src/runtime/watcher-runtime.js";
import "./process-config.watcher-config-value.js";

import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { join } from "node:path";

import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it } from "vitest";

import { WatcherPermanentRefusalError } from "../../src/runtime/permanent-refusal.js";
import {
  loadWatcherProcessConfigFile,
  parseWatcherProcessConfig,
  WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
} from "../../src/runtime/process-config.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import { freeOperationsEndpoint } from "../support/free-port.js";
import {
  directories,
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

  it("refuses permanently a node socket path that exists and is not a socket, and admits a socket or a missing path", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-process-config-");
    directories.push(directory);
    const load = async (socketPath: string) => {
      const value = watcherConfigValue();
      value.l1.source.chainSync.socketPath = socketPath;
      const path = join(directory, "process.json");
      await writeFile(
        path,
        JSON.stringify({ ...productionConfig(), watcherConfig: value }),
      );
      return await loadWatcherProcessConfigFile(path);
    };
    const file = join(directory, "node.socket");
    await writeFile(file, "");
    for (const [path, kind] of [
      [file, "a regular file"],
      [directory, "a directory"],
    ] as const) {
      const refused = load(path);
      await expect(refused).rejects.toBeInstanceOf(
        WatcherPermanentRefusalError,
      );
      await expect(refused).rejects.toThrow(
        `$.l1.source.chainSync.socketPath ${path} is ${kind}, not a socket`,
      );
    }
    await expect(load(join(directory, "absent.socket"))).resolves.toBeDefined();
    const socket = join(directory, "live.socket");
    const server = createServer();
    await new Promise<void>((resolve) => server.listen(socket, resolve));
    try {
      await expect(load(socket)).resolves.toBeDefined();
    } finally {
      await new Promise((resolve) => server.close(resolve));
    }
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
    ).toThrow("requires acceptance Preprod or Custom");
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        watcherRollbackKeySource:
          base.watcherConfig.storage.rollbackAuthorityKeySource,
      }),
    ).toThrow(
      'unknown or missing fields: missing=[] unknown=["watcherRollbackKeySource"]',
    );
    expect(() =>
      parseWatcherProcessConfig({
        ...base,
        readinessHeaderHash: h28(0x77),
      }),
    ).toThrow('missing=[] unknown=["readinessHeaderHash"]');
    expect(() => {
      const { ruleBundlePath: _omitted, ...withoutBundle } = base;
      return parseWatcherProcessConfig(withoutBundle);
    }).toThrow('missing=["ruleBundlePath"] unknown=[]');
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

  it("refuses the deleted trusted-head authority keys as unknown fields", () => {
    const base = productionConfig();
    for (const deleted of [
      { trustedHeadAuthorityEndpoint: "http://127.0.0.1:43123" },
      {
        httpBearerSecretSource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_TRUSTED_HEAD_BEARER",
        },
      },
    ])
      expect(() => parseWatcherProcessConfig({ ...base, ...deleted })).toThrow(
        "unknown or missing fields",
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

  it("binds finality depth to the compiled profile depth", () => {
    // Acceptance is tied to the compiled selection: only the selected profile's
    // depth parses, and the other profile family's depth is always refused.
    const base = productionConfig();
    expect(base.watcherConfig.l1.finality.depth).toBe(RELEASE_DEPTH);
    const otherDepth = RELEASE_DEPTH === 3 ? 30 : 3;
    const withDepth = (depth: number) => {
      const value = watcherConfigValue();
      return {
        ...base,
        watcherConfig: {
          ...value,
          l1: { ...value.l1, finality: { depth } },
        },
      };
    };
    expect(
      parseWatcherProcessConfig(withDepth(RELEASE_DEPTH)).watcherConfig.l1
        .finality.depth,
    ).toBe(RELEASE_DEPTH);
    expect(() => parseWatcherProcessConfig(withDepth(otherDepth))).toThrow(
      `requires finality depth ${RELEASE_DEPTH} from the deployment profile`,
    );
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
    // A file not written yet is one a restart may clear: startup exits.
    await expect(
      createWatcherRuntime({
        config: {
          ...config,
          operationsEndpoint: await freeOperationsEndpoint(),
        },
      }),
    ).rejects.toMatchObject({
      code: "ENOENT",
      path: config.ruleBundlePath,
    });
  });
});
