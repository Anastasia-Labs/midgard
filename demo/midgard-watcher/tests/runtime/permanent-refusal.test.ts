import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import { afterEach, describe, expect, it } from "vitest";

import {
  WATCHER_COMMAND_FAILURE_EXIT_CODE,
  WATCHER_PERMANENT_REFUSAL_EXIT_CODE,
  watcherFailureExitCode,
} from "../../src/cli.js";
import { WatcherL1UnavailableError } from "../../src/l1/transient-retry.js";
import { parseWatcherConfigJson } from "../../src/runtime/config.js";
import {
  isWatcherPermanentRefusal,
  refusePermanently,
  WatcherPermanentRefusalError,
} from "../../src/runtime/permanent-refusal.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { loadWatcherProcessConfigFile } from "../../src/runtime/process-config.js";
import { WATCHER_STARTUP_FAILED } from "../../src/runtime/startup-operations.js";
import {
  createWatcherRuntime,
  watcherStartupFailureExits,
  WatcherStartupHeldError,
} from "../../src/runtime/watcher-runtime.js";
import { freeOperationsEndpoint } from "../support/free-port.js";
import { writeWatcherRuntimeProcessConfig } from "../support/watcher-runtime-process-config.js";

const directories: string[] = [];

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const processConfig = async (): Promise<WatcherProcessConfig> => {
  const directory = await mkdtemp("/var/tmp/midgard-watcher-refusal-");
  directories.push(directory);
  return {
    ...(await writeWatcherRuntimeProcessConfig(directory)),
    operationsEndpoint: await freeOperationsEndpoint(),
  };
};

type RawWatcherConfig = Readonly<{
  l1: Readonly<{
    requestTimeoutMs: number;
    finality: Readonly<{ depth: number }>;
  }>;
}>;

const rawRuntimeConfig = async (config: WatcherProcessConfig) =>
  JSON.parse(
    await readFile(config.watcherRuntimeConfigPath, "utf8"),
  ) as RawWatcherConfig;

const startupFailure = async (config: WatcherProcessConfig) => {
  const failure = await createWatcherRuntime({ config }).then(
    () => {
      throw new Error("the watcher started");
    },
    (error: unknown) => error,
  );
  expect(failure).toBeInstanceOf(Error);
  return failure as Error;
};

const readyz = async (config: WatcherProcessConfig) => {
  try {
    const response = await fetch(`${config.operationsEndpoint}/readyz`);
    return {
      status: response.status,
      body: (await response.json()) as {
        reasons: string[];
        startup: { outcome: string; error: string };
      },
    };
  } catch {
    return undefined;
  }
};

/**
 * A refusal once the operations server bound holds the process up, unready
 * with `startup_failed` and the refusal; releasing the hold closes the server.
 */
const heldRefusal = async (config: WatcherProcessConfig) => {
  const failure = await startupFailure(config);
  expect(failure).toBeInstanceOf(WatcherStartupHeldError);
  const held = await readyz(config);
  expect(held).toMatchObject({
    status: 503,
    body: {
      reasons: [WATCHER_STARTUP_FAILED],
      startup: { outcome: "failed" },
    },
  });
  expect(held!.body.startup.error).toBe(
    (failure.cause as Error | undefined)?.message,
  );
  await (failure as WatcherStartupHeldError).release();
  expect(await readyz(config)).toBeUndefined();
  return failure;
};

describe("permanent refusals: held after the server binds, exit 78 before", () => {
  it("gives a byte-comparison refusal its own exit code, and keeps 70 for everything else", async () => {
    const refusal = await refusePermanently("deployment_authority", () => {
      throw new Error("watcher deployment authority trust roots are invalid");
    }).catch((error: unknown) => error);
    expect(refusal).toBeInstanceOf(WatcherPermanentRefusalError);
    expect(watcherFailureExitCode(refusal)).toBe(78);
    expect(
      watcherFailureExitCode(new Error("loader failed", { cause: refusal })),
    ).toBe(WATCHER_PERMANENT_REFUSAL_EXIT_CODE);

    const missing = Object.assign(new Error("no such file"), {
      code: "ENOENT",
    });
    const kept = await refusePermanently("deployment_authority", () => {
      throw new Error("read failed", { cause: missing });
    }).catch((error: unknown) => error);
    expect(isWatcherPermanentRefusal(kept)).toBe(false);
    expect(watcherFailureExitCode(kept)).toBe(70);
    expect(watcherFailureExitCode(new Error("Kupo timed out"))).toBe(
      WATCHER_COMMAND_FAILURE_EXIT_CODE,
    );
  });

  it("refuses tampered deployment-authority bytes permanently at startup", async () => {
    const config = await processConfig();
    const authority = JSON.parse(
      await readFile(config.deploymentAuthorityPath, "utf8"),
    ) as Record<string, unknown>;
    await writeFile(
      config.deploymentAuthorityPath,
      JSON.stringify({ ...authority, trustRoots: {} }),
    );
    const failure = await heldRefusal(config);
    expect(failure.message).toContain("watcher deployment_authority refused");
    expect(isWatcherPermanentRefusal(failure)).toBe(true);
  });

  it("refuses a runtime config that differs from the process config permanently", async () => {
    const config = await processConfig();
    const raw = await rawRuntimeConfig(config);
    await writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify({ ...raw, l1: { ...raw.l1, requestTimeoutMs: 20_000 } }),
    );
    const failure = await heldRefusal(config);
    expect(failure.message).toContain(
      "watcher process and workflow runtime configurations differ",
    );
  });

  it("refuses a finality policy that differs from the verified release permanently", async () => {
    const config = await processConfig();
    const raw = await rawRuntimeConfig(config);
    const { finality } = raw.l1;
    // The process-config parser binds the compiled profile's depth, so this
    // config differs only from the signed release, as a stale profile would.
    const text = JSON.stringify({
      ...raw,
      l1: {
        ...raw.l1,
        finality: { ...finality, depth: finality.depth + 1 },
      },
    });
    await writeFile(config.watcherRuntimeConfigPath, text);
    const failure = await heldRefusal({
      ...config,
      watcherConfig: parseWatcherConfigJson(text),
    });
    expect(failure.message).toContain(
      "watcher production finality differs from the verified release",
    );
  });

  it("keeps a deployment authority that is not written yet restartable", async () => {
    const config = await processConfig();
    await rm(config.deploymentAuthorityPath);
    const failure = await startupFailure(config);
    expect(failure).not.toBeInstanceOf(WatcherStartupHeldError);
    expect(isWatcherPermanentRefusal(failure)).toBe(false);
    expect(watcherFailureExitCode(failure)).toBe(70);
    // It exits: the server bound for it is closed.
    expect(await readyz(config)).toBeUndefined();
  });

  it("exits on a startup failure a restart may clear, and holds on any other", () => {
    const transient = new L1ProviderTransientError("transport", "down");
    const unavailable = new WatcherL1UnavailableError(600_000, 9, transient);
    expect(watcherStartupFailureExits(unavailable)).toBe(true);
    expect(
      watcherStartupFailureExits(
        new AggregateError([new Error("cleanup"), unavailable], "startup"),
      ),
    ).toBe(true);
    expect(
      watcherStartupFailureExits(
        new Error("read failed", {
          cause: Object.assign(new Error("no such file"), { code: "ENOENT" }),
        }),
      ),
    ).toBe(true);
    expect(
      watcherStartupFailureExits(
        new WatcherPermanentRefusalError("deployment_authority", "bad roots"),
      ),
    ).toBe(false);
    expect(watcherStartupFailureExits(new Error("malformed"))).toBe(false);
    expect(watcherStartupFailureExits(transient)).toBe(false);
  });

  it("refuses a process configuration file that does not parse permanently, before any server binds", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-watcher-refusal-");
    directories.push(directory);
    const path = join(directory, "process.json");
    await writeFile(path, JSON.stringify({ schemaVersion: "unknown" }));
    const refused = await loadWatcherProcessConfigFile(path).catch(
      (error: unknown) => error,
    );
    expect(isWatcherPermanentRefusal(refused)).toBe(true);
    expect(watcherFailureExitCode(refused)).toBe(78);
    // A file not written yet stays restartable.
    const missing = await loadWatcherProcessConfigFile(
      join(directory, "absent.json"),
    ).catch((error: unknown) => error);
    expect(isWatcherPermanentRefusal(missing)).toBe(false);
    expect(watcherFailureExitCode(missing)).toBe(70);
  });

  it("refuses permanently, before reading its configuration, a watcher started with a non-follower L1 setting", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-watcher-refusal-");
    directories.push(directory);
    const absent = join(directory, "absent.json");
    const refused = await loadWatcherProcessConfigFile(absent, {
      L1_OGMIOS_URL: "http://ogmios:1337",
      L1_ACCESS: "kupmios",
    }).catch((error: unknown) => error);
    expect(isWatcherPermanentRefusal(refused)).toBe(true);
    expect(watcherFailureExitCode(refused)).toBe(78);
    expect((refused as Error).message).toMatch(
      /watcher l1_access refused: midgard-watcher reads L1 only through its follower.*L1_ACCESS, L1_OGMIOS_URL/,
    );
    expect((refused as Error).cause).toMatchObject({
      reason: "role_non_follower_l1_config",
    });
    // The follower's own access passes on to the file read.
    const follower = await loadWatcherProcessConfigFile(absent, {
      L1_ACCESS: "follower",
    }).catch((error: unknown) => error);
    expect(isWatcherPermanentRefusal(follower)).toBe(false);
    expect(watcherFailureExitCode(follower)).toBe(70);
  });
});
