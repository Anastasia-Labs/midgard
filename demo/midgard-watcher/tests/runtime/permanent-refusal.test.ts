import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";

import { afterEach, describe, expect, it } from "vitest";

import {
  WATCHER_COMMAND_FAILURE_EXIT_CODE,
  WATCHER_PERMANENT_REFUSAL_EXIT_CODE,
  watcherFailureExitCode,
} from "../../src/cli.js";
import { parseWatcherConfigJson } from "../../src/runtime/config.js";
import {
  isWatcherPermanentRefusal,
  refusePermanently,
  WatcherPermanentRefusalError,
} from "../../src/runtime/permanent-refusal.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
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
  return await writeWatcherRuntimeProcessConfig(directory);
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

describe("permanent refusal exit code", () => {
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
    const failure = await startupFailure(config);
    expect(failure.message).toContain("watcher deployment_authority refused");
    expect(watcherFailureExitCode(failure)).toBe(78);
  });

  it("refuses a runtime config that differs from the process config permanently", async () => {
    const config = await processConfig();
    const raw = await rawRuntimeConfig(config);
    await writeFile(
      config.watcherRuntimeConfigPath,
      JSON.stringify({ ...raw, l1: { ...raw.l1, requestTimeoutMs: 20_000 } }),
    );
    const failure = await startupFailure(config);
    expect(failure.message).toContain(
      "watcher process and workflow runtime configurations differ",
    );
    expect(watcherFailureExitCode(failure)).toBe(78);
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
    const failure = await startupFailure({
      ...config,
      watcherConfig: parseWatcherConfigJson(text),
    });
    expect(failure.message).toContain(
      "watcher production finality differs from the verified release",
    );
    expect(watcherFailureExitCode(failure)).toBe(78);
  });

  it("keeps a deployment authority that is not written yet restartable", async () => {
    const config = await processConfig();
    await rm(config.deploymentAuthorityPath);
    const failure = await startupFailure(config);
    expect(isWatcherPermanentRefusal(failure)).toBe(false);
    expect(watcherFailureExitCode(failure)).toBe(70);
  });
});
