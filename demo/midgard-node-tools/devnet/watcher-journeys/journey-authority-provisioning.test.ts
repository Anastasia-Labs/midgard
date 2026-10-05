import { existsSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import {
  decodeWatcherAuthenticationKey32,
  loadWatcherSecretText,
  openWatcherTrustedHeadAuthorityStore,
} from "midgard-watcher";
import { policy } from "midgard-watcher/tests/runtime/process-config.watcher-config-value";
import { head } from "midgard-watcher/tests/runtime/trusted-head-authority.policy";
import { afterEach, expect, it } from "vitest";

import {
  journeyAuthorityKeySources,
  provisionJourneyAuthority,
} from "./journey-authority-provisioning.js";

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
});
const cliPath = fileURLToPath(
  new URL("../../../midgard-watcher/dist/cli.js", import.meta.url),
);
const scene = async () => {
  const runDirectory = await mkdtemp("/var/tmp/codex-rel-journey-authority-");
  roots.push(runDirectory);
  await mkdir(join(runDirectory, "secrets"));
  await mkdir(join(runDirectory, "work/journeys"), { recursive: true });
  return {
    runDirectory,
    runtimeDirectory: join(runDirectory, "work/journeys/runtime"),
    configPath: join(runDirectory, "work/journeys/session-authority.json"),
    endpoint: "http://127.0.0.1:43122",
    policy: policy(),
    publisherSeed: "synthetic publisher seed",
    availabilitySeed: "synthetic availability seed",
    initialize: true,
    cliPath,
  };
};
const descriptorPath = (runDirectory: string) =>
  join(runDirectory, "work/journeys/authority-provisioning.json");

it("ordinary fresh journey cannot create runtime, descriptor or watcher keys", async () => {
  const input = await scene();
  await expect(
    provisionJourneyAuthority({ ...input, initialize: false }),
  ).rejects.toThrow("explicit fresh authority ownership");
  expect(existsSync(input.runtimeDirectory)).toBe(false);
  expect(existsSync(descriptorPath(input.runDirectory))).toBe(false);
  expect(
    Object.values(journeyAuthorityKeySources(input.runDirectory)).some(
      (source) => existsSync(source.path),
    ),
  ).toBe(false);
});

it("explicit journey init uses the real compiled command and ordinary restart only verifies", async () => {
  const input = await scene();
  const config = await provisionJourneyAuthority(input);
  const descriptor = readFileSync(descriptorPath(input.runDirectory), "utf8");
  const selector = readFileSync(
    join(config.directory, "authority-backend.json"),
    "utf8",
  );
  expect(config.liveRecordLimit).toBe(64);
  const restarted = await provisionJourneyAuthority({
    ...input,
    initialize: false,
    endpoint: "http://127.0.0.1:43123",
    configPath: join(input.runDirectory, "work/journeys/next-session.json"),
    cliPath: "/synthetic/missing-cli.mjs",
  });
  expect(restarted.endpoint).toBe("http://127.0.0.1:43123");
  expect(readFileSync(descriptorPath(input.runDirectory), "utf8")).toBe(
    descriptor,
  );
  expect(
    readFileSync(join(config.directory, "authority-backend.json"), "utf8"),
  ).toBe(selector);
});

it("real compiled stdout loss retains pending UUID and retry finishes the same selected generation", async () => {
  const input = await scene();
  const wrapper = join(input.runDirectory, "lost-output.mjs");
  writeFileSync(
    wrapper,
    `const {main}=await import(${JSON.stringify(pathToFileURL(cliPath).href)});process.stdout.write=()=>{throw new Error("synthetic CLI output loss")};process.exitCode=await main(process.argv.slice(2));`,
  );
  await expect(
    provisionJourneyAuthority({ ...input, cliPath: wrapper }),
  ).rejects.toThrow();
  const descriptor = readFileSync(descriptorPath(input.runDirectory), "utf8");
  const selected = join(
    input.runtimeDirectory,
    "trusted-head/authority-backend.json",
  );
  const selector = readFileSync(selected, "utf8");
  expect(existsSync(`${descriptorPath(input.runDirectory)}.completed`)).toBe(
    false,
  );
  await expect(
    provisionJourneyAuthority({ ...input, initialize: false }),
  ).rejects.toThrow("pending authority provisioning");
  await provisionJourneyAuthority(input);
  expect(readFileSync(descriptorPath(input.runDirectory), "utf8")).toBe(
    descriptor,
  );
  expect(readFileSync(selected, "utf8")).toBe(selector);
  expect(existsSync(`${descriptorPath(input.runDirectory)}.completed`)).toBe(
    true,
  );
});

it("existing runtime cannot become fresh and completed namespace or key loss never reinitializes", async () => {
  const old = await scene();
  await mkdir(old.runtimeDirectory);
  await expect(provisionJourneyAuthority(old)).rejects.toThrow(
    "explicit fresh authority ownership",
  );
  const input = await scene();
  await provisionJourneyAuthority(input);
  const keys = journeyAuthorityKeySources(input.runDirectory);
  rmSync(keys.rollback.path);
  await expect(provisionJourneyAuthority(input)).rejects.toThrow(
    "established authority secret is missing",
  );
  expect(existsSync(keys.rollback.path)).toBe(false);
  // Restore only this synthetic test fixture so the separate namespace-loss
  // assertion reaches its intended boundary.
  writeFileSync(keys.rollback.path, "synthetic restored test key");
  rmSync(join(input.runtimeDirectory, "trusted-head"), { recursive: true });
  await expect(provisionJourneyAuthority(input)).rejects.toThrow(
    "completed authority namespace is missing",
  );
  expect(existsSync(join(input.runtimeDirectory, "trusted-head"))).toBe(false);
});

it("ordinary restart preserves a real advanced K64 checkpoint and refuses policy drift", async () => {
  const input = await scene();
  const config = await provisionJourneyAuthority(input);
  const descriptor = readFileSync(descriptorPath(input.runDirectory), "utf8");
  const open = async () =>
    openWatcherTrustedHeadAuthorityStore({
      directory: config.directory,
      policy: config.policy,
      liveRecordLimit: config.liveRecordLimit,
      recordAuthenticationKey: decodeWatcherAuthenticationKey32(
        await loadWatcherSecretText(config.recordAuthenticationKeySource),
      ),
    });
  const store = await open();
  let previous: ReturnType<typeof head> | null = null;
  try {
    for (let n = 0; n < 66; n++) {
      const next = head(config.policy, n, "77");
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: previous,
          nextTrustedHead: next,
        }),
      ).toEqual({ committed: true, head: next });
      previous = next;
    }
  } finally {
    store.close();
  }
  await provisionJourneyAuthority({
    ...input,
    initialize: false,
    cliPath: "/synthetic/missing-cli.mjs",
  });
  const reopened = await open();
  try {
    expect(await reopened.readCurrent()).toEqual(previous);
  } finally {
    reopened.close();
  }
  await expect(
    provisionJourneyAuthority({
      ...input,
      policy: { ...input.policy, policyDigest: "aa".repeat(32) },
    }),
  ).rejects.toThrow("descriptor identity differs");
  expect(readFileSync(descriptorPath(input.runDirectory), "utf8")).toBe(
    descriptor,
  );
});
