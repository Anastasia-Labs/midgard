import { randomUUID } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import {
  openWatcherTrustedHeadAuthorityStore,
  WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
} from "midgard-watcher";
import { policy } from "midgard-watcher/tests/runtime/process-config.watcher-config-value";
import {
  head,
  recordAuthenticationKey,
} from "midgard-watcher/tests/runtime/trusted-head-authority.policy";
import { afterEach, expect, it, vi } from "vitest";

import {
  finishWatcherAuthorityProvisioning,
  FRESH_AUTHORITY_PROFILE,
  prepareWatcherAuthorityProvisioning,
} from "../src/devnet-stack/watcher-authority-provisioning.js";

const fault = vi.hoisted(() => ({
  afterLink: "",
  sync: false,
  completion: "",
}));
vi.mock("node:fs", async (original) => {
  const actual = await original<typeof import("node:fs")>();
  return {
    ...actual,
    linkSync: (
      from: Parameters<typeof actual.linkSync>[0],
      to: Parameters<typeof actual.linkSync>[1],
    ) => {
      if (String(to) === fault.completion)
        throw Error("synthetic completion acknowledgement loss");
      actual.linkSync(from, to);
      if (String(to) === fault.afterLink) fault.sync = true;
    },
    fsyncSync: (fd: number) => {
      if (fault.sync) throw Error("synthetic descriptor sync refusal");
      actual.fsyncSync(fd);
    },
  };
});
const roots: string[] = [];
afterEach(async () => {
  fault.afterLink = "";
  fault.completion = "";
  fault.sync = false;
  await Promise.all(
    roots.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
});
const cliPath = fileURLToPath(
  new URL("../../midgard-watcher/dist/cli.js", import.meta.url),
);
const scene = async () => {
  const root = await mkdtemp("/var/tmp/codex-rel-authority-consumer-");
  roots.push(root);
  const record = join(root, "record.key"),
    bearer = join(root, "bearer.key"),
    configPath = join(root, "authority-process.json"),
    descriptorPath = join(root, "provisioning.json");
  const config = {
    schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    policy: policy(),
    liveRecordLimit: 64,
    directory: join(root, "selected"),
    endpoint: "http://127.0.0.1:43122",
    recordAuthenticationKeySource: { kind: "file" as const, path: record },
    httpBearerSecretSource: { kind: "file" as const, path: bearer },
  };
  const input = {
    config,
    descriptorPath,
    secretPaths: [record, bearer],
    protectedPaths: [configPath],
    initialize: true,
  };
  const secrets = () => {
    writeFileSync(
      record,
      Buffer.from(recordAuthenticationKey).toString("hex"),
      { mode: 0o600, flag: "wx" },
    );
    writeFileSync(bearer, "synthetic-bearer-key-for-authority-consumer-00001", {
      mode: 0o600,
      flag: "wx",
    });
    writeFileSync(configPath, JSON.stringify(config));
  };
  const finish = (
    prepared: ReturnType<typeof prepareWatcherAuthorityProvisioning>,
    path = cliPath,
  ) =>
    finishWatcherAuthorityProvisioning({
      prepared,
      config,
      configPath,
      descriptorPath,
      cliPath: path,
    });
  const open = () =>
    openWatcherTrustedHeadAuthorityStore({
      directory: config.directory,
      policy: config.policy,
      liveRecordLimit: 64,
      recordAuthenticationKey,
    });
  return {
    root,
    config,
    input,
    record,
    bearer,
    configPath,
    descriptorPath,
    secrets,
    finish,
    open,
  };
};
it("ordinary preparation never infers fresh ownership from absent state", async () => {
  const s = await scene();
  expect(() =>
    prepareWatcherAuthorityProvisioning({ ...s.input, initialize: false }),
  ).toThrow("explicit fresh authority ownership");
  expect(existsSync(s.descriptorPath)).toBe(false);
  expect(existsSync(s.record)).toBe(false);
});
it("explicit descriptor → actual compiled init → ordinary restart preserves UUID and ignores changed endpoint ports", async () => {
  const s = await scene(),
    prepared = prepareWatcherAuthorityProvisioning(s.input);
  s.secrets();
  await s.finish(prepared);
  const saved = readFileSync(s.descriptorPath, "utf8");
  expect(JSON.parse(saved).profile).toEqual(FRESH_AUTHORITY_PROFILE);
  const resumed = prepareWatcherAuthorityProvisioning({
    ...s.input,
    config: { ...s.config, endpoint: "http://127.0.0.1:43123" },
    initialize: false,
  });
  await s.finish(resumed, "/missing-init-must-not-run");
  expect(readFileSync(s.descriptorPath, "utf8")).toBe(saved);
});
it("completion loss after actual init retains pending UUID and resumes advanced retired-checkpoint state without reset", async () => {
  const s = await scene(),
    prepared = prepareWatcherAuthorityProvisioning(s.input);
  s.secrets();
  fault.completion = `${s.descriptorPath}.completed`;
  await expect(s.finish(prepared)).rejects.toThrow(
    "completion acknowledgement loss",
  );
  expect(existsSync(`${s.descriptorPath}.completed`)).toBe(false);
  const store = await s.open();
  let previous: ReturnType<typeof head> | null = null;
  try {
    for (let n = 0; n < 66; n++) {
      const next = head(s.config.policy, n, "77");
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
  fault.completion = "";
  const retry = prepareWatcherAuthorityProvisioning(s.input);
  expect(retry.descriptor.generation).toBe(prepared.descriptor.generation);
  await s.finish(retry);
  const reopened = await s.open();
  try {
    expect(await reopened.readCurrent()).toEqual(previous);
  } finally {
    reopened.close();
  }
});
it("pending attempts require explicit retry and persistent descriptor fsync refuses acknowledgement", async () => {
  const s = await scene();
  fault.afterLink = s.descriptorPath;
  expect(() => prepareWatcherAuthorityProvisioning(s.input)).toThrow(
    "descriptor sync refusal",
  );
  const held = readFileSync(s.descriptorPath, "utf8");
  expect(() => prepareWatcherAuthorityProvisioning(s.input)).toThrow(
    "descriptor sync refusal",
  );
  fault.afterLink = "";
  fault.sync = false;
  expect(() =>
    prepareWatcherAuthorityProvisioning({ ...s.input, initialize: false }),
  ).toThrow("pending authority provisioning");
  expect(
    prepareWatcherAuthorityProvisioning(s.input).descriptor.generation,
  ).toBe(JSON.parse(held).generation);
});
it("completed missing namespace or secret holds without regeneration", async () => {
  const s = await scene(),
    prepared = prepareWatcherAuthorityProvisioning(s.input);
  s.secrets();
  await s.finish(prepared);
  const held = readFileSync(s.descriptorPath, "utf8");
  rmSync(s.config.directory, { recursive: true });
  expect(() => prepareWatcherAuthorityProvisioning(s.input)).toThrow(
    "completed authority namespace is missing",
  );
  expect(readFileSync(s.descriptorPath, "utf8")).toBe(held);
  expect(existsSync(s.config.directory)).toBe(false);
  rmSync(s.record);
  expect(() => prepareWatcherAuthorityProvisioning(s.input)).toThrow(
    "established authority secret is missing",
  );
  expect(existsSync(s.record)).toBe(false);
});
it.each(["policy", "K", "source", "generation"])(
  "refuses %s drift without replacing selected bytes",
  async (kind) => {
    const s = await scene(),
      prepared = prepareWatcherAuthorityProvisioning(s.input);
    s.secrets();
    await s.finish(prepared);
    const selectorPath = join(s.config.directory, "authority-backend.json"),
      before = readFileSync(selectorPath, "utf8");
    if (kind === "generation") {
      const value = JSON.parse(readFileSync(s.descriptorPath, "utf8"));
      value.generation = `generation-${randomUUID()}`;
      writeFileSync(s.descriptorPath, JSON.stringify(value) + "\n");
      await expect(s.finish(prepared)).rejects.toThrow(
        "provisioning intent changed",
      );
    } else {
      const config =
        kind === "K"
          ? { ...s.config, liveRecordLimit: 8 }
          : kind === "source"
            ? {
                ...s.config,
                recordAuthenticationKeySource: {
                  kind: "file" as const,
                  path: join(s.root, "other.key"),
                },
              }
            : {
                ...s.config,
                policy: { ...s.config.policy, policyDigest: "aa".repeat(32) },
              };
      expect(() =>
        prepareWatcherAuthorityProvisioning({ ...s.input, config }),
      ).toThrow();
    }
    expect(readFileSync(selectorPath, "utf8")).toBe(before);
  },
);
it("refuses unknown existing namespace before initialization", async () => {
  const s = await scene();
  mkdirSync(s.config.directory);
  writeFileSync(join(s.config.directory, "unknown"), "owned elsewhere");
  expect(() => prepareWatcherAuthorityProvisioning(s.input)).toThrow(
    "existing state is never initialized",
  );
  expect(readFileSync(join(s.config.directory, "unknown"), "utf8")).toBe(
    "owned elsewhere",
  );
  expect(existsSync(s.descriptorPath)).toBe(false);
});

it("actual compiled CLI output loss keeps the same pending generation for retry", async () => {
  const s = await scene(),
    prepared = prepareWatcherAuthorityProvisioning(s.input);
  s.secrets();
  const wrapper = join(s.root, "lost-output.mjs");
  writeFileSync(
    wrapper,
    `const {main}=await import(${JSON.stringify(pathToFileURL(cliPath).href)});process.stdout.write=()=>{throw new Error("synthetic CLI output loss")};process.exitCode=await main(process.argv.slice(2));`,
  );
  await expect(s.finish(prepared, wrapper)).rejects.toThrow(
    "synthetic CLI output loss",
  );
  expect(existsSync(join(s.config.directory, "authority-backend.json"))).toBe(
    true,
  );
  expect(existsSync(`${s.descriptorPath}.completed`)).toBe(false);
  const retry = prepareWatcherAuthorityProvisioning(s.input);
  expect(retry.descriptor.generation).toBe(prepared.descriptor.generation);
  await s.finish(retry);
});
