import { type ChildProcess, spawn, spawnSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { once } from "node:events";
import { readdir, readFile, stat, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterEach, expect, it } from "vitest";

import { WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION } from "../../src/runtime/process-config.js";
import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { policy } from "./process-config.watcher-config-value.js";
import {
  authenticationKey,
  directory,
  head,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";

const cli = fileURLToPath(new URL("../../dist/cli.js", import.meta.url));
const faults = fileURLToPath(
  new URL("../support/authority-init-cli-faults.mjs", import.meta.url),
);
const children: ChildProcess[] = [];
afterEach(async () => {
  await Promise.all(
    children.splice(0).map(async (child) => {
      if (child.exitCode !== null || child.signalCode !== null) return;
      const ended = once(child, "exit");
      child.kill("SIGTERM");
      const force = setTimeout(() => child.kill("SIGKILL"), 1_000);
      try {
        await ended;
      } finally {
        clearTimeout(force);
      }
    }),
  );
});
const scene = async () => {
  const root = await directory(),
    authorityDirectory = join(root, "authority"),
    configPath = join(root, "authority-process.json");
  const recordPath = join(root, "record.key"),
    bearerPath = join(root, "bearer.key");
  const recordText = Buffer.from(recordAuthenticationKey).toString("hex"),
    bearer = "synthetic-cli-authority-bearer-00000000000000000001";
  await Promise.all([
    writeFile(recordPath, recordText, { mode: 0o600 }),
    writeFile(bearerPath, bearer, { mode: 0o600 }),
  ]);
  const value = {
    schemaVersion: WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    directory: authorityDirectory,
    endpoint: "http://127.0.0.1:49091",
    policy: policy(),
    liveRecordLimit: 64,
    recordAuthenticationKeySource: { kind: "file", path: recordPath },
    httpBearerSecretSource: { kind: "file", path: bearerPath },
  };
  await writeFile(configPath, JSON.stringify(value));
  return {
    root,
    authorityDirectory,
    configPath,
    value,
    bearer,
    recordText,
    recordPath,
    generation: `generation-${randomUUID()}`,
  };
};
const run = (
  value: Awaited<ReturnType<typeof scene>>,
  command = "authority-init",
  fault?: string,
  generation = value.generation,
) =>
  spawnSync(
    process.execPath,
    [
      ...(fault === undefined ? [] : ["--import", faults]),
      cli,
      command,
      "--config",
      value.configPath,
      ...(command === "authority-init" ? ["--generation", generation] : []),
    ],
    {
      encoding: "utf8",
      timeout: 10_000,
      env: {
        ...process.env,
        MIDGARD_CONFIG_MODE: "disabled",
        MIDGARD_DOTENV_MODE: "disabled",
        MIDGARD_TEST_AUTHORITY_FAULT_MODE: fault,
        MIDGARD_TEST_AUTHORITY_DIRECTORY: value.authorityDirectory,
      },
    },
  );
const initialized = (value: Awaited<ReturnType<typeof scene>>) => {
  const result = run(value);
  expect(result.status, result.stderr).toBe(0);
  expect(JSON.parse(result.stdout)).toEqual({
    packageName: "midgard-watcher",
    command: "authority-init",
    state: "initialized",
    productionReady: false,
    generation: value.generation,
    liveRecordLimit: 64,
    policyDigest: value.value.policy.policyDigest,
  });
  expect(result.stdout + result.stderr).not.toContain(value.recordText);
  expect(result.stdout + result.stderr).not.toContain(value.bearer);
};
const open = (value: Awaited<ReturnType<typeof scene>>) =>
  openWatcherTrustedHeadAuthorityStore({
    directory: value.authorityDirectory,
    policy: value.value.policy,
    recordAuthenticationKey,
    liveRecordLimit: 64,
  });

it("initializes explicit K64 using normal file loaders, then serves ordinary strict authority identity/tip/CAS", async () => {
  const value = await scene();
  initialized(value);
  const socket = createServer();
  await new Promise<void>((resolve, reject) => {
    socket.once("error", reject);
    socket.listen(0, "127.0.0.1", resolve);
  });
  const address = socket.address();
  if (address === null || typeof address === "string")
    throw new Error("missing owned test endpoint");
  value.value.endpoint = `http://127.0.0.1:${address.port}`;
  await new Promise<void>((resolve, reject) =>
    socket.close((error) => (error ? reject(error) : resolve())),
  );
  await writeFile(value.configPath, JSON.stringify(value.value));
  const child = spawn(
    process.execPath,
    [cli, "authority", "--config", value.configPath],
    {
      stdio: ["ignore", "pipe", "pipe"],
      env: {
        ...process.env,
        MIDGARD_CONFIG_MODE: "disabled",
        MIDGARD_DOTENV_MODE: "disabled",
      },
    },
  );
  children.push(child);
  await new Promise<void>((resolve, reject) => {
    let output = "",
      errors = "";
    const timeout = setTimeout(
      () => finish(new Error("owned CLI did not report authority startup")),
      5_000,
    );
    const finish = (error?: Error) => {
      clearTimeout(timeout);
      child.off("exit", exited);
      if (error) reject(error);
      else resolve();
    };
    const exited = () => finish(new Error(`owned authority exited: ${errors}`));
    child.once("exit", exited);
    child.stderr!.on("data", (bytes) => {
      errors += String(bytes);
    });
    child.stdout!.on("data", (bytes) => {
      output += String(bytes);
      if (output.includes('"state":"ready"')) finish();
    });
  });
  const client = createWatcherTrustedHeadAuthorityClient({
    endpoint: value.value.endpoint,
    httpSecret: value.bearer,
    policy: value.value.policy,
    authenticationKey,
    requestTimeoutMs: 2_000,
  });
  expect(await client.readRecordAuthenticationKeyId()).toMatch(
    /^[0-9a-f]{64}$/,
  );
  expect(await client.readCurrent()).toBeNull();
  const next = head(value.value.policy, 0, "88");
  expect(
    await client.compareAndSwap({
      expectedTrustedHead: null,
      nextTrustedHead: next,
    }),
  ).toBe(true);
  expect(await client.readCurrent()).toEqual(next);
});

it("reuses the retained generation after advance without resetting the current head", async () => {
  const value = await scene();
  initialized(value);
  const store = await open(value),
    next = head(value.value.policy, 0, "77");
  try {
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: next,
      }),
    ).toEqual({ committed: true, head: next });
  } finally {
    store.close();
  }
  initialized(value);
  const current = await open(value);
  try {
    expect(await current.readCurrent()).toEqual(next);
  } finally {
    current.close();
  }
});

it.each(["before", "after"])(
  "resumes the same actual CLI generation after SIGKILL %s selector publication",
  async (fault) => {
    const value = await scene(),
      interrupted = run(value, "authority-init", fault);
    expect(interrupted.signal, interrupted.stderr).toBe("SIGKILL");
    initialized(value);
    const store = await open(value);
    try {
      expect(await store.readCurrent()).toBeNull();
    } finally {
      store.close();
    }
  },
);

it("keeps output acknowledgement failure closed and safely retries the same selected generation", async () => {
  const value = await scene(),
    lost = run(value, "authority-init", "output");
  expect(lost.status, lost.stderr).toBe(70);
  expect(lost.stderr).toContain("output acknowledgement loss");
  initialized(value);
});

it("does not acknowledge while selected directory synchronization keeps refusing", async () => {
  const value = await scene();
  for (let attempt = 0; attempt < 2; attempt++) {
    const result = run(value, "authority-init", "sync");
    expect(result.status, result.stderr).toBe(70);
    expect(result.stderr).toContain("namespace sync refusal");
  }
  initialized(value);
});

it("ordinary authority refuses missing or malformed selected state without creating or repairing it", async () => {
  const value = await scene(),
    missing = run(value, "authority");
  expect(missing.status, missing.stderr).toBe(70);
  await expect(stat(value.authorityDirectory)).rejects.toMatchObject({
    code: "ENOENT",
  });
  initialized(value);
  const selector = join(value.authorityDirectory, "authority-backend.json");
  await writeFile(selector, "{}");
  const corrupt = run(value, "authority");
  expect(corrupt.status, corrupt.stderr).toBe(70);
  expect(await readFile(selector, "utf8")).toBe("{}");
});

it.each(["generation", "key", "K", "selector"])(
  "refuses a changed %s initialization binding without replacing existing selected bytes",
  async (change) => {
    const value = await scene();
    initialized(value);
    if (change === "key") await writeFile(value.recordPath, "aa".repeat(32));
    if (change === "K") {
      value.value.liveRecordLimit = 8;
      await writeFile(value.configPath, JSON.stringify(value.value));
    }
    const selectorPath = join(
      value.authorityDirectory,
      "authority-backend.json",
    );
    if (change === "selector") await writeFile(selectorPath, "{}");
    const prior = await readFile(selectorPath),
      names = await readdir(value.authorityDirectory);
    const result = run(
      value,
      "authority-init",
      undefined,
      change === "generation" ? `generation-${randomUUID()}` : value.generation,
    );
    expect(result.status, result.stderr).toBe(70);
    expect(await readFile(selectorPath)).toEqual(prior);
    expect(await readdir(value.authorityDirectory)).toEqual(names);
  },
);

it("refuses malformed generation and colliding normal-loader secrets before creating authority state", async () => {
  const value = await scene();
  expect(
    run(value, "authority-init", undefined, "generation-invalid").status,
  ).toBe(70);
  await writeFile(value.value.httpBearerSecretSource.path, value.recordText);
  const collision = run(value);
  expect(collision.status, collision.stderr).toBe(70);
  expect(collision.stderr).toContain("pairwise distinct");
  await expect(stat(value.authorityDirectory)).rejects.toMatchObject({
    code: "ENOENT",
  });
});
