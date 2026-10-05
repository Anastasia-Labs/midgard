import { fork } from "node:child_process";
import { randomUUID } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { build } from "esbuild";
import { afterAll, beforeAll, expect, it } from "vitest";

import {
  importLegacyAuthorityStore,
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";
import { legacyScene } from "./trusted-head-authority.legacy-fixture.js";
import {
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

let bundleDirectory: string;
let entry: string;
beforeAll(async () => {
  const support = join(dirname(fileURLToPath(import.meta.url)), "../support");
  bundleDirectory = await mkdtemp(join(support, ".sqlite-worker-"));
  entry = join(bundleDirectory, "entry.mjs");
  await build({
    entryPoints: [join(support, "trusted-head-sqlite-worker-entry.ts")],
    outfile: entry,
    bundle: true,
    platform: "node",
    target: "node22",
    format: "esm",
    conditions: ["midgard-source"],
    logLevel: "silent",
    banner: {
      js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
    },
    plugins: [
      {
        name: "external-npm",
        setup(builder) {
          builder.onResolve({ filter: /^[^./]/u }, async (args) => {
            if (args.pluginData === "external-resolution") return undefined;
            if (args.path.startsWith("@al-ft/midgard-")) return undefined;
            if (args.path.startsWith("node:"))
              return { path: args.path, external: true };
            const resolved = await builder.resolve(args.path, {
              importer: args.importer,
              resolveDir: args.resolveDir,
              kind: args.kind,
              pluginData: "external-resolution",
            });
            return { ...resolved, external: true };
          });
        },
      },
    ],
  });
});
afterAll(async () => {
  if (bundleDirectory !== undefined)
    await rm(bundleDirectory, { recursive: true, force: true });
});

type Input = Parameters<typeof initializeSelectedAuthorityStore>[0];
const worker = async (input: Input, request: Record<string, unknown>) => {
  const child = fork(entry, [], {
    execArgv: ["--conditions=midgard-source"],
    stdio: ["ignore", "ignore", "pipe", "ipc"],
  });
  let stderr = "";
  child.stderr!.on("data", (bytes: Buffer) => {
    stderr += bytes.toString();
  });
  const done = new Promise<{
    code: number | null;
    signal: string | null;
    result?: unknown;
    error?: string;
  }>((resolve, reject) => {
    let result: unknown, error: string | undefined;
    child.on(
      "message",
      (message: { kind: string; result?: unknown; message?: string }) => {
        if (message.kind === "result") result = message.result;
        if (message.kind === "error") error = message.message;
      },
    );
    child.once("error", reject);
    child.once("close", (code, signal) =>
      resolve({ code, signal, result, error }),
    );
  });
  const timeout = setTimeout(() => child.kill("SIGKILL"), 15_000);
  const prepared = new Promise<void>((resolve, reject) => {
    child.on("message", (message: { kind: string; message?: string }) => {
      if (message.kind === "prepared") resolve();
      if (message.kind === "error") reject(new Error(message.message));
    });
    child.once("exit", () => {
      if (request.pauseSelector === true)
        reject(new Error(`worker exited before selector: ${stderr}`));
    });
  });
  // Non-selector cases do not consume this observation promise.
  void prepared.catch(() => undefined);
  const ready = new Promise<void>((resolve, reject) => {
    child.once("message", (message: { kind: string; message?: string }) =>
      message.kind === "ready" ? resolve() : reject(new Error(message.message)),
    );
    child.once("exit", () =>
      reject(new Error(`worker exited before readiness: ${stderr}`)),
    );
  });
  child.send({
    ...request,
    input: {
      ...input,
      recordAuthenticationKey: [...input.recordAuthenticationKey],
    },
  });
  try {
    await ready;
  } catch (error) {
    child.kill("SIGKILL");
    await done;
    clearTimeout(timeout);
    throw error;
  }
  return {
    release: () => child.send({ kind: "run" }),
    prepared,
    select: () => child.send({ kind: "select" }),
    done: done.finally(() => clearTimeout(timeout)),
    close: async () => {
      child.kill("SIGKILL");
      await done;
      clearTimeout(timeout);
    },
  };
};

it.each(["before", "after"] as const)(
  "recovers after actual SIGKILL %s CAS commit with a retiring checkpoint",
  async (fault) => {
    const scene = await sqliteScene(1),
      prior = await scene.advance(1),
      next = head(scene.input.policy, 1, "88");
    scene.store.close();
    const child = await worker(scene.input, {
      operation: "cas",
      expectedTrustedHead: prior,
      nextTrustedHead: next,
      fault,
    });
    try {
      child.release();
      expect((await child.done).signal).toBe("SIGKILL");
      const store = await openWatcherTrustedHeadAuthorityStore(scene.input);
      try {
        expect(await store.readCurrent()).toEqual(
          fault === "before" ? prior : next,
        );
        if (fault === "after")
          expect(
            await store.compareAndSwap({
              expectedTrustedHead: prior,
              nextTrustedHead: next,
            }),
          ).toEqual({ committed: false, head: next });
      } finally {
        store.close();
      }
    } finally {
      await child.close();
    }
  },
);

it("selects exactly one authenticated generation when independent offline imports race", async () => {
  const legacy = await legacyScene(3),
    root = await directory();
  const inputs = [0, 1].map(() => ({
    directory: root,
    policy: legacy.policy,
    recordAuthenticationKey: legacy.recordAuthenticationKey,
    liveRecordLimit: 1,
    generation: `generation-${randomUUID()}`,
    legacyDirectory: legacy.path,
  }));
  const children = await Promise.all(
    inputs.map((input) =>
      worker(input, { operation: "import", pauseSelector: true }),
    ),
  );
  try {
    children.forEach((child) => child.release());
    await Promise.all(children.map((child) => child.prepared));
    children.forEach((child) => child.select());
    const results = await Promise.all(children.map((child) => child.done));
    expect(results.filter((value) => value.error === undefined)).toHaveLength(
      1,
    );
    expect(
      results.filter((value) => value.error !== undefined)[0]!.error,
    ).toMatch(/selection lost/);
    const winner =
      inputs[results.findIndex((value) => value.error === undefined)]!;
    const selector = await readFile(join(root, "authority-backend.json"));
    await importLegacyAuthorityStore(winner);
    expect(await readFile(join(root, "authority-backend.json"))).toEqual(
      selector,
    );
    const store = await openWatcherTrustedHeadAuthorityStore(winner);
    try {
      expect(await store.readCurrent()).toEqual(head(legacy.policy, 2, "77"));
    } finally {
      store.close();
    }
  } finally {
    await Promise.all(children.map((child) => child.close()));
  }
});

it.each(["before", "after"] as const)(
  "resumes authenticated initialization after actual SIGKILL %s commit",
  async (fault) => {
    const input = {
      directory: await directory(),
      policy: policy(),
      recordAuthenticationKey,
      liveRecordLimit: 2,
      generation: `generation-${randomUUID()}`,
    };
    const child = await worker(input, { operation: "initialize", fault });
    try {
      child.release();
      expect((await child.done).signal).toBe("SIGKILL");
      const intent = await readFile(
        join(input.directory, input.generation, "initialization-intent.json"),
      );
      await expect(
        readFile(join(input.directory, "authority-backend.json")),
      ).rejects.toThrow();
      await initializeSelectedAuthorityStore(input);
      expect(
        await readFile(
          join(input.directory, input.generation, "initialization-intent.json"),
        ),
      ).toEqual(intent);
      const store = await openWatcherTrustedHeadAuthorityStore(input);
      try {
        expect(await store.readCurrent()).toBeNull();
      } finally {
        store.close();
      }
    } finally {
      await child.close();
    }
  },
);

it.each(["before", "after"] as const)(
  "resumes the same generation after actual SIGKILL %s exclusive selector publication",
  async (selectorFault) => {
    const input = {
      directory: await directory(),
      policy: policy(),
      recordAuthenticationKey,
      liveRecordLimit: 2,
      generation: `generation-${randomUUID()}`,
    };
    const child = await worker(input, {
      operation: "initialize",
      selectorFault,
    });
    try {
      child.release();
      expect((await child.done).signal).toBe("SIGKILL");
      await initializeSelectedAuthorityStore(input);
      const store = await openWatcherTrustedHeadAuthorityStore(input);
      try {
        expect(await store.readCurrent()).toBeNull();
      } finally {
        store.close();
      }
    } finally {
      await child.close();
    }
  },
);

it("allows exactly one competing OS-process CAS through checkpoint retirement", async () => {
  const scene = await sqliteScene(1),
    prior = await scene.advance(1);
  scene.store.close();
  const a = head(scene.input.policy, 1, "88"),
    b = head(scene.input.policy, 1, "99");
  const children = await Promise.all(
    [a, b].map((nextTrustedHead) =>
      worker(scene.input, {
        operation: "cas",
        expectedTrustedHead: prior,
        nextTrustedHead,
      }),
    ),
  );
  try {
    children.forEach((child) => child.release());
    const results = await Promise.all(children.map((child) => child.done));
    expect(results.map((value) => value.error)).toEqual([undefined, undefined]);
    const receipts = results.map(
      (value) => value.result as { committed: boolean; head: unknown },
    );
    expect(receipts.filter((value) => value.committed)).toHaveLength(1);
    const winner = receipts.find((value) => value.committed)!.head;
    expect(receipts.map((value) => value.head)).toEqual([winner, winner]);
    const store = await openWatcherTrustedHeadAuthorityStore(scene.input);
    try {
      expect(await store.readCurrent()).toEqual(winner);
    } finally {
      store.close();
    }
  } finally {
    await Promise.all(children.map((child) => child.close()));
  }
});
