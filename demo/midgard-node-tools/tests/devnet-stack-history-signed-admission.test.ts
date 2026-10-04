import { spawn } from "node:child_process";
import {
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  symlinkSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import * as watcher from "midgard-watcher";
import { build } from "tsup";
import { afterEach, beforeAll, expect, it, vi } from "vitest";

import { HistoryConfigurationRefusal } from "../src/devnet-stack/history-configuration-refusal.js";
import { historyProofDeadline } from "../src/devnet-stack/history-proof-deadline.js";
import { historyRecordedBinding } from "../src/devnet-stack/history-recorded-binding.js";
import {
  historyAdmissionPublicPaths,
  type HistoryAdmissionScope,
  makeHistorySignedAdmission,
} from "../src/devnet-stack/history-signed-admission.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { releasePaths } from "../src/devnet-stack/watcher-release.js";

const loader = vi.hoisted(() => {
  const state: {
    calls: number;
    before?: () => Promise<void>;
    afterAuthority?: () => Promise<void>;
  } = { calls: 0 };
  return state;
});
// Use the actual public signed loader/verifier in this source unit; compiled
// dynamic/static native identity is covered independently by external3.
vi.mock("../src/devnet-stack/watcher-release.js", async (original) => ({
  ...(await original<
    typeof import("../src/devnet-stack/watcher-release.js")
  >()),
  loadWatcherModule: async () => {
    await loader.before?.();
    return {
      ...watcher,
      loadWatcherVerifiedDeploymentAuthority: async (
        input: Parameters<
          typeof watcher.loadWatcherVerifiedDeploymentAuthority
        >[0],
      ) => {
        loader.calls += 1;
        const verified =
          await watcher.loadWatcherVerifiedDeploymentAuthority(input);
        await loader.afterAuthority?.();
        return verified;
      },
    };
  },
}));
const closes: (() => Promise<void>)[] = [];
afterEach(async () => {
  loader.calls = 0;
  loader.before = undefined;
  loader.afterAuthority = undefined;
  for (const close of closes.splice(0)) await close();
});
const root = fileURLToPath(new URL("../", import.meta.url));
beforeAll(async () => {
  await build({
    config: join(root, "tsup.config.ts"),
    entry: ["tests/helpers/history-binding-fixture.ts"],
    outDir: "dist/history-binding-fixture",
    clean: true,
    target: "node22",
    noExternal: [
      "midgard-watcher/tests/support/deployment-authority-fixture",
      "midgard-watcher/tests/runtime/process-config.watcher-config-value",
    ],
  });
}, 30000);
const command = (directory: string) =>
  new Promise<void>((resolve, reject) => {
    const child = spawn(
      process.execPath,
      [
        join(root, "dist/history-binding-fixture/history-binding-fixture.js"),
        directory,
      ],
      { cwd: root, detached: true, stdio: ["ignore", "pipe", "pipe"] },
    );
    const pid = child.pid;
    const signal = (kind: NodeJS.Signals) => {
      if (pid === undefined) return;
      try {
        process.kill(-pid, kind);
      } catch {
        /* Already joined. */
      }
    };
    let output = "";
    let errors = "";
    let failure: Error | undefined;
    let killing: ReturnType<typeof setTimeout> | undefined;
    const stop = (reason: string) => {
      failure ??= Error(reason);
      signal("SIGTERM");
      killing ??= setTimeout(() => signal("SIGKILL"), 1000);
    };
    const timer = setTimeout(
      () => stop("owned fixture exceeded bounded deadline"),
      20000,
    );
    child.stdout.on("data", (bytes: Buffer) => {
      output += bytes.toString("utf8");
      if (output.length + errors.length > 1_048_576)
        stop("owned fixture output exceeded bound");
    });
    child.stderr.on("data", (bytes: Buffer) => {
      errors += bytes.toString("utf8");
      if (output.length + errors.length > 1_048_576)
        stop("owned fixture output exceeded bound");
    });
    child.once("error", (error) => {
      failure = error;
    });
    child.once("close", (code) => {
      clearTimeout(timer);
      clearTimeout(killing);
      signal("SIGKILL");
      if (failure !== undefined) reject(failure);
      else if (
        code !== 0 ||
        !output.includes("PASS synthetic signed history public evidence")
      )
        reject(Error(`owned signed fixture failed: ${errors}`));
      else resolve();
    });
  });
const setup = async () => {
  const directory = mkdtempSync("/var/tmp/codex-rel-history-binding-");
  closes.push(async () => rmSync(directory, { recursive: true, force: true }));
  await command(directory);
  const layout = makeLayout(directory);
  const run: RunEnv = {
    runId: "synthetic-history-binding",
    composeProject: "synthetic-history",
    networkMagic: 1,
    portOffset: 0,
    ogmiosPort: 2337,
    kupoPort: 2442,
    postgresPort: 5433,
    postgresUser: "unused",
    postgresPassword: "unused-synthetic",
    postgresDatabase: "unused",
    cardanoImage: "unused",
    postgresImage: "unused",
  };
  const paths = releasePaths(layout);
  const config: unknown = JSON.parse(
    readFileSync(layout.watcherProcessConfig, "utf8"),
  );
  watcher.parseWatcherProcessConfig(config);
  if (config === null || typeof config !== "object" || Array.isArray(config))
    throw Error("synthetic wire config malformed");
  return { layout, run, paths, config };
};
const deadline = () => {
  const result = historyProofDeadline(5000);
  if (result === null) throw Error("synthetic bounded deadline invalid");
  return result;
};
const prepare = async () => {
  const f = await setup();
  let scope: HistoryAdmissionScope = {
    codeStamp: "code-a",
    serviceSpecsDigest: "spec-a",
    incarnation: "actual-child-a",
  };
  const guard = (fingerprint?: string) =>
    makeHistorySignedAdmission({
      layout: f.layout,
      run: f.run,
      publicBindingDigest: historyRecordedBinding(f.layout, f.run, "Preprod")
        .digest,
      deploymentFingerprint:
        fingerprint ??
        historyRecordedBinding(f.layout, f.run, "Preprod").manifest.manifestId,
      expectedNetwork: "Preprod",
      deadline: deadline(),
      currentScope: () => scope,
    });
  const create = async () => {
    const cache = guard();
    await cache.admit(deadline());
    return cache;
  };
  return {
    ...f,
    create,
    guard,
    changeScope: (next: HistoryAdmissionScope) => {
      scope = next;
    },
  };
};
it("retains genuine verified identity and immutable metadata while rechecking unchanged public bytes", async () => {
  const f = await prepare();
  const cache = await f.create();
  const first = cache.current(deadline());
  const second = cache.current(deadline());
  expect(first).toBe(second);
  expect(loader.calls).toBe(1);
  expect(first.release.policy.automaticRecoveryMaxDepth).toBe(2160);
  expect(() =>
    Object.assign(first.providers[0]?.binding ?? {}, { sourceId: "forged" }),
  ).toThrow(TypeError);
  expect(() =>
    Object.assign(first.authorities[0] ?? {}, { releaseFinality: null }),
  ).toThrow(TypeError);
  expect(() =>
    Object.assign(first.config, { schemaVersion: "forged" }),
  ).toThrow(TypeError);
  expect(cache.current(deadline())).toBe(first);
});
it.each(Array.from({ length: 12 }, (_, index) => index))(
  "refuses raw input %i drift permanently, even when parsed semantics remain equal",
  async (index) => {
    const f = await prepare();
    const cache = await f.create();
    const paths = historyAdmissionPublicPaths(f.layout);
    expect(paths).toHaveLength(12);
    const path = paths[index];
    if (path === undefined) throw Error("fixture allowlist missing");
    const original = readFileSync(path);
    writeFileSync(path, Buffer.concat([original, Buffer.from("\n")]));
    expect(() => cache.current(deadline())).toThrow(
      HistoryConfigurationRefusal,
    );
    writeFileSync(path, original);
    expect(() => cache.current(deadline())).toThrow(
      HistoryConfigurationRefusal,
    );
    expect(loader.calls).toBe(1);
  },
);
it.each(["codeStamp", "serviceSpecsDigest", "incarnation"] as const)(
  "refuses observed %s drift without adopting or refilling",
  async (field) => {
    const f = await prepare();
    const cache = await f.create();
    f.changeScope({
      codeStamp: "code-a",
      serviceSpecsDigest: "spec-a",
      incarnation: "actual-child-a",
      [field]: "replacement",
    });
    expect(() => cache.current(deadline())).toThrow(
      HistoryConfigurationRefusal,
    );
    f.changeScope({
      codeStamp: "code-a",
      serviceSpecsDigest: "spec-a",
      incarnation: "actual-child-a",
    });
    expect(() => cache.current(deadline())).toThrow(
      HistoryConfigurationRefusal,
    );
    expect(loader.calls).toBe(1);
  },
);
it("rejects complete authority/provider changes after the genuine signed loader await", async () => {
  const f = await prepare();
  loader.afterAuthority = async () => {
    writeFileSync(
      f.paths.authority,
      readFileSync(f.paths.authority, "utf8") + "\n",
    );
  };
  await expect(f.create()).rejects.toThrow(HistoryConfigurationRefusal);
  expect(loader.calls).toBe(1);
});
it("does not turn a budget expiry into permanent drift or cache a failure as successful admission", async () => {
  const f = await prepare();
  const cache = await f.create();
  expect(() => cache.current(0)).toThrow("deadline elapsed");
  expect(
    cache.current(deadline()).release.policy.automaticRecoveryMaxDepth,
  ).toBe(2160);
  expect(loader.calls).toBe(1);
});
it("refuses a path replacement even with identical bytes", async () => {
  const f = await prepare();
  const cache = await f.create();
  const path = f.paths.rules;
  const saved = path + ".saved";
  renameSync(path, saved);
  writeFileSync(path, readFileSync(saved));
  expect(() => cache.current(deadline())).toThrow(HistoryConfigurationRefusal);
});
it("does not cache genuine signed-verifier failures", async () => {
  const f = await prepare();
  loader.before = async () => {
    throw Error("synthetic transport interruption");
  };
  await expect(f.create()).rejects.toThrow("synthetic transport interruption");
  loader.before = undefined;
  expect(
    (await f.create()).current(deadline()).release.policy
      .automaticRecoveryMaxDepth,
  ).toBe(2160);
  expect(loader.calls).toBe(1);
});

it.each(Array.from({ length: 12 }, (_, index) => index))(
  "rejects input %i changed across genuine signed verification despite equal parsed semantics",
  async (index) => {
    const f = await prepare();
    const path = historyAdmissionPublicPaths(f.layout)[index];
    if (path === undefined) throw Error("fixture public input absent");
    loader.afterAuthority = async () => {
      writeFileSync(path, readFileSync(path, "utf8") + "\n");
    };
    await expect(f.create()).rejects.toThrow(HistoryConfigurationRefusal);
    expect(loader.calls).toBe(1);
  },
);

it.each(["codeStamp", "serviceSpecsDigest", "incarnation"] as const)(
  "rejects %s changed during genuine signed-loader await",
  async (field) => {
    const f = await prepare();
    loader.afterAuthority = async () => {
      f.changeScope({
        codeStamp: "code-a",
        serviceSpecsDigest: "spec-a",
        incarnation: "actual-child-a",
        [field]: "replacement",
      });
    };
    await expect(f.create()).rejects.toThrow(HistoryConfigurationRefusal);
    expect(loader.calls).toBe(1);
  },
);
it.each(["missing", "utf8", "oversized"])(
  "refuses %s public evidence without a replacement write",
  async (kind) => {
    const f = await prepare();
    const cache = await f.create();
    const path = f.paths.rules;
    if (kind === "missing") rmSync(path);
    else
      writeFileSync(
        path,
        kind === "utf8"
          ? Buffer.from([0xff])
          : Buffer.alloc(16 * 1024 * 1024 + 1),
      );
    expect(() => cache.current(deadline())).toThrow(
      HistoryConfigurationRefusal,
    );
    if (kind !== "missing")
      expect(readFileSync(path).length).toBe(
        kind === "utf8" ? 1 : 16 * 1024 * 1024 + 1,
      );
    expect(loader.calls).toBe(1);
  },
);
it("rejects a genuinely invalid signature without granting an admission", async () => {
  const f = await prepare();
  const original = readFileSync(f.paths.authority, "utf8");
  writeFileSync(
    f.paths.authority,
    original.replace(
      /"signature":"[0-9a-f]{128}"/u,
      '"signature":"' + "00".repeat(64) + '"',
    ),
  );
  await expect(f.create()).rejects.toThrow(HistoryConfigurationRefusal);
  writeFileSync(f.paths.authority, original);
  expect(
    (await f.create()).current(deadline()).release.policy
      .automaticRecoveryMaxDepth,
  ).toBe(2160);
  expect(loader.calls).toBe(2);
});

it("retains first-admission drift refusal in the same closure after restoring public files", async () => {
  const f = await prepare();
  const cache = f.guard();
  const original = readFileSync(f.paths.authority);
  loader.afterAuthority = async () => {
    writeFileSync(
      f.paths.authority,
      Buffer.concat([original, Buffer.from("\n")]),
    );
  };
  await expect(cache.admit(deadline())).rejects.toThrow(
    HistoryConfigurationRefusal,
  );
  loader.afterAuthority = undefined;
  writeFileSync(f.paths.authority, original);
  await expect(cache.admit(deadline())).rejects.toThrow(
    HistoryConfigurationRefusal,
  );
  expect(() => cache.current(deadline())).toThrow(HistoryConfigurationRefusal);
  expect(loader.calls).toBe(1);
});
it("retries ordinary initial interruption only within its unchanged pinned generation", async () => {
  const f = await prepare();
  const cache = f.guard();
  loader.before = async () => {
    throw Error("synthetic ordinary interruption");
  };
  await expect(cache.admit(deadline())).rejects.toThrow(
    "synthetic ordinary interruption",
  );
  expect(() => cache.current(deadline())).toThrow("has not completed");
  loader.before = undefined;
  const admitted = await cache.admit(deadline());
  expect(cache.current(deadline())).toBe(admitted);
  expect(loader.calls).toBe(1);
});

it("requires the expected deployment fingerprint even after genuine admission", async () => {
  const f = await prepare();
  const cache = f.guard("ab".repeat(32));
  await expect(cache.admit(deadline())).rejects.toThrow(
    HistoryConfigurationRefusal,
  );
  await expect(cache.admit(deadline())).rejects.toThrow(
    HistoryConfigurationRefusal,
  );
  expect(loader.calls).toBe(1);
});

it("refuses newly symlinked public evidence before accepting cached admission", async () => {
  const f = await prepare();
  const cache = await f.create();
  const path = f.paths.rules;
  renameSync(path, path + ".saved");
  symlinkSync(path + ".saved", path);
  expect(() => cache.current(deadline())).toThrow(HistoryConfigurationRefusal);
  expect(loader.calls).toBe(1);
});
it("allows unchanged bytes and inode after a timestamp-only touch", async () => {
  const f = await prepare();
  const cache = await f.create();
  const before = cache.current(deadline());
  utimesSync(f.paths.rules, new Date(1), new Date(1));
  expect(cache.current(deadline())).toBe(before);
  expect(loader.calls).toBe(1);
});
