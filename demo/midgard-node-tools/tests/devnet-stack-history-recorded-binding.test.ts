import { spawn } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import * as watcher from "midgard-watcher";
import { build } from "tsup";
import { afterEach, beforeAll, expect, it, vi } from "vitest";

import { HistoryConfigurationRefusal } from "../src/devnet-stack/history-configuration-refusal.js";
import { historyProofDeadline } from "../src/devnet-stack/history-proof-deadline.js";
import {
  historyRecordedBinding,
  loadHistoryRoleAdmission,
} from "../src/devnet-stack/history-recorded-binding.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { HISTORY_ROLES } from "../src/devnet-stack/watcher-history.js";
import { releasePaths } from "../src/devnet-stack/watcher-release.js";

const loader = vi.hoisted(() => {
  const state: {
    before?: () => Promise<void>;
    afterAuthority?: () => Promise<void>;
  } = {};
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
const write = (path: string, value: unknown) =>
  writeFileSync(path, JSON.stringify(value));
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
it("binds recorded public provider certificates to the actual signed deployment and full configured policy", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  const before = [
    f.layout.watcherProcessConfig,
    f.paths.authority,
    f.paths.rules,
  ].map((path) => readFileSync(path, "utf8"));
  const admitted = await loadHistoryRoleAdmission(
    f.layout,
    f.run,
    binding.digest,
    deadline(),
    "Preprod",
  );
  expect(admitted.providers).toHaveLength(2);
  expect(admitted.release.policy.automaticRecoveryMaxDepth).toBe(2160);
  expect(admitted.manifest.manifestId).toBe(binding.manifest.manifestId);
  expect(
    [f.layout.watcherProcessConfig, f.paths.authority, f.paths.rules].map(
      (path) => readFileSync(path, "utf8"),
    ),
  ).toEqual(before);
});
it("refuses missing recorded public config without generating or writing replacement evidence", async () => {
  const f = await setup();
  rmSync(f.layout.watcherProcessConfig);
  expect(() => historyRecordedBinding(f.layout, f.run, "Preprod")).toThrow(
    HistoryConfigurationRefusal,
  );
  expect(() => readFileSync(f.layout.watcherProcessConfig)).toThrow();
});
it("refuses changed recorded provider/public configuration generation before signed admission", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  write(f.layout.watcherProcessConfig, {
    ...f.config,
    operationsEndpoint: "http://127.0.0.1:19999",
  });
  await expect(
    loadHistoryRoleAdmission(
      f.layout,
      f.run,
      binding.digest,
      deadline(),
      "Preprod",
    ),
  ).rejects.toMatchObject({
    message:
      "history recorded configuration changed from this service generation",
  });
});
it("rejects a changed signed release rather than granting authority from matching public identifiers", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  const authority: unknown = JSON.parse(
    readFileSync(f.paths.authority, "utf8"),
  );
  if (
    authority === null ||
    typeof authority !== "object" ||
    !("signedIdentity" in authority) ||
    authority.signedIdentity === null ||
    typeof authority.signedIdentity !== "object" ||
    !("attestation" in authority.signedIdentity) ||
    authority.signedIdentity.attestation === null ||
    typeof authority.signedIdentity.attestation !== "object"
  )
    throw Error("synthetic signed artifact malformed");
  write(f.paths.authority, {
    ...authority,
    signedIdentity: {
      ...authority.signedIdentity,
      attestation: {
        ...authority.signedIdentity.attestation,
        signature: "00".repeat(64),
      },
    },
  });
  const changed = historyRecordedBinding(f.layout, f.run, "Preprod");
  expect(changed.digest).not.toBe(binding.digest);
  await expect(
    loadHistoryRoleAdmission(
      f.layout,
      f.run,
      changed.digest,
      deadline(),
      "Preprod",
    ),
  ).rejects.toBeInstanceOf(HistoryConfigurationRefusal);
});

it("refuses a different independently declared deployment network", async () => {
  const f = await setup();
  expect(() => historyRecordedBinding(f.layout, f.run, "Custom")).toThrow(
    HistoryConfigurationRefusal,
  );
});

it("rechecks the public generation after asynchronous authenticated admission", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  loader.before = async () => {
    write(f.layout.watcherProcessConfig, {
      ...f.config,
      operationsEndpoint: "http://127.0.0.1:19999",
    });
  };
  await expect(
    loadHistoryRoleAdmission(
      f.layout,
      f.run,
      binding.digest,
      deadline(),
      "Preprod",
    ),
  ).rejects.toMatchObject({
    message: "history recorded configuration changed during signed admission",
  });
});

it("does not classify module I/O failure as a permanent configuration refusal", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  const failure = Object.assign(Error("synthetic module I/O failure"), {
    code: "EIO",
  });
  loader.before = async () => {
    throw failure;
  };
  await expect(
    loadHistoryRoleAdmission(
      f.layout,
      f.run,
      binding.digest,
      deadline(),
      "Preprod",
    ),
  ).rejects.toBe(failure);
});

it("expires the absolute budget across the asynchronous module boundary", async () => {
  const f = await setup();
  const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
  const budget = historyProofDeadline(20);
  if (budget === null) throw Error("synthetic budget invalid");
  loader.before = async () => {
    await new Promise<void>((resolve) => setTimeout(resolve, 30));
  };
  const result = loadHistoryRoleAdmission(
    f.layout,
    f.run,
    binding.digest,
    budget,
    "Preprod",
  );
  await expect(result).rejects.toThrow(
    "history role admission deadline elapsed",
  );
  await expect(result).rejects.not.toBeInstanceOf(HistoryConfigurationRefusal);
});

it("checks an expired budget before touching absent public evidence", () => {
  const layout = makeLayout("/var/tmp/codex-rel-history-never-created");
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
  const call = () =>
    historyRecordedBinding(layout, run, "Preprod", performance.now() - 1);
  expect(call).toThrow("history role admission deadline elapsed");
  expect(call).not.toThrow(HistoryConfigurationRefusal);
});

const authorityObject = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const publicAuthority = (path: string): Record<string, unknown> => {
  const value: unknown = JSON.parse(readFileSync(path, "utf8"));
  if (!authorityObject(value))
    throw Error("synthetic public authority document malformed");
  return value;
};
for (const field of ["trustRoots", "durableMarker", "policy"] as const) {
  const changed = (authority: Record<string, unknown>) => ({
    ...authority,
    [field]: field === "trustRoots" ? [] : null,
  });
  it(`binds complete public ${field} before asynchronous signed admission`, async () => {
    const f = await setup();
    const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
    write(f.paths.authority, changed(publicAuthority(f.paths.authority)));
    expect(historyRecordedBinding(f.layout, f.run, "Preprod").digest).not.toBe(
      binding.digest,
    );
    await expect(
      loadHistoryRoleAdmission(
        f.layout,
        f.run,
        binding.digest,
        deadline(),
        "Preprod",
      ),
    ).rejects.toMatchObject({
      message:
        "history recorded configuration changed from this service generation",
    });
  });
  it(`refuses ${field} changed after actual signed verification before final generation fence`, async () => {
    const f = await setup();
    const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
    loader.afterAuthority = async () => {
      write(f.paths.authority, changed(publicAuthority(f.paths.authority)));
    };
    const outcome = await loadHistoryRoleAdmission(
      f.layout,
      f.run,
      binding.digest,
      deadline(),
      "Preprod",
    ).then(
      (value) => ({ value }),
      (error: unknown) => ({ error }),
    );
    await expect(
      watcher.loadWatcherVerifiedDeploymentAuthority({
        path: f.paths.authority,
        ruleBundlePath: f.paths.rules,
      }),
    ).rejects.toThrow();
    expect("error" in outcome).toBe(true);
    if ("error" in outcome)
      expect(outcome.error).toMatchObject({
        message:
          "history recorded configuration changed during signed admission",
      });
  });
}

const providerPolicyMutation = (path: string) => {
  const authority = publicAuthority(path);
  const release = authority.releaseFinality;
  if (
    !authorityObject(release) ||
    !authorityObject(release.policy) ||
    release.policy.automaticRecoveryMaxDepth !== 2160
  )
    throw Error("synthetic authenticated provider recovery policy absent");
  write(path, {
    ...authority,
    releaseFinality: {
      ...release,
      policy: { ...release.policy, automaticRecoveryMaxDepth: 2161 },
    },
  });
};
for (const role of HISTORY_ROLES) {
  it(`binds the complete provider ${role} policy before asynchronous signed admission`, async () => {
    const f = await setup();
    const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
    providerPolicyMutation(
      join(f.layout.watcherHistoryArchive(role), "authority.json"),
    );
    expect(historyRecordedBinding(f.layout, f.run, "Preprod").digest).not.toBe(
      binding.digest,
    );
    await expect(
      loadHistoryRoleAdmission(
        f.layout,
        f.run,
        binding.digest,
        deadline(),
        "Preprod",
      ),
    ).rejects.toMatchObject({
      message:
        "history recorded configuration changed from this service generation",
    });
  });
  it(`refuses provider ${role} policy changed after actual signed verification before final generation fence`, async () => {
    const f = await setup();
    const binding = historyRecordedBinding(f.layout, f.run, "Preprod");
    loader.afterAuthority = async () => {
      providerPolicyMutation(
        join(f.layout.watcherHistoryArchive(role), "authority.json"),
      );
    };
    const outcome = await loadHistoryRoleAdmission(
      f.layout,
      f.run,
      binding.digest,
      deadline(),
      "Preprod",
    ).then(
      (value) => ({ value }),
      (error: unknown) => ({ error }),
    );
    loader.afterAuthority = undefined;
    const current = historyRecordedBinding(f.layout, f.run, "Preprod");
    // A fresh admission of the final document must fail full authenticated
    // policy comparison; the after-await fence cannot return its stale proof.
    await expect(
      loadHistoryRoleAdmission(
        f.layout,
        f.run,
        current.digest,
        deadline(),
        "Preprod",
      ),
    ).rejects.toMatchObject({
      message:
        "history authenticated release differs from recorded provider authorities",
    });
    expect("error" in outcome).toBe(true);
    if ("error" in outcome)
      expect(outcome.error).toMatchObject({
        message:
          "history recorded configuration changed during signed admission",
      });
  });
}
