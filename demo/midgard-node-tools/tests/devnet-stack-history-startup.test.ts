import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import {
  chmodSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath } from "node:url";

import { build } from "tsup";
import { afterEach, beforeAll, expect, it, vi } from "vitest";

import {
  codeStamp,
  runtimeDistTargets,
} from "../src/devnet-stack/dist-freshness.js";
import { HistoryAdmissionExpired } from "../src/devnet-stack/history-admission-expired.js";
import { historyChildClient } from "../src/devnet-stack/history-child-client.js";
import type { HistoryChildActor } from "../src/devnet-stack/history-child-evidence.js";
import {
  HistoryConfigurationRefusal,
  HistoryEvidenceContradiction,
} from "../src/devnet-stack/history-configuration-refusal.js";
import { historyProofDeadline } from "../src/devnet-stack/history-proof-deadline.js";
import { historyPublicFile } from "../src/devnet-stack/history-public-file.js";
import { historyRecordedBinding } from "../src/devnet-stack/history-recorded-binding.js";
import { historyChildEnvironment } from "../src/devnet-stack/history-role-context.js";
import { makeHistorySignedAdmission } from "../src/devnet-stack/history-signed-admission.js";
import { makeLayout, readRunEnv } from "../src/devnet-stack/layout.js";
import { untrustedHistoryOffer } from "./helpers/history-untrusted-offer.js";

const root = fileURLToPath(new URL("../", import.meta.url));
const cleanup: (() => Promise<void>)[] = [];
afterEach(async () => {
  vi.restoreAllMocks();
  for (const close of cleanup.splice(0).reverse()) await close();
});
beforeAll(async () => {
  await build({
    config: join(root, "tsup.config.ts"),
    entry: [
      "tests/helpers/history-binding-fixture.ts",
      "tests/helpers/history-startup-command-child.ts",
    ],
    outDir: "dist/history-startup-fixture",
    clean: true,
    target: "node22",
    noExternal: [
      "midgard-watcher/tests/support/deployment-authority-fixture",
      "midgard-watcher/tests/runtime/process-config.watcher-config-value",
    ],
  });
}, 30000);
const ownChild = (entry: string, args: string[], env = process.env) => {
  const child = spawn(
    process.execPath,
    [join(root, "dist/history-startup-fixture", entry), ...args],
    {
      cwd: root,
      env,
      stdio: ["ignore", "pipe", "pipe", "pipe"],
    },
  );
  let output = "";
  let errors = "";
  child.stdout?.on("data", (chunk: Buffer) => {
    output += chunk.toString();
  });
  child.stderr?.on("data", (chunk: Buffer) => {
    errors += chunk.toString();
  });
  const joined = new Promise<number | null>((resolve, reject) => {
    child.once("error", reject);
    child.once("close", resolve);
  });
  const stop = async () => {
    if (child.exitCode === null && child.signalCode === null)
      child.kill("SIGTERM");
    const timer = setTimeout(() => child.kill("SIGKILL"), 2000);
    try {
      return await joined;
    } finally {
      clearTimeout(timer);
    }
  };
  cleanup.push(async () => {
    await stop();
  });
  return { child, joined, stop, output: () => output, errors: () => errors };
};
const fixture = async () => {
  const directory = mkdtempSync("/var/tmp/codex-history-startup-");
  cleanup.push(async () => rmSync(directory, { recursive: true, force: true }));
  const written = ownChild("history-binding-fixture.js", [directory, "20000"]);
  expect(await written.joined, written.errors()).toBe(0);
  expect(written.output()).toContain(
    "PASS synthetic signed history public evidence",
  );
  const layout = makeLayout(directory);
  writeFileSync(
    layout.runEnv,
    [
      `MIDGARD_PHASE4_RUN_DIR=${directory}`,
      "MIDGARD_PHASE4_RUN_ID=synthetic-history-binding",
      "MIDGARD_PHASE4_COMPOSE_PROJECT=synthetic-history",
      "MIDGARD_PHASE4_NETWORK_MAGIC=1",
      "MIDGARD_PHASE4_OGMIOS_PORT=22337",
      "MIDGARD_PHASE4_KUPO_PORT=22442",
      "MIDGARD_PHASE4_POSTGRES_PORT=5433",
      "MIDGARD_PHASE4_POSTGRES_USER=unused",
      "MIDGARD_PHASE4_POSTGRES_PASSWORD=unused-synthetic",
      "MIDGARD_PHASE4_POSTGRES_DATABASE=unused",
      "MIDGARD_PHASE4_CARDANO_NODE_IMAGE=unused",
      "MIDGARD_PHASE4_POSTGRES_IMAGE=unused",
    ].join("\n"),
  );
  const run = readRunEnv(layout);
  const recorded = historyRecordedBinding(layout, run, "Preprod");
  const scope = {
    codeStamp: "a".repeat(64),
    serviceSpecsDigest: "b".repeat(64),
    incarnation: "owned-synthetic-incarnation",
  };
  const owner = () =>
    makeHistorySignedAdmission({
      layout,
      run,
      expectedNetwork: "Preprod",
      expectedScope: scope,
      publicBindingDigest: recorded.digest,
      deploymentFingerprint: recorded.manifest.manifestId,
      currentScope: () => scope,
    });
  return { directory, layout, run, recorded, scope, owner };
};
const deadline = () => {
  const value = historyProofDeadline(5000);
  if (value === null) throw Error("owned cutoff invalid");
  return value;
};
it("keeps one lazy owner after first capture expiry and authenticates the next complete capture", async () => {
  const f = await fixture();
  const scopeReads = vi.fn(() => f.scope);
  const owner = makeHistorySignedAdmission({
    layout: f.layout,
    run: f.run,
    expectedNetwork: "Preprod",
    expectedScope: f.scope,
    publicBindingDigest: f.recorded.digest,
    deploymentFingerprint: f.recorded.manifest.manifestId,
    currentScope: scopeReads,
  });
  expect(scopeReads).not.toHaveBeenCalled();
  await expect(owner.admit(0)).rejects.toBeInstanceOf(HistoryAdmissionExpired);
  const admitted = await owner.admit(deadline());
  expect(owner.current(deadline())).toBe(admitted);
  expect(admitted.release.policy.automaticRecoveryMaxDepth).toBe(2160);
});
const launchCommand = async (
  f: Awaited<ReturnType<typeof fixture>>,
  wrongCode = false,
) => {
  const scope = {
    codeStamp: wrongCode
      ? "0".repeat(64)
      : codeStamp(runtimeDistTargets(f.layout)),
    serviceSpecsDigest: "b".repeat(64),
  };
  const attemptId = randomUUID();
  const specification = {
    role: "history-archive-a" as const,
    runId: f.run.runId,
    deploymentFingerprint: f.recorded.manifest.manifestId,
    publicBindingDigest: f.recorded.digest,
    expectedNetwork: "Preprod" as const,
  };
  const launched = ownChild("history-startup-command-child.js", [f.directory], {
    ...process.env,
    ...historyChildEnvironment(specification, scope, attemptId),
  });
  const pipe = launched.child.stdio[3];
  if (!(pipe instanceof Duplex) || launched.child.pid === undefined)
    throw Error("owned FD3 missing");
  const actor: HistoryChildActor = {
    ...scope,
    role: specification.role,
    runId: f.run.runId,
    deploymentFingerprint: specification.deploymentFingerprint,
    attemptId,
    childPid: launched.child.pid,
  };
  const client = historyChildClient({ actor, pipe });
  cleanup.push(async () => client.close());
  return { ...launched, actor, client };
};
it("actual compiled archive command answers null during expiry then constructs in the same PID after genuine signed admission", async () => {
  const f = await fixture();
  const launched = await launchCommand(f);
  await vi.waitFor(
    () => expect(launched.output()).toContain('"firstAttemptExpired":true'),
    { timeout: 15000 },
  );
  const pending = await launched.client.request(
    "prove",
    untrustedHistoryOffer(launched.actor),
    5000,
  );
  expect(pending?.offer).toBeNull();
  expect(pending?.actor.childPid).toBe(launched.child.pid);
  expect(launched.output()).not.toContain('"state":"ready"');
  await vi.waitFor(
    () =>
      expect(launched.output(), launched.errors()).toContain('"state":"ready"'),
    { timeout: 20000 },
  );
  expect(launched.child.exitCode).toBeNull();
  const current = await launched.client.request(
    "prove",
    untrustedHistoryOffer(launched.actor),
    5000,
  );
  expect(current?.actor.childPid).toBe(pending?.actor.childPid);
  expect(current?.offer).toBeNull(); // No native seal was supplied; no readiness proof is fabricated.
  expect(await launched.stop(), launched.errors()).toBe(0);
  expect(launched.output()).toContain('"commandJoined":true');
});
it("actual compiled pending command stops its backoff and joins without constructing a listener", async () => {
  const f = await fixture();
  const launched = await launchCommand(f);
  await vi.waitFor(
    () => expect(launched.output()).toContain('"firstAttemptExpired":true'),
    { timeout: 15000 },
  );
  const pending = await launched.client.request(
    "prove",
    untrustedHistoryOffer(launched.actor),
    5000,
  );
  expect(pending?.offer).toBeNull();
  expect(await launched.stop(), launched.errors()).toBe(0);
  expect(launched.output()).not.toContain('"state":"ready"');
  expect(launched.output()).toContain('"commandJoined":true');
});
it("actual compiled retry refuses mismatched recorded code rather than adopting a new generation", async () => {
  const f = await fixture();
  const launched = await launchCommand(f, true);
  expect(await launched.joined).toBe(78);
  expect(launched.errors()).toContain(
    "runtime code differs from recorded scope",
  );
  expect(launched.output()).not.toContain('"state":"ready"');
});

it("pins a completed raw key through loader expiry and preserves drift even at an expired new cutoff", async () => {
  const f = await fixture();
  const cutoff = deadline();
  const before = performance.now();
  let calls = 0;
  const owner = makeHistorySignedAdmission({
    layout: f.layout,
    run: f.run,
    expectedNetwork: "Preprod",
    expectedScope: f.scope,
    publicBindingDigest: f.recorded.digest,
    deploymentFingerprint: f.recorded.manifest.manifestId,
    currentScope: () => {
      calls += 1;
      if (calls === 2)
        vi.spyOn(performance, "now")
          .mockReturnValueOnce(before)
          .mockReturnValue(cutoff + 1);
      return f.scope;
    },
  });
  await expect(owner.admit(cutoff)).rejects.toBeInstanceOf(
    HistoryAdmissionExpired,
  );
  vi.restoreAllMocks();
  const path = f.layout.watcherProcessConfig;
  const original = readFileSync(path);
  writeFileSync(path, Buffer.concat([original, Buffer.from("\n")]));
  await expect(owner.admit(deadline())).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  writeFileSync(path, original);
  await expect(owner.admit(0)).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
});
it("discards an incomplete first capture but keeps the recorded semantic authority binding", async () => {
  const f = await fixture();
  const cutoff = deadline();
  let calls = 0;
  const owner = makeHistorySignedAdmission({
    layout: f.layout,
    run: f.run,
    expectedNetwork: "Preprod",
    expectedScope: f.scope,
    publicBindingDigest: f.recorded.digest,
    deploymentFingerprint: f.recorded.manifest.manifestId,
    currentScope: () => {
      calls += 1;
      if (calls === 2) vi.spyOn(performance, "now").mockReturnValue(cutoff + 1);
      return f.scope;
    },
  });
  await expect(owner.admit(cutoff)).rejects.toBeInstanceOf(
    HistoryAdmissionExpired,
  );
  vi.restoreAllMocks();
  const path = f.layout.watcherProcessConfig;
  writeFileSync(path, readFileSync(path, "utf8") + "\n");
  const admitted = await owner.admit(deadline());
  expect(admitted.release.deploymentIdentityDigest).toBe(
    f.recorded.manifest.manifestId,
  );
});
it("owns the expected actor primitives before the first baseline and latches a positive scope contradiction", async () => {
  const f = await fixture();
  const owner = f.owner();
  const original = f.scope.codeStamp;
  f.scope.codeStamp = "c".repeat(64);
  await expect(owner.admit(deadline())).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  f.scope.codeStamp = original;
  await expect(owner.admit(0)).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
});
it("preserves positively observed missing raw evidence even when the final clock check would expire", () => {
  vi.spyOn(performance, "now").mockReturnValueOnce(0).mockReturnValue(6000);
  expect(() =>
    historyPublicFile(
      "/tmp/codex-history-definitely-missing-" + randomUUID(),
      5000,
    ),
  ).toThrow(HistoryEvidenceContradiction);
});
it("preserves actual EACCES instead of translating it into an expired admission", async () => {
  const f = await fixture();
  const path = f.layout.watcherProcessConfig;
  chmodSync(path, 0);
  try {
    vi.spyOn(performance, "now").mockReturnValueOnce(0).mockReturnValue(6000);
    expect(() => historyPublicFile(path, 5000)).toThrow(/EACCES/u);
  } finally {
    chmodSync(path, 0o600);
  }
});

it("current never establishes the first raw baseline or authenticates an unadmitted owner", async () => {
  const f = await fixture();
  const owner = f.owner();
  expect(() => owner.current(deadline())).toThrow("has not completed");
  const path = f.layout.watcherProcessConfig;
  writeFileSync(path, readFileSync(path, "utf8") + "\n");
  expect((await owner.admit(deadline())).release.deploymentIdentityDigest).toBe(
    f.recorded.manifest.manifestId,
  );
});
it("requires the immutable recorded semantic binding before first admission", async () => {
  const f = await fixture();
  const owner = makeHistorySignedAdmission({
    layout: f.layout,
    run: f.run,
    expectedNetwork: "Preprod",
    expectedScope: f.scope,
    publicBindingDigest: "0".repeat(64),
    deploymentFingerprint: f.recorded.manifest.manifestId,
    currentScope: () => f.scope,
  });
  await expect(owner.admit(deadline())).rejects.toThrow(
    "configuration changed from this service generation",
  );
  await expect(owner.admit(0)).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
});
it.each(["run", "path"])(
  "refuses caller %s mutation before first admission instead of adopting the mutated loader input",
  async (field) => {
    const f = await fixture();
    const owner = f.owner();
    if (field === "run")
      Object.assign(f.run, { portOffset: f.run.portOffset + 1 });
    else
      Object.assign(f.layout, {
        watcherProcessConfig: f.layout.watcherProcessConfig + ".replacement",
      });
    await expect(owner.admit(deadline())).rejects.toBeInstanceOf(
      HistoryConfigurationRefusal,
    );
    await expect(owner.admit(0)).rejects.toBeInstanceOf(
      HistoryConfigurationRefusal,
    );
  },
);
it("keeps a positively observed closing scope contradiction permanent even when that observation exhausts the attempt", async () => {
  const f = await fixture();
  const cutoff = deadline();
  let calls = 0;
  const owner = makeHistorySignedAdmission({
    layout: f.layout,
    run: f.run,
    expectedNetwork: "Preprod",
    expectedScope: f.scope,
    publicBindingDigest: f.recorded.digest,
    deploymentFingerprint: f.recorded.manifest.manifestId,
    currentScope: () => {
      calls += 1;
      if (calls === 2) {
        vi.spyOn(performance, "now").mockReturnValue(cutoff + 1);
        return { ...f.scope, codeStamp: "changed" };
      }
      return f.scope;
    },
  });
  await expect(owner.admit(cutoff)).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
  vi.restoreAllMocks();
  await expect(owner.admit(0)).rejects.toBeInstanceOf(
    HistoryConfigurationRefusal,
  );
});
it("keeps an over-budget decoder failure uncertain instead of latching an uncompleted first capture", async () => {
  const f = await fixture();
  const owner = f.owner();
  const path = f.layout.watcherProcessConfig;
  const original = readFileSync(path);
  writeFileSync(path, Buffer.from([0xff]));
  const cutoff = deadline();
  const decode = TextDecoder.prototype.decode;
  vi.spyOn(TextDecoder.prototype, "decode").mockImplementation(function (
    this: TextDecoder,
    ...args
  ) {
    try {
      return decode.apply(this, args);
    } finally {
      if (args[0] instanceof Uint8Array && args[0][0] === 0xff)
        vi.spyOn(performance, "now").mockReturnValue(cutoff + 1);
    }
  });
  await expect(owner.admit(cutoff)).rejects.toBeInstanceOf(
    HistoryAdmissionExpired,
  );
  vi.restoreAllMocks();
  writeFileSync(path, original);
  expect((await owner.admit(deadline())).release.deploymentIdentityDigest).toBe(
    f.recorded.manifest.manifestId,
  );
});
