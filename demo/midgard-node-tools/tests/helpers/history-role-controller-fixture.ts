import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { Duplex } from "node:stream";
import { fileURLToPath } from "node:url";

import { ensureArtifacts } from "../../src/devnet-stack/deploy.js";
import {
  codeStamp,
  runtimeDistTargets,
} from "../../src/devnet-stack/dist-freshness.js";
import { historyChildClient } from "../../src/devnet-stack/history-child-client.js";
import { historyRecordedBinding } from "../../src/devnet-stack/history-recorded-binding.js";
import { createHistoryRoleRegistry } from "../../src/devnet-stack/history-role-registry.js";
import { loadIdentities } from "../../src/devnet-stack/identities.js";
import { makeLayout, readRunEnv } from "../../src/devnet-stack/layout.js";
import {
  refuseService,
  requestServiceRecovery,
  serviceRefusal,
} from "../../src/devnet-stack/service-refusal.js";
import {
  DEFAULT_POLICY,
  type ServiceSpec,
  superviseServices,
  type SupervisorPaths,
} from "../../src/devnet-stack/supervisor.js";
import { watcherServiceSpecs } from "../../src/devnet-stack/watcher.js";
import { loadWatcherModule } from "../../src/devnet-stack/watcher-release.js";
import {
  type HistoryOwnedChild,
  historyOwnedChild,
} from "./history-owned-child.js";
import { exerciseHistoryUnix } from "./history-role-unix-fixture.js";

const runDir = process.argv[2];
if (runDir === undefined) throw Error("owned synthetic run directory required");
const root = fileURLToPath(new URL("../../", import.meta.url));
const layout = makeLayout(runDir);
const run = readRunEnv(layout);
const time = <T>(name: string, operation: () => T): T => {
  const started = performance.now();
  const value = operation();
  console.info(
    "owned compiled timing",
    name,
    Math.round(performance.now() - started),
  );
  return value;
};
time("codeStamp", () => codeStamp(runtimeDistTargets(layout)));
const binding = time("publicDescriptor", () =>
  historyRecordedBinding(layout, run, "Custom"),
);
for (let attempt = 0; attempt < 3; attempt++)
  time(`publicDescriptorWarm${attempt}`, () =>
    historyRecordedBinding(layout, run, "Custom"),
  );
const context = {
  layout,
  run,
  identities: loadIdentities(layout),
  artifacts: ensureArtifacts(layout),
};
const specs: ServiceSpec[] = watcherServiceSpecs(context, {
  txHash: "00".repeat(32),
  outputIndex: 0,
}).filter((spec) => spec.historyReadiness !== undefined);
const recorderSpec = specs.find(
  (spec) => spec.historyReadiness?.role === "history-recorder",
);
if (recorderSpec === undefined) throw Error("actual recorder spec absent");
console.info(
  "owned actual generated history names",
  specs.map((spec) => spec.name),
);
mkdirSync(layout.logs, { recursive: true, mode: 0o700 });
const boundaryMode = process.argv[3] === "supervisor-cas-drift";
let guardScopeReads = 0;
let proofStarted = 0;
let boundaryMutation = false;
const paths: SupervisorPaths = {
  runDir: layout.runDir,
  runtimeCodeStamp: () => {
    const stamp = codeStamp(runtimeDistTargets(layout));
    if (boundaryMode && !boundaryMutation) {
      const stack = new Error().stack ?? "";
      if (
        stack.includes("check") &&
        !stack.includes("guarded") &&
        !stack.includes("active") &&
        !stack.includes("key")
      ) {
        guardScopeReads = 0;
        proofStarted = performance.now();
      }
      if (
        stack.includes("guarded") &&
        ++guardScopeReads === 18 &&
        performance.now() - proofStarted < 4500
      )
        queueMicrotask(() => {
          writeFileSync(
            layout.watcherHistoryCa,
            Buffer.concat([
              readFileSync(layout.watcherHistoryCa),
              Buffer.from("\n"),
            ]),
          );
          boundaryMutation = true;
        });
    }
    return stamp;
  },
  serviceSpecs: specs,
  pidDir: join(layout.state, "services"),
  events: layout.supervisorEvents,
  serviceLog: layout.serviceLog,
};
const recorder = recorderSpec;
if (recorder === undefined) throw Error("owned recorder spec absent");
const registry = createHistoryRoleRegistry(paths);
const children: HistoryOwnedChild[] = [];
let stopping = false;
const abort = () => {
  stopping = true;
  registry.close();
  for (const child of children) {
    if (child.child.pid === undefined || child.child.exitCode !== null)
      continue;
    try {
      process.kill(-child.child.pid, "SIGKILL");
    } catch {
      /* Already exited. */
    }
  }
};
process.once("SIGTERM", abort);
process.once("SIGINT", abort);
let tunnel: HistoryOwnedChild | undefined;
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));
const check = async () => {
  const started = performance.now();
  const ready = await registry.check(recorder, 5000);
  console.info("owned compiled proof", {
    ready,
    elapsedMs: Math.round(performance.now() - started),
    ...registry.diagnostic(),
  });
  return ready;
};
const supervisorMode = process.argv[3] === "supervisor" || boundaryMode;
if (process.argv[3] === "child-refusal") {
  const attempt = registry.prepare(recorder);
  if (attempt === null) throw Error("owned recorder attempt absent");
  const child = historyOwnedChild(recorder.command, recorder.args, {
    cwd: root,
    env: { ...process.env, ...attempt.env },
    channel: true,
  });
  children.push(child);
  const pipe = child.child.stdio[3];
  if (!(pipe instanceof Duplex) || child.child.pid === undefined)
    throw Error("owned recorder channel absent");
  const client = historyChildClient({
    actor: {
      role: "history-recorder",
      runId: run.runId,
      deploymentFingerprint: binding.manifest.manifestId,
      ...attempt.scope,
      attemptId: attempt.attemptId,
      childPid: child.child.pid,
    },
    pipe,
  });
  try {
    const startup = performance.now() + 15000;
    while (!child.output().includes('"service":"history-recorder"')) {
      if (child.child.exitCode !== null || performance.now() >= startup)
        throw Error(child.output());
      await sleep(50);
    }
    let established = await client.request("seal", null, 5000);
    const acquisitionDeadline = performance.now() + 20000;
    while (
      established?.offer == null &&
      performance.now() < acquisitionDeadline
    ) {
      await sleep(100);
      established = await client.request("seal", null, 5000);
    }
    assert.notEqual(
      established?.offer ?? null,
      null,
      "actual native acquisition must precede expiry control",
    );
    const answered = new Promise<void>((resolve, reject) => {
      const timer = setTimeout(
        () => reject(Error("owned expired request did not physically answer")),
        5000,
      );
      pipe.once("data", () => {
        clearTimeout(timer);
        resolve();
      });
    });
    assert.equal((await client.request("seal", null, 50))?.offer ?? null, null);
    // Join the actual expired handler's unknown reply before issuing another
    // nonce; deliberate overlapping handlers retire an FD3 attempt.
    await answered;
    assert.equal(
      child.child.exitCode,
      null,
      "ordinary proof expiry must keep the child eligible for a later nonce",
    );
    const fresh = await client.request("seal", null, 5000);
    assert.notEqual(
      fresh?.offer ?? null,
      null,
      "a later unchanged nonce must prove the complete retained window",
    );
    const ca = readFileSync(layout.watcherHistoryCa);
    writeFileSync(
      layout.watcherHistoryCa,
      Buffer.concat([ca, Buffer.from("\n")]),
    );
    assert.equal(
      (await client.request("seal", null, 5000))?.offer ?? null,
      null,
    );
    const exitDeadline = performance.now() + 5000;
    while (child.child.exitCode === null && performance.now() < exitDeadline)
      await sleep(50);
    assert.equal(
      child.child.exitCode,
      78,
      "actual intrinsic recorded-input refusal must stop the owning child with78",
    );
  } finally {
    client.close();
    await child.close();
    registry.close();
  }
} else if (process.argv[3] === "unix" || process.argv[3] === "unix-stable") {
  await exerciseHistoryUnix(
    context,
    specs,
    paths,
    process.argv[3] === "unix-stable",
  );
} else if (supervisorMode) {
  const stopped = new AbortController();
  const stop = () => stopped.abort();
  process.once("SIGTERM", stop);
  process.once("SIGINT", stop);
  const refusal = refuseService(paths, recorder.name);
  const running = superviseServices(specs, paths, stopped.signal, {
    ...DEFAULT_POLICY,
    stopGraceMs: 1000,
  });
  const wait = async (
    predicate: () => boolean,
    message: string,
    timeoutMs = 30000,
  ) => {
    const deadline = performance.now() + timeoutMs;
    while (!predicate()) {
      if (stopped.signal.aborted || performance.now() >= deadline)
        throw Error(message);
      await sleep(50);
    }
  };
  try {
    await wait(
      () =>
        specs
          .filter((spec) => spec !== recorder)
          .every((spec) => {
            const log = paths.serviceLog(spec.name);
            return (
              existsSync(log) &&
              readFileSync(log, "utf8").includes(
                `"service":"${spec.historyReadiness?.role}"`,
              )
            );
          }),
      "actual supervised providers must start",
    );
    assert.equal(
      serviceRefusal(paths, recorder.name)?.refusalId,
      refusal.refusalId,
    );
    requestServiceRecovery(
      paths,
      recorder.name,
      refusal.refusalId,
      "owned synthetic unchanged-scope reattempt",
    );
    await wait(
      () =>
        boundaryMode
          ? boundaryMutation
          : serviceRefusal(paths, recorder.name) === undefined,
      "actual full-range proof must reach the supervisor CAS boundary",
      65000,
    );
    if (boundaryMode) {
      assert.equal(
        serviceRefusal(paths, recorder.name)?.refusalId,
        refusal.refusalId,
        "public drift after proof must retain the same authorized refusal",
      );
      assert.equal(
        readFileSync(paths.events, "utf8").includes(
          '"event":"refusal-recovered"',
        ),
        false,
      );
    } else console.info("owned actual supervisor full2160 CAS recovered");
  } finally {
    if (existsSync(paths.events))
      console.info(
        "owned supervisor public events",
        readFileSync(paths.events, "utf8"),
      );
    stopped.abort();
    await running;
    process.removeListener("SIGTERM", stop);
    process.removeListener("SIGINT", stop);
    registry.close();
  }
} else
  try {
    for (const spec of specs) {
      const attempt = registry.prepare(spec);
      if (attempt === null) throw Error("owned role scope absent");
      const child = historyOwnedChild(spec.command, spec.args, {
        cwd: root,
        env: { ...process.env, ...attempt.env },
        channel: true,
      });
      children.push(child);
      if (spec.historyReadiness?.role === "history-tunnel") tunnel = child;
      registry.register(attempt, child.child);
      const deadline = performance.now() + 10000;
      while (
        !child.output().includes(`"service":"${spec.historyReadiness?.role}"`)
      ) {
        if (
          stopping ||
          child.child.exitCode !== null ||
          performance.now() >= deadline
        )
          throw Error(
            `${spec.name} synthetic startup failed: ${child.output().slice(-1000)}`,
          );
        await sleep(50);
      }
    }
    let ready = await check();
    const watcherStarted = performance.now();
    const watcher = await loadWatcherModule(layout);
    console.info(
      "owned compiled timing",
      "watcherModule",
      Math.round(performance.now() - watcherStarted),
    );
    const signatureStarted = performance.now();
    const verified = await watcher.loadWatcherVerifiedDeploymentAuthority({
      path: binding.config.deploymentAuthorityPath,
      ruleBundlePath: binding.config.ruleBundlePath,
    });
    console.info(
      "owned compiled timing",
      "signedAuthority",
      Math.round(performance.now() - signatureStarted),
    );
    const releaseStarted = performance.now();
    await watcher
      .watcherDeploymentReleaseFinalityAuthority(verified.deploymentIdentity)
      .verifyForWorkflow({
        deploymentFingerprint: binding.manifest.manifestId,
      });
    console.info(
      "owned compiled timing",
      "releaseVerification",
      Math.round(performance.now() - releaseStarted),
    );
    const deadline = performance.now() + 20000;
    while (!stopping && !ready && performance.now() < deadline) {
      await sleep(100);
      ready = await check();
    }
    assert.equal(
      ready,
      true,
      "actual compiled four-child full2160 proof must succeed within5s per attempt",
    );
    if (process.argv[3] === "registry-drift") {
      const recorderIndex = specs.indexOf(recorder);
      const pending = check();
      const timer = setTimeout(() => {
        specs.splice(recorderIndex, 1, {
          ...recorder,
          env: { ...recorder.env, OWNED_SYNTHETIC_SCOPE: "changed" },
        });
      }, 100);
      try {
        assert.equal(
          await pending,
          false,
          "service-set drift during actual native proof must hold",
        );
      } finally {
        clearTimeout(timer);
        specs.splice(recorderIndex, 1, recorder);
      }
      assert.equal(
        await check(),
        false,
        "restoring service declarations cannot resurrect a refused cohort cache",
      );
      assert.equal(
        children.every((child) => child.child.exitCode === null),
        true,
        "parent-only spec drift leaves actual old child identities unchanged",
      );
    } else {
      const predecessor = join(
        layout.watcherHistoryArchive("a"),
        "canonical",
        "1.json",
      );
      const bytes = readFileSync(predecessor);
      writeFileSync(predecessor, "{}");
      assert.equal(
        await check(),
        false,
        "corrupt physical predecessor must hold",
      );
      writeFileSync(predecessor, bytes);
      assert.equal(
        await check(),
        true,
        "restored exact physical predecessor must allow fresh proof",
      );
      if (tunnel === undefined) throw Error("owned tunnel child absent");
      await tunnel.close();
      assert.equal(
        await check(),
        false,
        "actual tunnel loss must revoke readiness",
      );
    }
  } finally {
    registry.close();
    await Promise.all(children.map((child) => child.close()));
    process.removeListener("SIGTERM", abort);
    process.removeListener("SIGINT", abort);
  }
