import assert from "node:assert/strict";
import { readFileSync, writeFileSync } from "node:fs";

import {
  serviceReadiness,
  waitStablyReady,
} from "../../src/devnet-stack/chaos-stable.js";
import type { DeployContext } from "../../src/devnet-stack/deploy.js";
import { readJsonIfPresent } from "../../src/devnet-stack/durable.js";
import {
  type HistoryDaemonScope,
  queryHistoryDaemon,
} from "../../src/devnet-stack/history-daemon-query.js";
import { acquireLock } from "../../src/devnet-stack/lock.js";
import {
  recordSupervisorSpecs,
  serviceReports,
} from "../../src/devnet-stack/stack.js";
import {
  DEFAULT_POLICY,
  type ServiceSpec,
  superviseServices,
  type SupervisorPaths,
} from "../../src/devnet-stack/supervisor.js";

declare global {
  // eslint-disable-next-line no-var -- TypeScript global object augmentation requires var.
  var historyStableFixtureServiceSpecs: ServiceSpec[] | undefined;
}

/** Actual compiled supervisor, FD3 roles, native receipt and production reports. */
export const exerciseHistoryUnix = async (
  context: DeployContext,
  specs: readonly ServiceSpec[],
  paths: SupervisorPaths,
  stablePoll = false,
) => {
  const { layout, run } = context;
  const release = acquireLock(layout.supervisorPid);
  recordSupervisorSpecs(layout, specs);
  const stopped = new AbortController();
  const stop = () => stopped.abort();
  process.once("SIGTERM", stop);
  process.once("SIGINT", stop);
  let proofStarts = 0;
  let guardReads = 0;
  let proofStarted = 0;
  let armReplacement = false;
  let armPublicDrift = false;
  let publicDrift = false;
  let replaced = false;
  const discovery = () =>
    readJsonIfPresent<{
      scope: HistoryDaemonScope;
      socketPath: string | null;
      children: readonly {
        role: string;
        serviceName: string;
        pid: number;
        attemptId: string;
      }[];
    }>(layout.historyDaemonDescriptor);
  const daemonPaths: SupervisorPaths = {
    ...paths,
    historyDaemon: {
      descriptorPath: layout.historyDaemonDescriptor,
      supervisorPid: layout.supervisorPid,
      runId: run.runId,
    },
    runtimeCodeStamp: () => {
      const stamp = paths.runtimeCodeStamp?.();
      if (stamp === undefined) throw Error("owned fixture stamp required");
      const stack = new Error().stack ?? "";
      if (
        /at recoveryScope[^\n]*\n\s+at (?:Object\.)?check(?: \[as prove\])? /u.test(
          stack,
        )
      ) {
        proofStarts++;
        guardReads = 0;
        proofStarted = performance.now();
        if (armReplacement) {
          armReplacement = false;
          const tunnel = discovery()?.children.find(
            (child) => child.role === "history-tunnel",
          );
          if (tunnel === undefined) throw Error("owned tunnel absent");
          queueMicrotask(() => {
            process.kill(-tunnel.pid, "SIGTERM");
            replaced = true;
          });
        }
      }
      if (
        armPublicDrift &&
        stack.includes("guarded") &&
        ++guardReads === 18 &&
        performance.now() - proofStarted < 4500
      ) {
        armPublicDrift = false;
        queueMicrotask(() => {
          writeFileSync(
            layout.watcherHistoryCa,
            Buffer.concat([
              readFileSync(layout.watcherHistoryCa),
              Buffer.from("\n"),
            ]),
          );
          publicDrift = true;
        });
      }
      return stamp;
    },
  };
  const running = superviseServices(specs, daemonPaths, stopped.signal, {
    ...DEFAULT_POLICY,
    stopGraceMs: 1000,
  });
  const ready = async () => {
    const end = performance.now() + 30000;
    let latest = "";
    while (!stopped.signal.aborted && performance.now() < end) {
      const before = proofStarts;
      const reports = await serviceReports(layout, specs);
      latest = JSON.stringify(reports);
      if (reports.every((report) => report.alive && report.ready === true)) {
        assert.equal(
          proofStarts - before,
          1,
          "one whole four-role proof per production poll",
        );
        return reports;
      }
      assert.equal(reports.filter((report) => report.ready === true).length, 0);
      await new Promise<void>((resolve) => setTimeout(resolve, 100));
    }
    console.info(
      "owned unknown reports",
      latest,
      "proof starts",
      proofStarts,
      "descriptor",
      discovery(),
    );
    console.info(
      "owned supervisor events",
      readFileSync(paths.events, "utf8").slice(-12000),
    );
    throw Error("owned production reports must prove all four current roles");
  };
  try {
    await ready();
    if (stablePoll) {
      // Only the factory scope is selected in this compiled test bundle. These
      // are the same actual generated specs bound to this real supervisor.
      // Readiness/native/FD3/Unix proofs and all production pollers are unchanged.
      globalThis.historyStableFixtureServiceSpecs = [...specs];
      const oneShot = { txHash: "00".repeat(32), outputIndex: 0 };
      const poll = serviceReadiness(context, oneShot, process.pid);
      const before = proofStarts;
      const reports = await poll();
      const pollProofs = proofStarts - before;
      console.info("owned genuine exported stable poll", reports);
      await waitStablyReady(context, oneShot, 15000, {
        supervisorPid: process.pid,
        windowMs: 0,
      });
      assert.equal(
        reports.every((report) => report.alive && report.ready === true),
        true,
      );
      assert.equal(
        pollProofs,
        1,
        "one aggregate proof per exported stable poll",
      );
      console.info(
        "owned genuine exported awaitStable final poll passed full2160",
      );
      return;
    }
    const previous = discovery();
    assert.ok(previous?.socketPath);
    assert.deepEqual(
      previous.children.map((child) => [child.role, child.serviceName]),
      [
        "history-recorder",
        "history-archive-a",
        "history-archive-b",
        "history-tunnel",
      ].map((role) => {
        const spec = specs.find(
          (service) => service.historyReadiness?.role === role,
        );
        assert.ok(spec);
        assert.notEqual(
          spec.name,
          role,
          "fixture must preserve actual generated process names",
        );
        return [role, spec.name];
      }),
      "discovery binds actual factory service identities to protocol roles",
    );
    armReplacement = true;
    const interrupted = await serviceReports(layout, specs);
    assert.equal(
      replaced,
      true,
      "actual proof must trigger the owned child replacement",
    );
    assert.equal(
      interrupted.every((report) => report.ready === false),
      true,
    );
    await ready();
    const current = discovery();
    assert.ok(current?.socketPath);
    assert.notEqual(current.scope.incarnation, previous.scope.incarnation);
    assert.notEqual(current.socketPath, previous.socketPath);

    // Direct helper client deliberately checks only daemon/cohort scope here:
    // this isolates the server's post-check raw-input fence from report fencing.
    armPublicDrift = true;
    const end = performance.now() + 20000;
    while (!publicDrift && performance.now() < end) {
      const result = await queryHistoryDaemon({
        socketPath: current.socketPath,
        expectedScope: current.scope,
        scope: () => discovery()?.scope,
        serviceName: "history-recorder",
        timeoutMs: 5000,
      });
      if (publicDrift)
        assert.equal(
          result,
          "unknown",
          "queued public drift after genuine proof must revoke the relay reply",
        );
    }
    assert.equal(
      publicDrift,
      true,
      "actual signed/native/TLS proof must reach the owned interleaving",
    );
    const held = await serviceReports(layout, specs);
    assert.equal(
      held.every((report) => report.ready === false),
      true,
    );
    console.info(
      "owned actual Unix full2160 reports, cohort renewal and post-proof public fence passed",
    );
  } finally {
    globalThis.historyStableFixtureServiceSpecs = undefined;
    stopped.abort();
    try {
      await running;
    } finally {
      release();
      process.removeListener("SIGTERM", stop);
      process.removeListener("SIGINT", stop);
    }
  }
};
