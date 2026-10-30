import { execFile } from "node:child_process";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs";
import { watcherDeploymentReleaseFinalityAuthority } from "midgard-watcher";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture";
import { openWatcherWorkflowStoreForTest } from "midgard-watcher/tests/support/watcher-workflow-store";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  captureJourneyWorkflowBaseline,
  openAttachedJourneySession,
} from "./journey-session.js";
import type { StackJourneyContext } from "./stack-context.js";
import {
  WATCHER_CONTAINER_PROCESS_CONFIG_PATH,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

const watcherImage = fileURLToPath(
  new URL("../../../midgard-watcher/", import.meta.url),
);
/**
 * The watcher container: its Node, run locally in the watcher package, with
 * `mountedProcessConfig` mounted at the container's process configuration path.
 */
const watcherContainer =
  (mountedProcessConfig: string): WatcherEvidenceExec =>
  ({ script, request }) => {
    const parsed = JSON.parse(request) as { processConfigPath: string };
    expect(parsed.processConfigPath).toBe(
      WATCHER_CONTAINER_PROCESS_CONFIG_PATH,
    );
    return new Promise((resolve, reject) => {
      execFile(
        process.execPath,
        [
          "--input-type=module",
          "-e",
          script,
          JSON.stringify({
            ...parsed,
            processConfigPath: mountedProcessConfig,
          }),
        ],
        { cwd: watcherImage, encoding: "utf8" },
        (error, stdout, stderr) =>
          error === null ? resolve(stdout) : reject(new Error(stderr)),
      );
    });
  };

const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
const fingerprint = deploymentIdentity.manifestId;

const directories: string[] = [];
const servers: Server[] = [];
afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(async () => {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
});

/** A running stack watcher: its operations endpoint and its mounted state. */
const runningStackWatcher = async (status: Record<string, unknown>) => {
  const root = await mkdtemp(join(tmpdir(), "attached-session-"));
  directories.push(root);
  const storePath = join(root, "watcher.sqlite");
  const processConfigPath = join(root, "watcher-process.json");
  await writeFile(
    processConfigPath,
    JSON.stringify({
      workflowJournalDirectory: join(root, "workflows"),
      watcherConfig: { storage: { path: storePath } },
    }),
  );
  const served = { status };
  const requests: string[] = [];
  const server = createServer((request, response) => {
    requests.push(request.url ?? "");
    const body =
      request.url === "/v1/status"
        ? served.status
        : request.url === "/v1/metrics"
          ? { queuedProofCount: "0" }
          : undefined;
    response.statusCode = body === undefined ? 404 : 200;
    response.end(JSON.stringify(body ?? {}));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  servers.push(server);
  const operationsEndpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port}`;
  const runDirectory = join(root, "run/fault-journeys/drill-1");
  // The parts of a stack context an attached session reads.
  const context = {
    mode: "stack",
    runDirectory,
    layout: { manifestPath: join(root, "contract-deployment-info.json") },
    release: { deploymentIdentity },
    deployment: {
      manifest: { manifestId: fingerprint },
      blueprintPath: join(root, "plutus.json"),
    },
    watcher: {
      composeProject: "midgard-node-drill",
      processConfigPath,
      operationsEndpoint,
    },
  } as unknown as StackJourneyContext;
  return {
    root,
    storePath,
    runDirectory,
    context,
    served,
    requests,
    exec: watcherContainer(processConfigPath),
  };
};

const liveStatus = () => ({
  deploymentFingerprint: fingerprint,
  liveness: "live",
  readiness: "not_ready",
  readinessReasons: ["l1_follower_catching_up"],
  l1Readiness: [{ reason: "l1_follower_catching_up", detail: "block 7" }],
});

describe("attached journey session", () => {
  it("attaches to the stack's watcher and starts nothing of its own", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(stack.context, {
      exec: stack.exec,
    });
    try {
      expect(session.mode).toBe("attached");
      expect(session.directory.startsWith(stack.runDirectory)).toBe(true);
      expect(await readdir(join(stack.runDirectory, "work/journeys"))).toEqual([
        "sessions",
      ]);
      expect(session.releaseFinality).toEqual(
        await watcherDeploymentReleaseFinalityAuthority(
          deploymentIdentity,
        ).verifyForWorkflow({ deploymentFingerprint: fingerprint }),
      );
      expect(session.authority.deploymentAuthority.deploymentIdentity).toBe(
        deploymentIdentity,
      );
      expect(await session.watcher.operations("/v1/status")).toEqual(
        liveStatus(),
      );
      expect(JSON.parse((await session.watcher.watcherProgress())!)).toEqual({
        readinessReasons: liveStatus().readinessReasons,
        l1Readiness: liveStatus().l1Readiness,
      });
      expect(await session.watcher.diagnostics()).toEqual({
        status: liveStatus(),
        metrics: { queuedProofCount: "0" },
      });
      await session.assertHealthy();
      expect(await session.evidence.readDecisions()).toEqual([]);
    } finally {
      await session.close();
    }
  });

  it("captures its baseline from the stack watcher's workflow journal", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const headerHash = "12".repeat(28);
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: fingerprint,
      category: "doubleSpend" as const,
      target: { kind: "state_queue_header" as const, headerHash },
    };
    const workflows = openWatcherWorkflowStoreForTest(stack.storePath);
    await workflows
      .forObjective({ category: "doubleSpend", headerHash })
      .append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId: computeFraudProofWorkflowId(identity),
          identity,
          sequence: 0,
          recordedAt: "2026-10-29T00:00:00.000Z",
          event: { kind: "started" },
        },
        0,
      );
    workflows.close();
    const session = await openAttachedJourneySession(stack.context, {
      exec: stack.exec,
    });
    try {
      const baseline = await session.captureWorkflowBaseline("doubleSpend");
      expect(baseline.get(headerHash)).toHaveLength(1);
      expect(baseline).toEqual(
        await captureJourneyWorkflowBaseline(stack.storePath, "doubleSpend"),
      );
    } finally {
      await session.close();
    }
  });

  it("refuses a watcher of another deployment, a watcher that is not live, or none", async () => {
    for (const status of [
      { ...liveStatus(), deploymentFingerprint: "cd".repeat(32) },
      { ...liveStatus(), liveness: "blocked" },
      { liveness: "live" },
    ]) {
      const stack = await runningStackWatcher(status);
      await expect(
        openAttachedJourneySession(stack.context, { exec: stack.exec }),
      ).rejects.toThrow(/another deployment|not live/u);
    }
    const stack = await runningStackWatcher(liveStatus());
    await Promise.all(
      servers
        .splice(0)
        .map((server) => new Promise((resolve) => server.close(resolve))),
    );
    await expect(
      openAttachedJourneySession(stack.context, { exec: stack.exec }),
    ).rejects.toThrow();
  });

  it("refuses evidence that does not come from this stack's watcher", async () => {
    const stack = await runningStackWatcher(liveStatus());
    // Another stack's watcher: the same journals under another configuration.
    const foreign = join(stack.root, "foreign-watcher-process.json");
    await writeFile(
      foreign,
      JSON.stringify({
        workflowJournalDirectory: join(stack.root, "workflows"),
        watcherConfig: { storage: { path: stack.storePath } },
        operationsEndpoint: "http://127.0.0.1:7402",
      }),
    );
    const otherWatcher = watcherContainer(foreign);
    await expect(
      openAttachedJourneySession(stack.context, { exec: otherWatcher }),
    ).rejects.toThrow(/another process configuration/u);
  });

  it("closes when the watcher stops serving this deployment, and stays closed", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(stack.context, {
      exec: stack.exec,
    });
    stack.served.status = { ...liveStatus(), liveness: "stopping" };
    await expect(session.assertHealthy()).rejects.toThrow(/not live/u);
    const served = stack.requests.length;
    expect(() => session.watcher.requireLive()).toThrow(/closed/u);
    await expect(session.watcher.operations("/v1/status")).rejects.toThrow(
      /closed/u,
    );
    expect(stack.requests).toHaveLength(served);
    await session.close();
  });
});
