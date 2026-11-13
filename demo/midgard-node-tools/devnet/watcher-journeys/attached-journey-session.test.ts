import { readdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { watcherDeploymentReleaseFinalityAuthority } from "midgard-watcher";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  closeStackWatchers,
  deploymentIdentity,
  fingerprint,
  liveStatus,
  PROVER_MINIMUM_LOVELACE,
  removeStackDirectories,
  runningStackWatcher,
  seedWorkflowStarted,
  watcherContainer,
} from "./attached-journey-session.test-support.js";
import {
  captureJourneyWorkflowBaseline,
  openAttachedJourneySession,
} from "./journey-session.js";

afterAll(removeStackDirectories);
afterEach(closeStackWatchers);

describe("attached journey session", () => {
  it("attaches to the stack's watcher and starts nothing of its own", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    try {
      expect(session.mode).toBe("attached");
      expect(session.directory.startsWith(stack.runDirectory)).toBe(true);
      expect(
        (await readdir(join(stack.runDirectory, "work/journeys"))).sort(),
      ).toEqual(["replay", "sessions"]);
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
    await seedWorkflowStarted(stack.storePath, headerHash);
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
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

  it("refuses a watcher of another deployment, a watcher still starting or not live, or none", async () => {
    // A starting watcher's status, as it serves one before binding a deployment.
    const starting = {
      observedAtMs: "1",
      liveness: "live",
      readiness: "not_ready",
      readinessReasons: ["startup:l1_follower"],
      l1Readiness: [],
      startup: { stage: "l1_follower", outcome: "started" },
    };
    for (const [status, refusal] of [
      [
        { ...liveStatus(), deploymentFingerprint: "cd".repeat(32) },
        /serves another deployment/u,
      ],
      [starting, /still starting and not attached to a deployment/u],
      [{ ...liveStatus(), liveness: "blocked" }, /not live: blocked/u],
    ] as const) {
      const stack = await runningStackWatcher(status);
      await expect(
        openAttachedJourneySession(stack.context, stack.options()),
      ).rejects.toThrow(refusal);
    }
    const stack = await runningStackWatcher(liveStatus());
    await closeStackWatchers();
    await expect(
      openAttachedJourneySession(stack.context, stack.options()),
    ).rejects.toThrow();
  });

  it("refuses at drill start a run whose replay store was never warmed, naming the warm-up command, before any L1 work", async () => {
    const stack = await runningStackWatcher(liveStatus(), { replay: "cold" });
    await expect(
      openAttachedJourneySession(stack.context, stack.options()),
    ).rejects.toThrow(/pnpm run journey-replay:warm-up/u);
    expect(stack.requests).toEqual([]);
    expect(stack.operator2.calls).toEqual([]);
  });

  it("opens the drill on the replay store the warm-up filled", async () => {
    const stack = await runningStackWatcher(liveStatus(), { replay: "cold" });
    await stack.warmReplay();
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    await session.close();
  });

  it("refuses at drill start a prover below the stack's configured minimum, by address and shortfall", async () => {
    const shortfall = 1_500_000n;
    const stack = await runningStackWatcher(liveStatus(), {
      proverLovelace: PROVER_MINIMUM_LOVELACE - shortfall,
    });
    const refusal = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    ).then(
      () => undefined,
      (error: unknown) => String(error),
    );
    expect(refusal).toContain(stack.proverAddress);
    expect(refusal).toContain(`${shortfall.toString()} lovelace short`);
    expect(stack.operator2.calls).toEqual([]);
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
      openAttachedJourneySession(stack.context, stack.options(otherWatcher)),
    ).rejects.toThrow(/another process configuration/u);
  });

  it("holds operator-2's node and starts it again when the run ends", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    expect(stack.operator2.calls).toEqual([]);
    await session.operator2.hold();
    await session.operator2.assertStopped();
    expect(stack.operator2.running()).toBe(false);
    await session.close();
    expect(stack.operator2.running()).toBe(true);
    expect(stack.operator2.calls).toEqual(["stop", "start"]);
  });

  it("proves operator-2's node stopped before every signature of operator-2's wallet", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    await expect(stack.signAsOperator2()).rejects.toThrow(
      "Operator-2's node is not held stopped by this drill; refusing to sign as operator-2",
    );
    await session.operator2.hold();
    await stack.signAsOperator2();
    // Started behind the drill's back: the next signature is refused.
    await stack.operator2.control.start();
    await expect(stack.signAsOperator2()).rejects.toThrow(
      "is still running; refusing to sign as operator-2",
    );
    await stack.operator2.control.stop();
    await stack.signAsOperator2();
    await session.close();
    await expect(stack.signAsOperator2()).rejects.toThrow(
      "refusing to sign as operator-2",
    );
  });

  it("proves operator-2's node stopped before every publication as operator-2's DA producer", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
    const refused = "refusing to publish as operator-2's DA producer";
    await expect(stack.publishAsOperator2()).rejects.toThrow(refused);
    await session.operator2.hold();
    await expect(stack.publishAsOperator2()).rejects.toThrow(
      stack.producerTransportOpened,
    );
    // Started behind the drill's back: the next publication is refused.
    await stack.operator2.control.start();
    await expect(stack.publishAsOperator2()).rejects.toThrow(refused);
    await stack.operator2.control.stop();
    await session.close();
    await expect(stack.publishAsOperator2()).rejects.toThrow(refused);
    expect(stack.producerTransportOpens()).toBe(1);
  });

  it("refuses to pause this stack's own node as operator-2", async () => {
    const stack = await runningStackWatcher(liveStatus());
    for (const [operator2, refusal] of [
      [
        {
          composeProject: "midgard-node-drill",
          endpoint: "http://127.0.0.1:3100",
        },
        /Compose project is this stack's own project/u,
      ],
      [
        {
          composeProject: "midgard-node-op2",
          endpoint: `${stack.context.stackConfig.endpoint}/`,
        },
        /endpoint is this stack's own node endpoint/u,
      ],
    ] as const)
      await expect(
        openAttachedJourneySession(stack.context, {
          ...stack.options(),
          operator2,
        }),
      ).rejects.toThrow(refusal);
    expect(stack.operator2.calls).toEqual([]);
  });

  it("closes when the watcher stops serving this deployment, and stays closed", async () => {
    const stack = await runningStackWatcher(liveStatus());
    const session = await openAttachedJourneySession(
      stack.context,
      stack.options(),
    );
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
