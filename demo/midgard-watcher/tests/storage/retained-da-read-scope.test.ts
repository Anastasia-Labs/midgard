import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { parseWatcherConfig } from "../../src/runtime/config.js";
import { createWatcherRetainedDaRuntime } from "../../src/storage/retained-da-runtime.js";
import { withWatcherRetainedDaReadScope } from "../../src/storage/retained-da-runtime.read-scope.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { rawConfig, transportFactory } from "./retained-da-runtime.fixtures.js";

// Existing retained-DA suites cover lifetime closure; this composition covers
// one actor's absolute scope without shortening another actor's runtime lease.
describe("request-local retained DA deadline", () => {
  it("forwards the remaining cap and aborts an in-flight request without leaking into the next scope", async () => {
    const identity = makeWatcherDeploymentAuthorityFixture().result;
    const transport = transportFactory();
    let observed: AbortSignal | undefined;
    let started!: () => void;
    const ready = new Promise<void>((resolve) => {
      started = resolve;
    });
    transport.request.mockImplementationOnce(async (request) => {
      observed = request.signal;
      expect(request.timeoutMs).toBeLessThanOrEqual(1000);
      started();
      return await new Promise<Uint8Array>((_resolve, reject) =>
        request.signal!.addEventListener(
          "abort",
          () => reject(request.signal!.reason),
          { once: true },
        ),
      );
    });
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: parseWatcherConfig(rawConfig()),
      deploymentIdentity: identity,
      unsafeTransportFactoryForTest: transport.factory,
    });
    const first = createDaAvailabilityReadScope({
      deadlineEpochMs: Date.now() + 1000,
      attemptTimeoutMs: 10_000,
    });
    try {
      const pending = withWatcherRetainedDaReadScope(
        { identity, scope: first },
        () => runtime.sources[0]!.fetchPayloadByHeaderHash("11".repeat(28)),
      );
      await ready;
      first.close();
      await expect(pending).rejects.toThrow("scope closed");
      expect(observed?.aborted).toBe(true);
      const second = createDaAvailabilityReadScope({
        attemptTimeoutMs: 10_000,
      });
      try {
        await withWatcherRetainedDaReadScope({ identity, scope: second }, () =>
          runtime.sources[0]!.fetchPayloadByHeaderHash("22".repeat(28)),
        );
        expect(transport.request).toHaveBeenCalledTimes(2);
        const next = transport.request.mock.calls[1]![0];
        expect(next.signal?.aborted).toBe(false);
        expect(next.timeoutMs).toBeGreaterThan(1000);
      } finally {
        second.close();
      }
    } finally {
      first.close();
      await runtime.close();
    }
  });

  it("rejects a source from another signed deployment before transport", async () => {
    const identity = makeWatcherDeploymentAuthorityFixture().result;
    const other = makeWatcherDeploymentAuthorityFixture({
      hubOracleOneShotOutRef: `${"99".repeat(32)}#0`,
    }).result;
    const transport = transportFactory();
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: parseWatcherConfig(rawConfig()),
      deploymentIdentity: identity,
      unsafeTransportFactoryForTest: transport.factory,
    });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 10_000 });
    try {
      const result = await withWatcherRetainedDaReadScope(
        { identity: other, scope },
        () => runtime.sources[0]!.fetchPayloadByHeaderHash("11".repeat(28)),
      );
      expect(result.ok).toBe(false);
      expect(
        result.attempts.some((attempt) =>
          attempt.detail?.includes("another deployment"),
        ),
      ).toBe(true);
      expect(transport.request).not.toHaveBeenCalled();
    } finally {
      scope.close();
      await runtime.close();
    }
  });
});
