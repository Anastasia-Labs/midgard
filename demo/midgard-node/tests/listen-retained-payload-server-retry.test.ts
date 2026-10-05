import { Effect, Fiber } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  retainedPayloadServerStatus,
  retainedPayloadServerThread,
} from "../src/commands/listen.retained-payload-server-thread.js";

const retrieve = async () => undefined;

describe("retained-payload server start retries", () => {
  it("retries a failing bind until it serves, and never ends the thread", async () => {
    const close = vi.fn(async () => undefined);
    let attempts = 0;
    const start = vi.fn(async () => {
      attempts += 1;
      if (attempts <= 3)
        throw Object.assign(new Error("listen EADDRINUSE: 0.0.0.0:4001"), {
          code: "EADDRINUSE",
        });
      return {
        configured: true as const,
        deploymentFingerprint: "ab".repeat(32),
        localPeerId: "peer",
        listenMultiaddrs: ["/ip4/0.0.0.0/tcp/4001"],
        announceMultiaddrs: [],
        close,
      };
    });
    const fiber = Effect.runFork(
      retainedPayloadServerThread(retrieve, {
        start,
        retryInitialMs: 10,
        retryMaxMs: 20,
      }),
    );
    await vi.waitFor(() => {
      const status = retainedPayloadServerStatus();
      expect(status.state).toBe("retrying");
      if (status.state === "retrying") {
        expect(status.attempts).toBeGreaterThanOrEqual(1);
        expect(status.lastError).toMatch(/EADDRINUSE/u);
      }
    });
    await vi.waitFor(() =>
      expect(retainedPayloadServerStatus().state).toBe("serving"),
    );
    expect(start).toHaveBeenCalledTimes(4);
    expect(retainedPayloadServerStatus()).not.toHaveProperty("lastError");
    // Serving holds the thread open until the node stops it.
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(fiber.unsafePoll()).toBeNull();
    await Effect.runPromise(Fiber.interrupt(fiber));
    expect(close).toHaveBeenCalledTimes(1);
  });

  it("doubles its retry delay up to the cap", async () => {
    const delays: number[] = [];
    const start = vi.fn(async () => {
      const status = retainedPayloadServerStatus();
      if (status.state === "retrying") delays.push(status.retryInMs);
      if (start.mock.calls.length > 5)
        return { configured: false as const, reason: "off" };
      throw new Error("bind failed");
    });
    await Effect.runPromise(
      retainedPayloadServerThread(retrieve, {
        start,
        retryInitialMs: 5,
        retryMaxMs: 20,
      }),
    );
    expect(delays).toEqual([5, 10, 20, 20, 20]);
    expect(retainedPayloadServerStatus()).toEqual({
      state: "not_configured",
      reason: "off",
    });
  });
});
