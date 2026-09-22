import { afterEach, beforeEach, expect, it, vi } from "vitest";

import {
  readJourneyOperations,
  waitForRestartedJourneyOperations,
} from "./operations.js";

vi.mock("node:timers/promises", () => ({
  setTimeout: (milliseconds: number) =>
    new Promise<void>((resolve) => setTimeout(resolve, milliseconds)),
}));

const controllers: AbortController[] = [];

beforeEach(() => {
  vi.useFakeTimers();
  controllers.length = 0;
  vi.spyOn(AbortSignal, "timeout").mockImplementation(() => {
    const controller = new AbortController();
    controllers.push(controller);
    return controller.signal;
  });
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

const timeout = () => {
  const error = new DOMException("Request timed out", "TimeoutError");
  controllers.at(-1)!.abort(error);
  return error;
};

const refused = () =>
  new TypeError("fetch failed", {
    cause: Object.assign(new Error("connect ECONNREFUSED"), {
      code: "ECONNREFUSED",
    }),
  });

it.each([0, -1, Number.NaN, Number.POSITIVE_INFINITY])(
  "rejects an invalid restart timeout %s before querying",
  async (timeoutMs) => {
    const readStatus = vi.fn();
    await expect(
      waitForRestartedJourneyOperations({
        readStatus,
        requireLive: () => {},
        timeoutMs,
      }),
    ).rejects.toThrow("finite and positive");
    expect(readStatus).not.toHaveBeenCalled();
  },
);

it("waits for a restarted listener and returns its actual status", async () => {
  const status = { readiness: "not_ready", readinessReasons: ["catching_up"] };
  const readStatus = vi
    .fn()
    .mockRejectedValueOnce(refused())
    .mockResolvedValue(status);
  const requireLive = vi.fn();
  const result = waitForRestartedJourneyOperations({ readStatus, requireLive });
  await vi.advanceTimersByTimeAsync(1000);
  expect(await result).toBe(status);
  expect(requireLive).toHaveBeenCalledTimes(2);
});

it("stops waiting when the restarted process fails", async () => {
  const failure = new Error("watcher exited 1");
  const requireLive = vi
    .fn()
    .mockImplementationOnce(() => {})
    .mockImplementation(() => {
      throw failure;
    });
  const readStatus = vi.fn().mockRejectedValue(refused());
  const result = waitForRestartedJourneyOperations({ readStatus, requireLive });
  const rejected = expect(result).rejects.toBe(failure);
  await vi.advanceTimersByTimeAsync(1000);
  await rejected;
  expect(readStatus).toHaveBeenCalledTimes(1);
});

it("bounds restart readiness without swallowing HTTP or parsing failures", async () => {
  const readStatus = vi.fn().mockRejectedValue(refused());
  const result = waitForRestartedJourneyOperations({
    readStatus,
    requireLive: () => {},
    timeoutMs: 1000,
  });
  const rejected = expect(result).rejects.toThrow(
    "Timed out waiting for restarted watcher operations",
  );
  // A wall-clock regression cannot extend the monotonic restart deadline.
  vi.setSystemTime(Date.now() - 60_000);
  await vi.advanceTimersByTimeAsync(1000);
  await rejected;
  const malformed = new SyntaxError("invalid JSON");
  readStatus.mockClear().mockRejectedValue(malformed);
  await expect(
    waitForRestartedJourneyOperations({ readStatus, requireLive: () => {} }),
  ).rejects.toBe(malformed);
  expect(readStatus).toHaveBeenCalledTimes(1);
});

it.each(["request", "body"])(
  "retries a timed-out %s with a fresh five-second signal and returns actual health",
  async (phase) => {
    const health = { ready: false, activeAlerts: ["real-alert"] };
    const fetch = vi
      .spyOn(globalThis, "fetch")
      .mockImplementationOnce(async () => {
        if (phase === "request") throw timeout();
        const response = new Response();
        vi.spyOn(response, "json").mockImplementation(async () => {
          timeout();
          throw new DOMException("Body aborted", "AbortError");
        });
        return response;
      })
      .mockResolvedValueOnce(Response.json(health));
    const result = readJourneyOperations("http://localhost:1234", "/v1/status");
    await vi.advanceTimersByTimeAsync(249);
    expect(fetch).toHaveBeenCalledTimes(1);
    await vi.advanceTimersByTimeAsync(1);
    expect(await result).toEqual(health);
    expect(fetch).toHaveBeenCalledTimes(2);
    expect(AbortSignal.timeout).toHaveBeenNthCalledWith(1, 5000);
    expect(AbortSignal.timeout).toHaveBeenNthCalledWith(2, 5000);
    expect(controllers[0]!.signal).not.toBe(controllers[1]!.signal);
    expect(fetch).toHaveBeenLastCalledWith("http://localhost:1234/v1/status", {
      signal: controllers[1]!.signal,
    });
  },
);

it("propagates the final timeout after exactly three attempts", async () => {
  const errors: Error[] = [];
  const fetch = vi.spyOn(globalThis, "fetch").mockImplementation(async () => {
    const error = timeout();
    errors.push(error);
    throw error;
  });
  const result = readJourneyOperations("http://localhost:1234", "/v1/status");
  const rejected = expect(result).rejects.toMatchObject({
    name: "TimeoutError",
  });
  await vi.advanceTimersByTimeAsync(500);
  await rejected;
  await expect(result).rejects.toBe(errors[2]);
  expect(fetch).toHaveBeenCalledTimes(3);
  expect(vi.getTimerCount()).toBe(0);
});

it.each([400, 401, 403, 503])("does not retry HTTP %s", async (status) => {
  const fetch = vi
    .spyOn(globalThis, "fetch")
    .mockResolvedValue(new Response("failure", { status }));
  await expect(
    readJourneyOperations("http://localhost:1234", "/v1/status"),
  ).rejects.toThrow(`Operations HTTP returned ${status}`);
  expect(fetch).toHaveBeenCalledTimes(1);
});

it("does not retry malformed JSON even if its signal timed out", async () => {
  const fetch = vi.spyOn(globalThis, "fetch").mockImplementation(async () => {
    timeout();
    return new Response("not-json");
  });
  await expect(
    readJourneyOperations("http://localhost:1234", "/v1/status"),
  ).rejects.toBeInstanceOf(SyntaxError);
  expect(fetch).toHaveBeenCalledTimes(1);
});

it.each([
  new TypeError("fetch failed"),
  new DOMException("External abort", "AbortError"),
  new DOMException("Unrelated timeout", "TimeoutError"),
])(
  "does not retry an error without its own timeout signal: %s",
  async (error) => {
    const fetch = vi.spyOn(globalThis, "fetch").mockRejectedValue(error);
    await expect(
      readJourneyOperations("http://localhost:1234", "/v1/status"),
    ).rejects.toBe(error);
    expect(fetch).toHaveBeenCalledTimes(1);
  },
);
