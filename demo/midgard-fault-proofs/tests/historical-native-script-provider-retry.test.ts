import { describe, expect, it, vi } from "vitest";

import {
  fetchHistoricalProviderRecord,
  HistoricalNativeScriptProviderUnavailableError,
} from "../src/workflow/historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import { LocalKupmiosTransportUnavailableError } from "../src/workflow/local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";

const URL_A = new URL("http://127.0.0.1:9/midgard/v1/historical-payload/a/b");
const RECORD = Object.freeze({ schemaVersion: "record", value: "exact" });

const refused = () =>
  Object.assign(new TypeError("fetch failed"), {
    cause: Object.assign(new Error("connect ECONNREFUSED"), {
      code: "ECONNREFUSED",
    }),
  });

/** Answers each call with the next scripted step; the last one repeats. */
const scripted = (steps: readonly (number | "refused" | Error)[]) =>
  vi.fn<typeof fetch>(async () => {
    const step = steps[Math.min(scripted.calls, steps.length - 1)]!;
    scripted.calls += 1;
    if (step === "refused") throw refused();
    if (step instanceof Error) throw step;
    return step === 200
      ? new Response(JSON.stringify(RECORD), { status: 200 })
      : new Response("not yet", { status: step });
  });
scripted.calls = 0;

const read = (
  fetchImpl: typeof fetch,
  options: Readonly<{ deadlineMs?: number }> = {},
) => {
  const sleeps: number[] = [];
  const outcome = fetchHistoricalProviderRecord({
    url: URL_A,
    sourceId: "history-a",
    fetchImpl,
    sleep: async (ms) => {
      sleeps.push(ms);
    },
    ...options,
  });
  return { outcome, sleeps };
};

describe("historical provider read retry", () => {
  it.each([
    ["a lagging provider's 404", 404],
    ["a restarting provider's 503", 503],
    ["a refused connection", "refused"],
  ] as const)(
    "waits out %s and returns the record exactly once",
    async (_label, transient) => {
      scripted.calls = 0;
      const fetchImpl = scripted([transient, transient, 200]);
      const { outcome, sleeps } = read(fetchImpl);
      await expect(outcome).resolves.toEqual(RECORD);
      expect(fetchImpl).toHaveBeenCalledTimes(3);
      expect(sleeps).toEqual([100, 200]);
    },
  );

  it.each([400, 401, 403, 410])(
    "refuses HTTP %i at once without retrying",
    async (status) => {
      scripted.calls = 0;
      const fetchImpl = scripted([status, 200]);
      const { outcome, sleeps } = read(fetchImpl);
      const failure = await outcome.catch((error: unknown) => error);
      expect(failure).toBeInstanceOf(Error);
      expect(failure).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
      expect((failure as Error).message).toBe(
        `historical provider history-a returned HTTP ${status.toString()}`,
      );
      expect(fetchImpl).toHaveBeenCalledTimes(1);
      expect(sleeps).toEqual([]);
    },
  );

  it("rethrows an unreadable body and any non-transport error unchanged", async () => {
    const odd = new Error("ECONNRESET in a message is not a network failure");
    scripted.calls = 0;
    const thrower = scripted([odd, 200]);
    await expect(read(thrower).outcome).rejects.toBe(odd);
    expect(thrower).toHaveBeenCalledTimes(1);
    const malformed = vi.fn<typeof fetch>(
      async () => new Response("{not json", { status: 200 }),
    );
    const failure = await read(malformed).outcome.catch(
      (error: unknown) => error,
    );
    expect(failure).toBeInstanceOf(SyntaxError);
    expect(malformed).toHaveBeenCalledTimes(1);
  });

  it("gives up inside the one deadline as a typed transport error", async () => {
    scripted.calls = 0;
    const fetchImpl = scripted([503]);
    const sleeps: number[] = [];
    const outcome = fetchHistoricalProviderRecord({
      url: URL_A,
      sourceId: "history-a",
      fetchImpl,
      deadlineMs: 450,
      // Real waits: the remaining deadline decides whether to wait again.
      sleep: async (ms) => {
        sleeps.push(ms);
        await new Promise((resolve) => setTimeout(resolve, ms));
      },
    });
    const failure = await outcome.catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(
      HistoricalNativeScriptProviderUnavailableError,
    );
    // The supervisor's existing transport classifier waits on this type.
    expect(failure).toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    expect(failure).toMatchObject({
      sourceId: "history-a",
      message: "historical provider history-a returned HTTP 503",
    });
    // 100 + 200 fit in 450 ms; the next 400 ms backoff does not.
    expect(sleeps).toEqual([100, 200]);
    expect(fetchImpl).toHaveBeenCalledTimes(3);
  });

  it("caps the backoff at two seconds", async () => {
    scripted.calls = 0;
    const fetchImpl = scripted(["refused"]);
    const sleeps: number[] = [];
    const stop = new Error("test stops the wait");
    const outcome = fetchHistoricalProviderRecord({
      url: URL_A,
      sourceId: "history-a",
      fetchImpl,
      // The scripted sleep passes no time, so it ends the loop itself.
      sleep: async (ms) => {
        sleeps.push(ms);
        if (sleeps.length === 8) throw stop;
      },
    });
    await expect(outcome).rejects.toBe(stop);
    expect(sleeps).toEqual([100, 200, 400, 800, 1_600, 2_000, 2_000, 2_000]);
  });
});
