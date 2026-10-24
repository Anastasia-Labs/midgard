import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { describe, expect, it } from "vitest";

import {
  awaitLedgerTipSlot,
  isTransientTransportError,
  readOgmiosTipSlot,
  retryOgmiosTransport,
} from "./ledger-tip.js";

const fetchFailed = () =>
  new TypeError("fetch failed", { cause: new Error("ECONNREFUSED") });

describe("ledger tip wait", () => {
  it("returns once the tip reaches the target slot", async () => {
    const tips = [100, 100, 118, 131];
    const slept: number[] = [];
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => tips.shift() ?? 131,
        timeoutMs: 60_000,
        pollMs: 250,
        now: () => 0,
        sleep: async (ms) => {
          slept.push(ms);
        },
      }),
    ).resolves.toBe(131);
    expect(slept).toEqual([250, 250, 250]);
  });

  it("does not wait when the tip is already past the target", async () => {
    let reads = 0;
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => {
          reads += 1;
          return 140;
        },
        timeoutMs: 1,
        now: () => 0,
        sleep: async () => {
          throw new Error("must not sleep");
        },
      }),
    ).resolves.toBe(140);
    expect(reads).toBe(1);
  });

  it("fails once the tip stalls past the deadline", async () => {
    let clock = 0;
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => 100,
        timeoutMs: 5_000,
        pollMs: 1_000,
        now: () => clock,
        sleep: async (ms) => {
          clock += ms;
        },
      }),
    ).rejects.toThrow("stalled at slot 100 before reaching slot 130");
  });

  it("reads through a transport blip while waiting for the tip", async () => {
    const reads: Array<() => number> = [
      () => 100,
      () => {
        throw fetchFailed();
      },
      () => 131,
    ];
    await expect(
      awaitLedgerTipSlot({
        targetSlot: 130,
        readTipSlot: async () => (reads.shift() ?? (() => 131))(),
        timeoutMs: 60_000,
        now: () => 0,
        sleep: async () => undefined,
      }),
    ).resolves.toBe(131);
  });
});

describe("transport retry", () => {
  it("classifies only lost answers as transport errors", () => {
    expect(isTransientTransportError(fetchFailed())).toBe(true);
    expect(isTransientTransportError(new TypeError("terminated"))).toBe(true);
    expect(
      isTransientTransportError(
        new DOMException("The operation was aborted", "TimeoutError"),
      ),
    ).toBe(true);
    expect(
      isTransientTransportError(new Error("Ogmios did not report a tip slot")),
    ).toBe(false);
    expect(
      isTransientTransportError(new TypeError("x is not a function")),
    ).toBe(false);
    expect(isTransientTransportError("fetch failed")).toBe(false);
  });

  it("retries a transport error until the read answers", async () => {
    let calls = 0;
    await expect(
      retryOgmiosTransport(
        async () => {
          calls += 1;
          if (calls < 3) throw fetchFailed();
          return 7;
        },
        { now: () => 0, sleep: async () => undefined },
      ),
    ).resolves.toBe(7);
    expect(calls).toBe(3);
  });

  it("rethrows any other error at once", async () => {
    let calls = 0;
    const answer = new Error("Ogmios did not report a block height");
    await expect(
      retryOgmiosTransport(
        async () => {
          calls += 1;
          throw answer;
        },
        { now: () => 0, sleep: async () => undefined },
      ),
    ).rejects.toBe(answer);
    expect(calls).toBe(1);
  });

  it("gives up after the outage cap with the last blip as its cause", async () => {
    let clock = 0;
    const failure = await retryOgmiosTransport(
      async () => {
        throw fetchFailed();
      },
      {
        maxOutageMs: 5_000,
        pollMs: 1_000,
        now: () => clock,
        sleep: async (ms) => {
          clock += ms;
        },
      },
    ).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect((failure as Error).message).toBe(
      "Ogmios stayed unreachable for 5000ms",
    );
    expect(((failure as Error).cause as Error).message).toBe("fetch failed");
    expect(clock).toBe(5_000);
  });

  it("aborts a tip read whose connection never answers", async () => {
    const server = createServer(() => {
      // Never answers.
    });
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const { port } = server.address() as AddressInfo;
    try {
      const failure = await readOgmiosTipSlot(`http://127.0.0.1:${port}`, {
        timeoutMs: 200,
      }).catch((error: unknown) => error);
      expect(isTransientTransportError(failure)).toBe(true);
    } finally {
      server.closeAllConnections();
      await new Promise((resolve) => server.close(resolve));
    }
  }, 10_000);
});
