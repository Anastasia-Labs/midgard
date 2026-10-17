import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { expect, it, vi } from "vitest";

import { captureAcceptanceNativeRead } from "../src/devnet-stack/acceptance-native-boundary.js";
import {
  nativeFixture,
  ogmiosFixture,
  readConfig,
} from "./acceptance-native.fixture.js";
const refs = [{ txHash: "ee".repeat(32), outputIndex: 2 }];
const scope = (signal?: AbortSignal, timeoutMs = 2000) =>
  createDaAvailabilityReadScope({
    deadlineEpochMs: Date.now() + timeoutMs,
    attemptTimeoutMs: timeoutMs,
    signal,
  });
it("expires one absolute deadline during actual stalled I/O and joins native/TCP without renewing a budget", async () => {
  const server = await ogmiosFixture({ stallQuery: true });
  const helper = await nativeFixture();
  vi.useFakeTimers({
    toFake: ["Date", "performance", "setTimeout", "clearTimeout"],
  });
  vi.setSystemTime(0);
  const read = scope(undefined, 100);
  try {
    const result = captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => await current.queryExactOutRefs(refs),
      helper,
    );
    const rejected = expect(result).rejects.toThrow();
    await server.waitForRequest("queryLedgerState/utxo");
    await vi.advanceTimersByTimeAsync(100);
    await rejected;
    expect(read.signal.aborted).toBe(true);
    expect(read.signal.reason).toMatchObject({
      name: "DaAvailabilityReadScopeExpiredError",
      deadlineEpochMs: 100,
    });
    expect(helper.closed()).toBe(true);
    await server.waitForClosed(1);
    expect(server.active()).toBe(0);
  } finally {
    read.close();
    vi.useRealTimers();
    await server.close();
  }
});

it("rejects an already expired absolute scope before transport or native startup", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = createDaAvailabilityReadScope({
    deadlineEpochMs: 0,
    attemptTimeoutMs: 100,
  });
  try {
    await expect(
      captureAcceptanceNativeRead(
        readConfig(server.endpoint, helper),
        read,
        async () => true,
        helper,
      ),
    ).rejects.toMatchObject({ name: "DaAvailabilityReadScopeExpiredError" });
    expect(server.requests).toEqual([]);
    expect(server.active()).toBe(0);
    expect(helper.opened()).toBe(false);
  } finally {
    read.close();
    await server.close();
  }
});

it("refuses stale query-service success after consuming a genuine newer native forward", async () => {
  const server = await ogmiosFixture();
  const helper = await nativeFixture();
  const read = scope();
  let consumed!: () => void;
  const consumedForward = new Promise<void>((resolve) => {
    consumed = resolve;
  });
  const config = readConfig(server.endpoint, helper);
  const observedConfig = {
    ...config,
    native: {
      ...config.native,
      startWatcherNativeChainSync: async (
        input: Parameters<typeof config.native.startWatcherNativeChainSync>[0],
      ) =>
        await config.native.startWatcherNativeChainSync({
          ...input,
          onEvent: async (event) => {
            await input.onEvent(event);
            if (
              event.kind === "roll_forward" &&
              event.blockHash === "cc".repeat(32)
            )
              consumed();
          },
        }),
    },
  };
  try {
    await expect(
      captureAcceptanceNativeRead(
        observedConfig,
        read,
        async (current) => {
          await current.queryExactOutRefs(refs);
          helper.send("forward");
          await consumedForward;
          current.assertCurrent();
          return true;
        },
        helper,
      ),
    ).rejects.toThrow();
    expect(helper.closed()).toBe(true);
  } finally {
    read.close();
    await server.close();
  }
});

it("physically terminates a query peer that refuses the WebSocket close handshake", async () => {
  const server = await ogmiosFixture({ stallQuery: true });
  const helper = await nativeFixture();
  const controller = new AbortController();
  const read = scope(controller.signal);
  let result: Promise<unknown> | undefined;
  try {
    result = captureAcceptanceNativeRead(
      readConfig(server.endpoint, helper),
      read,
      async (current) => await current.queryExactOutRefs(refs),
      helper,
    );
    const disposition = result.then(
      () => "resolved",
      () => "rejected",
    );
    await server.waitForRequest("queryLedgerState/utxo");
    controller.abort(new Error("owned read cancelled"));
    expect(
      await Promise.race([
        disposition,
        server.closeFrameReady.then(() => "unanswered close handshake"),
      ]),
    ).toBe("rejected");
    expect(helper.closed()).toBe(true);
  } finally {
    read.close();
    await server.close();
    await result?.catch(() => undefined);
  }
});
