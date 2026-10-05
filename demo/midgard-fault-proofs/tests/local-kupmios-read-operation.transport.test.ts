import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { ogmiosSessionBudgets } from "../src/workflow/local-kupmios-http-ogmios-source.acquire-ogmios-session.js";
import { createLocalKupmiosHttpOgmiosRawSource } from "../src/workflow/local-kupmios-http-ogmios-source.create-local-kupmios-http-ogmios-raw-source.js";
import { withLocalKupmiosReadOperation } from "../src/workflow/local-kupmios-read-operation.js";
import {
  ANCESTOR,
  EARLIER,
  OgmiosBoundarySocket,
  releaseFinality,
  response,
  TARGET,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  vi.useRealTimers();
});
const concrete = (
  scope: ReturnType<typeof createDaAvailabilityReadScope>,
  timeoutMs: number,
  stallEvery = false,
) => {
  const sockets: OgmiosBoundarySocket[] = [];
  const source = createLocalKupmiosHttpOgmiosRawSource({
    sourceId: "bounded-read-transport",
    kupoHttpUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "http://127.0.0.1:1337",
    releaseFinality,
    signal: scope.signal,
    timeoutMs,
    fetchImpl: async (url) => {
      const match = /\/checkpoints\/(\d+)$/u.exec(url);
      if (match === null) throw new Error("unexpected HTTP read");
      const slot = Number(match[1]);
      return response(
        slot >= 400
          ? { slot_no: 400, header_hash: TARGET }
          : slot >= 380
            ? { slot_no: 380, header_hash: ANCESTOR }
            : { slot_no: 360, header_hash: EARLIER },
        true,
      );
    },
    webSocketFactory: () => {
      const socket = new OgmiosBoundarySocket([], ANCESTOR, 70, {
        respond: !(stallEvery || sockets.length === 0),
      });
      sockets.push(socket);
      return socket;
    },
  });
  return { source, sockets };
};

describe("concrete Ogmios owning-operation cancellation", () => {
  it("physically closes a timed-out session then restarts the whole boundary read", async () => {
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1200,
      attemptTimeoutMs: 200,
      monotonicMs: Date.now,
    });
    const { source, sockets } = concrete(scope, 30);
    try {
      const result = withLocalKupmiosReadOperation(
        source,
        () => source.readBoundary(),
        { scope },
      );
      await vi.advanceTimersByTimeAsync(30);
      expect(await result).toMatchObject({
        kupoCheckpoint: { slot: "400", blockHash: TARGET },
      });
      expect(sockets.length).toBeGreaterThan(1);
      expect(sockets[0]!.closeCount).toBe(1);
      expect(sockets.every((socket) => socket.closeCount === 1)).toBe(true);
      expect(ogmiosSessionBudgets.has("ws://127.0.0.1:1337")).toBe(false);
    } finally {
      scope.close();
    }
  });

  it("propagates absolute scope cancellation into the actual session without starting a retry", async () => {
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1040,
      attemptTimeoutMs: 200,
      monotonicMs: Date.now,
    });
    const { source, sockets } = concrete(scope, 100, true);
    try {
      const result = withLocalKupmiosReadOperation(
        source,
        () => source.readBoundary(),
        { scope },
      );
      const rejected = expect(result).rejects.toThrow(/deadline 1040 reached/);
      await vi.advanceTimersByTimeAsync(40);
      await rejected;
      await vi.advanceTimersByTimeAsync(0);
      expect(sockets).toHaveLength(1);
      expect(sockets[0]!.closeCount).toBe(1);
      expect(ogmiosSessionBudgets.has("ws://127.0.0.1:1337")).toBe(false);
      expect(sockets[0]!.listeners.get("message")).toEqual([]);
    } finally {
      scope.close();
    }
  });
});
