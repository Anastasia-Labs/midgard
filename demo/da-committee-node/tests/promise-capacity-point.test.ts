import { describe, expect, it, vi } from "vitest";

import { promiseCanonicalPointReader } from "../src/l1/promise-capacity-point.js";
import type { StateQueueReplayWebSocketFactory } from "../src/l1/state-queue-replay-provider.open-rpc.js";

const point = { slot: 100, blockHash: "ab".repeat(32), blockNo: 100 };
const tip = { slot: 2261, blockHash: "cd".repeat(32), blockNo: 2261 };
const rawTip = { slot: tip.slot, id: tip.blockHash, height: tip.blockNo };
const found = {
  intersection: { slot: point.slot, id: point.blockHash },
  tip: rawTip,
};
const forward = {
  direction: "forward",
  block: {
    ancestor: point.blockHash,
    id: "ef".repeat(32),
    slot: 101,
    height: 101,
  },
  tip: rawTip,
};
const handshake = {
  direction: "backward",
  point: found.intersection,
  tip: rawTip,
};

const rpc = (
  replies: readonly { result?: unknown; error?: unknown }[],
  failOpening = false,
) => {
  const listeners = new Map<string, ((event: never) => void)[]>();
  const requests: { method: string; params: unknown }[] = [];
  const emit = (type: string, event: unknown) => {
    for (const listener of listeners.get(type) ?? []) listener(event as never);
  };
  const close = vi.fn(() => emit("close", {}));
  const factory: StateQueueReplayWebSocketFactory = () => {
    queueMicrotask(() => emit(failOpening ? "error" : "open", {}));
    return {
      close,
      addEventListener: (type, listener) => {
        const existing = listeners.get(type) ?? [];
        existing.push(listener);
        listeners.set(type, existing);
      },
      send: (data) => {
        const request = JSON.parse(data) as {
          id: number;
          method: string;
          params: unknown;
        };
        requests.push(request);
        const reply = replies[requests.length - 1] ?? {
          error: { code: -1, message: "Unexpected extra request" },
        };
        queueMicrotask(() =>
          emit("message", {
            data: JSON.stringify({ id: request.id, ...reply }),
          }),
        );
      },
    };
  };
  return { factory, close, requests };
};

describe("promise capacity exact canonical point authority", () => {
  it("binds retained native height to the immediate raw successor and same-response selected tip", async () => {
    const socket = rpc([{ result: found }, { result: forward }]);
    const read = promiseCanonicalPointReader(
      "ws://ogmios",
      socket.factory,
      tip,
    );
    expect(await read(point)).toEqual({ point, tip });
    expect(socket.requests.map((request) => request.method)).toEqual([
      "findIntersection",
      "nextBlock",
    ]);
    expect(socket.requests[0]?.params).toEqual({
      points: [{ slot: point.slot, id: point.blockHash }],
    });
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it("accepts one exact intersection announcement before its raw successor", async () => {
    const socket = rpc([
      { result: found },
      { result: handshake },
      { result: forward },
    ]);
    expect(
      await promiseCanonicalPointReader(
        "ws://ogmios",
        socket.factory,
        tip,
      )(point),
    ).toEqual({ point, tip });
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it.each([
    [
      "different rollback",
      { ...handshake, point: { slot: 99, id: "12".repeat(32) } },
    ],
    ["repeated backward", handshake],
  ])("holds on %s", async (label, backward) => {
    const replies = [{ result: found }, { result: backward }];
    if (label === "repeated backward") replies.push({ result: backward });
    const socket = rpc(replies);
    await expect(
      promiseCanonicalPointReader("ws://ogmios", socket.factory, tip)(point),
    ).rejects.toThrow("rolled back");
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it.each([
    ["changed tip", { ...forward, tip: { ...rawTip, id: "12".repeat(32) } }],
    [
      "missing tip height",
      { ...forward, tip: { id: rawTip.id, slot: rawTip.slot } },
    ],
    [
      "stale retained height",
      { ...forward, block: { ...forward.block, height: 102 } },
    ],
    [
      "different predecessor",
      { ...forward, block: { ...forward.block, ancestor: "12".repeat(32) } },
    ],
  ])("holds on %s without a height fallback", async (_label, next) => {
    const socket = rpc([{ result: found }, { result: next }]);
    await expect(
      promiseCanonicalPointReader("ws://ogmios", socket.factory, tip)(point),
    ).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it("only reports exact absence when the not-found response carries the current selected tip", async () => {
    const socket = rpc([
      {
        error: {
          code: 1000,
          message: "Intersection not found",
          data: { tip: rawTip },
        },
      },
    ]);
    expect(
      await promiseCanonicalPointReader(
        "ws://ogmios",
        socket.factory,
        tip,
      )(point),
    ).toBeNull();
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it.each([
    {
      code: 1000,
      message: "Intersection not found",
      data: { tip: { ...rawTip, id: "12".repeat(32) } },
    },
    { code: 1000, message: "Intersection not found", data: {} },
    { code: -1, message: "Source unavailable" },
  ])("holds when absence or transport authority is unknown", async (error) => {
    const socket = rpc([{ error }]);
    await expect(
      promiseCanonicalPointReader("ws://ogmios", socket.factory, tip)(point),
    ).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });
  it("checks the native height even when the retained point is the selected tip", async () => {
    const socket = rpc([
      {
        result: {
          ...found,
          tip: { slot: point.slot, id: point.blockHash, height: 99 },
        },
      },
    ]);
    await expect(
      promiseCanonicalPointReader("ws://ogmios", socket.factory, point)(point),
    ).rejects.toThrow("precedes");
    expect(socket.requests).toHaveLength(1);
  });
  it("closes a partially opened socket and preserves the opening error", async () => {
    const socket = rpc([], true);
    socket.close.mockImplementationOnce(() => {
      throw new Error("secondary close failure");
    });
    await expect(
      promiseCanonicalPointReader("ws://ogmios", socket.factory, tip)(point),
    ).rejects.toThrow("socket failed while opening");
    expect(socket.close).toHaveBeenCalledOnce();
    expect(socket.requests).toHaveLength(0);
  });
});
