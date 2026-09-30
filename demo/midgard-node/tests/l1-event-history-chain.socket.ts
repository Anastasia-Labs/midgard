import { expect, vi } from "vitest";

import { followEventHistoryChain } from "../src/l1-event-history-chain.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";

export const anchor = { slot: 10, id: "aa".repeat(32) };

export const next = { slot: 20, id: "bb".repeat(32), height: 3 };

export const fork = { slot: 21, id: "cc".repeat(32), height: 3 };

type Request = { id: number; method: string; params: unknown };

export class Socket implements WebSocketLike {
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  readonly requests: Request[] = [];
  pendingBlock: Request | undefined;
  closed = false;
  health = true;
  intersection: unknown = anchor;
  intersectionTip: unknown = next;
  // Ogmios v6 queryNetwork/tip carries no height; blockHeight carries it.
  networkTip: () => unknown = () => ({ slot: next.slot, id: next.id });
  blockHeight: unknown = next.height;
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  answer(request: Request, result: unknown) {
    this.emit("message", { data: JSON.stringify({ id: request.id, result }) });
  }
  send(data: string) {
    const request = JSON.parse(data) as Request;
    this.requests.push(request);
    if (request.method === "nextBlock") {
      expect(this.pendingBlock).toBeUndefined();
      this.pendingBlock = request;
    } else if (request.method === "findIntersection")
      this.answer(request, {
        intersection: this.intersection,
        tip: this.intersectionTip,
      });
    else if (request.method === "queryNetwork/tip" && this.health)
      this.answer(request, this.networkTip());
    else if (request.method === "queryNetwork/blockHeight" && this.health)
      this.answer(request, this.blockHeight);
  }
  block(result: unknown) {
    const request = this.pendingBlock!;
    this.pendingBlock = undefined;
    this.answer(request, result);
  }
  close() {
    if (this.closed) return;
    this.closed = true;
    this.emit("close");
  }
}

export const forward = (point = next, parent = anchor.id) => ({
  direction: "forward",
  tip: point,
  block: {
    ...point,
    type: "praos",
    era: "conway",
    ancestor: parent,
    transactions: [],
  },
});

export const start = (
  socket = new Socket(),
  retainedPointLimit = 3,
  consume: () => void | Promise<void> = () => undefined,
) => {
  const controller = new AbortController();
  const events: string[] = [];
  const onIntersection = vi.fn(() => events.push("intersection"));
  const onForward = vi.fn(() => {
    events.push("forward");
    return consume();
  });
  const onRollback = vi.fn(() => events.push("rollback"));
  const onTip = vi.fn();
  const onUnavailable = vi.fn(() => events.push("unavailable"));
  const completion = followEventHistoryChain({
    ogmiosUrl: "http://localhost:1337",
    intersections: [anchor],
    retainedPointLimit,
    requestTimeoutMs: 30,
    heartbeatIntervalMs: 5,
    signal: controller.signal,
    onIntersection,
    onForward,
    onRollback,
    onTip,
    onUnavailable,
    webSocketFactory: () => {
      queueMicrotask(() => socket.emit("open"));
      return socket;
    },
  }).catch((error: unknown) => error);
  return {
    socket,
    controller,
    completion,
    events,
    onIntersection,
    onForward,
    onRollback,
    onTip,
    onUnavailable,
  };
};

export const flush = async () => {
  await new Promise<void>((resolve) => setImmediate(resolve));
};

export const stop = async (run: ReturnType<typeof start>) => {
  run.controller.abort();
  expect(await run.completion).toBeInstanceOf(Error);
  expect(run.socket.closed).toBe(true);
  expect(run.onUnavailable).toHaveBeenCalledTimes(1);
};
