import {
  loadL1Recording,
  ogmiosExchanges,
  ogmiosResult,
  recordedOgmiosWebSocket,
} from "@al-ft/midgard-test-support/l1-recordings";
import { describe, expect, it, vi } from "vitest";

import { followEventHistoryChain } from "../src/l1-event-history-chain.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";

const anchor = { slot: 10, id: "aa".repeat(32) };
const next = { slot: 20, id: "bb".repeat(32), height: 3 };
const fork = { slot: 21, id: "cc".repeat(32), height: 3 };
type Request = { id: number; method: string; params: unknown };

class Socket implements WebSocketLike {
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

const forward = (point = next, parent = anchor.id) => ({
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
const start = (
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
const flush = async () => {
  await new Promise<void>((resolve) => setImmediate(resolve));
};
const stop = async (run: ReturnType<typeof start>) => {
  run.controller.abort();
  expect(await run.completion).toBeInstanceOf(Error);
  expect(run.socket.closed).toBe(true);
  expect(run.onUnavailable).toHaveBeenCalledTimes(1);
};

describe("continuous node history ChainSync", () => {
  it("backpressures the next block until persistence finishes while heartbeats continue", async () => {
    let acknowledge!: () => void;
    const pending = new Promise<void>((resolve) => {
      acknowledge = resolve;
    });
    const run = start(new Socket(), 3, () => pending);
    await flush();
    run.socket.block(forward());
    await flush();
    await vi.waitFor(() =>
      expect(run.onTip.mock.calls.length).toBeGreaterThan(3),
    );
    expect(
      run.socket.requests.filter(({ method }) => method === "nextBlock"),
    ).toHaveLength(1);
    acknowledge();
    await flush();
    expect(
      run.socket.requests.filter(({ method }) => method === "nextBlock"),
    ).toHaveLength(2);
    await stop(run);
  });

  it.each(["abort", "heartbeat"] as const)(
    "revokes during a pending consumer on %s and absorbs its late rejection",
    async (cause) => {
      let reject!: (error: Error) => void;
      const pending = new Promise<void>((_resolve, fail) => {
        reject = fail;
      });
      const run = start(new Socket(), 3, () => pending);
      await flush();
      run.socket.block(forward());
      await flush();
      if (cause === "abort") run.controller.abort();
      else run.socket.health = false;
      expect(await run.completion).toBeInstanceOf(Error);
      expect(run.onUnavailable).toHaveBeenCalledTimes(1);
      expect(run.socket.closed).toBe(true);
      reject(new Error("late persistence cancellation"));
      await flush();
      expect(
        run.socket.requests.filter(({ method }) => method === "nextBlock"),
      ).toHaveLength(1);
    },
  );

  it("revokes a failed persistence acknowledgement before requesting another block", async () => {
    const run = start(new Socket(), 3, () =>
      Promise.reject(new Error("persistence failed")),
    );
    await flush();
    run.socket.block(forward());
    expect(await run.completion).toMatchObject({
      message: "persistence failed",
    });
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(
      run.socket.requests.filter(({ method }) => method === "nextBlock"),
    ).toHaveLength(1);
    expect(run.socket.closed).toBe(true);
  });

  it("keeps one nextBlock pending across idle deadlines and still receives a rollback", async () => {
    const run = start();
    await flush();
    run.socket.block(forward());
    await flush();
    await vi.waitFor(
      () => expect(run.onTip.mock.calls.length).toBeGreaterThan(10),
      { interval: 5 },
    );
    expect(
      run.socket.requests.filter(({ method }) => method === "nextBlock"),
    ).toHaveLength(2);
    expect(run.onTip.mock.calls.length).toBeGreaterThan(10);
    expect(run.onUnavailable).not.toHaveBeenCalled();
    run.socket.block({ direction: "backward", point: anchor, tip: fork });
    await flush();
    expect(run.events).toEqual(["intersection", "forward", "rollback"]);
    run.socket.block(forward(fork));
    await flush();
    expect(run.onForward).toHaveBeenCalledTimes(2);
    await stop(run);
  });

  it("accepts only an exact initial intersection acknowledgement", async () => {
    const run = start();
    await flush();
    run.socket.block({ direction: "backward", point: anchor, tip: next });
    await flush();
    expect(run.onRollback).not.toHaveBeenCalled();
    run.socket.block({ direction: "backward", point: anchor, tip: next });
    await flush();
    expect(run.onRollback).toHaveBeenCalledTimes(1);
    await stop(run);
  });

  it.each(["origin", { slot: anchor.slot, id: fork.id }])(
    "refuses an unrequested intersection %j",
    async (intersection) => {
      const socket = new Socket();
      socket.intersection = intersection;
      const run = start(socket);
      await flush();
      expect(await run.completion).toBeInstanceOf(Error);
      expect(run.onIntersection).not.toHaveBeenCalled();
      expect(socket.closed).toBe(true);
    },
  );

  it("closes the whole session when heartbeat stalls, without replacing nextBlock", async () => {
    const socket = new Socket();
    socket.health = false;
    const run = start(socket);

    expect(await run.completion).toMatchObject({
      message: expect.stringContaining("queryNetwork/tip did not answer"),
    });
    expect(
      socket.requests.filter(({ method }) => method === "nextBlock"),
    ).toHaveLength(1);
    expect(socket.closed).toBe(true);
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
  });

  it("publishes a heartbeat tip bracketing its block height between two tip reads", async () => {
    const socket = new Socket();
    socket.intersectionTip = { ...anchor, height: 2 };
    socket.blockHeight = 7;
    const run = start(socket);
    await vi.waitFor(() =>
      expect(run.onTip).toHaveBeenCalledWith({
        slot: next.slot,
        id: next.id,
        height: 7,
      }),
    );
    expect(
      socket.requests
        .filter(({ method }) => method.startsWith("queryNetwork/"))
        .slice(0, 3)
        .map(({ method }) => method),
    ).toEqual([
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
    ]);
    expect(run.onUnavailable).not.toHaveBeenCalled();
    await stop(run);
  });

  it("publishes an origin heartbeat tip only with an origin block height", async () => {
    const socket = new Socket();
    socket.networkTip = () => "origin";
    socket.blockHeight = "origin";
    const run = start(socket);
    await vi.waitFor(() => expect(run.onTip).toHaveBeenCalledWith("origin"));
    expect(run.onUnavailable).not.toHaveBeenCalled();
    await stop(run);
  });

  it("publishes no heartbeat tip while the tip moves between reads and stays alive", async () => {
    const socket = new Socket();
    let reads = 0;
    socket.networkTip = () =>
      ++reads % 2 === 0
        ? { slot: fork.slot, id: fork.id }
        : { slot: next.slot, id: next.id };
    const run = start(socket);
    await vi.waitFor(() =>
      expect(
        socket.requests.filter(
          ({ method }) => method === "queryNetwork/blockHeight",
        ).length,
      ).toBeGreaterThan(3),
    );
    // Only the intersection tip was published; the follower is still alive.
    expect(run.onTip).toHaveBeenCalledTimes(1);
    expect(run.onUnavailable).not.toHaveBeenCalled();
    expect(socket.closed).toBe(false);
    run.socket.block(forward());
    await flush();
    expect(run.onForward).toHaveBeenCalledTimes(1);
    expect(run.onTip).toHaveBeenCalledTimes(2);
    await stop(run);
  });

  it.each<[string, () => unknown, unknown]>([
    [
      "origin height at a point tip",
      () => ({ slot: 20, id: next.id }),
      "origin",
    ],
    ["block height at an origin tip", () => "origin", 3],
    ["missing block height", () => ({ slot: 20, id: next.id }), undefined],
    ["negative block height", () => ({ slot: 20, id: next.id }), -1],
    ["string block height", () => ({ slot: 20, id: next.id }), "3"],
    ["tip carrying no id", () => ({ slot: 20 }), 3],
  ])(
    "fails closed on an inconsistent heartbeat tip: %s",
    async (_label, networkTip, blockHeight) => {
      const socket = new Socket();
      socket.networkTip = networkTip;
      socket.blockHeight = blockHeight;
      const run = start(socket);
      expect(await run.completion).toBeInstanceOf(Error);
      expect(run.onTip).toHaveBeenCalledTimes(1);
      expect(run.onUnavailable).toHaveBeenCalledTimes(1);
      expect(socket.closed).toBe(true);
    },
  );

  it("revokes a disconnected stable-tip session and ignores late blocks", async () => {
    const run = start();
    await flush();
    run.socket.close();
    expect(await run.completion).toBeInstanceOf(Error);
    run.socket.block(forward());
    await flush();
    expect(run.onForward).not.toHaveBeenCalled();
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
  });

  it.each([
    ["parent", forward(next, fork.id)],
    ["slot", forward({ ...next, slot: anchor.slot })],
    ["direction", { direction: "sideways", tip: next }],
    ["tip", { ...forward(), tip: { ...next, height: -1 } }],
    ["origin tip", { ...forward(), tip: "origin" }],
    ["earlier tip", { ...forward(), tip: { ...anchor, height: 2 } }],
    ["same height fork", { ...forward(), tip: fork }],
    [
      "same slot fork",
      { ...forward(), tip: { ...fork, slot: next.slot, height: 4 } },
    ],
  ])(
    "rejects invalid %s before publishing a block",
    async (_label, response) => {
      const run = start();
      await flush();
      run.socket.block(response);
      expect(await run.completion).toBeInstanceOf(Error);
      expect(run.onForward).not.toHaveBeenCalled();
      expect(run.socket.closed).toBe(true);
    },
  );

  it("rejects a discontinuous height after its first forward", async () => {
    const run = start();
    await flush();
    run.socket.block(forward());
    await flush();
    run.socket.block(forward({ ...fork, height: 5 }, next.id));
    expect(await run.completion).toMatchObject({
      message: expect.stringContaining("breaks retained ancestry"),
    });
    expect(run.onForward).toHaveBeenCalledTimes(1);
  });

  it("uses the intersection tip height to validate the first forward", async () => {
    const socket = new Socket();
    socket.intersectionTip = { ...anchor, height: 2 };
    const run = start(socket);
    await flush();
    run.socket.block(forward({ ...next, height: 4 }));
    expect(await run.completion).toMatchObject({
      message: expect.stringContaining("breaks retained ancestry"),
    });
    expect(run.onForward).not.toHaveBeenCalled();
  });

  it.each([false, true])(
    "checks retained rollback height, including initial acknowledgement: %s",
    async (initial) => {
      const socket = new Socket();
      socket.intersectionTip = { ...anchor, height: 2 };
      const run = start(socket);
      await flush();
      if (!initial) {
        socket.block(forward());
        await flush();
      }
      socket.block({
        direction: "backward",
        point: anchor,
        tip: { ...anchor, height: 1 },
      });
      expect(await run.completion).toMatchObject({
        message: expect.stringContaining("contradicts"),
      });
      expect(run.onUnavailable).toHaveBeenCalledTimes(1);
      expect(socket.closed).toBe(true);
    },
  );

  it.each(["origin", { ...fork, slot: anchor.slot }])(
    "rejects an intersection contradicting its tip: %j",
    async (observedTip) => {
      const socket = new Socket();
      socket.intersectionTip = observedTip;
      const run = start(socket);
      expect(await run.completion).toMatchObject({
        message: expect.stringContaining("contradicts"),
      });
      expect(run.onIntersection).not.toHaveBeenCalled();
      expect(socket.closed).toBe(true);
    },
  );

  it.each(["origin", anchor])(
    "fences before refusing a rollback beyond retained ancestry: %j",
    async (rollback) => {
      const run = start(new Socket(), 2);
      await flush();
      run.socket.block(forward());
      await flush();
      run.socket.block(forward({ ...fork, height: 4 }, next.id));
      await flush();
      run.socket.block({ direction: "backward", point: rollback, tip: next });
      expect(await run.completion).toMatchObject({
        message: expect.stringContaining("exceeds retained ancestry"),
      });
      expect(run.events.slice(-2)).toEqual(["rollback", "unavailable"]);
      expect(run.socket.closed).toBe(true);
    },
  );

  // The doubles above restate what their author believes Ogmios sends. This
  // one serves what a live Ogmios v7.0.0 on preprod answered, through the
  // follower's own socket: a `queryNetwork/tip` with no height, a bare
  // `queryNetwork/blockHeight` number, and chain-sync tips and a praos block
  // that do carry heights.
  it("follows a recorded preprod session and publishes its heartbeat tip from the heightless network tip", async () => {
    const recording = loadL1Recording("preprod-ogmios-network");
    const [intersect] = ogmiosExchanges(recording, "findIntersection");
    const found = ogmiosResult(intersect!) as {
      intersection: { slot: number; id: string };
    };
    // The capture offered [tip, "origin"]; the follower offers only its
    // retained points. Ogmios answers both with the same first intersection,
    // so only the request is narrowed; every answer stays as recorded.
    recording.exchanges[recording.exchanges.indexOf(intersect!)] = {
      ...intersect!,
      request: {
        ...intersect!.request,
        params: { points: [found.intersection] },
      },
    };
    const [firstTip] = ogmiosExchanges(recording, "queryNetwork/tip");
    const [blockHeight] = ogmiosExchanges(
      recording,
      "queryNetwork/blockHeight",
    );
    const heartbeatTip = {
      ...(ogmiosResult(firstTip!) as { slot: number; id: string }),
      height: ogmiosResult(blockHeight!) as number,
    };
    expect(ogmiosResult(firstTip!)).not.toHaveProperty("height");

    const replay = recordedOgmiosWebSocket(recording);
    const controller = new AbortController();
    const forwards: unknown[] = [];
    const tips: unknown[] = [];
    const onUnavailable = vi.fn();
    const completion = followEventHistoryChain({
      ogmiosUrl: "http://localhost:1337",
      intersections: [found.intersection],
      retainedPointLimit: 3,
      requestTimeoutMs: 1_000,
      heartbeatIntervalMs: 5,
      signal: controller.signal,
      onIntersection: () => undefined,
      onForward: ({ point }) => void forwards.push(point),
      onRollback: () => undefined,
      onTip: (tip) => {
        tips.push(tip);
        // Stop after the first heartbeat: the recording holds one height.
        if (
          replay
            .requests()
            .some(({ method }) => method === "queryNetwork/blockHeight")
        )
          controller.abort();
      },
      onUnavailable,
      webSocketFactory: (url) =>
        new replay.WebSocket(url) as unknown as WebSocketLike,
    }).catch((error: unknown) => error);

    // Only the abort ended the session.
    expect(await completion).toMatchObject({ name: "AbortError" });
    expect(onUnavailable).toHaveBeenCalledTimes(1);
    const [adopted] = ogmiosExchanges(recording, "nextBlock")
      .map((exchange) => ogmiosResult(exchange) as { block?: unknown })
      .flatMap(({ block }) => (block === undefined ? [] : [block]));
    const { slot, id, height } = adopted as {
      slot: number;
      id: string;
      height: number;
    };
    expect(forwards).toEqual([{ slot, id, height }]);
    expect(height).toBe(heartbeatTip.height + 1);
    expect(tips.at(-1)).toEqual(heartbeatTip);
    expect(
      replay
        .requests()
        .filter(({ method }) => method.startsWith("queryNetwork/"))
        .map(({ method }) => method),
    ).toEqual([
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
    ]);
  });

  it("closes and drains if an owner callback fails", async () => {
    const run = start();
    await flush();
    run.onForward.mockImplementation(() => {
      throw new Error("owner failed");
    });
    run.socket.block(forward());
    expect(await run.completion).toMatchObject({ message: "owner failed" });
    expect(run.socket.closed).toBe(true);
  });
});
