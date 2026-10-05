import { describe, expect, it, vi } from "vitest";

import { followEventHistoryChain } from "../src/l1-event-history-chain.js";
import {
  L1SourceUnavailable,
  OgmiosRequestTimeout,
} from "../src/l1-source-unavailable.js";
import {
  anchor,
  flush,
  forward,
  next,
  Socket,
} from "./l1-event-history-chain.socket.js";

/** Answers each network query after a fixed delay on an open socket. */
class SlowSocket extends Socket {
  constructor(private readonly answerAfterMs: number) {
    super();
  }
  override send(data: string) {
    const request = JSON.parse(data) as { id: number; method: string };
    if (!request.method.startsWith("queryNetwork/")) return super.send(data);
    this.requests.push({ ...request, params: {} });
    setTimeout(() => {
      if (!this.closed)
        this.answer(
          { ...request, params: {} },
          request.method === "queryNetwork/tip"
            ? this.networkTip()
            : this.blockHeight,
        );
    }, this.answerAfterMs);
  }
}

const follow = (
  socket = new Socket(),
  missed: (misses: number) => void = () => undefined,
) => {
  const controller = new AbortController();
  const onTip = vi.fn();
  const onRollback = vi.fn();
  const onUnavailable = vi.fn();
  const onHeartbeatMiss = vi.fn((misses: number, _cause: unknown) =>
    missed(misses),
  );
  const completion = followEventHistoryChain({
    ogmiosUrl: "http://localhost:1337",
    intersections: [anchor],
    retainedPointLimit: 3,
    requestTimeoutMs: 30,
    heartbeatIntervalMs: 5,
    signal: controller.signal,
    onIntersection: () => undefined,
    onForward: () => undefined,
    onRollback,
    onTip,
    onUnavailable,
    onHeartbeatMiss,
    webSocketFactory: () => {
      queueMicrotask(() => socket.emit("open"));
      return socket;
    },
  }).catch((error: unknown) => error);
  return {
    socket,
    controller,
    completion,
    onTip,
    onRollback,
    onUnavailable,
    onHeartbeatMiss,
  };
};

describe("history ChainSync heartbeat tolerance", () => {
  it("survives two consecutive missed heartbeats and resumes on the next answer", async () => {
    let tips = 0;
    // The source answers again right after the second miss, before a third
    // heartbeat can be sent.
    const socket = new Socket();
    const run = follow(socket, (misses) => {
      if (misses < 2) return;
      tips = run.onTip.mock.calls.length;
      socket.health = true;
    });
    await vi.waitFor(() => expect(run.onTip).toHaveBeenCalled());
    run.socket.health = false;
    await vi.waitFor(() =>
      expect(run.onHeartbeatMiss).toHaveBeenCalledTimes(2),
    );
    await vi.waitFor(() =>
      expect(run.onTip.mock.calls.length).toBeGreaterThan(tips),
    );
    expect(run.onUnavailable).not.toHaveBeenCalled();
    expect(run.socket.closed).toBe(false);
    expect(run.onHeartbeatMiss.mock.calls.map(([misses]) => misses)).toEqual([
      1, 2,
    ]);
    for (const [, cause] of run.onHeartbeatMiss.mock.calls)
      expect(cause).toBeInstanceOf(OgmiosRequestTimeout);
    // An answer resets the count: two more misses are tolerated again.
    run.socket.health = false;
    await vi.waitFor(() =>
      expect(run.onHeartbeatMiss).toHaveBeenCalledTimes(4),
    );
    expect(run.onHeartbeatMiss.mock.calls.at(-1)?.[0]).toBe(2);
    expect(run.onUnavailable).not.toHaveBeenCalled();
    run.controller.abort();
    expect(await run.completion).toBeInstanceOf(Error);
    expect(run.socket.closed).toBe(true);
  });

  // Each read answers inside its own 30 ms deadline, but the three reads of
  // one probe together outlive it: the owner's lease budgets one deadline.
  it("counts a probe whose reads together outlive one deadline as a miss", async () => {
    const run = follow(new SlowSocket(18));
    await vi.waitFor(() => expect(run.onHeartbeatMiss).toHaveBeenCalled());
    expect(run.onHeartbeatMiss.mock.calls[0]?.[1]).toBeInstanceOf(
      OgmiosRequestTimeout,
    );
    // Only the intersection reported a tip; no probe answered.
    expect(run.onTip).toHaveBeenCalledTimes(1);
    run.controller.abort();
    await run.completion;
  });

  it("answers a probe whose three reads fit one deadline", async () => {
    const run = follow(new SlowSocket(2));
    await vi.waitFor(() =>
      expect(run.onTip.mock.calls.length).toBeGreaterThan(2),
    );
    expect(run.onHeartbeatMiss).not.toHaveBeenCalled();
    expect(run.onUnavailable).not.toHaveBeenCalled();
    run.controller.abort();
    await run.completion;
  });

  it("fails the session on the third consecutive miss", async () => {
    const run = follow();
    await vi.waitFor(() => expect(run.onTip).toHaveBeenCalled());
    run.socket.health = false;
    const failure = await run.completion;
    expect(failure).toBeInstanceOf(OgmiosRequestTimeout);
    expect(run.onHeartbeatMiss).toHaveBeenCalledTimes(2);
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
    expect(run.onUnavailable).toHaveBeenCalledWith(failure);
    expect(run.socket.closed).toBe(true);
  });

  it("does not tolerate a closed socket as a miss", async () => {
    const run = follow();
    await vi.waitFor(() => expect(run.onTip).toHaveBeenCalled());
    run.socket.close();
    const failure = await run.completion;
    expect(failure).toBeInstanceOf(L1SourceUnavailable);
    expect(failure).not.toBeInstanceOf(OgmiosRequestTimeout);
    expect(run.onHeartbeatMiss).not.toHaveBeenCalled();
    expect(run.onUnavailable).toHaveBeenCalledTimes(1);
  });

  it("hands a rollback behind its own intersection to the journal as a reconnect", async () => {
    const run = follow();
    await flush();
    run.socket.block(forward());
    await flush();
    const behind = { slot: anchor.slot - 1, id: "dd".repeat(32) };
    run.socket.block({ direction: "backward", point: behind, tip: next });
    const failure = await run.completion;
    expect(run.onRollback).toHaveBeenCalledWith(behind);
    expect(failure).toBeInstanceOf(L1SourceUnavailable);
    expect((failure as Error).message).toMatch(/exceeds retained ancestry/u);
  });
});
