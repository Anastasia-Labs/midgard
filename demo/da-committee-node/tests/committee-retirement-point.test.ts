import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { readCommitteeRetirementPoint } from "../src/l1/retirement-canonical-point.js";
import type { StateQueueReplayWebSocket } from "../src/l1/state-queue-replay-provider.open-rpc.js";

const checkpoint = { slot: 100, blockHash: "11".repeat(32) },
  tip = { slot: 10000, blockHash: "22".repeat(32), blockNo: 3000 };
const wireTip = { slot: tip.slot, id: tip.blockHash, height: tip.blockNo };
const found = {
  intersection: { slot: checkpoint.slot, id: checkpoint.blockHash },
  tip: wireTip,
};
const forward = {
  direction: "forward",
  block: {
    slot: 101,
    id: "33".repeat(32),
    ancestor: checkpoint.blockHash,
    height: 101,
  },
  tip: wireTip,
};
const handshake = {
  direction: "backward",
  point: { slot: checkpoint.slot, id: checkpoint.blockHash },
  tip: wireTip,
};
const run = async (replies: readonly unknown[], point = checkpoint) => {
  const listeners = new Map<string, (event: never) => void>();
  let sent = 0,
    closed = 0;
  const socket: StateQueueReplayWebSocket = {
    addEventListener: (type, callback) => {
      listeners.set(type, callback);
      if (type === "open") queueMicrotask(() => callback(undefined as never));
    },
    send: (data) => {
      const { id } = JSON.parse(data) as { id: number };
      const reply = replies[sent++];
      queueMicrotask(() =>
        listeners.get("message")?.({
          data: JSON.stringify({
            jsonrpc: "2.0",
            id,
            ...(reply && typeof reply === "object" && "error" in reply
              ? reply
              : { result: reply }),
          }),
        } as never),
      );
    },
    close: () => {
      closed++;
    },
  };
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
  try {
    return {
      proof: await readCommitteeRetirementPoint({
        ogmiosUrl: "ws://fabricated-local.invalid",
        point,
        boundary: tip,
        scope,
        limits: {
          requestRefusalMs: 100,
          httpResponseBytes: 4096,
          webSocketMessageBytes: 4096,
          rawUtxos: 20,
        },
        webSocketFactory: () => socket,
      }),
      sent,
      closed,
    };
  } finally {
    scope.close();
    expect(closed).toBe(1);
  }
};
describe("native retirement ancestry and selected-boundary absence", () => {
  it("derives the unknown checkpoint height from its raw immediate successor after the exact6.9 initial handshake", async () => {
    const result = await run([found, handshake, forward]);
    expect(result.proof).toEqual({
      point: { ...checkpoint, blockNo: 100 },
      tip,
    });
    expect(result.sent).toBe(3);
  });
  it("accepts direct forward progress and authenticates current-C height without a guessed predecessor", async () => {
    expect((await run([found, forward])).proof?.point.blockNo).toBe(100);
    const result = await run(
      [{ intersection: { slot: tip.slot, id: tip.blockHash }, tip: wireTip }],
      tip,
    );
    expect(result.proof).toEqual({ point: tip, tip });
    expect(result.sent).toBe(1);
  });
  it("returns absence only for explicit intersection-not-found carrying this exact C", async () => {
    expect(
      (
        await run([
          {
            error: {
              code: 1000,
              message: "Intersection not found",
              data: { tip: wireTip },
            },
          },
        ])
      ).proof,
    ).toBeNull();
    await expect(
      run([
        {
          error: {
            code: 1000,
            message: "Intersection not found",
            data: { tip: { ...wireTip, id: "44".repeat(32) } },
          },
        },
      ]),
    ).rejects.toThrow("selected tip");
    await expect(
      run([{ error: { code: 1000, message: "Intersection not found" } }]),
    ).rejects.toThrow("unavailable");
  });
  it.each([
    [
      "unrelated initial rollback",
      [
        found,
        { ...handshake, point: { slot: 99, id: checkpoint.blockHash } },
        forward,
      ],
    ],
    ["second rollback", [found, handshake, handshake]],
    [
      "foreign raw ancestor",
      [
        found,
        { ...forward, block: { ...forward.block, ancestor: "55".repeat(32) } },
      ],
    ],
    [
      "moving selected tip",
      [found, { ...forward, tip: { ...wireTip, height: 3001 } }],
    ],
    [
      "successor beyond C",
      [found, { ...forward, block: { ...forward.block, height: 3001 } }],
    ],
  ])(
    "refuses %s and closes the owned protocol session",
    async (_name, replies) => {
      await expect(run(replies)).rejects.toThrow(/rolled back|incoherent/);
    },
  );
});
