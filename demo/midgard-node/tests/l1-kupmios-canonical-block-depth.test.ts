import { describe, expect, it } from "vitest";

import { canonicalOgmiosBlockDepth } from "../src/l1-kupmios.js";
import { intersectionSocket } from "./helpers/ogmios-intersection-socket.js";

const h32 = (digit: string) => digit.repeat(64);

const block = { blockHash: h32("9"), slot: 100, blockNo: 90n };
const tip = { id: h32("f"), slot: 9999, height: 2251 };
const read = (factory: ReturnType<typeof intersectionSocket>["factory"]) =>
  canonicalOgmiosBlockDepth({
    ogmiosUrl: "ws://ogmios.test",
    ...block,
    timeoutMs: 100,
    webSocketFactory: factory,
  });

describe("selected-chain canonical depth", () => {
  it("uses the exact intersection and its same-response tip; repeated reads see rollback", async () => {
    let canonical = true;
    const socket = intersectionSocket((request) =>
      canonical
        ? { result: { intersection: request.params.points[0], tip } }
        : {
            error: {
              code: 1000,
              message: "Intersection not found",
              data: { tip },
            },
          },
    );
    expect(await read(socket.factory)).toBe(2162n);
    canonical = false;
    expect(await read(socket.factory)).toBeNull();
    expect(socket.requests).toHaveLength(2);
    expect(socket.close).toHaveBeenCalledTimes(2);
    expect(socket.requests[0]).toMatchObject({
      method: "findIntersection",
      params: { points: [{ slot: block.slot, id: block.blockHash }] },
    });
  });

  it.each([
    { intersection: { slot: 100, id: h32("8") }, tip },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { id: tip.id, slot: tip.slot },
    },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { ...tip, height: 89 },
    },
    { intersection: "origin", tip },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { id: block.blockHash, slot: 100, height: 2251 },
    },
  ])(
    "rejects mismatch, missing height, older tip and fabricated intersection %#",
    async (result) => {
      const socket = intersectionSocket(() => ({ result }));
      await expect(read(socket.factory)).rejects.toThrow("exact intersection");
      expect(socket.close).toHaveBeenCalledOnce();
    },
  );

  it("treats a transport/protocol error as unavailable rather than rollback evidence", async () => {
    const socket = intersectionSocket(() => ({
      error: { code: 1001, message: "Interleaved request" },
    }));
    await expect(read(socket.factory)).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });

  it("bounds a socket that never answers", async () => {
    const socket = intersectionSocket(() => ({ result: {} }));
    const silent = (url: string) => ({
      ...socket.factory(url),
      send: () => undefined,
    });
    await expect(read(silent)).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });
});
