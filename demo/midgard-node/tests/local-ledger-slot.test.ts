import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  fetchLocalOgmiosSubmitSlotSnapshot,
  normalizeOgmiosHttpUrl,
} from "@/local-ledger-slot.js";

const jsonResponse = (body: unknown, status = 200): Response =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "content-type": "application/json" },
  });

describe("local Ogmios submit slot snapshots", () => {
  it("normalizes websocket URLs to HTTP health/query URLs", () => {
    expect(normalizeOgmiosHttpUrl("ws://127.0.0.1:1337/")).toBe(
      "http://127.0.0.1:1337",
    );
    expect(normalizeOgmiosHttpUrl("wss://ogmios.example/ws")).toBe(
      "https://ogmios.example/ws",
    );
  });

  it("derives a live submit slot from health freshness evidence", async () => {
    const fetchImpl = vi
      .fn()
      .mockResolvedValueOnce(
        jsonResponse({
          connectionStatus: "connected",
          networkSynchronization: 0.9999,
          lastKnownTip: { slot: "126544938" },
          lastTipUpdate: "2026-06-24T12:00:00.000Z",
        }),
      )
      .mockResolvedValueOnce(
        jsonResponse({
          jsonrpc: "2.0",
          result: { slot: "126544940" },
          id: "midgard-submit-slot",
        }),
      );

    const snapshot = await Effect.runPromise(
      fetchLocalOgmiosSubmitSlotSnapshot({
        ogmiosUrl: "ws://127.0.0.1:1337/",
        fetchImpl,
        nowMs: Date.parse("2026-06-24T12:00:03.000Z"),
      }),
    );

    expect(snapshot).toMatchObject({
      source: "local_ogmios_tip",
      currentSlot: 126544941,
      slotLengthMs: 1_000,
      health: {
        connectionStatus: "connected",
        networkSynchronization: 0.9999,
        lastKnownTipSlot: 126544938,
      },
    });
    expect(fetchImpl.mock.calls.map((call) => call[0])).toEqual([
      "http://127.0.0.1:1337/health",
      "http://127.0.0.1:1337",
    ]);
  });

  it("does not move the submit slot behind the queried tip", async () => {
    const fetchImpl = vi
      .fn()
      .mockResolvedValueOnce(
        jsonResponse({
          connectionStatus: "connected",
          networkSynchronization: 1,
          lastKnownTip: { slot: "126544938" },
          lastTipUpdate: "2026-06-24T12:00:00.000Z",
        }),
      )
      .mockResolvedValueOnce(
        jsonResponse({
          jsonrpc: "2.0",
          result: { slot: "126544950" },
          id: "midgard-submit-slot",
        }),
      );

    const snapshot = await Effect.runPromise(
      fetchLocalOgmiosSubmitSlotSnapshot({
        ogmiosUrl: "http://127.0.0.1:1337",
        fetchImpl,
        nowMs: Date.parse("2026-06-24T12:00:03.000Z"),
      }),
    );

    expect(snapshot.currentSlot).toBe(126544950);
  });

  it("fails closed when Ogmios health is disconnected or stale", async () => {
    const disconnected = vi.fn().mockResolvedValue(
      jsonResponse({
        connectionStatus: "disconnected",
        networkSynchronization: 1,
        lastKnownTip: { slot: 1 },
        lastTipUpdate: "2026-06-24T12:00:00.000Z",
      }),
    );
    const stale = vi.fn().mockResolvedValue(
      jsonResponse({
        connectionStatus: "connected",
        networkSynchronization: 1,
        lastKnownTip: { slot: 1 },
        lastTipUpdate: "2026-06-24T11:57:59.000Z",
      }),
    );

    await expect(
      Effect.runPromise(
        fetchLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: disconnected,
          nowMs: Date.parse("2026-06-24T12:00:00.000Z"),
        }),
      ),
    ).rejects.toThrow("Ogmios is not connected");
    await expect(
      Effect.runPromise(
        fetchLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: stale,
          nowMs: Date.parse("2026-06-24T12:00:00.000Z"),
        }),
      ),
    ).rejects.toThrow("Ogmios lastTipUpdate is stale");
  });

  it("fails closed when Ogmios health lacks freshness evidence", async () => {
    const missingFreshness = vi.fn().mockResolvedValue(
      jsonResponse({
        connectionStatus: "connected",
        networkSynchronization: 1,
      }),
    );

    await expect(
      Effect.runPromise(
        fetchLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: missingFreshness,
          nowMs: Date.parse("2026-06-24T12:00:00.000Z"),
        }),
      ),
    ).rejects.toThrow("lastKnownTip.slot");
  });
});
