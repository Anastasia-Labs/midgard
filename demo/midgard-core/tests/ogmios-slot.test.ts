import { describe, expect, it, vi } from "vitest";

import {
  customSlotConfigFromShelleyGenesis,
  queryLocalOgmiosShelleyGenesisSlotConfig,
  queryLocalOgmiosSubmitSlotSnapshot,
} from "../src/ogmios-slot.js";

const jsonResponse = (body: unknown, status = 200): Response =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "content-type": "application/json" },
  });

const GENESIS_START = "2026-07-14T04:56:19Z";
const GENESIS_START_MS = Date.parse(GENESIS_START);

const genesisResponse = (slotLengthMs: number) =>
  jsonResponse({
    jsonrpc: "2.0",
    result: {
      networkMagic: 424_242,
      startTime: GENESIS_START,
      slotLength: { milliseconds: slotLengthMs },
    },
    id: "midgard-custom-slot-config",
  });

/** A synchronized Ogmios whose tip is `slot`, last updated at `updatedAtMs`. */
const snapshotResponses = (slot: number, updatedAtMs: number) => [
  jsonResponse({
    connectionStatus: "connected",
    networkSynchronization: 1,
    lastKnownTip: { slot },
    lastTipUpdate: new Date(updatedAtMs).toISOString(),
  }),
  jsonResponse({ jsonrpc: "2.0", result: { slot }, id: "midgard-submit-slot" }),
];

describe("the local Ogmios slot queries", () => {
  it("reads the Shelley genesis epoch with one JSON-RPC query", async () => {
    const fetchImpl = vi.fn().mockResolvedValueOnce(genesisResponse(1_000));

    await expect(
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl: "ws://127.0.0.1:1337/",
        fetchImpl,
      }),
    ).resolves.toMatchObject({
      startTimeMs: GENESIS_START_MS,
      slotLengthMs: 1_000,
    });
    expect(fetchImpl).toHaveBeenCalledTimes(1);
    expect(fetchImpl.mock.calls[0]?.[0]).toBe("http://127.0.0.1:1337");
    const init = fetchImpl.mock.calls[0]?.[1] as RequestInit | undefined;
    expect(JSON.parse(String(init?.body))).toEqual({
      jsonrpc: "2.0",
      method: "queryNetwork/genesisConfiguration",
      params: { era: "shelley" },
      id: "midgard-custom-slot-config",
    });
  });

  it("refuses a failed genesis query", async () => {
    const fetchImpl = vi
      .fn()
      .mockResolvedValueOnce(jsonResponse({ error: "unavailable" }, 500));

    await expect(
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl: "http://127.0.0.1:1337",
        fetchImpl,
      }),
    ).rejects.toThrow("HTTP 500 from http://127.0.0.1:1337");
  });

  it("maps a live snapshot onto the genesis epoch, and refuses disagreeing evidence", async () => {
    const slot = 4_494;
    const nowMs = GENESIS_START_MS + slot * 1_000 + 542;
    const [health, tip] = snapshotResponses(slot, nowMs);
    const snapshot = await queryLocalOgmiosSubmitSlotSnapshot({
      ogmiosUrl: "http://127.0.0.1:1337",
      fetchImpl: vi
        .fn()
        .mockResolvedValueOnce(health)
        .mockResolvedValueOnce(tip),
      nowMs,
    });
    expect(snapshot).toMatchObject({
      source: "local_ogmios_tip",
      currentSlot: slot,
      slotLengthMs: 1_000,
    });

    const genesis = await queryLocalOgmiosShelleyGenesisSlotConfig({
      ogmiosUrl: "http://127.0.0.1:1337",
      fetchImpl: vi.fn().mockResolvedValueOnce(genesisResponse(1_000)),
    });
    expect(customSlotConfigFromShelleyGenesis(genesis, snapshot)).toEqual({
      zeroTime: GENESIS_START_MS,
      zeroSlot: 0,
      slotLength: 1_000,
    });

    const twoSecondGenesis = await queryLocalOgmiosShelleyGenesisSlotConfig({
      ogmiosUrl: "http://127.0.0.1:1337",
      fetchImpl: vi.fn().mockResolvedValueOnce(genesisResponse(2_000)),
    });
    expect(() =>
      customSlotConfigFromShelleyGenesis(twoSecondGenesis, snapshot),
    ).toThrow(/slot length disagreement/u);
    expect(() =>
      customSlotConfigFromShelleyGenesis(genesis, {
        ...snapshot,
        currentSlot: slot + 3,
      }),
    ).toThrow(/clock disagreement/u);
  });
});
