import { describe, expect, it, vi } from "vitest";

import {
  customSlotConfigFromShelleyGenesis,
  customSlotConfigFromShelleyGenesisAtWallClock,
  DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS,
  ogmiosSlotEvidenceUnavailableCause,
  ogmiosTipMaxAgeMsFromShelleyGenesis,
  parseOgmiosShelleyGenesisSlotConfig,
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

  it("reports the ledger tip apart from the wall-clock submit slot in a block gap", async () => {
    // The last block was 80 s ago: the submit slot runs on wall time, while
    // the mempool still checks validity against the ledger tip's block.
    const tipSlot = 32_861;
    const updatedAtMs = GENESIS_START_MS + tipSlot * 1_000;
    const [health, tip] = snapshotResponses(tipSlot, updatedAtMs);
    const snapshot = await queryLocalOgmiosSubmitSlotSnapshot({
      ogmiosUrl: "http://127.0.0.1:1337",
      fetchImpl: vi
        .fn()
        .mockResolvedValueOnce(health)
        .mockResolvedValueOnce(tip),
      nowMs: updatedAtMs + 80_000,
    });
    expect(snapshot.currentSlot).toBe(tipSlot + 80);
    expect(snapshot.ledgerTipSlot).toBe(tipSlot);
  });
});

/** The transient reason a promise rejects with, or `terminal: <message>`. */
const outcome = (promise: Promise<unknown>): Promise<string> =>
  promise.then(
    () => "ok",
    (error: unknown) =>
      ogmiosSlotEvidenceUnavailableCause(error)?.reason ??
      `terminal: ${error instanceof Error ? error.message : String(error)}`,
  );

const syncOutcome = (run: () => unknown): Promise<string> =>
  outcome(Promise.resolve().then(run));

const devnetGenesis = (activeSlotsCoefficient?: unknown) =>
  parseOgmiosShelleyGenesisSlotConfig({
    result: {
      startTime: GENESIS_START,
      slotLength: { milliseconds: 1_000 },
      ...(activeSlotsCoefficient === undefined
        ? {}
        : { activeSlotsCoefficient }),
    },
  });

describe("slot-config construction apart from tip health", () => {
  it("derives the slot config while a stale lastTipUpdate is present", async () => {
    const tipSlot = 9_000;
    const updatedAtMs = GENESIS_START_MS + tipSlot * 1_000;
    const nowMs = updatedAtMs + 300_000;
    const genesis = devnetGenesis("1/20");
    const [health, tip] = snapshotResponses(tipSlot, updatedAtMs);

    await expect(
      outcome(
        queryLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: vi
            .fn()
            .mockResolvedValueOnce(health)
            .mockResolvedValueOnce(tip),
          nowMs,
          maxHealthAgeMs: ogmiosTipMaxAgeMsFromShelleyGenesis(genesis),
        }),
      ),
    ).resolves.toBe("ogmios_tip_stale");
    expect(
      customSlotConfigFromShelleyGenesisAtWallClock(genesis, { nowMs }),
    ).toEqual({ zeroTime: GENESIS_START_MS, zeroSlot: 0, slotLength: 1_000 });
  });

  it("reads the tip-age bound from the genesis active-slot coefficient", () => {
    expect(ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis("1/20"))).toBe(
      200_000,
    );
    expect(ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis(0.05))).toBe(
      200_000,
    );
    expect(ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis("1/10"))).toBe(
      100_000,
    );
    expect(ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis("1/20"), 5)).toBe(
      100_000,
    );
    expect(ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis())).toBe(
      DEFAULT_OGMIOS_HEALTH_MAX_AGE_MS,
    );
    for (const malformed of ["0/1", "2/1", "1/0", "abc", 0, -0.5]) {
      expect(() => devnetGenesis(malformed)).toThrow(
        /activeSlotsCoefficient is invalid/u,
      );
    }
    expect(() =>
      ogmiosTipMaxAgeMsFromShelleyGenesis(devnetGenesis("1/20"), 0),
    ).toThrow(/block interval count/u);
  });

  it("still refuses a submit snapshot past the derived bound", async () => {
    const genesis = devnetGenesis("1/20");
    const maxHealthAgeMs = ogmiosTipMaxAgeMsFromShelleyGenesis(genesis);
    const tipSlot = 12_000;
    const updatedAtMs = GENESIS_START_MS + tipSlot * 1_000;
    const read = (ageMs: number) => {
      const [health, tip] = snapshotResponses(tipSlot, updatedAtMs);
      return outcome(
        queryLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: vi
            .fn()
            .mockResolvedValueOnce(health)
            .mockResolvedValueOnce(tip),
          nowMs: updatedAtMs + ageMs,
          maxHealthAgeMs,
        }),
      );
    };
    // A 150 s block gap is normal at f = 1/20; it failed the old 120 s bound.
    await expect(read(150_000)).resolves.toBe("ok");
    await expect(read(maxHealthAgeMs)).resolves.toBe("ok");
    await expect(read(maxHealthAgeMs + 1)).resolves.toBe("ogmios_tip_stale");
  });

  it("waits only on a genesis that has not started, and refuses a foreign slot length", async () => {
    const genesis = devnetGenesis("1/20");
    await expect(
      syncOutcome(() =>
        customSlotConfigFromShelleyGenesisAtWallClock(genesis, {
          nowMs: GENESIS_START_MS - 1,
        }),
      ),
    ).resolves.toBe("genesis_not_started");
    await expect(
      syncOutcome(() =>
        customSlotConfigFromShelleyGenesisAtWallClock(
          { ...genesis, slotLengthMs: 2_000 },
          { nowMs: GENESIS_START_MS + 1 },
        ),
      ),
    ).resolves.toMatch(/^terminal: Custom slot length disagreement/u);
    const snapshot = {
      currentSlot: 100,
      observedAtMs: GENESIS_START_MS + 100_000,
      slotLengthMs: 1_000,
    };
    await expect(
      syncOutcome(() =>
        customSlotConfigFromShelleyGenesis(genesis, {
          ...snapshot,
          currentSlot: 103,
        }),
      ),
    ).resolves.toBe("ogmios_clock_disagreement");
    await expect(
      syncOutcome(() =>
        customSlotConfigFromShelleyGenesis(
          { ...genesis, slotLengthMs: 2_000 },
          snapshot,
        ),
      ),
    ).resolves.toMatch(/^terminal: Custom slot length disagreement/u);
  });
});

describe("the transient and terminal Ogmios slot evidence", () => {
  const nowMs = GENESIS_START_MS + 50_000_000;
  const fresh = new Date(nowMs - 1_000).toISOString();
  const tipOk = () =>
    jsonResponse({ jsonrpc: "2.0", result: { slot: 49_999 }, id: "x" });
  const healthRows: ReadonlyArray<readonly [string, unknown, string]> = [
    [
      "disconnected",
      { connectionStatus: "disconnected", networkSynchronization: null },
      "ogmios_not_connected",
    ],
    [
      "resyncing",
      {
        connectionStatus: "connected",
        networkSynchronization: 0.5,
        lastKnownTip: { slot: 1 },
        lastTipUpdate: fresh,
      },
      "ogmios_not_synchronized",
    ],
    [
      "no block seen yet",
      {
        connectionStatus: "connected",
        networkSynchronization: null,
        lastKnownTip: null,
        lastTipUpdate: null,
      },
      "ogmios_no_tip",
    ],
    [
      "tip at origin",
      {
        connectionStatus: "connected",
        networkSynchronization: 1,
        lastKnownTip: "origin",
        lastTipUpdate: fresh,
      },
      "ogmios_no_tip",
    ],
    [
      "missing connectionStatus",
      { networkSynchronization: 1, lastKnownTip: { slot: 1 } },
      "terminal: Ogmios health response is missing connectionStatus",
    ],
    [
      "unparseable tip slot",
      {
        connectionStatus: "connected",
        networkSynchronization: 1,
        lastKnownTip: { slot: "abc" },
        lastTipUpdate: fresh,
      },
      "terminal: Ogmios health response is missing lastKnownTip.slot",
    ],
    [
      "unparseable synchronization",
      {
        connectionStatus: "connected",
        networkSynchronization: "abc",
        lastKnownTip: { slot: 1 },
        lastTipUpdate: fresh,
      },
      "terminal: Ogmios health response is missing networkSynchronization",
    ],
    [
      "unparseable lastTipUpdate",
      {
        connectionStatus: "connected",
        networkSynchronization: 1,
        lastKnownTip: { slot: 1 },
        lastTipUpdate: "yesterday",
      },
      "terminal: Ogmios lastTipUpdate is not parseable: yesterday",
    ],
  ];
  it.each(healthRows)("health: %s", async (_name, payload, expected) => {
    await expect(
      outcome(
        queryLocalOgmiosSubmitSlotSnapshot({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: vi
            .fn()
            .mockResolvedValueOnce(jsonResponse(payload))
            .mockResolvedValueOnce(tipOk()),
          nowMs,
        }),
      ),
    ).resolves.toBe(expected);
  });

  const genesisRows: ReadonlyArray<
    readonly [string, () => Promise<Response>, string | RegExp]
  > = [
    [
      "connection refused",
      () => Promise.reject(new TypeError("fetch failed")),
      "ogmios_unreachable",
    ],
    ["HTTP 503", async () => jsonResponse({}, 503), "ogmios_unreachable"],
    ["HTTP 429", async () => jsonResponse({}, 429), "ogmios_unreachable"],
    ["HTTP 404", async () => jsonResponse({}, 404), /^terminal: HTTP 404/u],
    [
      "malformed JSON",
      async () => new Response("{not json", { status: 200 }),
      /^terminal: Failed to parse Ogmios Shelley genesis JSON/u,
    ],
    [
      "JSON-RPC method not found",
      async () =>
        jsonResponse({ error: { code: -32601, message: "no such method" } }),
      /^terminal: Ogmios Shelley genesis query failed: code=-32601/u,
    ],
    [
      "Ogmios query error",
      async () =>
        jsonResponse({ error: { code: 2001, message: "era mismatch" } }),
      "ogmios_query_unavailable",
    ],
  ];
  it.each(genesisRows)("genesis: %s", async (_name, answer, expected) => {
    const result = await outcome(
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl: "http://127.0.0.1:1337",
        fetchImpl: vi.fn().mockImplementationOnce(answer),
      }),
    );
    if (typeof expected === "string") {
      expect(result).toBe(expected);
    } else {
      expect(result).toMatch(expected);
    }
  });

  it("treats its own timeout as unreachable but a caller's abort as cancellation", async () => {
    const hang = (_url: string, init?: RequestInit) =>
      new Promise<Response>((_resolve, reject) => {
        init?.signal?.addEventListener("abort", () =>
          reject(new DOMException("aborted", "AbortError")),
        );
      });
    await expect(
      outcome(
        queryLocalOgmiosShelleyGenesisSlotConfig({
          ogmiosUrl: "http://127.0.0.1:1337",
          fetchImpl: hang,
          timeoutMs: 5,
        }),
      ),
    ).resolves.toBe("ogmios_unreachable");
    const controller = new AbortController();
    const pending = outcome(
      queryLocalOgmiosShelleyGenesisSlotConfig({
        ogmiosUrl: "http://127.0.0.1:1337",
        fetchImpl: hang,
        timeoutMs: 60_000,
        signal: controller.signal,
      }),
    );
    controller.abort();
    await expect(pending).resolves.toBe("terminal: aborted");
  });
});
