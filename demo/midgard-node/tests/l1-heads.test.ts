import {
  normalizeOgmiosHttpUrl,
  parseOgmiosShelleyGenesisSlotConfig,
} from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Provider,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  fetchLocalOgmiosShelleyGenesisSlotConfig,
  fetchLocalOgmiosSubmitSlotSnapshot,
  L1_TIP_REFRESH_MS,
  l1NowUnixTimeMs,
  l1SlotNow,
  observeL1Tip,
  registerL1TipSource,
} from "../src/l1-heads.js";
import { operatorStatusProgram } from "../src/transactions/operators/status.js";
import { TEN_MINUTES_MS, tipSnapshot } from "./helpers/l1-tip.js";

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

  it("queries the authoritative Shelley genesis slot epoch exactly once", async () => {
    const fetchImpl = vi.fn().mockResolvedValueOnce(
      jsonResponse({
        jsonrpc: "2.0",
        result: {
          startTime: "2026-07-14T04:56:19Z",
          slotLength: { milliseconds: 1_000 },
        },
        id: "midgard-custom-slot-config",
      }),
    );

    await expect(
      Effect.runPromise(
        fetchLocalOgmiosShelleyGenesisSlotConfig({
          ogmiosUrl: "ws://127.0.0.1:1337/",
          fetchImpl,
        }),
      ),
    ).resolves.toEqual({
      startTimeMs: 1_784_004_979_000,
      slotLengthMs: 1_000,
      configurationSha256:
        "758747f21adb257941483959dfb37f5dc8c94262eebe7d5aef8ebe2d1cce88f8",
    });
    expect(fetchImpl).toHaveBeenCalledTimes(1);
    expect(fetchImpl.mock.calls[0]?.[0]).toBe("http://127.0.0.1:1337");
    expect(JSON.parse(String(fetchImpl.mock.calls[0]?.[1]?.body))).toEqual({
      jsonrpc: "2.0",
      method: "queryNetwork/genesisConfiguration",
      params: { era: "shelley" },
      id: "midgard-custom-slot-config",
    });
  });

  it("rejects invalid Shelley genesis time and slot length data", () => {
    expect(() =>
      parseOgmiosShelleyGenesisSlotConfig({
        result: {
          startTime: "2026-07-14 04:56:19",
          slotLength: { milliseconds: 1_000 },
        },
      }),
    ).toThrow(/startTime is invalid/);
    expect(() =>
      parseOgmiosShelleyGenesisSlotConfig({
        result: {
          startTime: "2026-07-14T04:56:19Z",
          slotLength: { milliseconds: 1.5 },
        },
      }),
    ).toThrow(/positive integer slotLength/);
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

/** A stub client: only the identity the tip-source registry keys on. */
const stubClient = (): LucidEvolution => ({}) as LucidEvolution;

/** A live (non-emulator) Lucid client on Preprod, whose own slot follows the
 * wall clock. */
const liveClient = async (): Promise<LucidEvolution> =>
  await Lucid(
    {
      getProtocolParameters: async () => PROTOCOL_PARAMETERS_DEFAULT,
    } as unknown as Provider,
    "Preprod",
  );

describe("l1SlotNow", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  it("is unknown until a tip is read, then counts slots from the last tip on the monotonic clock", async () => {
    const api = stubClient();
    let monotonicMs = 0;
    let tip: number | null = null;
    registerL1TipSource(
      [api],
      () =>
        tip === null
          ? Effect.fail(new Error("Ogmios unreachable"))
          : Effect.succeed(tipSnapshot(tip)),
      { slotLengthMs: 1_000, monotonicNowMs: () => monotonicMs },
    );
    const unknown = await Effect.runPromise(Effect.either(l1SlotNow(api)));
    expect(Either.isLeft(unknown) && unknown.left._tag).toBe(
      "L1SlotUnknownError",
    );
    tip = 1_000;
    monotonicMs += L1_TIP_REFRESH_MS;
    expect(await Effect.runPromise(l1SlotNow(api))).toBe(1_000);
    // The source stops answering: the estimate keeps counting from the last
    // tip instead of failing.
    tip = null;
    monotonicMs += 7_500;
    expect(await Effect.runPromise(l1SlotNow(api))).toBe(1_007);
    // A tip behind the estimate (a rollback) never moves it back.
    tip = 990;
    monotonicMs += L1_TIP_REFRESH_MS;
    expect(await Effect.runPromise(l1SlotNow(api))).toBe(1_008);
  });

  it("reads the tip at most once per refresh window, and counts observed reads", async () => {
    const api = stubClient();
    let monotonicMs = 0;
    let reads = 0;
    registerL1TipSource(
      [api],
      () =>
        Effect.sync(() => {
          reads += 1;
          return tipSnapshot(50);
        }),
      { slotLengthMs: 1_000, monotonicNowMs: () => monotonicMs },
    );
    await Effect.runPromise(l1SlotNow(api));
    await Effect.runPromise(l1SlotNow(api));
    expect(reads).toBe(1);
    // A submit-slot read elsewhere is observed and refreshes the window.
    monotonicMs += L1_TIP_REFRESH_MS - 1;
    observeL1Tip(api, tipSnapshot(60));
    monotonicMs += L1_TIP_REFRESH_MS - 1;
    expect(await Effect.runPromise(l1SlotNow(api))).toBe(60);
    expect(reads).toBe(1);
    monotonicMs += 1;
    await Effect.runPromise(l1SlotNow(api));
    expect(reads).toBe(2);
  });

  it("refuses a live client with no tip source, and answers an emulator client with its chain slot", async () => {
    const live = await liveClient();
    const unknown = await Effect.runPromise(Effect.either(l1SlotNow(live)));
    expect(Either.isLeft(unknown)).toBe(true);
    const emulator = new Emulator([]);
    const emulated = await Lucid(emulator, "Custom");
    emulator.awaitSlot(25);
    expect(await Effect.runPromise(l1SlotNow(emulated))).toBe(emulator.slot);
  });

  it("does not move when the wall clock runs 10 minutes fast, while Lucid's own slot does", async () => {
    const live = await liveClient();
    const tipSlot = live.currentSlot();
    let monotonicMs = 0;
    registerL1TipSource([live], () => Effect.succeed(tipSnapshot(tipSlot)), {
      slotLengthMs: 1_000,
      monotonicNowMs: () => monotonicMs,
    });
    const slot = await Effect.runPromise(l1SlotNow(live));
    const nowMs = await Effect.runPromise(l1NowUnixTimeMs(live));
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(Date.now() + TEN_MINUTES_MS);
    expect(live.currentSlot()).toBeGreaterThanOrEqual(tipSlot + 599);
    // Unchanged on the wall clock alone.
    expect(await Effect.runPromise(l1SlotNow(live))).toBe(slot);
    expect(await Effect.runPromise(l1NowUnixTimeMs(live))).toBe(nowMs);
    // One second on the monotonic clock (and a fresh read of the same tip)
    // moves it by one slot, not by the 600 the wall clock jumped.
    monotonicMs += L1_TIP_REFRESH_MS;
    expect(await Effect.runPromise(l1SlotNow(live))).toBe(slot + 1);
    expect(await Effect.runPromise(l1NowUnixTimeMs(live))).toBe(nowMs + 1_000);
  });
});

const OPERATOR = "aa".repeat(28);

const utxo = (byte: string): UTxO =>
  ({
    txHash: byte.repeat(32),
    outputIndex: 0,
    address: "addr_test1vqfakeaddressfakeaddressfakeaddressfakeaddress",
    assets: { lovelace: 900_000_000n },
  }) as UTxO;

/** A directory list node: a root (null key) linking to the registration at
 * `nextActivationTime`, or a registration node keyed by its activation time. */
const directoryNode = (
  key: string | null,
  nextActivationTime: bigint | null,
  operator: string,
): SDK.NodeWithDatum => ({
  utxo: utxo(key === null ? "aa" : "11"),
  datum: {
    key: key === null ? "Empty" : { Key: { key } },
    next:
      nextActivationTime === null
        ? "Empty"
        : {
            Key: {
              key: SDK.posixTimeToRegisteredNodeKey(nextActivationTime),
            },
          },
    data: SDK.castRegisteredOperatorDatumToData({
      operator: key === null ? "00".repeat(28) : operator,
    }) as SDK.LinkedListNodeView["data"],
  },
  assetName: key === null ? "root" : "node",
});

describe("operator decisions read L1 now", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  it("does not call an activation time reached on a wall clock 10 minutes fast", async () => {
    const live = await liveClient();
    const tipSlot = live.currentSlot();
    registerL1TipSource([live], () => Effect.succeed(tipSnapshot(tipSlot)), {
      slotLengthMs: 1_000,
      monotonicNowMs: () => 0,
    });
    const l1NowMs = BigInt(live.slotToUnixTime(tipSlot));
    // Activation is 5 minutes after L1 now: before it on L1, after it on a
    // wall clock 10 minutes fast.
    const activationTime = l1NowMs + 5n * 60_000n;
    const snapshot = {
      registered: [
        directoryNode(null, activationTime, OPERATOR),
        directoryNode(
          SDK.posixTimeToRegisteredNodeKey(activationTime),
          null,
          OPERATOR,
        ),
      ],
      active: [directoryNode(null, null, OPERATOR)],
      retired: [directoryNode(null, null, OPERATOR)],
      scheduler: {
        utxo: utxo("44"),
        datum: "NoActiveOperators",
        assetName: "scheduler",
      },
    } as unknown as SDK.OperatorDirectorySnapshot;
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(Date.now() + TEN_MINUTES_MS);
    const report = await Effect.runPromise(
      operatorStatusProgram(live, {} as never, {
        operatorKeyHash: OPERATOR,
        watchdog: { enabled: false, patienceMs: 0 },
        snapshot,
      }),
    );
    expect(report).toMatchObject({
      registeredActivationTime: activationTime.toString(),
      activationTimeReached: false,
      asOf: new Date(Number(l1NowMs)).toISOString(),
    });
  });
});
