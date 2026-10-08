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
  L1_TIP_REFRESH_MS,
  l1NowUnixTimeMs,
  l1SlotNow,
  observeL1Tip,
  registerL1TipSource,
} from "../src/l1-heads.js";
import { operatorStatusProgram } from "../src/transactions/operators/status.js";
import { TEN_MINUTES_MS } from "./helpers/l1-tip.js";

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
          : Effect.succeed(tip),
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
          return 50;
        }),
      { slotLengthMs: 1_000, monotonicNowMs: () => monotonicMs },
    );
    await Effect.runPromise(l1SlotNow(api));
    await Effect.runPromise(l1SlotNow(api));
    expect(reads).toBe(1);
    // A submit-slot read elsewhere is observed and refreshes the window.
    monotonicMs += L1_TIP_REFRESH_MS - 1;
    observeL1Tip(api, 60);
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
    registerL1TipSource([live], () => Effect.succeed(tipSlot), {
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
    registerL1TipSource([live], () => Effect.succeed(tipSlot), {
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
