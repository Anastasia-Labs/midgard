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

import { openL1Access } from "../src/l1-access.js";
import {
  L1_TIP_REFRESH_MS,
  l1NowUnixTimeMs,
  l1SlotNow,
  observeL1Tip,
} from "../src/l1-heads.js";
import { operatorStatusProgram } from "../src/transactions/operators/status.js";
import { attachTestL1Access, TEN_MINUTES_MS } from "./helpers/l1-tip.js";

/** A stub client: nothing but the access attached to it. */
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
    attachTestL1Access(api, 0, {
      tipSlot: async () => {
        if (tip === null) throw new Error("the L1 node is unreachable");
        return tip;
      },
      monotonicNowMs: () => monotonicMs,
    });
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
    attachTestL1Access(api, 0, {
      tipSlot: async () => {
        reads += 1;
        return 50;
      },
      monotonicNowMs: () => monotonicMs,
    });
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

  it("refuses a live client built over no L1 access, and answers an emulator client with its chain slot", async () => {
    const live = await liveClient();
    const unknown = await Effect.runPromise(Effect.either(l1SlotNow(live)));
    expect(Either.isLeft(unknown) && unknown.left.message).toBe(
      "L1 slot unknown: this Lucid client is not built over an L1 access adapter",
    );
    const emulator = new Emulator([]);
    const emulated = await Lucid(emulator, "Custom");
    emulator.awaitSlot(25);
    expect(await Effect.runPromise(l1SlotNow(emulated))).toBe(emulator.slot);
  });

  it("gives every live client built over an adapter's provider that adapter's clock, with no registration", async () => {
    let tipReads = 0;
    const provider = {
      getProtocolParameters: async () => PROTOCOL_PARAMETERS_DEFAULT,
    } as unknown as Provider;
    const access = openL1Access({
      kind: "node",
      provider,
      endpoint: "test",
      slotConfig: async () => ({
        zeroTime: 1_000_000,
        zeroSlot: 0,
        slotLength: 1_000,
      }),
      tipSlot: async () => {
        tipReads += 1;
        return 4_242;
      },
      viewPoint: async () => ({ slot: 4_242, id: "ab".repeat(32) }),
      synchronizedViewPoint: async () => ({ slot: 4_242, id: "ab".repeat(32) }),
      submitSlotSnapshot: () => Promise.reject(new Error("unused")),
      close: async () => undefined,
    });
    // Through the port, and straight over the adapter's provider: both carry
    // the clock.
    const viaPort = await access.lucid("Custom");
    const direct = await Lucid(provider, "Custom", {
      slotConfig: { zeroTime: 1_000_000, zeroSlot: 0, slotLength: 1_000 },
    });
    expect(await Effect.runPromise(l1SlotNow(viaPort))).toBe(4_242);
    expect(await Effect.runPromise(l1SlotNow(direct))).toBe(4_242);
    expect(await Effect.runPromise(l1NowUnixTimeMs(viaPort))).toBe(
      1_000_000 + 4_242_000,
    );
    // One clock per access: the second client read no tip of its own.
    expect(tipReads).toBe(1);
    // One provider belongs to one access.
    expect(() =>
      openL1Access({ ...access, provider, kind: "kupmios" }),
    ).toThrow("this provider already belongs to an open node L1 access");
  });

  it("does not move when the wall clock runs 10 minutes fast, while Lucid's own slot does", async () => {
    const live = await liveClient();
    const tipSlot = live.currentSlot();
    let monotonicMs = 0;
    attachTestL1Access(live, tipSlot, { monotonicNowMs: () => monotonicMs });
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
    attachTestL1Access(live, tipSlot, { monotonicNowMs: () => 0 });
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
