/**
 * How long the confirmation wait of a submitted transaction lasts. A
 * transaction with an upper validity bound is waited on until that bound is
 * settled (it landed, or the bound lies the deployment's confirmation depth
 * under the tip with every input unspent), however long the chain takes to
 * get there; only an unbounded transaction ends at the fixed budget. Each
 * provider read has its own deadline. Lucid and the L1 access are faked;
 * the node's wait runs as is, on the production confirmation options.
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { OutRef } from "@lucid-evolution/lucid";
import { Effect, Fiber, Option, TestClock, TestContext } from "effect";
import { describe, expect, it } from "vitest";

import { type L1ViewPoint, openL1Access } from "../src/l1-access.js";
import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

const DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

/**
 * Block slots at Preprod cadence (1 s slots, a block in a slot with
 * probability f = 0.05, a 20 s mean gap) from a fixed seed, with no block
 * inside `stall`.
 */
const preprodBlockSlots = (
  seed: number,
  untilSlot: number,
  stall: Readonly<{ from: number; to: number }>,
): number[] => {
  let state = seed >>> 0;
  const random = () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4_294_967_296;
  };
  const slots: number[] = [];
  for (let slot = 1; slot <= untilSlot; slot++)
    if (random() < 0.05 && (slot < stall.from || slot > stall.to))
      slots.push(slot);
  return slots;
};

/**
 * A fake L1 on a virtual clock of 1 s slots that the wait sleeps on. Its
 * access answers the tip `tipAt` gives for the clock's slot.
 */
const fakeL1 = (tipAt: (slot: number) => L1ViewPoint) => {
  const provider = {
    slot: 0,
    now: () => provider.slot * 1_000,
    awaitSlot: (length = 1) => {
      provider.slot += length;
    },
  };
  const clock = provider;
  const tip = async () => tipAt(clock.slot);
  openL1Access({
    kind: "kupmios",
    provider: provider as never,
    endpoint: "fake",
    slotConfig: async () => ({ zeroTime: 0, zeroSlot: 0, slotLength: 1_000 }),
    tipSlot: async () => (await tip()).slot,
    viewPoint: tip,
    synchronizedViewPoint: tip,
    submitSlotSnapshot: async () => {
      throw new Error("not used");
    },
    close: async () => {},
  });
  return { clock, provider };
};

/** The latest of `blocks` at or before `slot`. */
const latestBlock = (blocks: readonly number[], slot: number): L1ViewPoint => {
  const at = blocks.filter((block) => block <= slot).at(-1) ?? 0;
  return { slot: at, id: `block-${at.toString()}` };
};

const blocksWithin = (blocks: readonly number[], from: number, to: number) =>
  blocks.filter((block) => block >= from && block <= to).length;

describe("confirmation wait of a bounded transaction at Preprod cadence", () => {
  // The bound is five minutes out, and the chain stalls for four minutes
  // across it: longer than the fixed budget (2 x 90 s) before the bound is
  // even reached.
  const BOUND = 300;
  const blocks = preprodBlockSlots(5, 3_000, { from: 260, to: 500 });

  it("waits past the fixed budget until the bound is the confirmation depth deep, then reports the transaction expired", async () => {
    const { clock, provider } = fakeL1((slot) => latestBlock(blocks, slot));
    const inputReadTips: number[] = [];
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider }),
            transactionStatus: async (txHash: string) => ({
              status: "not_found" as const,
              txHash,
            }),
            utxosByOutRef: async (refs: readonly OutRef[]) => {
              inputReadTips.push(latestBlock(blocks, clock.slot).slot);
              return refs;
            },
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-expired-at-preprod-cadence",
            signedTxCbor: signedTxCbor({ invalidHereafterSlot: BOUND }),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxExpiredError",
      txHash: "tx-expired-at-preprod-cadence",
      invalidHereafterSlot: BOUND,
    });
    const { tipSlot } = result.left as unknown as { tipSlot: number };
    expect(blocksWithin(blocks, BOUND, tipSlot)).toBeGreaterThanOrEqual(DEPTH);
    expect(inputReadTips.length).toBeGreaterThan(0);
    for (const tip of inputReadTips)
      expect(blocksWithin(blocks, BOUND, tip)).toBeGreaterThanOrEqual(DEPTH);
  });

  it("waits past the fixed budget for a transaction that lands in the last block before its bound", async () => {
    // The stall ends at slot 290 with the block that holds the transaction.
    const landing = [
      ...preprodBlockSlots(7, 50, { from: 0, to: 0 }),
      290,
      ...preprodBlockSlots(7, 3_000, { from: 0, to: 300 }),
    ];
    const { clock, provider } = fakeL1((slot) => latestBlock(landing, slot));
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider }),
            transactionStatus: async (txHash: string) =>
              clock.slot >= 290
                ? {
                    status: "confirmed" as const,
                    txHash,
                    confirmation: { blockHeight: 1, slot: 290 },
                  }
                : { status: "not_found" as const, txHash },
            utxosByOutRef: async (refs: readonly OutRef[]) =>
              clock.slot >= 290 ? [] : refs,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-landed-before-its-bound",
            signedTxCbor: signedTxCbor({ invalidHereafterSlot: BOUND }),
          },
        ),
      ),
    );

    expect(result).toMatchObject({
      _tag: "Right",
      right: "tx-landed-before-its-bound",
    });
  });
});

describe("confirmation wait across a rollback over the bound", () => {
  it("does not report a transaction expired on a tip past its bound that a rollback then replaces with a fork holding it", async () => {
    // Inside the fixed budget: up to slot 120 the tip is a block at 102
    // (past the bound) with every input unspent; then a fork wins whose
    // block at 99 holds the transaction, and the chain grows on it every 20
    // slots.
    const BOUND = 100;
    const tipAt = (slot: number): L1ViewPoint =>
      slot < 105
        ? latestBlock([20, 40, 60, 80, 90], slot)
        : slot < 120
          ? { slot: 102, id: "abandoned-102" }
          : {
              slot: 99 + 20 * Math.floor((slot - 120) / 20),
              id: `fork-${Math.floor((slot - 120) / 20).toString()}`,
            };
    const { clock, provider } = fakeL1(tipAt);
    const result = await Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider }),
            transactionStatus: async (txHash: string) =>
              clock.slot >= 120
                ? {
                    status: "confirmed" as const,
                    txHash,
                    confirmation: { blockHeight: 1, slot: 99 },
                  }
                : { status: "not_found" as const, txHash },
            utxosByOutRef: async (refs: readonly OutRef[]) =>
              clock.slot >= 120 ? [] : refs,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-revived-by-a-rollback",
            signedTxCbor: signedTxCbor({ invalidHereafterSlot: BOUND }),
          },
        ),
      ),
    );

    expect(result).toMatchObject({
      _tag: "Right",
      right: "tx-revived-by-a-rollback",
    });
  });
});

describe("confirmation wait over a provider read that never answers", () => {
  /** Runs the wait on the test clock, moving it on by `stepMs` up to `totalMs`. */
  const runOnTestClock = <A, E>(
    wait: Effect.Effect<A, E>,
    stepMs: number,
    totalMs: number,
  ) =>
    Effect.runPromise(
      Effect.gen(function* () {
        const fiber = yield* Effect.fork(Effect.either(wait));
        for (let elapsed = 0; elapsed < totalMs; elapsed += stepMs)
          yield* TestClock.adjust(stepMs);
        const exit = yield* Fiber.poll(fiber);
        yield* Fiber.interrupt(fiber);
        return Option.map(exit, (done) =>
          done._tag === "Success" ? done.value : undefined,
        );
      }).pipe(Effect.provide(TestContext.TestContext)),
    );

  it("ends an unbounded transaction's wait at the fixed budget though its status read hangs", async () => {
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider: undefined }),
          transactionStatus: () => new Promise<never>(() => {}),
          wallet: () => ({}),
        } as never,
        { txHash: "tx-status-hangs", signedTxCbor: signedTxCbor({}) },
      ),
      1_000,
      400_000,
    );

    expect(Option.isSome(outcome)).toBe(true);
    const result = Option.getOrThrow(outcome);
    expect(result?._tag).toBe("Left");
    if (result?._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxConfirmError",
      txHash: "tx-status-hangs",
    });
    expect(String(result.left.cause)).toContain(
      "Timed out waiting for transaction tx-status-hangs",
    );
  });

  it("settles a bounded transaction though its first tip read hangs", async () => {
    // The chain grows one block every poll; the first tip read never
    // answers.
    let polls = 0;
    let tipReads = 0;
    const provider = {};
    const tip = async (): Promise<L1ViewPoint> => {
      tipReads += 1;
      if (tipReads === 1) return new Promise<never>(() => {});
      return { slot: polls * 10, id: `block-${polls.toString()}` };
    };
    openL1Access({
      kind: "kupmios",
      provider: provider as never,
      endpoint: "fake",
      slotConfig: async () => ({ zeroTime: 0, zeroSlot: 0, slotLength: 1_000 }),
      tipSlot: async () => (await tip()).slot,
      viewPoint: tip,
      synchronizedViewPoint: tip,
      submitSlotSnapshot: async () => {
        throw new Error("not used");
      },
      close: async () => {},
    });
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          transactionStatus: async (txHash: string) => {
            polls += 1;
            return { status: "not_found" as const, txHash };
          },
          utxosByOutRef: async (refs: readonly OutRef[]) => refs,
          wallet: () => ({}),
        } as never,
        {
          txHash: "tx-tip-read-hangs",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 50 }),
        },
      ),
      1_000,
      90_000 + (DEPTH + 10) * 5_000,
    );

    expect(Option.isSome(outcome)).toBe(true);
    const result = Option.getOrThrow(outcome);
    expect(result?._tag).toBe("Left");
    if (result?._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxExpiredError",
      txHash: "tx-tip-read-hangs",
      invalidHereafterSlot: 50,
    });
  });
});
