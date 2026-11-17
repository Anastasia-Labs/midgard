/**
 * The confirmation wait over provider reads that answer late or never. Each
 * read has its own deadline; a status read that misses it is aborted, and
 * each kind of read is made one at a time, so a provider that never answers
 * holds one read of each kind, not one per poll. Lucid and the L1 access are
 * faked; the node's wait runs as is, on the production confirmation options
 * and the test clock.
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { OutRef } from "@lucid-evolution/lucid";
import { Effect, Fiber, Option, TestClock, TestContext } from "effect";
import { describe, expect, it } from "vitest";

import { type L1ViewPoint, openL1Access } from "../src/l1-access.js";
import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

const DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

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

  /** Opens a fake kupmios access over `provider` whose tip read is `tip`. */
  const openFakeAccess = (provider: object, tip: () => Promise<L1ViewPoint>) =>
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

  /** Counts the reads of one kind in flight, and the most there ever were. */
  const inFlight = () => {
    const reads = { now: 0, most: 0 };
    const track = <A>(read: Promise<A>): Promise<A> => {
      reads.now += 1;
      reads.most = Math.max(reads.most, reads.now);
      const settle = () => {
        reads.now -= 1;
      };
      read.then(settle, settle);
      return read;
    };
    return { reads, track };
  };

  it("settles a bounded transaction whose first tip read answers only after its deadline, without a second tip read meanwhile", async () => {
    // The chain grows one block every poll; the first tip read answers only
    // at the third poll, two read deadlines late.
    let polls = 0;
    let answerFirstTipRead: (() => void) | undefined;
    const tips = inFlight();
    const provider = {};
    const chainTip = (): L1ViewPoint => ({
      slot: polls * 10,
      id: `block-${polls.toString()}`,
    });
    openFakeAccess(provider, () =>
      tips.track(
        answerFirstTipRead === undefined && polls < 3
          ? new Promise<L1ViewPoint>((resolve) => {
              answerFirstTipRead = () => resolve(chainTip());
            })
          : Promise.resolve(chainTip()),
      ),
    );
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          transactionStatus: async (txHash: string) => {
            polls += 1;
            if (polls === 3) answerFirstTipRead?.();
            return { status: "not_found" as const, txHash };
          },
          utxosByOutRef: async (refs: readonly OutRef[]) => refs,
          wallet: () => ({}),
        } as never,
        {
          txHash: "tx-tip-read-answers-late",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 50 }),
        },
      ),
      1_000,
      3 * 95_000 + (DEPTH + 10) * 5_000,
    );

    expect(Option.isSome(outcome)).toBe(true);
    const result = Option.getOrThrow(outcome);
    expect(result?._tag).toBe("Left");
    if (result?._tag !== "Left") throw new Error("expected a failure");
    expect(result.left).toMatchObject({
      _tag: "TxExpiredError",
      txHash: "tx-tip-read-answers-late",
      invalidHereafterSlot: 50,
    });
    expect(tips.reads.most).toBe(1);
  });

  it("aborts a status read that misses its deadline, and keeps at most one status read and one tip read in flight against a provider that never answers", async () => {
    const statuses = inFlight();
    const tips = inFlight();
    const statusSignals: (AbortSignal | undefined)[] = [];
    const provider = {};
    openFakeAccess(provider, () => tips.track(new Promise<never>(() => {})));
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          transactionStatus: (
            _txHash: string,
            options?: { signal?: AbortSignal },
          ) => {
            statusSignals.push(options?.signal);
            return statuses.track(new Promise<never>(() => {}));
          },
          utxosByOutRef: async (refs: readonly OutRef[]) => refs,
          wallet: () => ({}),
        } as never,
        {
          txHash: "tx-provider-never-answers",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 50 }),
        },
      ),
      1_000,
      20 * 90_000,
    );

    // A bounded transaction's wait goes on while nothing is settled.
    expect(Option.isNone(outcome)).toBe(true);
    expect(statusSignals.length).toBeGreaterThan(0);
    expect(statusSignals[0]?.aborted).toBe(true);
    expect(statuses.reads.most).toBe(1);
    expect(tips.reads.most).toBe(1);
  });

  it("keeps at most one inputs read in flight against a provider that never answers it", async () => {
    // The chain grows one block every poll past the bound; the inputs read
    // never answers.
    let polls = 0;
    const inputs = inFlight();
    const provider = {};
    openFakeAccess(provider, async () => ({
      slot: polls * 10,
      id: `block-${polls.toString()}`,
    }));
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          transactionStatus: async (txHash: string) => {
            polls += 1;
            return { status: "not_found" as const, txHash };
          },
          utxosByOutRef: () => inputs.track(new Promise<never>(() => {})),
          wallet: () => ({}),
        } as never,
        {
          txHash: "tx-inputs-read-never-answers",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 50 }),
        },
      ),
      1_000,
      (DEPTH + 10) * 5_000 + 20 * 90_000,
    );

    expect(Option.isNone(outcome)).toBe(true);
    expect(inputs.reads.most).toBe(1);
  });
});
