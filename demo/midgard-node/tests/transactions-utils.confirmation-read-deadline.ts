/**
 * The confirmation wait over provider reads that answer late or never. Each
 * read has its own deadline and is aborted when it misses it. A read that
 * ignores its abort and never settles costs one later read its deadline,
 * after which the reads go on beside it, so the wait still settles once the
 * provider answers again; and a provider that never answers holds at most
 * two reads of each kind, not one per poll. A bounded wait logs one line
 * when its reads stop deciding and one when they decide again. A tip read
 * is handed its abort, so one that ends at its abort ends at its deadline.
 * Lucid and the L1 access are faked; the node's wait runs as is, on the
 * production confirmation options and the test clock.
 */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { OutRef } from "@lucid-evolution/lucid";
import { Effect, Fiber, Logger, Option, TestClock, TestContext } from "effect";
import { describe, expect, it } from "vitest";

import {
  type L1ReadOptions,
  type L1ViewPoint,
  openL1Access,
} from "../src/l1-access.js";
import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.js";
import { signedTxCbor } from "./transactions-utils.parse-outside-validity-interval-details.js";

const DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

describe("confirmation wait over a provider read that never answers", () => {
  /** Runs the wait on the test clock, moving it on by `stepMs` up to `totalMs`. */
  const runOnTestClock = <A, E>(
    wait: Effect.Effect<A, E>,
    stepMs: number,
    totalMs: number,
    logs: string[] = [],
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
      }).pipe(
        Effect.provide(TestContext.TestContext),
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ logLevel, message }) => {
              logs.push(`${logLevel.label} ${String(message)}`);
            }),
          ),
        ),
      ),
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
  const openFakeAccess = (
    provider: object,
    tip: (options?: L1ReadOptions) => Promise<L1ViewPoint>,
  ) =>
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

  it("aborts a status read that misses its deadline, and keeps at most two status reads and two tip reads in flight against a provider that never answers", async () => {
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

    // A bounded transaction's wait goes on while nothing is settled; the
    // provider holds the read being made and one that ignored its abort.
    expect(Option.isNone(outcome)).toBe(true);
    expect(statusSignals.length).toBeGreaterThan(0);
    expect(statusSignals[0]?.aborted).toBe(true);
    expect(statuses.reads.most).toBe(2);
    expect(tips.reads.most).toBe(2);
  });

  it("keeps at most two inputs reads in flight against a provider that never answers it", async () => {
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
    expect(inputs.reads.most).toBe(2);
  });

  it("confirms a bounded transaction once the provider answers again, though its first status read ignores its abort and never settles", async () => {
    // The tip stays before the bound, so only the status read can end the
    // wait. The first status read never settles, whatever its signal does.
    const statuses = inFlight();
    let calls = 0;
    const provider = {};
    openFakeAccess(provider, async () => ({ slot: 0, id: "block-0" }));
    const outcome = await runOnTestClock(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          transactionStatus: (txHash: string) => {
            calls += 1;
            return statuses.track(
              calls === 1
                ? new Promise<never>(() => {})
                : Promise.resolve({
                    status: "confirmed" as const,
                    txHash,
                    confirmation: { txHash, slot: 0 },
                  }),
            );
          },
          utxosByOutRef: async (refs: readonly OutRef[]) => refs,
          wallet: () => ({}),
        } as never,
        {
          txHash: "tx-status-read-stuck-then-answers",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 10_000 }),
        },
      ),
      1_000,
      6 * 95_000,
    );

    expect(Option.isSome(outcome)).toBe(true);
    expect(Option.getOrThrow(outcome)?._tag).toBe("Right");
    expect(statuses.reads.most).toBe(2);
  });

  it("settles a bounded transaction expired once the provider answers again, though its first tip read ignores its abort and never settles", async () => {
    // The chain grows one block every poll; the first tip read never
    // settles, every later one answers.
    let polls = 0;
    let tipCalls = 0;
    const tips = inFlight();
    const provider = {};
    openFakeAccess(provider, () => {
      tipCalls += 1;
      return tips.track(
        tipCalls === 1
          ? new Promise<never>(() => {})
          : Promise.resolve({
              slot: polls * 10,
              id: `block-${polls.toString()}`,
            }),
      );
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
          txHash: "tx-tip-read-stuck-then-answers",
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
      txHash: "tx-tip-read-stuck-then-answers",
      invalidHereafterSlot: 50,
    });
    expect(tips.reads.most).toBe(2);
  });

  it("ends a tip read at its deadline when the read ends at its abort, as the follower store's read does", async () => {
    // The chain grows one block every poll; the first tip read answers
    // nothing and ends only when its signal fires, every later one answers.
    let polls = 0;
    let tipCalls = 0;
    let firstTipReadEnded = false;
    const provider = {};
    openFakeAccess(provider, (options) => {
      tipCalls += 1;
      if (tipCalls > 1)
        return Promise.resolve({
          slot: polls * 10,
          id: `block-${polls.toString()}`,
        });
      return new Promise<L1ViewPoint>((_, reject) => {
        options?.signal?.addEventListener(
          "abort",
          () => {
            firstTipReadEnded = true;
            reject(options.signal?.reason as Error);
          },
          { once: true },
        );
      });
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
          txHash: "tx-tip-read-ends-at-its-abort",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 50 }),
        },
      ),
      1_000,
      3 * 95_000 + (DEPTH + 10) * 5_000,
    );

    expect(Option.isSome(outcome)).toBe(true);
    expect(Option.getOrThrow(outcome)).toMatchObject({
      _tag: "Left",
      left: { _tag: "TxExpiredError" },
    });
    expect(firstTipReadEnded).toBe(true);
  });

  it("logs one undecided line while the tip read keeps failing, and one line when the reads decide again", async () => {
    // The tip read fails for the first ten polls, then answers a chain that
    // grows one block every poll.
    let polls = 0;
    const logs: string[] = [];
    const provider = {};
    openFakeAccess(provider, async () => {
      if (polls <= 10) throw new Error("the follower store is unavailable");
      return { slot: polls * 10, id: `block-${polls.toString()}` };
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
          txHash: "tx-tip-read-fails-then-answers",
          signedTxCbor: signedTxCbor({ invalidHereafterSlot: 150 }),
        },
      ),
      1_000,
      (DEPTH + 30) * 5_000,
      logs,
    );

    expect(Option.isSome(outcome)).toBe(true);
    const undecided = logs.filter((line) =>
      line.includes("tx-tip-read-fails-then-answers is undecided"),
    );
    expect(undecided).toHaveLength(1);
    expect(undecided[0]).toMatch(/^WARN /u);
    expect(undecided[0]).toContain(
      "the L1 tip could not be read: Error: the follower store is unavailable",
    );
    expect(
      logs.filter((line) =>
        line.includes(
          "tx-tip-read-fails-then-answers: its confirmation reads decide again",
        ),
      ),
    ).toHaveLength(1);
  });
});
