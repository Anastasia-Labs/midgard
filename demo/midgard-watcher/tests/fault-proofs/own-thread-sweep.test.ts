/**
 * The sweep of our own threads over the follower's simulator chain, with a
 * canceller that records what it is asked: no workflow record exists
 * anywhere here, so whatever the sweep reclaims it found on the chain
 * alone. A cancel "lands" when the test forwards the thread's spend.
 */
import type {
  OwnThreadCanceller,
  OwnThreadCancelOutcome,
} from "@al-ft/midgard-fault-proofs/own-thread-cancel";
import { describe, expect, it } from "vitest";

import {
  createWatcherOwnThreadSweep,
  FAULT_PROOF_OWN_THREAD_UNCANCELLABLE,
  FAULT_PROOF_OWN_THREAD_UNRECLAIMED,
} from "../../src/fault-proofs/own-thread-sweep.js";
import {
  FINAL_DEPTH,
  OUR_KEY,
  THEIR_KEY,
  THREAD_POLICY,
  threadScene,
} from "../support/own-thread-sim.js";

type Scene = Awaited<ReturnType<typeof threadScene>>;
type Attempt = OwnThreadCancelOutcome | "send";

const UNCANCELLABLE_CATEGORY = "00000002";
const NOW = 1_000_000;

/** A canceller whose next outcomes are scripted; "send" asks `stillDue`
 * and, when still due, reports the cancel submitted. */
const fakeCanceller = (script: Attempt[] = []) => {
  const attempts: string[] = [];
  const outcomes: string[] = [];
  let beforeSend: () => Promise<void> = async () => undefined;
  const canceller: OwnThreadCanceller = {
    proverKeyHash: OUR_KEY,
    plan: (subject) =>
      subject.categoryId === UNCANCELLABLE_CATEGORY
        ? {
            kind: "uncancellable",
            category: "validationTraceDispute",
            reason: "it stands past the dispute opener",
          }
        : {
            kind: "cancellable",
            category: "doubleSpend",
            role: "step01",
            contractName: "doubleSpendStep01",
          },
    cancel: async ({ subject, stillDue }) => {
      attempts.push(subject.threadOutRef);
      const next = script.shift() ?? "send";
      if (next !== "send") {
        outcomes.push(next.kind);
        return next;
      }
      await beforeSend();
      const due = await stillDue();
      outcomes.push(due ? "submitted" : "withdrawn");
      if (!due) return { kind: "withdrawn" };
      return {
        kind: "submitted",
        txHash: "ab".repeat(32),
        reclaimedLovelace: 3_000_000n,
        validToMs: NOW + 180_000,
      };
    },
  };
  return {
    canceller,
    attempts,
    outcomes,
    onBeforeSend: (hook: () => Promise<void>) => {
      beforeSend = hook;
    },
  };
};

const sweepOver = (
  s: Scene,
  fake: ReturnType<typeof fakeCanceller>,
  options: Readonly<{ cancelsPerPass?: number; pageLimit?: number }> = {},
) => {
  const logs: string[] = [];
  let now = NOW;
  const sweep = createWatcherOwnThreadSweep({
    store: s.store,
    policyId: THREAD_POLICY,
    finalDepth: FINAL_DEPTH,
    openCanceller: async () => ({
      kind: "cancelling",
      canceller: fake.canceller,
    }),
    nowMs: () => now,
    log: (line) => logs.push(line),
    ...options,
  });
  return {
    sweep,
    logs,
    advance: (ms: number) => {
      now += ms;
    },
  };
};

/** One header removed `FINAL_DEPTH` deep under the given threads. */
const removedUnder = async (
  s: Scene,
  open: (header: string) => Promise<string[]>,
): Promise<string[]> => {
  const threads = await open(s.headers[0]!);
  await s.empty(FINAL_DEPTH);
  await s.removeTail();
  await s.empty(FINAL_DEPTH - 1);
  return threads;
};

describe("own-thread sweep", () => {
  it("reclaims our thread on a header another prover removed, once, and quietly after it lands", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
        await s.open(header, THEIR_KEY),
      ]);
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(fake.outcomes).toEqual(["submitted"]);
      // Sent and not yet landed: not sent again within its validity.
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      await s.forward([s.cancelTx(thread!)]);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("reclaims the threads of a merged header, and a spare thread our own removal left", async () => {
    const s = await threadScene({ headers: 2 });
    try {
      const [first, second] = s.headers as [string, string];
      const merged = await s.open(first);
      const used = await s.open(second);
      const spare = await s.open(second);
      await s.empty(FINAL_DEPTH);
      // Our conviction of `second` spent one thread; the other is spare.
      await s.forward([s.cancelTx(used)]);
      await s.mergeOldest();
      await s.removeTail();
      await s.empty(FINAL_DEPTH - 1);
      const fake = fakeCanceller();
      await sweepOver(s, fake).sweep.pass();
      expect([...fake.attempts].sort()).toEqual([merged, spare].sort());
    } finally {
      await s.store.close();
    }
  });

  it("reclaims two threads on one header, across passes when capped", async () => {
    const s = await threadScene();
    try {
      const threads = await removedUnder(s, async (header) => [
        await s.open(header),
        await s.open(header),
      ]);
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake, { cancelsPerPass: 1 });
      await sweep.pass();
      expect(fake.attempts).toHaveLength(1);
      await sweep.pass();
      expect([...fake.attempts].sort()).toEqual([...threads].sort());
    } finally {
      await s.store.close();
    }
  });

  it("pages through more threads than one page holds", async () => {
    const s = await threadScene();
    try {
      const threads = await removedUnder(s, async (header) => {
        const opened: string[] = [];
        for (let i = 0; i < 5; i += 1) opened.push(await s.open(header));
        return opened;
      });
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake, { pageLimit: 2, cancelsPerPass: 2 });
      for (let i = 0; i < 4; i += 1) await sweep.pass();
      expect([...fake.attempts].sort()).toEqual([...threads].sort());
    } finally {
      await s.store.close();
    }
  });

  it("sends nothing one block short of the depth, nor when a rollback returns the header", async () => {
    const s = await threadScene();
    try {
      const thread = await s.open(s.headers[0]!);
      await s.empty(FINAL_DEPTH);
      await s.removeTail();
      await s.empty(FINAL_DEPTH - 2);
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake);
      await sweep.pass();
      expect(fake.attempts).toEqual([]);

      await s.empty(1);
      // The header returns between the cancel's build and its send.
      fake.onBeforeSend(async () => {
        await s.backward(FINAL_DEPTH);
      });
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(fake.outcomes).toEqual(["withdrawn"]);
      fake.onBeforeSend(async () => undefined);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("names a thread with no cancel, and never attempts it", async () => {
    const s = await threadScene();
    try {
      const header = s.headers[0]!;
      const [stuck] = await removedUnder(s, async () => [
        await s.open(
          header,
          OUR_KEY,
          5_000_000n,
          UNCANCELLABLE_CATEGORY + header,
        ),
      ]);
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake);
      await sweep.pass();
      await sweep.pass();
      expect(fake.attempts).toEqual([]);
      expect(sweep.degradations()).toEqual([
        {
          reason: FAULT_PROOF_OWN_THREAD_UNCANCELLABLE,
          count: 1,
          detail: `${stuck!} lovelace=5000000 it stands past the dispute opener`,
        },
      ]);
    } finally {
      await s.store.close();
    }
  });

  it("names a thread unreclaimed after two failed attempts in a row, and clears it once spent", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
      ]);
      const fake = fakeCanceller([
        { kind: "unfunded", reason: "no fee coin" },
        { kind: "unfunded", reason: "no fee coin" },
      ]);
      const { sweep, logs } = sweepOver(s, fake);
      await sweep.pass();
      expect(sweep.degradations()).toEqual([]);
      await sweep.pass();
      expect(sweep.degradations()).toEqual([
        {
          reason: FAULT_PROOF_OWN_THREAD_UNRECLAIMED,
          count: 1,
          detail: `${thread!} lovelace=3000000 unfunded: no fee coin`,
        },
      ]);
      expect(logs.some((line) => line.includes("unfunded"))).toBe(true);
      await s.forward([s.cancelTx(thread!)]);
      await sweep.pass();
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("settles quietly beside a workflow's reclaim of the same thread", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
      ]);
      // The workflow's reclaim is pending: this attempt cannot be funded,
      // and the next finds the thread spent.
      const fake = fakeCanceller([
        { kind: "unfunded", reason: "the fee coin is pending" },
        { kind: "spent_elsewhere" },
      ]);
      const { sweep } = sweepOver(s, fake);
      await sweep.pass();
      expect(sweep.degradations()).toEqual([]);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread, thread]);
      expect(sweep.degradations()).toEqual([]);
      await s.forward([s.cancelTx(thread!)]);
      await sweep.pass();
      expect(fake.attempts).toHaveLength(2);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("reclaims our thread again once a rollback drops the workflow's landed cancel, though that cancel already resolved the thread's objective", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
      ]);
      // The workflow's own reclaim lands: its objective is resolved and no
      // workflow acts on this thread again.
      await s.forward([s.cancelTx(thread!)]);
      const fake = fakeCanceller();
      const { sweep } = sweepOver(s, fake);
      await sweep.pass();
      expect(fake.attempts).toEqual([]);

      // A rollback drops that cancel and keeps the removal: the thread
      // stands again, and the sweep alone reclaims it.
      await s.backward(1);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(fake.outcomes).toEqual(["submitted"]);
      await s.forward([s.cancelTx(thread!)]);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread]);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("after a restart between build and landing, lands no second cancel and raises nothing", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
      ]);
      const before = fakeCanceller();
      await sweepOver(s, before).sweep.pass();
      expect(before.attempts).toEqual([thread]);

      // Restarted: no memory of the cancel sent. The rebuilt cancel is
      // refused for its spent input, and the first lands.
      const after = fakeCanceller([
        { kind: "failed", reason: "BadInputsUTxO" },
      ]);
      const { sweep } = sweepOver(s, after);
      await sweep.pass();
      expect(after.attempts).toEqual([thread]);
      expect(sweep.degradations()).toEqual([]);
      await s.forward([s.cancelTx(thread!)]);
      await sweep.pass();
      expect(after.attempts).toEqual([thread]);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });

  it("sends a cancel again once its validity has ended without it landing", async () => {
    const s = await threadScene();
    try {
      const [thread] = await removedUnder(s, async (header) => [
        await s.open(header),
      ]);
      const fake = fakeCanceller();
      const { sweep, advance } = sweepOver(s, fake);
      await sweep.pass();
      advance(180_000);
      await sweep.pass();
      expect(fake.attempts).toEqual([thread, thread]);
    } finally {
      await s.store.close();
    }
  });

  it("does nothing while the application only observes", async () => {
    const s = await threadScene();
    try {
      await removedUnder(s, async (header) => [await s.open(header)]);
      let opened = 0;
      const sweep = createWatcherOwnThreadSweep({
        store: s.store,
        policyId: THREAD_POLICY,
        finalDepth: FINAL_DEPTH,
        openCanceller: async () => {
          opened += 1;
          return { kind: "observing" };
        },
      });
      await sweep.pass();
      await sweep.pass();
      // Asked again at each pass, so an application admitted later cancels.
      expect(opened).toBe(2);
      expect(sweep.degradations()).toEqual([]);
    } finally {
      await s.store.close();
    }
  });
});
