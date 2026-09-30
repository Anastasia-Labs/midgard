import { describe, expect, it } from "vitest";

import {
  awaitDaBondPoolCommitteeSync,
  daBondPoolCommitteeExpectedView,
  daBondPoolCommitteeStopSettleMs,
  daBondPoolCommitteeSyncBoundMs,
  daBondPoolCommitteeViewAgrees,
  daBondPoolSubmitterUtxoChange,
} from "./da-bond-pool-committee-process.js";
import {
  answer,
  fakeClock,
  quarantined,
  REPLAY_STUCK,
  shortReason,
  T0,
  withdrawingReason,
} from "./da-bond-pool-committee-process.the-journey-committee-node.js";

describe("the committee node's view of the port's pool snapshot", () => {
  const view = (
    snapshot: Parameters<typeof daBondPoolCommitteeExpectedView>[0],
  ) => daBondPoolCommitteeExpectedView(snapshot, 100n);

  it("agrees exactly, in both directions", () => {
    const short = view({ state: "bonded", backing: 40n });
    expect(daBondPoolCommitteeViewAgrees([shortReason(40n, T0)], short)).toBe(
      true,
    );
    expect(daBondPoolCommitteeViewAgrees([shortReason(41n, T0)], short)).toBe(
      false,
    );
    expect(daBondPoolCommitteeViewAgrees([], short)).toBe(false);

    const backed = view({ state: "bonded", backing: 100n });
    expect(daBondPoolCommitteeViewAgrees([], backed)).toBe(true);
    expect(daBondPoolCommitteeViewAgrees([shortReason(100n, T0)], backed)).toBe(
      false,
    );

    const withdrawing = view({
      state: "withdrawing",
      backing: 100n,
      unlockAt: 777,
    });
    expect(
      daBondPoolCommitteeViewAgrees([withdrawingReason(777n, T0)], withdrawing),
    ).toBe(true);
    expect(
      daBondPoolCommitteeViewAgrees([withdrawingReason(778n, T0)], withdrawing),
    ).toBe(false);
    expect(
      daBondPoolCommitteeViewAgrees(
        [withdrawingReason(777n, T0), "da_bond_pool_something_else"],
        withdrawing,
      ),
    ).toBe(false);
    expect(view({ state: "missing", backing: 0n })).toBeUndefined();
    expect(daBondPoolCommitteeViewAgrees([], undefined)).toBe(false);
  });

  it("waits for a pool read that started after the action, not an older answer", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const expected = view({ state: "bonded", backing: 100n });
    // The node's last tick started before the action, then a later tick.
    const answers = [
      answer([], since - 5_000),
      answer([], since + 1_000),
      answer([], since + 1_000),
      answer([], since + 2_000),
    ];
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answers.shift()!,
      expected,
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: true, reads: 4 });
  });

  it("polls past a read that got no answer while the node lives", async () => {
    const clock = fakeClock();
    const since = clock.now();
    let calls = 0;
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => {
        calls += 1;
        if (calls === 1)
          throw new TypeError("fetch failed", {
            cause: new Error("ECONNRESET"),
          });
        return answer([shortReason(40n, since + 10)], since - 1);
      },
      alive: () => true,
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: true, reads: 2 });
  });

  it("keeps the last answer when later reads get none by the bound", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const stale = answer([shortReason(40n, since - 1)], since - 1);
    let calls = 0;
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => {
        calls += 1;
        if (calls === 1) return stale;
        throw new DOMException("The operation timed out", "TimeoutError");
      },
      expected: view({ state: "bonded", backing: 140n }),
      since,
      timeoutMs: 2_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: false, synced: false, read: stale });
    expect(sync.readyz.poolReasons).toEqual([shortReason(40n, since - 1)]);
  });

  it("rethrows a read failure with no answer, from a dead node, or that is not transport", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const base = {
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 2_000,
      pollMs: 500,
      ...clock,
    };
    const lost = new TypeError("fetch failed");
    await expect(
      awaitDaBondPoolCommitteeSync({
        ...base,
        read: async () => {
          throw lost;
        },
      }),
    ).rejects.toBe(lost);
    let deadCalls = 0;
    await expect(
      awaitDaBondPoolCommitteeSync({
        ...base,
        read: async () => {
          deadCalls += 1;
          throw lost;
        },
        alive: () => false,
      }),
    ).rejects.toBe(lost);
    expect(deadCalls).toBe(1);
    const malformed = new Error("readyz body is not JSON");
    let malformedCalls = 0;
    await expect(
      awaitDaBondPoolCommitteeSync({
        ...base,
        read: async () => {
          malformedCalls += 1;
          throw malformed;
        },
      }),
    ).rejects.toBe(malformed);
    expect(malformedCalls).toBe(1);
  });

  it("returns at once when the node's L1 source is quarantined", async () => {
    const clock = fakeClock();
    const since = clock.now();
    let reads = 0;
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => {
        reads += 1;
        return answer(
          [`L1 source is quarantined: ${REPLAY_STUCK}`],
          since - 1,
          quarantined,
        );
      },
      expected: view({ state: "bonded", backing: 100n }),
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ synced: false, reads: 1 });
    expect(sync.readyz.l1Source).toEqual(quarantined);
    expect(reads).toBe(1);
    expect(clock.now()).toBe(since);
  });

  it("takes a pool reason checked after the action as fresh", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answer([shortReason(40n, since + 10)], since - 1),
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: true, reads: 1 });
  });

  it("returns the stale answer unsynced once the bound passes", async () => {
    const clock = fakeClock();
    const since = clock.now();
    // A reason the top-up should have cleared, checked before the top-up.
    const stale = answer([shortReason(40n, since - 1)], since - 1);
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => stale,
      expected: view({ state: "bonded", backing: 140n }),
      since,
      timeoutMs: 2_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: false, synced: false, reads: 5 });
    expect(sync.readyz.poolReasons).toEqual([shortReason(40n, since - 1)]);
  });

  it("returns a fresh answer that disagrees unsynced", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answer([shortReason(39n, since + 1)]),
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 1_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: false });
  });

  it("bounds the sync wait by polls plus twice the ideal confirmation time", () => {
    // The devnet: 2 s polls, depth 3, 1 s slots, f = 0.05.
    expect(
      daBondPoolCommitteeSyncBoundMs({
        pollIntervalMs: 2_000,
        confirmationDepth: 3,
        slotLengthMs: 1_000,
        activeSlotsCoeff: 0.05,
      }),
    ).toBe(10 * 2_000 + 2 * 3 * 20_000);
    expect(
      daBondPoolCommitteeSyncBoundMs({
        pollIntervalMs: 1_000,
        confirmationDepth: 1,
        slotLengthMs: 1_000,
        activeSlotsCoeff: 1,
        polls: 3,
      }),
    ).toBe(3_000 + 2_000);
    for (const bad of [
      { pollIntervalMs: 0 },
      { confirmationDepth: 0 },
      { confirmationDepth: 1.5 },
      { slotLengthMs: 0 },
      { activeSlotsCoeff: 0 },
      { activeSlotsCoeff: 1.5 },
      { activeSlotsCoeff: Number.NaN },
      { polls: 0 },
    ]) {
      expect(() =>
        daBondPoolCommitteeSyncBoundMs({
          pollIntervalMs: 2_000,
          confirmationDepth: 3,
          slotLengthMs: 1_000,
          activeSlotsCoeff: 0.05,
          ...bad,
        }),
      ).toThrow("Invalid committee sync bound inputs");
    }
  });

  it("watches the submitters after a stop for a node poll plus the finality lag", () => {
    // The devnet: 2 s polls, depth 3, 1 s slots, f = 0.05.
    const cadence = {
      pollIntervalMs: 2_000,
      confirmationDepth: 3,
      slotLengthMs: 1_000,
      activeSlotsCoeff: 0.05,
    };
    expect(daBondPoolCommitteeStopSettleMs(cadence)).toBe(
      2_000 + 2 * 3 * 20_000,
    );
    expect(
      daBondPoolCommitteeStopSettleMs({ ...cadence, activeSlotsCoeff: 1 }),
    ).toBe(2_000 + 2 * 3 * 1_000);
    expect(() =>
      daBondPoolCommitteeStopSettleMs({ ...cadence, confirmationDepth: 0 }),
    ).toThrow("Invalid committee sync bound inputs");
  });

  it("names the UTxOs a submitter spent or created", () => {
    expect(daBondPoolSubmitterUtxoChange(["a#0", "b#1"], ["b#1", "a#0"])).toBe(
      undefined,
    );
    expect(daBondPoolSubmitterUtxoChange(["a#0", "b#1"], ["b#1", "c#0"])).toBe(
      "spent [a#0], created [c#0]",
    );
  });
});
