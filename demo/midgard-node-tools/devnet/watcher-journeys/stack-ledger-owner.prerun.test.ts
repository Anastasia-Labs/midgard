import type { OperatorShiftAppointment } from "midgard-watcher/tests/support/published-block-actor.operator-shift";
import { describe, expect, it } from "vitest";

import { lateShiftMinimumLeadMs } from "./honest-successor.observables.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import { StackQueueTailUnattestedError } from "./stack-journey-da.js";
import {
  fundStackLedgerOwnerBeforeRun,
  requireStackLedgerOwnerFunded,
  type StackLedgerOwnerDeposit,
  StackLedgerOwnerFundingRefusal,
  StackLedgerOwnerUnfundedRefusal,
} from "./stack-ledger-owner.js";
import {
  stackUserEventFloorOffsetMs,
  stackUserEventGate,
} from "./stack-user-event-floor.js";
import {
  eventTiming,
  OPERATOR_2,
  OUR_NODE,
  SHIFT_START_MS,
} from "./stack-user-event-floor.test-support.js";

const timing = eventTiming("preprod-testing");
const SHIFT_MS = timing.shiftDurationMs;
/** Our node's shift ends here and operator-2's starts. */
const OPERATOR_2_START_MS = SHIFT_START_MS + SHIFT_MS;
const POLL_MS = 30_000;
const DA_ATTESTATION_TIMEOUT_MS = 600_000;
const ABSORB_WITHIN_MS = 2 * SHIFT_MS + DA_ATTESTATION_TIMEOUT_MS;
const COMMIT_ALLOWANCE_MS = 190_000;
/** How long before a live shift's end a deposit must fall due. */
const MARGIN_MS = lateShiftMinimumLeadMs(COMMIT_ALLOWANCE_MS);
/** A deposit built now falls due this long after, at the latest. */
const DUE_AFTER_MS = timing.eventTtlMs + 1_000 + timing.eventWaitMs;
const LANDS_AFTER_MS = 60_000;
const ATTESTED_AFTER_MS = 120_000;

const txHash = (n: number) => n.toString(16).padStart(64, "0");

/** Our node's shifts alternate with operator-2's, ours first. */
const appointmentAt = (
  nowMs: number,
): NonNullable<OperatorShiftAppointment> => {
  const index = Math.floor((nowMs - SHIFT_START_MS) / SHIFT_MS);
  return {
    operator: index % 2 === 0 ? OUR_NODE : OPERATOR_2,
    startTime: BigInt(SHIFT_START_MS + index * SHIFT_MS),
  };
};

/** The first instant at or after `timeMs` inside one of our node's shifts. */
const ourShiftFrom = (timeMs: number) =>
  appointmentAt(timeMs).operator === OUR_NODE
    ? timeMs
    : Number(appointmentAt(timeMs).startTime) + SHIFT_MS;

/**
 * An attached stack on one clock that moves only while the run waits or
 * builds. A deposit is dated from the clock once its build's chain queries
 * are done, `buildTakesMs` after the build starts. Our node's ledger holds
 * the owner's output once our node absorbs a landed deposit, in its first
 * shift at or after the deposit is due; its committee attests that block a
 * little later. `ourLedgerFromMs` and `attestedFromMs` hold an output that
 * is already there; `pending` is a deposit an earlier run recorded, which
 * landed when `pendingLanded`. A `stuckScheduler` never leaves our node's
 * first shift. A `stalledKupo` keeps answering while its checkpoint never
 * reaches a deposit's validity end, so every deposit stays pending.
 * `attestedTailFails` is how every read of the attested tail fails.
 */
const stack = (input: {
  startMs: number;
  ourLedgerFromMs?: number;
  attestedFromMs?: number;
  pending?: StackLedgerOwnerDeposit;
  pendingLanded?: boolean;
  neverAbsorbs?: boolean;
  buildTakesMs?: number;
  stuckScheduler?: boolean;
  stalledKupo?: boolean;
  attestedTailFails?: Error;
}) => {
  let clock = input.startMs;
  let pending = input.pending;
  let ourLedgerFromMs = input.ourLedgerFromMs ?? Infinity;
  let attestedFromMs = input.attestedFromMs ?? Infinity;
  let schedulerReads = 0;
  let statusReads = 0;
  const landsAt = new Map<string, number>();
  const builds: { builtAtMs: number; inclusionTime: number }[] = [];
  const deposits: (StackLedgerOwnerDeposit & { builtAtMs: number })[] = [];
  const stages: string[] = [];
  const absorb = (deposit: StackLedgerOwnerDeposit, landsAtMs: number) => {
    landsAt.set(deposit.txHash, landsAtMs);
    if (input.neverAbsorbs === true) return;
    const absorbedMs = ourShiftFrom(Math.max(deposit.inclusionTime, landsAtMs));
    ourLedgerFromMs = Math.min(ourLedgerFromMs, absorbedMs);
    attestedFromMs = Math.min(attestedFromMs, absorbedMs + ATTESTED_AFTER_MS);
  };
  if (input.pending !== undefined && input.pendingLanded === true)
    absorb(input.pending, 0);
  const gate = stackUserEventGate({
    readAppointment: async () => {
      // A gate that never stops waiting would otherwise hang the test.
      if (++schedulerReads > 100_000)
        throw new Error("The gate read the scheduler without bound");
      return input.stuckScheduler === true
        ? appointmentAt(SHIFT_START_MS)
        : appointmentAt(clock);
    },
    timing,
    commitAllowanceMs: COMMIT_ALLOWANCE_MS,
    readUndeliveredEvent: async () => null,
    now: () => clock,
    wallClock: () => clock,
    awaitLedgerTime: async (timeMs) => {
      clock = Math.max(clock, timeMs);
    },
    onStage: (name) => stages.push(name),
  });
  return {
    builds,
    deposits,
    stages,
    pending: () => pending,
    now: () => clock,
    attestedFromMs: () => attestedFromMs,
    fund: () =>
      fundStackLedgerOwnerBeforeRun({
        ourLedgerHoldsOutput: async () => clock >= ourLedgerFromMs,
        ownerHoldsOutput: async () => {
          if (input.attestedTailFails !== undefined)
            throw input.attestedTailFails;
          return clock >= attestedFromMs;
        },
        readPendingDeposit: async () => pending,
        depositStatus: async (deposit) => {
          // A wait that never stops polling would otherwise hang the test.
          if (++statusReads > 100_000)
            throw new Error("The run read the deposit's status without bound");
          if (input.stalledKupo === true) return "pending";
          return clock >= (landsAt.get(deposit.txHash) ?? Infinity)
            ? "landed"
            : clock > deposit.validTo
              ? "expired"
              : "pending";
        },
        archivePendingDeposit: async () => {
          pending = undefined;
        },
        beforeDeposit: (notAfterMs) =>
          gate.beforeFunding(OPERATOR_2, notAfterMs),
        buildDeposit: async () => {
          const builtAtMs = clock;
          clock += input.buildTakesMs ?? 0;
          const deposit = {
            txHash: txHash(builds.length + 1),
            inclusionTime: clock + DUE_AFTER_MS,
            validTo: clock + DUE_AFTER_MS - timing.eventWaitMs,
          };
          builds.push({ builtAtMs, inclusionTime: deposit.inclusionTime });
          return {
            ...deposit,
            submit: async () => {
              deposits.push({ ...deposit, builtAtMs });
              pending = deposit;
              absorb(deposit, clock + LANDS_AFTER_MS);
            },
          };
        },
        admitsDeposit: (inclusionTimeMs) =>
          gate.admitsFunding(OPERATOR_2, inclusionTimeMs),
        clearPendingDeposit: async () => {
          pending = undefined;
        },
        now: () => clock,
        pause: async () => {
          clock += POLL_MS;
        },
        absorbWithinMs: ABSORB_WITHIN_MS,
        onStage: (name) => stages.push(name),
      }),
  };
};

/** A deposit an earlier run recorded, whose validity ended before this run. */
const stale: StackLedgerOwnerDeposit = {
  txHash: txHash(0xee),
  inclusionTime: OPERATOR_2_START_MS - 50_000,
  validTo: OPERATOR_2_START_MS - 100_000,
};

const operator2FloorMs =
  OPERATOR_2_START_MS + stackUserEventFloorOffsetMs(timing);

// The ledger owner is funded once, before the first family's timer starts:
// a family's limit holds only its own staging, not a deposit our node may
// absorb only after operator-2's whole shift and its committee's attestation.
describe("funding the drill's ledger owner before any family starts", () => {
  it("starts the first family's timer only once our node absorbed the deposit and its committee attested it, when the run starts in operator-2's shift", async () => {
    const run = stack({ startMs: OPERATOR_2_START_MS + 1_000 });
    let familyTimerStartsMs: number | undefined;
    await run.fund().then(() => {
      familyTimerStartsMs = run.now();
    });
    expect(run.deposits).toHaveLength(1);
    // The strike-floor gate still holds the deposit in operator-2's shift.
    expect(run.deposits[0]!.builtAtMs).toBeGreaterThanOrEqual(operator2FloorMs);
    // Our node absorbs it only in its own next shift, after operator-2's.
    expect(run.attestedFromMs()).toBeGreaterThanOrEqual(
      OPERATOR_2_START_MS + SHIFT_MS,
    );
    expect(familyTimerStartsMs).toBeGreaterThanOrEqual(run.attestedFromMs());
    expect(familyTimerStartsMs).toBeLessThanOrEqual(
      run.deposits[0]!.inclusionTime + ABSORB_WITHIN_MS,
    );
    expect(run.pending()).toBeUndefined();
  });

  it("deposits nothing, and does not wait for the floor, when our node's ledger already holds an attested output of the owner's", async () => {
    const startMs = OPERATOR_2_START_MS + 1_000;
    const run = stack({
      startMs,
      ourLedgerFromMs: 0,
      attestedFromMs: 0,
      pending: stale,
    });
    await run.fund();
    expect(run.builds).toEqual([]);
    expect(run.now()).toBe(startMs);
    expect(run.stages).toEqual([]);
    expect(run.pending()).toBeUndefined();
  });

  it("deposits nothing when our node's ledger holds the owner's output, and waits only for its attestation", async () => {
    const startMs = OPERATOR_2_START_MS + 1_000;
    const run = stack({
      startMs,
      ourLedgerFromMs: 0,
      attestedFromMs: startMs + 90_000,
    });
    await run.fund();
    expect(run.builds).toEqual([]);
    expect(run.now()).toBeGreaterThanOrEqual(startMs + 90_000);
  });

  it("refuses the run, by name, when the stack's attested tail cannot be read within its bound", async () => {
    const unattested = new StackQueueTailUnattestedError("ab".repeat(32));
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      ourLedgerFromMs: 0,
      attestedTailFails: unattested,
    });
    const refusal = await run.fund().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(StackLedgerOwnerFundingRefusal);
    expect((refusal as Error).cause).toBe(unattested);
  });

  it("resumes an earlier run's landed deposit without depositing again", async () => {
    const landed = {
      ...stale,
      txHash: txHash(0xdd),
      validTo: OPERATOR_2_START_MS + 300_000,
    };
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      pending: landed,
      pendingLanded: true,
    });
    await run.fund();
    expect(run.builds).toEqual([]);
    expect(run.now()).toBeGreaterThanOrEqual(run.attestedFromMs());
    expect(run.pending()).toBeUndefined();
  });

  it("reads whether the owner is funded from our node's ledger, not from the attested tail alone", async () => {
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      attestedFromMs: 0,
    });
    await run.fund();
    expect(run.deposits).toHaveLength(1);
  });

  it("refuses the run, by name, when the stack does not absorb the deposit by its deadline", async () => {
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      neverAbsorbs: true,
    });
    const refusal = await run.fund().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(StackLedgerOwnerFundingRefusal);
    expect((refusal as Error).name).toBe("StackLedgerOwnerFundingRefusal");
    expect((refusal as Error).message).toMatch(/before any family started/u);
    expect(((refusal as Error).cause as Error).message).toMatch(
      /did not absorb the ledger owner's deposit/u,
    );
    const [deposit] = run.deposits;
    expect(run.now()).toBeGreaterThan(
      deposit!.inclusionTime + ABSORB_WITHIN_MS,
    );
    expect(run.now()).toBeLessThanOrEqual(
      deposit!.inclusionTime + ABSORB_WITHIN_MS + POLL_MS,
    );
    expect(run.pending()).toEqual({
      txHash: deposit!.txHash,
      inclusionTime: deposit!.inclusionTime,
      validTo: deposit!.validTo,
    });
  });

  it("refuses the run, by name, when the stack's Kupo shows its deposit neither landed nor expired by the funding's deadline", async () => {
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      stalledKupo: true,
    });
    const refusal = await run.fund().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(StackLedgerOwnerFundingRefusal);
    expect(((refusal as Error).cause as Error).message).toMatch(
      /neither landed nor expired on L1/u,
    );
    // The same deadline as the deposit's absorption, met within one poll.
    const [deposit] = run.deposits;
    expect(run.deposits).toHaveLength(1);
    expect(run.now()).toBeGreaterThan(
      deposit!.inclusionTime + ABSORB_WITHIN_MS,
    );
    expect(run.now()).toBeLessThanOrEqual(
      deposit!.inclusionTime + ABSORB_WITHIN_MS + POLL_MS,
    );
    // Its record stays for the next run, which awaits it while it can land.
    expect(run.pending()).toEqual({
      txHash: deposit!.txHash,
      inclusionTime: deposit!.inclusionTime,
      validTo: deposit!.validTo,
    });
  });

  it("refuses the run, by name, when an earlier run's deposit stays pending past its deadline", async () => {
    const run = stack({
      startMs: OPERATOR_2_START_MS + 1_000,
      pending: stale,
      stalledKupo: true,
    });
    const refusal = await run.fund().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(StackLedgerOwnerFundingRefusal);
    expect(((refusal as Error).cause as Error).message).toMatch(
      /neither landed nor expired on L1/u,
    );
    // Neither set aside nor replaced: Kupo has not shown it cannot land.
    expect(run.deposits).toHaveLength(0);
    expect(run.pending()).toEqual(stale);
  });
});

// The gate foresees, before the build, when a deposit built now falls due.
// The deposit itself decides: a live operator must still be able to commit
// a block covering it before its shift ends, or operator-2 may be struck
// for it early in its next shift.
describe("the ledger owner's deposit in a live operator's shift", () => {
  it("submits a deposit built to fall due early enough for our node to commit it in its shift", async () => {
    const startMs = OPERATOR_2_START_MS - MARGIN_MS - DUE_AFTER_MS - 1_000;
    const run = stack({ startMs });
    await run.fund();
    expect(run.builds).toHaveLength(1);
    expect(run.deposits[0]!.builtAtMs).toBe(startMs);
    expect(run.deposits[0]!.inclusionTime + MARGIN_MS).toBeLessThanOrEqual(
      OPERATOR_2_START_MS,
    );
  });

  it("holds back a deposit that would fall due too close to our node's shift end for it to commit", async () => {
    // Due 10 s before the shift ends: inside the shift, but not by a commit.
    const run = stack({
      startMs: OPERATOR_2_START_MS - DUE_AFTER_MS - 10_000,
    });
    await run.fund();
    expect(run.deposits).toHaveLength(1);
    expect(run.deposits[0]!.builtAtMs).toBeGreaterThanOrEqual(operator2FloorMs);
  });

  it("drops, unsubmitted, a deposit whose slow build made it fall due too late, and builds it again past operator-2's floor", async () => {
    // Foreseen safe by a second, but the build's chain queries take five.
    const run = stack({
      startMs: OPERATOR_2_START_MS - MARGIN_MS - DUE_AFTER_MS - 1_000,
      buildTakesMs: 5_000,
    });
    await run.fund();
    expect(run.builds).toHaveLength(2);
    expect(run.builds[0]!.inclusionTime + MARGIN_MS).toBeGreaterThan(
      OPERATOR_2_START_MS,
    );
    expect(run.deposits).toHaveLength(1);
    expect(run.deposits[0]!.txHash).toBe(txHash(2));
    expect(run.deposits[0]!.builtAtMs).toBeGreaterThanOrEqual(operator2FloorMs);
  });

  it("refuses the run, by name, when the scheduler never moves on before the funding's deadline", async () => {
    const startMs = OPERATOR_2_START_MS - 1_000;
    const run = stack({ startMs, stuckScheduler: true });
    const refusal = await run.fund().then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(StackLedgerOwnerFundingRefusal);
    expect(run.builds).toEqual([]);
    expect(run.now()).toBeLessThanOrEqual(startMs + ABSORB_WITHIN_MS);
  });
});

describe("a family on a stack whose ledger owner is unfunded", () => {
  it("refuses, by name, rather than funding the owner itself", () => {
    const tail = {
      ledger: { entries: [] },
    } as unknown as JourneyLedgerPredecessor;
    let refusal: unknown;
    try {
      requireStackLedgerOwnerFunded(tail, {
        keyHash: "03".repeat(28),
        address: "addr_test1",
      });
    } catch (error) {
      refusal = error;
    }
    expect(refusal).toBeInstanceOf(StackLedgerOwnerUnfundedRefusal);
    expect((refusal as Error).name).toBe("StackLedgerOwnerUnfundedRefusal");
    expect((refusal as Error).message).toMatch(/funded before the run/u);
  });
});
