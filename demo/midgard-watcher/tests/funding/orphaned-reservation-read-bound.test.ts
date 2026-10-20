import { describe, expect, it } from "vitest";

import {
  createWatcherOrphanedReservations,
  WatcherProverFundingOrphanedReservationError,
} from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import type {
  WatcherProverFundingReservationRecord,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import { createFixtureChain } from "../support/fault-proof-funding-fixture.signed-read.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import { signReplacement } from "./superseded-attempt-replacement.js";

const ORPHANS = 5;
const ATTEMPTS = 3;
const BOUND = 4;

/** `orphans` reservations, each with `attempts` signed attempts of its own,
 * over a chain where every attempt is retired past release finality; a pass
 * makes at most `bound` reads. */
const orphanedMany = (
  shape = { orphans: ORPHANS, attempts: ATTEMPTS, bound: BOUND },
) => {
  const { chain, observeSignedTransaction } = createFixtureChain();
  chain.mockImplementation(async (txHash) => ({
    kind: "not_found",
    retirement: retirementFixture(txHash),
  }));
  const attempts = new Map<string, unknown[]>();
  const records = Array.from({ length: shape.orphans }, (_, orphan) => {
    const reservationId = `${orphan + 1}`.repeat(64);
    attempts.set(
      reservationId,
      Array.from({ length: shape.attempts }, (_, attempt) => ({
        actionKind: "proof.init",
        ...signReplacement(
          `${"7".repeat(63)}${orphan}#${attempt}`,
          5_000_000n,
          200n,
        ),
      })),
    );
    return {
      reservationId,
      decisionDigest: "d".repeat(64),
    } as unknown as WatcherProverFundingReservationRecord;
  });
  const settled = new Set<string>();
  let clock = 1_000_000;
  const orphans = createWatcherOrphanedReservations({
    store: {
      readRecordedSubmissions: async ({ reservationId }) =>
        attempts.get(reservationId)!.map((transition) => ({ transition })),
      readPendingTransition: async () => null,
      readCompletionHandoff: async () => null,
      settleOrphanedReservation: async ({ orphan }) => {
        settled.add(orphan.reservationId);
        return true;
      },
    } satisfies Partial<WatcherProverFundingReservationStore> as unknown as WatcherProverFundingReservationStore,
    observe: observeSignedTransaction,
    now: () => clock,
    perPass: shape.bound,
  });
  return {
    orphans,
    records,
    settled,
    reads: observeSignedTransaction,
    chain,
    later: () => {
      clock += 3_600_000;
    },
  };
};

describe("orphaned reservations read L1 within the per-pass bound", () => {
  it("a sweep makes at most its bound of attempt reads, and every orphan still settles", async () => {
    const { orphans, records, settled, reads, later } = orphanedMany();
    const perPass: number[] = [];
    for (let pass = 0; pass < 8 && settled.size < ORPHANS; pass += 1) {
      reads.mockClear();
      const left = records.filter(
        ({ reservationId }) => !settled.has(reservationId),
      );
      const holds = await orphans.sweep(left);
      perPass.push(reads.mock.calls.length);
      expect(holds).toHaveLength(
        left.filter(({ reservationId }) => !settled.has(reservationId)).length,
      );
      later();
    }
    expect(Math.max(...perPass)).toBeLessThanOrEqual(BOUND);
    expect(settled.size).toBe(ORPHANS);
    // A retired attempt is never read again.
    expect(perPass.reduce((sum, count) => sum + count, 0)).toBe(
      ORPHANS * ATTEMPTS,
    );
  });

  it("resolving a target makes at most its bound of attempt reads, and holds until every orphan settles", async () => {
    const { orphans, records, settled, reads, later } = orphanedMany();
    const perPass: number[] = [];
    let resolved: Awaited<ReturnType<typeof orphans.resolveTarget>> | null =
      null;
    for (let pass = 0; pass < 8 && resolved === null; pass += 1) {
      reads.mockClear();
      try {
        resolved = await orphans.resolveTarget({
          target: "e".repeat(64),
          orphans: records.filter(
            ({ reservationId }) => !settled.has(reservationId),
          ),
          handOver: true,
        });
      } catch (error) {
        expect(error).toBeInstanceOf(
          WatcherProverFundingOrphanedReservationError,
        );
      }
      perPass.push(reads.mock.calls.length);
      later();
    }
    expect(Math.max(...perPass)).toBeLessThanOrEqual(BOUND);
    expect(settled.size).toBe(ORPHANS);
    expect(resolved).toMatchObject({ handOver: [] });
    expect(perPass.reduce((sum, count) => sum + count, 0)).toBe(
      ORPHANS * ATTEMPTS,
    );
  });

  it("an orphan left unread for want of reads is read at the next pass, without waiting out a backoff", async () => {
    const { orphans, records, settled, reads } = orphanedMany({
      orphans: 2,
      attempts: 4,
      bound: 4,
    });
    const perPass: number[] = [];
    for (let pass = 0; pass < 2; pass += 1) {
      reads.mockClear();
      await orphans.sweep(
        records.filter(({ reservationId }) => !settled.has(reservationId)),
      );
      perPass.push(reads.mock.calls.length);
    }
    expect(perPass).toEqual([4, 4]);
    expect(settled.size).toBe(2);
  });

  it("an orphan with more open attempts than the bound is still read whole, so it can be taken over", async () => {
    const { orphans, records, reads, chain } = orphanedMany({
      orphans: 1,
      attempts: 6,
      bound: 4,
    });
    // Absent at the tip: never final, but a fresh execution may take it over.
    chain.mockResolvedValue({ kind: "not_found" });
    expect(
      await orphans.resolveTarget({
        target: "e".repeat(64),
        orphans: records,
        handOver: true,
      }),
    ).toEqual({ handOver: records, settled: [] });
    expect(reads).toHaveBeenCalledTimes(6);
  });
});
