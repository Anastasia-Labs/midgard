import { afterEach, describe, expect, it, vi } from "vitest";

import { signWatcherOrphanCancel } from "../../src/funding/prover-funding-orphan-cancel.transaction.js";
import type { WatcherProverFundingReservationInput } from "../../src/funding/prover-funding-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  cancelWallet,
  hashOf,
  orphanToCancel,
  recordedCancel,
} from "./orphan-cancel-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import type { Fixture } from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const fees = { minFeeA: 44n, minFeeB: 155_381n, coinsPerUtxoByte: 4_310n };
let clock = 1_000_000_000;

/** Signed cancel bytes spending `inputs`; each call signs distinct bytes. */
const signCancel = async (
  inputs: readonly WatcherProverFundingReservationInput[],
) =>
  await signWatcherOrphanCancel({
    inputs,
    wallet: cancelWallet().wallet,
    fees,
    now: (clock += 60_000),
  });

const recordOf = async (fixture: Fixture) =>
  (await fixture.records()).find(
    ({ reservationId }) => reservationId === fixture.plan.reservationId,
  )!;

const leasedAs = async (fixture: Fixture, role: "funding" | "collateral") =>
  (await recordOf(fixture)).activeInputs.filter((input) => input.role === role);

const record = async (fixture: Fixture, signedTransactionCborHex: string) =>
  await fixture.store.recordOrphanCancel!({
    orphan: await recordOf(fixture),
    signedTransactionCborHex,
  });

const MISMATCH = "prover funding cancel mismatch";

describe("the store's record of an orphan's cancel", () => {
  it("holds one live cancel of the orphan's own funding, and refuses a second", async () => {
    const { fixture } = await orphanToCancel();
    const funding = await leasedAs(fixture, "funding");
    const first = await signCancel(funding);
    await record(fixture, first);
    expect(await recordedCancel(fixture)).toMatchObject({
      transactionHash: hashOf(first),
      signedTransactionCborHex: first,
    });
    await expect(record(fixture, await signCancel(funding))).rejects.toThrow(
      MISMATCH,
    );
    expect((await recordedCancel(fixture))?.signedTransactionCborHex).toBe(
      first,
    );
  });

  it("refuses a cancel spending anything the orphan does not lease as funding", async () => {
    const { fixture } = await orphanToCancel();
    const funding = await leasedAs(fixture, "funding");
    const collateral = await leasedAs(fixture, "collateral");
    expect(collateral).not.toEqual([]);
    for (const inputs of [
      [...funding, collateral[0]!],
      [{ ...funding[0]!, outRef: `${"bb".repeat(32)}#7` }],
    ])
      await expect(record(fixture, await signCancel(inputs))).rejects.toThrow(
        MISMATCH,
      );
    expect(await recordedCancel(fixture)).toBeNull();
  });

  it("refuses a cancel against a superseded revision of the orphan", async () => {
    const { fixture } = await orphanToCancel();
    const funding = await leasedAs(fixture, "funding");
    const stale = await recordOf(fixture);
    const first = await signCancel(funding);
    await record(fixture, first);
    await fixture.store.dropRetiredOrphanCancel!({
      orphan: await recordOf(fixture),
      retirement: { ...retirementFixture(hashOf(first)), reason: "expired" },
    });
    expect(await recordedCancel(fixture)).toBeNull();
    await expect(
      fixture.store.recordOrphanCancel!({
        orphan: stale,
        signedTransactionCborHex: await signCancel(funding),
      }),
    ).rejects.toThrow(MISMATCH);
    await record(fixture, await signCancel(funding));
    expect(await recordedCancel(fixture)).not.toBeNull();
  });

  it("refuses a cancel of a reservation that never signed anything", async () => {
    const fixture = await setupFundingRecoveryFixture(false, false, true, true);
    const funding = await leasedAs(fixture, "funding");
    expect(funding).not.toEqual([]);
    await expect(record(fixture, await signCancel(funding))).rejects.toThrow(
      MISMATCH,
    );
  });

  it("never drops a cancel that landed, and drops one that expired", async () => {
    const { fixture } = await orphanToCancel();
    const cancel = await signCancel(await leasedAs(fixture, "funding"));
    await record(fixture, cancel);
    const retired = retirementFixture(hashOf(cancel));
    await expect(
      fixture.store.dropRetiredOrphanCancel!({
        orphan: await recordOf(fixture),
        retirement: { ...retired, reason: "included" },
      }),
    ).rejects.toThrow(MISMATCH);
    expect(await recordedCancel(fixture)).not.toBeNull();
    await fixture.store.dropRetiredOrphanCancel!({
      orphan: await recordOf(fixture),
      retirement: { ...retired, reason: "expired" },
    });
    expect(await recordedCancel(fixture)).toBeNull();
  });

  it("keeps the orphan until its cancel retires too, then settles it with no row left", async () => {
    const { fixture } = await orphanToCancel();
    const cancel = await signCancel(await leasedAs(fixture, "funding"));
    await record(fixture, cancel);
    const attempt = {
      transactionHash: fixture.transactionHash,
      retirement: retirementFixture(fixture.transactionHash),
    };
    expect(
      await fixture.store.settleOrphanedReservation!({
        orphan: await recordOf(fixture),
        retirements: [attempt],
      }),
    ).toBe(false);
    expect(await recordOf(fixture)).toMatchObject({ state: "active" });
    expect(
      await fixture.store.settleOrphanedReservation!({
        orphan: await recordOf(fixture),
        retirements: [
          attempt,
          {
            transactionHash: hashOf(cancel),
            retirement: {
              ...retirementFixture(hashOf(cancel)),
              reason: "included",
            },
          },
        ],
      }),
    ).toBe(true);
    expect(await recordOf(fixture)).toMatchObject({
      state: "released",
      activeInputs: [],
    });
    expect(await recordedCancel(fixture)).toBeNull();
  });
});
