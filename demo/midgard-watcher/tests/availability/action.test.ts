import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  orderWatcherAvailabilityActions,
  selectWatcherAvailabilityAction,
  selectWatcherAvailabilityActions,
  watcherAvailabilityOpenDeadlineMissed,
  type WatcherAvailabilityOpenWindow,
} from "../../src/availability/action.js";
import {
  selectWatcherAvailabilityFunding,
  watcherAvailabilityTimeoutCollateralLovelace,
} from "../../src/availability/runtime.js";
import {
  fixture,
  HEADER_END_TIME,
  parametersFixture,
  utxo,
} from "../support/availability-challenge-fixture.js";

/** Open window used throughout: the header ends at 10_000, the window is 1_000. */
const WINDOW_MS = 1_000n;
const openWindow = (inclusiveValidityUpper: bigint) =>
  ({
    inclusiveValidityUpper,
    daChallengeWindowMs: WINDOW_MS,
  }) satisfies WatcherAvailabilityOpenWindow;
const BEFORE_DEADLINE = openWindow(HEADER_END_TIME + WINDOW_MS - 1n);
const AT_DEADLINE = openWindow(HEADER_END_TIME + WINDOW_MS);

describe("watcher availability lifecycle action selection", () => {
  it("opens for post-attestation public withholding and leaves available bytes unchallenged", () => {
    const { attested } = fixture();
    expect(
      selectWatcherAvailabilityAction(attested, false, 2_000n, BEFORE_DEADLINE),
    ).toEqual({ action: "open" });
    expect(
      selectWatcherAvailabilityAction(attested, true, 2_000n, BEFORE_DEADLINE),
    ).toBeNull();
  });

  it("never opens once the header's Open deadline has passed, and reports it", () => {
    const { attested } = fixture();
    expect(
      selectWatcherAvailabilityAction(attested, false, 2_000n, AT_DEADLINE),
    ).toBeNull();
    expect(
      watcherAvailabilityOpenDeadlineMissed(attested, false, AT_DEADLINE),
    ).toBe(true);
    expect(
      watcherAvailabilityOpenDeadlineMissed(attested, false, BEFORE_DEADLINE),
    ).toBe(false);
    expect(
      watcherAvailabilityOpenDeadlineMissed(attested, true, AT_DEADLINE),
    ).toBe(false);
  });

  it("refuses an Attested header that already carries a challenge record", () => {
    const { attested, challenged } = fixture();
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...attested, record: challenged.record },
        false,
        2_000n,
        BEFORE_DEADLINE,
      ),
    ).toThrow("Attested availability header has a challenge record");
  });

  it("refuses a challenge record that differs from the queue node's Challenged status", () => {
    const { challenged, plan } = fixture();
    const other = fixture("45");
    // Each half on its own: the same challenge over another commitment, and
    // the same commitment under another challenge name.
    for (const recordDatum of [
      { ...plan.record, commitment: other.plan.record.commitment },
      {
        ...plan.record,
        challenge_asset_name: other.plan.record.challenge_asset_name,
      },
    ]) {
      expect(() =>
        selectWatcherAvailabilityAction(
          { ...challenged, recordDatum },
          false,
          2_000n,
          BEFORE_DEADLINE,
        ),
      ).toThrow(
        "Challenge record differs from the queue node's Challenged status",
      );
    }
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...challenged, record: undefined, recordDatum: undefined },
        false,
        2_000n,
        BEFORE_DEADLINE,
      ),
    ).toThrow("Challenged availability header has no challenge record");
  });

  it("preserves a partial response until the deadline, then settles the timed-out tranche", () => {
    const { challenged, plan, bytes, parameters } = fixture();
    const record = plan.record;
    const publications = SDK.planDaAvailabilityPublications({
      commitment: record.commitment,
      payload: bytes,
      challengeAssetName: record.challenge_asset_name,
    });
    const partial = SDK.advanceDaAvailabilityTranche({
      active: challenged.tranches[0]!.datum,
      publication: publications[0]!.publications[0]!,
      responseGeometry: parameters.response_geometry,
      inclusiveValidityUpper: 2_000n,
      carrierOutputIndex: 1n,
    });
    const continued = {
      ...challenged,
      tranches: [{ utxo: utxo(5), datum: partial, carrier: utxo(1) }],
    };
    expect(
      selectWatcherAvailabilityAction(
        continued,
        false,
        record.response_deadline - 1n,
        AT_DEADLINE,
      ),
    ).toBeNull();
    expect(
      selectWatcherAvailabilityAction(
        continued,
        false,
        record.response_deadline,
        AT_DEADLINE,
      ),
    ).toMatchObject({
      action: "settle",
      challengeAssetName: record.challenge_asset_name,
    });
  });

  it("chooses terminal timeout or answered close only after every tranche was settled", () => {
    const { challenged } = fixture();
    const terminal = { ...challenged.terminalDatum!, next_tranche_index: 1n };
    expect(
      selectWatcherAvailabilityAction(
        {
          ...challenged,
          terminalDatum: { ...terminal, has_timed_out_tranche: true },
        },
        false,
        9_000_000n,
        AT_DEADLINE,
      )?.action,
    ).toBe("timeout");
    expect(
      selectWatcherAvailabilityAction(
        {
          ...challenged,
          terminalDatum: { ...terminal, has_timed_out_tranche: false },
        },
        false,
        9_000_000n,
        AT_DEADLINE,
      )?.action,
    ).toBe("close");
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...challenged, tranches: [] },
        false,
        9_000_000n,
        AT_DEADLINE,
      ),
    ).toThrow("next unsettled tranche");
  });

  it("waits for earlier queue headers before starting timeout removal", () => {
    const { challenged } = fixture();
    const later = {
      ...challenged,
      confirmedState: {
        ...challenged.confirmedState,
        datum: {
          ...challenged.confirmedState.datum,
          next: { Key: { key: "aa".repeat(28) } },
        },
      },
      terminalDatum: {
        ...challenged.terminalDatum!,
        next_tranche_index: 1n,
        has_timed_out_tranche: true,
      },
    };
    expect(
      selectWatcherAvailabilityAction(later, false, 9_000_000n, AT_DEADLINE),
    ).toBeNull();
  });

  it("resumes descendant pruning after Timeout spent the record and finally removes the head", () => {
    const { challenged, plan } = fixture();
    const lock: SDK.CorrectionLockDatum = {
      Locked: {
        target_header_hash: challenged.headerHash,
        correction_identity: {
          AvailabilityChallenge: {
            challenge_asset_name: plan.challengeAssetName,
          },
        },
      },
    };
    const timedOut = {
      ...challenged,
      record: undefined,
      recordDatum: undefined,
      correctionLock: {
        ...utxo(4),
        datum: Data.to(lock, SDK.CorrectionLockDatum),
      },
    };
    expect(
      selectWatcherAvailabilityAction(
        { ...timedOut, descendant: challenged.queue },
        false,
        9_000_000n,
        AT_DEADLINE,
      ),
    ).toEqual({
      action: "prune",
      challengeAssetName: plan.challengeAssetName,
    });
    expect(
      selectWatcherAvailabilityAction(timedOut, false, 9_000_000n, AT_DEADLINE)
        ?.action,
    ).toBe("remove");
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...timedOut, record: utxo(0) },
        false,
        9_000_000n,
        AT_DEADLINE,
      ),
    ).toThrow("live challenge record");
    // Another header's removal lock stops every availability step here.
    const elsewhere = fixture("46");
    expect(
      selectWatcherAvailabilityAction(
        { ...elsewhere.attested, correctionLock: timedOut.correctionLock },
        false,
        9_000_000n,
        BEFORE_DEADLINE,
      ),
    ).toBeNull();
  });
});

describe("watcher availability concurrency (spec #685 E3)", () => {
  it("opens a withheld descendant while its Challenged ancestor's challenge is live", () => {
    const first = fixture("44", HEADER_END_TIME);
    const second = fixture("45", HEADER_END_TIME + 100n);
    // The second header follows the first in the queue. Its Open is never
    // deferred to the ancestor's Timeout: if the ancestor settles, the
    // descendant would merge unchallenged (DECISIONS P9(5)).
    const descendant: SDK.DaAvailabilityChallengeSnapshot = {
      ...second.attested,
      confirmedState: first.challenged.confirmedState,
    };
    // The first header's challenge is mid-response: nothing to do for it yet.
    const selected = selectWatcherAvailabilityActions(
      [
        { snapshot: first.challenged, publiclyAvailable: false },
        { snapshot: descendant, publiclyAvailable: false },
      ],
      2_000n,
      BEFORE_DEADLINE,
    );
    expect(
      selected.map(({ snapshot, action }) => [snapshot.headerHash, action]),
    ).toEqual([[second.attested.headerHash, { action: "open" }]]);
  });

  it("puts every due Open ahead of other steps, earliest Open deadline first", () => {
    const first = fixture("44", HEADER_END_TIME);
    const second = fixture("45", HEADER_END_TIME - 100n);
    const third = fixture("46", HEADER_END_TIME + 100n);
    const settleReady = {
      ...first.challenged,
      terminalDatum: {
        ...first.challenged.terminalDatum!,
        next_tranche_index: 1n,
        has_timed_out_tranche: false,
      },
    };
    const selected = selectWatcherAvailabilityActions(
      [
        { snapshot: settleReady, publiclyAvailable: false },
        { snapshot: third.attested, publiclyAvailable: false },
        { snapshot: second.attested, publiclyAvailable: false },
      ],
      2_000n,
      openWindow(HEADER_END_TIME - 100n),
    );
    expect(
      orderWatcherAvailabilityActions(selected).map(({ snapshot, action }) => [
        snapshot.headerHash,
        action.action,
      ]),
    ).toEqual([
      [second.attested.headerHash, "open"],
      [third.attested.headerHash, "open"],
      [settleReady.headerHash, "close"],
    ]);
  });
});

describe("watcher availability funding", () => {
  it("keeps collateral disjoint and preserves working capital beyond the locked bond", () => {
    const selection = selectWatcherAvailabilityFunding({
      utxos: [utxo(0, 5n), utxo(1, 100n), utxo(2, 30n)],
      collateralLovelace: 5n,
      openingLovelace: 100n,
      requiredWorkingLovelace: 130n,
    });
    expect(selection.collateral.map(({ outputIndex }) => outputIndex)).toEqual([
      0,
    ]);
    expect(selection.exactOpening?.outputIndex).toBe(1);
    expect(() =>
      selectWatcherAvailabilityFunding({
        utxos: [utxo(0, 5n), utxo(1, 100n)],
        collateralLovelace: 5n,
        openingLovelace: 100n,
        requiredWorkingLovelace: 130n,
      }),
    ).toThrow("timeout removal path");
  });
  it("refuses to count datum-bearing outputs as collateral or fee capital", () => {
    expect(() =>
      selectWatcherAvailabilityFunding({
        utxos: [{ ...utxo(0, 5n), datum: Data.void() }, utxo(1, 100n)],
        collateralLovelace: 5n,
        openingLovelace: 100n,
        requiredWorkingLovelace: 100n,
      }),
    ).toThrow("separate plain-ADA collateral");
  });
  it("excludes reserved spending capital while allowing the actor's existing collateral", () => {
    const collateral = utxo(0, 5n);
    const reserved = utxo(1, 100n);
    const args = {
      utxos: [collateral, reserved, utxo(2, 30n)],
      collateralLovelace: 5n,
      openingLovelace: 100n,
      requiredWorkingLovelace: 30n,
      reservedOutRefs: new Set([
        `${collateral.txHash}#${collateral.outputIndex}`,
        `${reserved.txHash}#${reserved.outputIndex}`,
      ]),
    };
    const selection = selectWatcherAvailabilityFunding(args);
    expect(selection.collateral.map(({ outputIndex }) => outputIndex)).toEqual([
      0,
    ]);
    expect(selection.funding.outputIndex).toBe(2);
    expect(selection.exactOpening).toBeUndefined();
    expect(() =>
      selectWatcherAvailabilityFunding({
        ...args,
        requiredWorkingLovelace: 31n,
      }),
    ).toThrow("timeout removal path");
  });
});

describe("watcher Timeout collateral (spec #685 G9)", () => {
  it("sizes collateral for a full slash plus the maximum Timeout fee", () => {
    const parameters = parametersFixture();
    expect(
      watcherAvailabilityTimeoutCollateralLovelace({
        parameters,
        collateralPercentage: 150,
        minimumReturnLovelace: 1_000_000n,
      }),
    ).toBe(
      ((parameters.da_slash_penalty_lovelace +
        parameters.max_timeout_fee_lovelace) *
        150n +
        99n) /
        100n +
        1_000_000n,
    );
    // Rounds a fractional requirement up, never down.
    expect(
      watcherAvailabilityTimeoutCollateralLovelace({
        parameters: {
          da_slash_penalty_lovelace: 1n,
          max_timeout_fee_lovelace: 0n,
        },
        collateralPercentage: 150,
        minimumReturnLovelace: 0n,
      }),
    ).toBe(2n);
  });

  it("combines up to three coins, largest first, and refuses when three cannot cover it", () => {
    const coins = [utxo(0, 40n), utxo(1, 50n), utxo(2, 60n), utxo(3, 70n)];
    const selection = selectWatcherAvailabilityFunding({
      utxos: [...coins, utxo(4, 1_000n)],
      collateralLovelace: 1_100n,
      openingLovelace: 999n,
      requiredWorkingLovelace: 40n,
    });
    expect(selection.collateral.map(({ outputIndex }) => outputIndex)).toEqual([
      4, 3, 2,
    ]);
    // 70 + 60 + 50 = 180 < 181: a fourth coin could cover it, but the ledger
    // admits at most three collateral inputs, so the watcher fails closed.
    expect(() =>
      selectWatcherAvailabilityFunding({
        utxos: coins,
        collateralLovelace: 181n,
        openingLovelace: 999n,
        requiredWorkingLovelace: 0n,
      }),
    ).toThrow(
      "Availability wallet needs separate plain-ADA collateral of at least 181 lovelace in at most 3 coins",
    );
  });
});
