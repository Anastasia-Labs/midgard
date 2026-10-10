/**
 * The commit's validity lower bound is backdated from the ledger tip, not the
 * wall-clock submit slot. Between blocks Ogmios derives the submit slot from
 * wall time, so it runs ahead of the tip; the mempool checks the lower bound
 * against the tip, and refuses a bound past it as outside the validity
 * interval. The slot numbers replay the devnet refusal: ledger tip 32861,
 * derived submit slot 32940, `validTo` slot 33161.
 */
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  COMMIT_VALIDITY_BACKDATE_MS,
  commitValidityEndTimeCapMs,
  resolveCommitValidityInterval,
} from "../src/workers/utils/commit-end-time.js";

const makeLucid = async (): Promise<LucidEvolution> => {
  const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
  return Lucid(new Emulator([operator]), "Custom");
};

const INCIDENT_TIP_SLOT = 32_861;
const INCIDENT_SUBMIT_SLOT = 32_940;
const INCIDENT_VALID_TO_SLOT = 33_161;

const snapshot = (
  lucid: LucidEvolution,
  currentSlot: number,
  ledgerTipSlot?: number,
): SubmitSlotSnapshot => ({
  source: "l1_node_tip",
  currentSlot,
  ...(ledgerTipSlot === undefined ? {} : { ledgerTipSlot }),
  observedAtMs: lucid.slotToUnixTime(currentSlot),
  slotLengthMs: 1_000,
});

const slotOf = (lucid: LucidEvolution, unixTimeMs: number) =>
  Number(lucid.unixTimeToSlot(unixTimeMs));

/** The mempool admits a transaction whose lower bound is at most the slot
 * after the ledger tip, and whose upper bound is past it. */
const admittedAtTip = (
  lucid: LucidEvolution,
  interval: { validFromMs: number; validToMs: number },
  tipSlot: number,
) =>
  slotOf(lucid, interval.validFromMs) <= tipSlot + 1 &&
  tipSlot + 1 < slotOf(lucid, interval.validToMs);

const widthOf = (interval: { validFromMs: number; validToMs: number }) =>
  interval.validToMs - interval.validFromMs;

describe("commit validity lower bound against a stale ledger tip", () => {
  it("replays the devnet refusal: the bound moves from slot 32880 to 32801", async () => {
    const lucid = await makeLucid();
    const validToMs = lucid.slotToUnixTime(INCIDENT_VALID_TO_SLOT);
    const interval = resolveCommitValidityInterval({
      lucid,
      submitSlotSnapshot: snapshot(
        lucid,
        INCIDENT_SUBMIT_SLOT,
        INCIDENT_TIP_SLOT,
      ),
      validToMs,
    });
    expect(slotOf(lucid, interval.validFromMs)).toBe(
      INCIDENT_TIP_SLOT - COMMIT_VALIDITY_BACKDATE_MS / 1_000,
    );
    expect(admittedAtTip(lucid, interval, INCIDENT_TIP_SLOT)).toBe(true);
    expect(widthOf(interval)).toBeLessThanOrEqual(
      SDK.COMMIT_MAX_VALIDITY_RANGE_MS,
    );
    expect(interval.inclusiveUpperBoundMs).toBe(validToMs - 1);

    // Anchored to the submit slot, as before, the bound is slot 32880: past
    // the tip, which is the interval the node refused.
    const submitAnchored = resolveCommitValidityInterval({
      lucid,
      submitSlotSnapshot: snapshot(lucid, INCIDENT_SUBMIT_SLOT),
      validToMs,
    });
    expect(slotOf(lucid, submitAnchored.validFromMs)).toBe(32_880);
    expect(admittedAtTip(lucid, submitAnchored, INCIDENT_TIP_SLOT)).toBe(false);
  });

  it("backdates from the tip however far the submit slot has run ahead", async () => {
    const lucid = await makeLucid();
    const tipSlot = 40_000;
    for (const gapSlots of [1, 30, 80, 150]) {
      const currentSlot = tipSlot + gapSlots;
      const interval = resolveCommitValidityInterval({
        lucid,
        submitSlotSnapshot: snapshot(lucid, currentSlot, tipSlot),
        validToMs: lucid.slotToUnixTime(currentSlot + 240),
      });
      expect(interval.validFromMs).toBe(
        lucid.slotToUnixTime(tipSlot) - COMMIT_VALIDITY_BACKDATE_MS,
      );
      expect(admittedAtTip(lucid, interval, tipSlot)).toBe(true);
    }
  });

  it("keeps the submit-slot backdate when the tip is the submit slot or unknown", async () => {
    const lucid = await makeLucid();
    const slot = 50_000;
    const validToMs = lucid.slotToUnixTime(slot + 300);
    const expected = lucid.slotToUnixTime(slot) - COMMIT_VALIDITY_BACKDATE_MS;
    for (const submitSlotSnapshot of [
      snapshot(lucid, slot, slot),
      snapshot(lucid, slot),
    ]) {
      expect(
        resolveCommitValidityInterval({ lucid, submitSlotSnapshot, validToMs })
          .validFromMs,
      ).toBe(expected);
    }
  });

  it("bounds the range at 480 s before validTo, whatever the tip", async () => {
    const lucid = await makeLucid();
    const tipSlot = 60_000;
    const currentSlot = tipSlot + 150;
    const validToMs = lucid.slotToUnixTime(currentSlot + 300);
    const interval = resolveCommitValidityInterval({
      lucid,
      submitSlotSnapshot: snapshot(lucid, currentSlot, tipSlot),
      validToMs,
    });
    expect(interval.validFromMs).toBe(
      validToMs - SDK.COMMIT_MAX_VALIDITY_RANGE_MS,
    );
    expect(widthOf(interval)).toBe(SDK.COMMIT_MAX_VALIDITY_RANGE_MS);
  });

  describe("residual: validTo 420 s or more past the submit slot", () => {
    // Where the range bound binds, the lower bound is validTo - 480 s, which
    // is 60 s or less before the submit slot. A tip staler than that still
    // leaves the bound past it: the ledger admits the transaction only once
    // the tip reaches it.
    for (const leadMs of [420_000, 450_000]) {
      it(`lead ${String(leadMs / 1_000)} s: the range bound binds past an 80 s stale tip`, async () => {
        const lucid = await makeLucid();
        const tipSlot = INCIDENT_TIP_SLOT;
        const currentSlot = tipSlot + 80;
        const validToMs = lucid.slotToUnixTime(currentSlot) + leadMs;
        const interval = resolveCommitValidityInterval({
          lucid,
          submitSlotSnapshot: snapshot(lucid, currentSlot, tipSlot),
          validToMs,
        });
        expect(interval.validFromMs).toBe(
          validToMs - SDK.COMMIT_MAX_VALIDITY_RANGE_MS,
        );
        expect(widthOf(interval)).toBe(SDK.COMMIT_MAX_VALIDITY_RANGE_MS);
        expect(admittedAtTip(lucid, interval, tipSlot)).toBe(false);
        const boundSlot = slotOf(lucid, interval.validFromMs);
        expect(admittedAtTip(lucid, interval, boundSlot - 1)).toBe(true);
      });
    }

    it("at the submit-slot validity cap, the bound sits 61 s before the submit slot", async () => {
      const lucid = await makeLucid();
      const tipSlot = INCIDENT_TIP_SLOT;
      const currentSlot = tipSlot + 80;
      const capMs = commitValidityEndTimeCapMs(lucid, currentSlot);
      expect(capMs - lucid.slotToUnixTime(currentSlot)).toBe(
        COMMIT_MINIMUM_FUTURE_BUFFER_MS,
      );
      const interval = resolveCommitValidityInterval({
        lucid,
        submitSlotSnapshot: snapshot(lucid, currentSlot, tipSlot),
        // The latest header end the cap admits is capMs - 1: validTo is capMs.
        validToMs: capMs,
      });
      expect(slotOf(lucid, interval.validFromMs)).toBe(currentSlot - 61);
      expect(admittedAtTip(lucid, interval, tipSlot)).toBe(false);
      // A tip at most 62 s behind the submit slot admits it.
      expect(admittedAtTip(lucid, interval, currentSlot - 62)).toBe(true);
    });
  });
});
