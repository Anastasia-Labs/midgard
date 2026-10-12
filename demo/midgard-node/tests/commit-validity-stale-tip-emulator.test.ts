/**
 * The real `CommitBlockHeader` over the lower bound the node now signs, on the
 * real validators. In the emulator the ledger tip is the emulator's slot; the
 * submit slot is placed 79 slots ahead of it, as Ogmios derives it from wall
 * time between blocks (the devnet refusal had tip 32861, submit slot 32940).
 */
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  COMMIT_VALIDITY_BACKDATE_MS,
  resolveCommitValidityInterval,
} from "../src/workers/utils/commit-end-time.js";
import {
  assertAvailabilityRefusal,
  type AvailabilityCommitFixture,
  commitAvailabilityBlock,
  createAvailabilityCommitFixture,
} from "./helpers/availability-challenge-emulator.js";

const EMULATOR_TIMEOUT_MS = 600_000;
const SUBMIT_SLOT_LEAD = 79;

/** A fixture whose emulator has run long enough that a lower bound backdated
 * from its tip is still after the scheduler's shift start. */
const staleTipFixture = async (): Promise<AvailabilityCommitFixture> => {
  const f = await createAvailabilityCommitFixture();
  f.emulator.awaitSlot(150);
  return f;
};

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

/** The node's interval for a commit valid to `validToLead` slots past the
 * derived submit slot, with or without the tip the snapshot reports. */
const intervalAtStaleTip = (
  f: AvailabilityCommitFixture,
  validToLead: number,
  reportLedgerTip: boolean,
) => {
  const tipSlot = f.emulator.slot;
  const currentSlot = tipSlot + SUBMIT_SLOT_LEAD;
  return resolveCommitValidityInterval({
    lucid: f.lucid,
    submitSlotSnapshot: snapshot(
      f.lucid,
      currentSlot,
      reportLedgerTip ? tipSlot : undefined,
    ),
    validToMs: f.lucid.slotToUnixTime(currentSlot + validToLead),
  });
};

/** Every builder `lucid` creates signs `validFrom - byMs` instead: the SDK's
 * own range guard has already passed, so only the validators judge it. */
const widenSignedLowerBound = (lucid: LucidEvolution, byMs: number) => {
  const newTx = lucid.newTx.bind(lucid);
  return vi.spyOn(lucid, "newTx").mockImplementation((): TxBuilder => {
    const builder = newTx();
    const validFrom = builder.validFrom.bind(builder);
    builder.validFrom = (unixTime: number) => validFrom(unixTime - byMs);
    return builder;
  });
};

describe("commit validity lower bound on the real validators", () => {
  it(
    "admits the commit backdated from a stale ledger tip (the devnet interval shape)",
    async () => {
      const f = await staleTipFixture();
      const tipSlot = f.emulator.slot;
      const interval = intervalAtStaleTip(f, 221, true);
      expect(interval.validFromMs).toBe(
        f.lucid.slotToUnixTime(tipSlot) - COMMIT_VALIDITY_BACKDATE_MS,
      );
      const block = await commitAvailabilityBlock(f, { validity: interval });
      expect(block.headerEndTime).toBe(BigInt(interval.inclusiveUpperBoundMs));
      expect(
        await f.lucid.utxosAtWithUnit(
          f.contracts.stateQueue.spendingScriptAddress,
          block.queueUnit,
        ),
      ).toHaveLength(1);
    },
    EMULATOR_TIMEOUT_MS,
  );

  it(
    "refuses the same commit anchored to the wall-clock submit slot: its lower bound is past the tip",
    async () => {
      const f = await staleTipFixture();
      const interval = intervalAtStaleTip(f, 221, false);
      expect(Number(f.lucid.unixTimeToSlot(interval.validFromMs))).toBe(
        f.emulator.slot +
          SUBMIT_SLOT_LEAD -
          COMMIT_VALIDITY_BACKDATE_MS / 1_000,
      );
      await expect(
        commitAvailabilityBlock(f, { validity: interval }),
      ).rejects.toThrow(/Lower bound .* not in slot range/u);
    },
    EMULATOR_TIMEOUT_MS,
  );

  it(
    "residual: validTo 420 s past the submit slot keeps the bound past the stale tip until the tip reaches it",
    async () => {
      const f = await staleTipFixture();
      const tipSlot = f.emulator.slot;
      const interval = intervalAtStaleTip(f, 420, true);
      expect(interval.validToMs - interval.validFromMs).toBe(
        SDK.COMMIT_MAX_VALIDITY_RANGE_MS,
      );
      const boundSlot = Number(f.lucid.unixTimeToSlot(interval.validFromMs));
      expect(boundSlot).toBe(tipSlot + SUBMIT_SLOT_LEAD - 60);
      await expect(
        commitAvailabilityBlock(f, { validity: interval }),
      ).rejects.toThrow(/Lower bound .* not in slot range/u);
      f.emulator.awaitSlot(boundSlot - f.emulator.slot);
      const block = await commitAvailabilityBlock(f, { validity: interval });
      expect(block.headerEndTime).toBe(BigInt(interval.inclusiveUpperBoundMs));
    },
    EMULATOR_TIMEOUT_MS,
  );

  it(
    "admits a commit at exactly the 480 s range, and the state-queue commit withdrawal refuses one 1 s wider",
    async () => {
      const f = await staleTipFixture();
      const slot = f.emulator.slot;
      const validity = resolveCommitValidityInterval({
        lucid: f.lucid,
        submitSlotSnapshot: snapshot(f.lucid, slot, slot),
        validToMs: f.lucid.slotToUnixTime(slot + 420),
      });
      expect(validity.validToMs - validity.validFromMs).toBe(
        SDK.COMMIT_MAX_VALIDITY_RANGE_MS,
      );

      const widened = widenSignedLowerBound(f.lucid, 1_000);
      try {
        await assertAvailabilityRefusal(
          commitAvailabilityBlock(f, { validity }),
          {
            purpose: "withdraw",
            script: f.contracts.stateQueue.yields.commit.withdrawalScriptHash,
          },
        );
      } finally {
        widened.mockRestore();
      }

      const block = await commitAvailabilityBlock(f, { validity });
      expect(block.headerEndTime).toBe(BigInt(validity.inclusiveUpperBoundMs));
    },
    EMULATOR_TIMEOUT_MS,
  );
});
