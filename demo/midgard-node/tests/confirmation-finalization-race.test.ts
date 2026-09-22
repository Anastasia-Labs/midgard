import { describe, expect, it } from "vitest";

import { resolveAuthoritativeLocalFinalizationPreflight } from "../src/fibers/block-commitment.js";
import {
  type ActivePendingFinalizationIdentity,
  confirmationPendingSnapshotChanged,
  staleRecoveryMustPreserveNewActiveJournal,
} from "../src/fibers/block-confirmation.js";
import type { SerializedStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";

const identity = (headerHash: string): ActivePendingFinalizationIdentity => ({
  headerHash,
  submittedTxHash: "bb".repeat(32),
  status: "submitted_unconfirmed",
});

const tailBlock = {
  utxo: "aa",
  datum: "bb",
} as SerializedStateQueueUTxO;

describe("confirmation/local-finalization race guards", () => {
  it("discards a delayed confirmation result when a new active journal appears", async () => {
    let current: ActivePendingFinalizationIdentity | null = null;
    const captured = current;
    let releaseWorker!: () => void;
    const worker = new Promise<void>((resolve) => {
      releaseWorker = resolve;
    });

    current = identity("aa".repeat(32));
    releaseWorker();
    await worker;

    expect(confirmationPendingSnapshotChanged({ captured, current })).toBe(
      true,
    );
  });

  it("routes a raced confirmed journal to recovery before candidate processing", () => {
    const result = resolveAuthoritativeLocalFinalizationPreflight({
      localFinalizationPending: false,
      availableLocalFinalizationBlock: "",
      activeJournalHeaderHash: "aa".repeat(32),
      activeJournalSubmittedTxHash: "bb".repeat(32),
      activeJournalStatus: "submitted_unconfirmed",
      tailHeaderHash: "aa".repeat(32),
      tailBlock,
    });

    expect(result).toEqual({
      localFinalizationPending: true,
      availableLocalFinalizationBlock: tailBlock,
      recoveredRacedJournal: true,
    });
  });

  it("defers an active submitted journal before it reaches the live tail", () => {
    expect(
      resolveAuthoritativeLocalFinalizationPreflight({
        localFinalizationPending: false,
        availableLocalFinalizationBlock: "",
        activeJournalHeaderHash: "aa".repeat(32),
        activeJournalSubmittedTxHash: "bb".repeat(32),
        activeJournalStatus: "submitted_unconfirmed",
        tailHeaderHash: "cc".repeat(32),
        tailBlock,
      }),
    ).toEqual({
      localFinalizationPending: true,
      availableLocalFinalizationBlock: "",
      recoveredRacedJournal: false,
    });
  });

  it("does not defer a prepared journal that has not been submitted", () => {
    expect(
      resolveAuthoritativeLocalFinalizationPreflight({
        localFinalizationPending: false,
        availableLocalFinalizationBlock: "",
        activeJournalHeaderHash: "aa".repeat(32),
        activeJournalSubmittedTxHash: null,
        activeJournalStatus: "pending_submission",
        tailHeaderHash: "cc".repeat(32),
        tailBlock,
      }),
    ).toEqual({
      localFinalizationPending: false,
      availableLocalFinalizationBlock: "",
      recoveredRacedJournal: false,
    });
  });

  it("preserves a journal inserted after stale recovery abandons the captured row", () => {
    expect(
      staleRecoveryMustPreserveNewActiveJournal({
        captured: identity("aa".repeat(32)),
        current: identity("cc".repeat(32)),
      }),
    ).toBe(true);
    expect(
      staleRecoveryMustPreserveNewActiveJournal({
        captured: identity("aa".repeat(32)),
        current: null,
      }),
    ).toBe(false);
  });
});
