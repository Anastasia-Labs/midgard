import type { CheckedLedger, LedgerAt } from "@al-ft/midgard-fault-proofs";
import { fixtureLedgerEntriesByRoot } from "@al-ft/midgard-fault-proofs/test-support/fixture-ledger";
import { depositEventsRetainedBlock as buildDeposit } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import { checkedJourneyConstruction } from "./journey-ledger.js";

/** Explicit test-only recorded state; production journey consumers require a real keeper. */
export const checkedFixtureBlock = <T extends VerifiableJourneyBlock>(
  block: T,
) =>
  checkedJourneyConstruction(
    block,
    fixtureLedgerEntriesByRoot(block.header.utxosRoot),
  );

export const depositEventsRetainedBlock = async (
  ...args: Parameters<typeof buildDeposit>
) => {
  const block = await buildDeposit(...args);
  return checkedJourneyConstruction(block, block.postLedgerEntries);
};

export const fixtureJourneyLedgerAt =
  (
    blocks: readonly (VerifiableJourneyBlock & { ledger?: CheckedLedger })[],
  ): LedgerAt =>
  async (headerHash) => {
    const block = blocks.find(
      (candidate) => candidate.headerHash === headerHash,
    );
    return block === undefined
      ? undefined
      : (block.ledger?.entries ??
          fixtureLedgerEntriesByRoot(block.header.utxosRoot));
  };
