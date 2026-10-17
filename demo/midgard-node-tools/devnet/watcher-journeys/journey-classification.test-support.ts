import { verifyJourneyFixture as verify } from "./fixture-verification.js";
import { classifyFullCatalogueTransactionFixture as classifyTransaction } from "./full-catalogue-verification.js";
import { classifyLocalHistoryEventFixture as classifyEvent } from "./history-event-verification.js";
import { fixtureJourneyLedgerAt } from "./journey-fixture-ledger.test-support.js";

/** Fixture state is injected only here, never by a runtime journey verifier. */
export const verifyJourneyFixture = (
  input: Omit<Parameters<typeof verify>[0], "ledgerAt">,
) =>
  verify({
    ...input,
    ledgerAt: fixtureJourneyLedgerAt(
      input.predecessor === undefined ? [] : [input.predecessor],
    ),
  });

export const classifyFullCatalogueTransactionFixture = (
  input: Omit<Parameters<typeof classifyTransaction>[0], "ledgerAt">,
) =>
  classifyTransaction({
    ...input,
    ledgerAt: fixtureJourneyLedgerAt([input.predecessor, ...input.history]),
  });

export const classifyLocalHistoryEventFixture = (
  input: Omit<Parameters<typeof classifyEvent>[0], "ledgerAt">,
) =>
  classifyEvent({
    ...input,
    ledgerAt: fixtureJourneyLedgerAt([
      input.predecessor,
      ...(input.history ?? []),
    ]),
  });
