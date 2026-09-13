import {
  buildJourneyHistoryTransaction,
  JOURNEY_HISTORY_TRANSACTION_CATEGORIES,
} from "./history-cases.js";
import {
  createWithdrawalJourneyFixture,
  createWithdrawnInputJourneyFixture,
  fabricatedDepositJourneyFixture,
} from "./history-event-staging.js";
import { crossBlockDuplicateEventJourneyFixture } from "./history-settlement.js";
import { createTransactionJourneyFixture } from "./staging.js";

/** Locally verified candidates; the catalogue admits them after full-scope checks. */
export const JOURNEY_HISTORY_FIXTURES = [
  ...JOURNEY_HISTORY_TRANSACTION_CATEGORIES.map((category) =>
    createTransactionJourneyFixture(category, (input) =>
      buildJourneyHistoryTransaction({ ...input, category }),
    ),
  ),
  // Verified against real published L1 withdrawal events on an isolated
  // emulator staging chain by history-event-cases.test.ts: installed selector,
  // exact proof preparation and healthy control.
  createWithdrawnInputJourneyFixture("withdrawnInput"),
  createWithdrawnInputJourneyFixture("withdrawnReferenceInput"),
];

/** Staging implementations awaiting real event material and full-scope gates. */
export const JOURNEY_HISTORY_EVENT_FIXTURE_CANDIDATES = [
  crossBlockDuplicateEventJourneyFixture,
  fabricatedDepositJourneyFixture,
  createWithdrawalJourneyFixture("fabricatedWithdrawal"),
  createWithdrawalJourneyFixture("withdrawalMistag"),
  createWithdrawalJourneyFixture("doubleWithdraw"),
];
