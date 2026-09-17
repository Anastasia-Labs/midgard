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
  // Verified against real published L1 events on an isolated emulator chain
  // by history-event-cases.test.ts: exact installed selection and healthy control.
  createWithdrawnInputJourneyFixture("withdrawnInput"),
  createWithdrawnInputJourneyFixture("withdrawnReferenceInput"),
  fabricatedDepositJourneyFixture,
  createWithdrawalJourneyFixture("fabricatedWithdrawal"),
  createWithdrawalJourneyFixture("withdrawalMistag"),
];

/** Existing fixtures whose faults are selected as an earlier family by the full catalogue. */
export const JOURNEY_HISTORY_EVENT_FIXTURE_CANDIDATES = [
  crossBlockDuplicateEventJourneyFixture,
  createWithdrawalJourneyFixture("doubleWithdraw"),
];
