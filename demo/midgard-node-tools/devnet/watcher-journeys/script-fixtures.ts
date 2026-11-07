import { createForcedTransactionJourneyFixture } from "./forced-order.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
  prepareJourneyScriptForcedTransaction,
} from "./script-cases.js";
import { buildJourneyValidationTraceFault } from "./script-validation-trace-case.js";
import { createTransactionJourneyFixture } from "./staging.js";

/** Staging adapters; scheduling additionally requires the full catalogue gate. */
export const JOURNEY_SCRIPT_FIXTURES = JOURNEY_SCRIPT_FORCED_CATEGORIES.map(
  (category) =>
    createForcedTransactionJourneyFixture(
      category,
      (input) => prepareJourneyScriptForcedTransaction({ ...input, category }),
      {
        buildFault: (input) =>
          buildJourneyScriptForcedFault({ ...input, category }),
        buildSuccessor: (input) =>
          buildJourneyScriptForcedSuccessor({ ...input, category }),
      },
    ),
);

/**
 * Script staging adapters not yet registered for a live journey: the
 * operator commits a transaction whose Plutus policy fails.
 */
export const JOURNEY_SCRIPT_FIXTURE_CANDIDATES = [
  createTransactionJourneyFixture("validationTraceDispute", (input) =>
    buildJourneyValidationTraceFault(input),
  ),
];
