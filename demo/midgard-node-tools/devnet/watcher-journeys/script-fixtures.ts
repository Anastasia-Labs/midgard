import { createForcedTransactionJourneyFixture } from "./forced-order.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
  prepareJourneyScriptForcedTransaction,
} from "./script-cases.js";

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
