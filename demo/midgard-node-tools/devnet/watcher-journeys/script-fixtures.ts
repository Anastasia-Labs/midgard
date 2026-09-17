import { createForcedTransactionJourneyFixture } from "./forced-order.js";
import {
  buildJourneyScriptForcedFault,
  buildJourneyScriptForcedSuccessor,
  JOURNEY_SCRIPT_FORCED_CATEGORIES,
  prepareJourneyScriptForcedTransaction,
} from "./script-cases.js";
import {
  buildJourneyScriptHistoryConsumer,
  buildJourneyScriptHistoryFault,
  buildJourneyScriptHistoryProducer,
} from "./script-history-cases.js";
import { createPreparedJourneyFixture } from "./staging.js";

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

/** History adapters require the retained L1 native-script publication at staging. */
export const JOURNEY_SCRIPT_HISTORY_FIXTURES = [
  createPreparedJourneyFixture("missingNativeScriptTx", async () => ({
    buildFault: async (input) =>
      (
        await buildJourneyScriptHistoryFault({
          ...input,
          category: "missingNativeScriptTx",
        })
      ).fault,
    buildSuccessor: async (input) =>
      (
        await buildJourneyScriptHistoryFault({
          ...input,
          category: "missingNativeScriptTx",
        })
      ).control,
  })),
  createPreparedJourneyFixture(
    "missingNativeScriptUtxo",
    async ({ commitHistoryBlock }) => {
      const predecessor = await commitHistoryBlock(
        "native-script-producer",
        buildJourneyScriptHistoryProducer,
      );
      return {
        predecessor,
        buildFault: (input) =>
          buildJourneyScriptHistoryConsumer({ ...input, honest: false }),
        buildSuccessor: (input) =>
          buildJourneyScriptHistoryConsumer({ ...input, honest: true }),
      };
    },
  ),
];
