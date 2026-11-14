import type { FraudProofWorkflowJournalEntry } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";

/**
 * A watcher store whose workflow rows each test sets, read through the same
 * port an attached watcher's evidence reader serves; counts its reads.
 */
export const workflowLoad = () => {
  let answer: () => readonly FraudProofWorkflowJournalEntry[] = () => [];
  let reads = 0;
  return {
    store: {
      readWorkflowRows: async (
        _category: SDK.FraudProofCatalogueCategoryName,
        headerHash?: string,
      ) => {
        reads += 1;
        return new Map([[headerHash!, answer()]]);
      },
    },
    serve: (records: readonly FraudProofWorkflowJournalEntry[]) => {
      answer = () => records;
    },
    refuse: (error: Error) => {
      answer = () => {
        throw error;
      };
    },
    reads: () => reads,
  };
};
