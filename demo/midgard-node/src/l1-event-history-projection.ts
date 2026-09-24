import {
  type BoundHistoryCapture,
  projectEventHistoryLedgerBlock,
} from "./l1-event-history-ledger-projection.js";

export type { BoundHistoryCapture } from "./l1-event-history-ledger-projection.js";

/** Stage a whole block from an initialized, source-bound paired capture.
 * Persistence and publication remain the fenced source owner's responsibility.
 */
export const projectEventHistoryBlock = async ({
  previous,
  ...input
}: Omit<Parameters<typeof projectEventHistoryLedgerBlock>[0], "ledger"> & {
  readonly previous: BoundHistoryCapture;
}): ReturnType<typeof projectEventHistoryLedgerBlock> => {
  if (previous.bindingDigest !== input.binding.digest)
    throw new Error("History projection does not extend its bound capture");
  return projectEventHistoryLedgerBlock({
    ...input,
    ledger: previous.history.ledger,
  });
};
