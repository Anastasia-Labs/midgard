/** The completed-workflow check's read of a removed block from its
 * authenticated L1 view. */
import {
  admitFraudProofRawL1Snapshot,
  deriveFraudProofRawL1CompletedTerminal,
  fraudProofRawL1HistoryReader,
  fraudProofRawL1SnapshotRequestForFamily,
} from "../../src/workflow/index.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";
import type { RecordedRemoval } from "./validation-trace-dispute-removal-l1.js";

/** The terminal the completed-workflow check reads from the block's
 * authenticated L1 view, as `proverCredential` sees it; by default as we
 * do. */
export const completedTerminal = async (
  {
    scene,
    removal,
  }: { readonly scene: Scene; readonly removal: RecordedRemoval },
  proverCredential?: string,
) => {
  const { authority, releaseFinality, releaseEconomics } = removal.raw;
  const headerHash = scene.failing.evidence.headerHash;
  const base = removal.raw.definition(headerHash);
  const definition = {
    ...base,
    proverCredential: proverCredential ?? base.proverCredential,
  };
  const request = fraudProofRawL1SnapshotRequestForFamily({
    definition,
    releaseFinality,
  });
  return await deriveFraudProofRawL1CompletedTerminal({
    snapshot: admitFraudProofRawL1Snapshot({
      value: await authority.capture(request),
      request,
      releaseFinality,
      observationDepth: "inclusion",
    }),
    definition,
    releaseEconomics,
    history: fraudProofRawL1HistoryReader({
      authority,
      releaseFinality,
      headerHash,
    }),
  });
};
