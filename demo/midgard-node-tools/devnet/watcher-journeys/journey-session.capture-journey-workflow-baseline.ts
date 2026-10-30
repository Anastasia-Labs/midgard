import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { readJourneyWorkflowRows } from "./correction.js";
import type { JourneyWatcherEvidence } from "./watcher-evidence-reader.js";

export const WATCHER_FAILED_CLOSED_EXIT_CODE = 70;

export const WATCHER_RESTART_LIMIT = 12;

/**
 * Capture immutable journal prefixes before a running observer can act on new
 * staging: from the journey watcher's store path, or through an attached
 * watcher's evidence reader.
 */
export const captureJourneyWorkflowBaseline = async (
  watcherStore: string | Pick<JourneyWatcherEvidence, "readWorkflowRows">,
  category: FraudProofCatalogueCategoryName,
) =>
  new Map(
    typeof watcherStore === "string"
      ? readJourneyWorkflowRows(watcherStore, category)
      : await watcherStore.readWorkflowRows(category),
  );
