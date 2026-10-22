import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { readJourneyWorkflowRows } from "./correction.js";

export const WATCHER_FAILED_CLOSED_EXIT_CODE = 70;

export const WATCHER_RESTART_LIMIT = 12;

/** Capture immutable journal prefixes before a running observer can act on new staging. */
export const captureJourneyWorkflowBaseline = async (
  watcherStorePath: string,
  category: FraudProofCatalogueCategoryName,
) => new Map(readJourneyWorkflowRows(watcherStorePath, category));
