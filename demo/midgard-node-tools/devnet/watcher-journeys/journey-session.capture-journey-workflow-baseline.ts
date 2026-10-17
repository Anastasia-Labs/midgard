import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { readJourneyWorkflowEntries } from "./correction.js";

export const WATCHER_FAILED_CLOSED_EXIT_CODE = 70;

export const WATCHER_RESTART_LIMIT = 12;

/** Capture immutable journal prefixes before a running observer can act on new staging. */
export const captureJourneyWorkflowBaseline = async (
  workflowJournalDirectory: string,
  category: FraudProofCatalogueCategoryName,
) => {
  const directory = join(workflowJournalDirectory, "fault-proofs", category);
  if (!existsSync(directory))
    return new Map<
      string,
      Awaited<ReturnType<typeof readJourneyWorkflowEntries>>
    >();
  const headers = (await readdir(directory, { withFileTypes: true })).filter(
    (entry) => entry.isDirectory() && /^[0-9a-f]{56}$/u.test(entry.name),
  );
  return new Map(
    await Promise.all(
      headers.map(
        async ({ name }) =>
          [
            name,
            await readJourneyWorkflowEntries({
              workflowJournalDirectory,
              category,
              headerHash: name,
            }),
          ] as const,
      ),
    ),
  );
};
