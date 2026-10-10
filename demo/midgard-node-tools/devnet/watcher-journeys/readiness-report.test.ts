import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { readJourneyReadiness } from "./readiness.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

// Every catalogue family has a journey except the interactive Plutus dispute.
const catalogueFamilies = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
  (category) => category !== "validationTraceDispute",
);

// Read-only entrypoint through the package's existing source-aware runtime.
it.skipIf(runDirectory === undefined)(
  "reports every catalogue journey family without launching services",
  async () => {
    const report = await readJourneyReadiness(runDirectory!);
    expect(report.counts.families).toBe(catalogueFamilies.length);
    expect(report.families.map(({ category }) => category)).toEqual(
      catalogueFamilies,
    );
    console.info(JSON.stringify(report, null, 2));
  },
);
