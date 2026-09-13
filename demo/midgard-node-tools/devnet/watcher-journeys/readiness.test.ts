import { mkdir, mkdtemp, readdir, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { JOURNEY_FIXTURES } from "./catalogue.js";
import { readJourneyReadiness } from "./readiness.js";
import {
  type JourneyEvidenceDeployment,
  readJourneyCanonicalTransactions,
} from "./readiness-evidence.js";

it("reports fixture coverage without creating deployment or runtime state", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-readiness-"));
  try {
    const report = await readJourneyReadiness(directory);
    expect(report.counts).toEqual({
      families: 54,
      fixtureReady: 54,
      locallyVerified: JOURNEY_FIXTURES.length,
      liveComplete: 0,
    });
    expect(
      report.families.every((family) =>
        family.pending.includes("deployment_unavailable"),
      ),
    ).toBe(true);
    expect(await readdir(directory)).toEqual([]);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

const retained = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(retained === undefined)(
  "re-admits the actual retained native capture and its rollback history",
  async () => {
    const chain = await readJourneyCanonicalTransactions(
      join(retained!, "work/journeys/transition-trace/native-chain.ndjson"),
    );
    expect(chain.transactions.size).toBeGreaterThan(0);
    expect(chain.blocks.get(chain.tip.hash)?.blockNo).toBe(chain.tip.blockNo);
  },
);

it.skipIf(retained === undefined)(
  "refuses passing labels, foreign deployments and absent native evidence",
  async () => {
    const directory = await mkdtemp(join(tmpdir(), "journey-evidence-"));
    try {
      const deployment = await readJourneyArtifact<JourneyEvidenceDeployment>(
        join(retained!, "deploymentInfo/live-deployment.json"),
      );
      await mkdir(join(directory, "deploymentInfo"));
      await writeJourneyArtifact(
        join(directory, "deploymentInfo/live-deployment.json"),
        deployment,
      );
      const familyDirectory = join(directory, "work/journeys/zeroInput");
      await mkdir(familyDirectory, { recursive: true });
      for (const claim of [
        { status: "passed" },
        {
          status: "passed",
          category: "zeroInput",
          deploymentFingerprint: "another-deployment",
        },
        {
          status: "passed",
          category: "zeroInput",
          deploymentFingerprint: deployment.manifest.manifestId,
        },
      ]) {
        await writeJourneyArtifact(join(familyDirectory, "result.json"), claim);
        const report = await readJourneyReadiness(directory);
        expect(report.deploymentFingerprint).toBe(
          deployment.manifest.manifestId,
        );
        expect(report.counts.liveComplete).toBe(0);
        expect(
          report.families.find(({ category }) => category === "zeroInput")
            ?.pending,
        ).toContain("invalid_live_evidence");
      }
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  },
);
