import { existsSync } from "node:fs";
import { isAbsolute, join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";

import { readJourneyArtifact } from "./artifacts.js";
import {
  JOURNEY_CATEGORIES,
  JOURNEY_FIXTURE_CANDIDATES,
  JOURNEY_FIXTURE_OWNERS,
  JOURNEY_FIXTURES,
} from "./catalogue.js";
import type { JourneyCategory } from "./fixture.js";
import {
  type JourneyEvidenceDeployment,
  verifyJourneyResultEvidence,
} from "./readiness-evidence.js";

export const JOURNEY_READINESS_REASONS = {
  fixture_missing: "No staging adapter implements this family.",
  local_gate_pending:
    "Full installed classifier, exact proof material and accepted-control gate has not passed.",
  native_history_pending:
    "Native-script publication and canonical history must be admitted before live staging.",
  event_gate_pending:
    "Stage actual L1 events and verify full classification plus exact family material before live scheduling.",
  maturity_and_settlement_pending:
    "Real source history must mature for seven days and be settled before the duplicate-event journey.",
  deployment_unavailable:
    "No verified finalized deployment is available for binding live evidence.",
  live_journey_pending:
    "No passing result has been independently verified against this deployment's native chain evidence.",
  invalid_live_evidence:
    "Saved result failed independent manifest, journal, decision or native transaction verification.",
} as const;
type PendingReason = keyof typeof JOURNEY_READINESS_REASONS;

/** Read-only report: never opens a journal writer, starts services or loads keys. */
export const readJourneyReadiness = async (runDirectory: string) => {
  if (!isAbsolute(runDirectory))
    throw new Error("Readiness requires an absolute run directory");
  let deployment: JourneyEvidenceDeployment | undefined;
  let deploymentIssue: string | undefined;
  try {
    deployment = await readJourneyArtifact<JourneyEvidenceDeployment>(
      join(runDirectory, "deploymentInfo/live-deployment.json"),
    );
    verifyFinalizedDeploymentManifest(deployment.manifest);
  } catch (cause) {
    deployment = undefined;
    deploymentIssue = cause instanceof Error ? cause.message : String(cause);
  }
  const candidates = new Set(
    JOURNEY_FIXTURE_CANDIDATES.map(({ category }) => category),
  );
  const verified = new Set(JOURNEY_FIXTURES.map(({ category }) => category));
  const families = [];
  for (const category of JOURNEY_CATEGORIES) {
    const directory = join(
      runDirectory,
      "work/journeys",
      category === "transitionTrace" ? "transition-trace" : category,
    );
    const pending: PendingReason[] = [];
    const fixtureReady = candidates.has(category);
    const locallyVerified = verified.has(category);
    if (!fixtureReady) pending.push("fixture_missing");
    if (!locallyVerified)
      pending.push(
        JOURNEY_FIXTURE_OWNERS[category] === "history"
          ? "event_gate_pending"
          : "local_gate_pending",
      );
    if (category === "crossBlockDuplicateEvent")
      pending.push("maturity_and_settlement_pending");
    let liveComplete = false;
    let liveEvidenceIssue: string | undefined;
    let evidence:
      | Awaited<ReturnType<typeof verifyJourneyResultEvidence>>
      | undefined;
    if (deployment === undefined) pending.push("deployment_unavailable");
    else if (existsSync(join(directory, "result.json"))) {
      try {
        evidence = await verifyJourneyResultEvidence(
          runDirectory,
          directory,
          category,
          deployment,
        );
        liveComplete = true;
      } catch (cause) {
        liveEvidenceIssue =
          cause instanceof Error ? cause.message : String(cause);
        pending.push("invalid_live_evidence");
      }
    }
    if (!liveComplete) {
      pending.push("live_journey_pending");
      if (
        category === "missingNativeScriptTx" ||
        category === "missingNativeScriptUtxo"
      )
        pending.push("native_history_pending");
    }
    // A valid completed journey itself supplies the real maturity/settlement
    // evidence. A checkpoint or elapsed wall clock alone never clears that gate.
    const remaining = liveComplete
      ? pending.filter((reason) => reason !== "maturity_and_settlement_pending")
      : pending;
    let reportedMaturity: string | undefined;
    let historyEvidenceIssue: string | undefined;
    const historyPath = join(directory, "duplicate-event-history.json");
    if (
      category === "crossBlockDuplicateEvent" &&
      deployment !== undefined &&
      existsSync(historyPath)
    ) {
      try {
        const history = await readJourneyArtifact<{
          deploymentFingerprint: string;
          readyAt: bigint;
          source: { header: SDK.Header };
        }>(historyPath);
        if (
          history.deploymentFingerprint === deployment.manifest.manifestId &&
          history.readyAt ===
            history.source.header.endTime + SDK.MATURITY_DURATION_MS
        )
          reportedMaturity = new Date(Number(history.readyAt)).toISOString();
        else
          throw new Error(
            "Source maturity checkpoint has an invalid deployment or timing binding",
          );
      } catch (cause) {
        historyEvidenceIssue =
          cause instanceof Error ? cause.message : String(cause);
      }
    }
    families.push({
      category,
      owner: JOURNEY_FIXTURE_OWNERS[category],
      fixtureReady,
      locallyVerified,
      liveComplete,
      pending: remaining,
      ...(liveEvidenceIssue === undefined ? {} : { liveEvidenceIssue }),
      ...(evidence === undefined ? {} : { evidence }),
      ...(reportedMaturity === undefined ? {} : { reportedMaturity }),
      ...(historyEvidenceIssue === undefined ? {} : { historyEvidenceIssue }),
    });
  }
  return {
    schemaVersion: "midgard-watcher-journey-readiness-v1",
    runDirectory,
    deploymentFingerprint: deployment?.manifest.manifestId ?? null,
    ...(deploymentIssue === undefined ? {} : { deploymentIssue }),
    counts: {
      families: families.length,
      fixtureReady: families.filter((family) => family.fixtureReady).length,
      locallyVerified: families.filter((family) => family.locallyVerified)
        .length,
      liveComplete: families.filter((family) => family.liveComplete).length,
    },
    reasons: JOURNEY_READINESS_REASONS,
    families,
  };
};

export type JourneyReadinessFamily = Awaited<
  ReturnType<typeof readJourneyReadiness>
>["families"][number] & { category: JourneyCategory };
