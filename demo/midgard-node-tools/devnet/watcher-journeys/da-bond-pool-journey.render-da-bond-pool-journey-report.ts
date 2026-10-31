import {
  DA_BOND_POOL_JOURNEY_STEP_NAMES,
  type DaBondPoolJourneyAssertion,
  type DaBondPoolJourneyOptions,
  type DaBondPoolJourneyPort,
} from "./da-bond-pool-journey.da-bond-pool-journey-port.js";
import {
  DaBondPoolJourneyFailure,
  type DaBondPoolJourneyObservation,
  type DaBondPoolJourneyRecord,
} from "./da-bond-pool-journey.stage-context.js";
import { tryRunDaBondPoolJourney } from "./da-bond-pool-journey.try-run-da-bond-pool-journey.js";

/** Runs the journey; throws `DaBondPoolJourneyFailure` (carrying the ledger) on failure. */
export const runDaBondPoolJourney = async (
  port: DaBondPoolJourneyPort,
  options: DaBondPoolJourneyOptions = {},
): Promise<DaBondPoolJourneyRecord> => {
  const { record, error } = await tryRunDaBondPoolJourney(port, options);
  if (record.status === "failed")
    throw new DaBondPoolJourneyFailure(record, error);
  return record;
};

export type DaBondPoolJourneyReportMeta = Readonly<{
  adapter: "emulator" | "live-devnet";
  runDir?: string;
  deploymentManifestId?: string;
  networkMagic?: number;
  gitHead?: string;
}>;

const cell = (value: string): string =>
  value.replaceAll("\\", "\\\\").replaceAll("|", "\\|").replaceAll("\n", " ");

const json = (value: unknown): string =>
  JSON.stringify(value, (_key, item: unknown) =>
    typeof item === "bigint" ? item.toString() : item,
  );

const observationText = (observation: DaBondPoolJourneyObservation): string => {
  switch (observation.kind) {
    case "block":
      return `${observation.value} (${observation.headerHash})`;
    case "value":
      return String(observation.value);
    case "process":
      return `exit ${String(observation.value.exitCode)}: ${observation.value.argv.join(" ")} -> ${observation.value.stdout.trim().replace(/\s+/gu, " ")}`;
    case "pool":
    case "alerts":
    case "timeout":
      return json(observation.value);
  }
};

const assertionResult = (ok: DaBondPoolJourneyAssertion["ok"]): string =>
  ok === "not-observable" ? "not-observable" : ok ? "pass" : "FAIL";

/**
 * The journey report as markdown, one section per spec step (1-6), whatever
 * order the steps ran in. The header names the adapter, so an emulator dry
 * run can never pass for devnet evidence.
 */
export const renderDaBondPoolJourneyReport = (
  record: DaBondPoolJourneyRecord,
  meta: DaBondPoolJourneyReportMeta,
): string => {
  const lines: string[] = [];
  const evidence =
    record.resumedAfterStep !== undefined
      ? `RESUMED after step ${record.resumedAfterStep} (smoke, not journey evidence): steps ${record.chronology.join(" and ")} ran on the chain an earlier run left (adapter \`${meta.adapter}\`).`
      : meta.adapter === "live-devnet"
        ? "Observed on the process devnet (adapter `live-devnet`)."
        : "EMULATOR DRY RUN (adapter `emulator`): not devnet evidence.";
  const result =
    record.status === "failed"
      ? `FAILED at step ${record.failure?.step ?? "?"}: ${record.failure?.message ?? "unknown error"}`
      : record.status;
  lines.push(
    `# Pooled DA bond journey (${meta.adapter})`,
    "",
    `> ${evidence}`,
    "",
    "| Field | Value |",
    "| --- | --- |",
    `| Adapter | ${meta.adapter} |`,
    `| Result | ${cell(result)} |`,
    `| Run dir | ${cell(meta.runDir ?? "not recorded")} |`,
    `| Deployment manifest | ${cell(meta.deploymentManifestId ?? "not recorded")} |`,
    `| Network magic | ${meta.networkMagic ?? "not recorded"} |`,
    `| Git HEAD | ${cell(meta.gitHead ?? "not recorded")} |`,
    `| Started | ${record.startedAt} |`,
    `| Finished | ${record.finishedAt ?? "not finished"} |`,
    `| Run order | ${record.chronology.map((step) => `step ${step}`).join(" -> ")} |`,
    "",
  );
  if (record.params !== undefined) {
    lines.push("## Parameters", "", "| Parameter | Value |", "| --- | --- |");
    for (const [key, value] of Object.entries(record.params))
      lines.push(`| ${key} | ${String(value)} |`);
    lines.push("");
  }
  for (const step of [1, 2, 3, 4, 5, 6] as const) {
    lines.push(`## Step ${step}: ${DA_BOND_POOL_JOURNEY_STEP_NAMES[step]}`, "");
    const stage = record.stages.find((candidate) => candidate.step === step);
    if (stage === undefined) {
      lines.push(
        record.chronology.includes(step) ? "Not reached." : "Not run.",
        "",
      );
      continue;
    }
    lines.push(
      `Status: **${stage.status}** (ran ${stage.order} of ${record.chronology.length}), ${stage.startedAt} to ${stage.finishedAt ?? "unfinished"}.`,
      "",
    );
    if (stage.error !== undefined) lines.push(`Error: ${stage.error}`, "");
    lines.push("### Transactions", "");
    const txs = Object.entries(stage.txIds);
    if (txs.length === 0) lines.push("None.", "");
    else {
      lines.push("| Label | Tx id |", "| --- | --- |");
      for (const [label, txId] of txs)
        lines.push(`| ${cell(label)} | \`${txId}\` |`);
      lines.push("");
    }
    lines.push("### Assertions", "");
    if (stage.assertions.length === 0) lines.push("None.", "");
    else {
      lines.push("| Result | Assertion | Detail |", "| --- | --- | --- |");
      for (const assertion of stage.assertions)
        lines.push(
          `| ${assertionResult(assertion.ok)} | ${cell(assertion.name)} | ${cell(assertion.detail)} |`,
        );
      lines.push("");
    }
    lines.push("### Observations", "");
    if (stage.observations.length === 0) lines.push("None.", "");
    else {
      for (const observation of stage.observations)
        lines.push(
          `- ${observation.label}: \`${observationText(observation).replaceAll("`", "'")}\``,
        );
      lines.push("");
    }
  }
  return lines.join("\n");
};
