import { appendFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";

import {
  AcceptanceJourney,
  requireInjectedDrills,
} from "./acceptance-journey.js";
import {
  acceptanceJourneyOwner,
  assertAcceptanceActive,
} from "./acceptance-process.js";
import {
  drillCatalogue,
  type DrillRecord,
  productionChaosDeps,
  runDrills,
  selectDrills,
} from "./chaos.js";
import { owedRestorePath } from "./chaos-restore.js";
import type { DeployContext } from "./deploy.js";
import type { TestAsset } from "./funding.js";
import { runScenario } from "./journey.js";
import type { HubOracleOneShot } from "./node-env.js";
import { serviceSpecs, supervisorPaths } from "./services.js";

export const ACCEPTANCE_FIRST_DRILLS = [
  "restart-cardano-node",
  "stop-kupo",
  "stop-ogmios",
  "kill-public-retained-da",
] as const;
export const ACCEPTANCE_LAST_DRILLS = [
  "kill-node",
  "kill-da-member-0",
  "kill-da-member-1",
  "kill-watcher",
  "pause-postgres",
] as const;

export const runAcceptanceSequence = async (
  journey: AcceptanceJourney,
  batch: (names: readonly string[]) => Promise<readonly DrillRecord[]>,
  scenario: () => Promise<void> = () =>
    runScenario(journey, { idleMs: 300_000 }),
) => {
  const records = [...(await batch(ACCEPTANCE_FIRST_DRILLS))];
  await scenario();
  records.push(...journey.drillRecords);
  records.push(...(await batch(ACCEPTANCE_LAST_DRILLS)));
  return records;
};

/** One opt-in finite pass. It never deploys, rebuilds, repairs, or retries a drill. */
export const runFinalAcceptance = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  assets: readonly TestAsset[],
  options: { readonly deadlineMs: number; readonly signal?: AbortSignal },
) => {
  if (
    !Number.isSafeInteger(options.deadlineMs) ||
    options.deadlineMs <= 0 ||
    options.deadlineMs > 2_147_483_647
  )
    throw new Error("acceptance needs a finite positive overall deadline");
  const abort = new AbortController();
  const cancel = () => abort.abort();
  const owner = acceptanceJourneyOwner(context, oneShot, abort.signal);
  const catalogue = drillCatalogue(
    serviceSpecs(context, oneShot).map((service) => service.name),
  );
  const chaos = {
    runDir: context.layout.runDir,
    pidDir: supervisorPaths(context).pidDir,
    drillsLog: context.layout.drillsLog,
    deps: productionChaosDeps(context, oneShot),
    gapMs: 120_000,
    recoveryMs: 900_000,
    stableMs: 300_000,
    triggerTimeoutMs: 1_200_000,
    pollMs: 10,
  };
  const observations = join(
    context.layout.journeyDir,
    "acceptance-observations.ndjson",
  );
  const journey = new AcceptanceJourney(
    context,
    oneShot,
    assets,
    {
      drills: catalogue,
      chaos,
      abort,
      drain: owner.drain,
      observe: (receipt) => {
        mkdirSync(dirname(observations), { recursive: true, mode: 0o700 });
        appendFileSync(observations, `${JSON.stringify(receipt)}\n`, {
          mode: 0o600,
        });
      },
    },
    owner.options,
  );
  options.signal?.addEventListener("abort", cancel, { once: true });
  if (options.signal?.aborted) cancel();
  const timer = setTimeout(cancel, options.deadlineMs);
  const batch = async (names: readonly string[]) => {
    assertAcceptanceActive(abort.signal);
    const drills = selectDrills(catalogue, names);
    const summary = await runDrills({
      ...chaos,
      drills,
      rounds: 1,
      signal: abort.signal,
    });
    requireInjectedDrills(drills, summary.records);
    return summary.records;
  };
  try {
    // No completed phase from an old pass can be promoted into fresh drill coverage.
    if (journey.journal.withPrefix("").length > 0)
      throw new Error(
        "final acceptance requires an unstarted full journey; preserve this existing journal",
      );
    if (existsSync(owedRestorePath(chaos.drillsLog)))
      throw new Error("final acceptance refuses an existing owed restore");
    if (existsSync(chaos.drillsLog) || existsSync(observations))
      throw new Error(
        "final acceptance refuses prior drill/observation evidence; preserve this attempt",
      );
    // Validate the full catalogue before the first injection.
    const expected = selectDrills(catalogue, [
      ...ACCEPTANCE_FIRST_DRILLS,
      "kill-node-on-admission",
      "kill-node-on-block-submitted",
      "kill-node-on-settlement",
      ...ACCEPTANCE_LAST_DRILLS,
    ]);
    const records = await runAcceptanceSequence(journey, batch);
    requireInjectedDrills(expected, records);
    if (journey.journal.get("phase:holdings") !== "done")
      throw new Error("final acceptance full journey did not complete");
    if (existsSync(owedRestorePath(chaos.drillsLog)))
      throw new Error("final acceptance left an owed restore");
    assertAcceptanceActive(abort.signal);
    return { records, observations, journeyComplete: true as const };
  } finally {
    abort.abort();
    await owner.drain();
    clearTimeout(timer);
    options.signal?.removeEventListener("abort", cancel);
  }
};
