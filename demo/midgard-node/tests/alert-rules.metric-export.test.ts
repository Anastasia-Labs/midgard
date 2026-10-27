/**
 * Every rule in `demo/midgard-node/alert-rules.yml` selects a metric family
 * the node really exports under `listen --with-monitoring`: the production
 * alert-signal sampler and the forced-admission counter, scraped through the
 * real Prometheus exporter. A rule over a family the node never exports
 * would never fire.
 */
import "./utils.js";

import { readFile } from "node:fs/promises";

import { Effect, Metric, Ref } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { sampleAlertSignals } from "../src/fibers/alert-signals.js";
import {
  forcedAdmissionDivergenceCounter,
  SCREEN_FAILED,
} from "../src/forced-orders/admission-divergence.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  applyCommitPlanningNotice,
  makeCommitPlanningNotice,
} from "../src/workers/commit-planning-notice.js";
import { alertRuleSelectors } from "./helpers/alert-rule-selectors.js";
import { commitPlanningSelection } from "./helpers/commit-planning-selection.js";
import {
  prometheusFamilies,
  startPrometheusExporter,
} from "./prometheus-scrape.fixture.js";
import { runningFollower } from "./readiness-l1-follower.fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// Only the manifest id the sampler reads settlement jobs under.
const identity = {
  kind: "manifest",
  manifestId: "a7".repeat(32),
} as unknown as ContractDeploymentIdentity;

let exporter: Awaited<ReturnType<typeof startPrometheusExporter>>;
beforeAll(async () => {
  exporter = await startPrometheusExporter();
});
afterAll(async () => {
  await exporter.stop();
});

/** The families a monitored node exports once each signal has a value. */
const exportedFamilies = () =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const globals = yield* Globals;
        yield* applyCommitPlanningNotice(
          globals,
          makeCommitPlanningNotice(commitPlanningSelection("normal")),
        );
        yield* Ref.set(globals.L1_FOLLOWER, runningFollower());
        yield* sampleAlertSignals(120_000);
        // Reading the production counter registers its series, unchanged.
        yield* Metric.value(
          Metric.tagged(
            forcedAdmissionDivergenceCounter,
            "refusal_code",
            SCREEN_FAILED,
          ),
        );
        return prometheusFamilies(
          yield* Effect.promise(() => exporter.scrape()),
        );
      }),
    ).pipe(
      Effect.provideService(ContractDeploymentIdentity, identity),
      Effect.provide(Globals.Default),
    ) as Effect.Effect<ReadonlySet<string>, unknown, never>,
  );

describe("the node's alert rules", () => {
  it("select only metric families the node exports", async () => {
    const families = await exportedFamilies();
    const selectors = alertRuleSelectors(
      await readFile(new URL("../alert-rules.yml", import.meta.url), "utf8"),
    );
    expect(new Set(selectors.map(({ alert }) => alert)).size).toBe(6);
    for (const { alert, metric } of selectors)
      expect(families, `${alert} selects ${metric}`).toContain(metric);
  });
});
