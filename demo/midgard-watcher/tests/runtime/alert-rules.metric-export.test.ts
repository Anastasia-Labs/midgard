/**
 * Every rule in `demo/midgard-watcher/alert-rules.yml` selects a metric
 * family the watcher's `/metrics` really serves, and every label value it
 * matches is one the watcher names: a rule over a family or reason the
 * watcher never produces would never fire.
 */
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { WATCHER_FAULT_PROOF_CONTINUATION_STALLED } from "../../src/fault-proofs/fault-proof-continuation-stall.js";
import { WATCHER_FAULT_PROOF_L1_REFUSED } from "../../src/fault-proofs/fault-proof-execution-holds.js";
import { WATCHER_PROVER_FUNDING_UNAVAILABLE } from "../../src/fault-proofs/fault-proof-funding-hold.js";
import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED } from "../../src/fault-proofs/fault-proof-resubmission-hold.js";
import {
  createWatcherReplayRefusals,
  WATCHER_REPLAY_REFUSAL_KINDS,
  WATCHER_REPLAY_REFUSAL_PHASES,
} from "../../src/fault-proofs/replay-refusals.js";
import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import {
  L1_EVENT_REFUSALS_UNREADABLE,
  L1_USER_EVENT_REFUSED,
} from "../../src/l1-follower/event-refusals.js";
import type { RawReadRefusal } from "../../src/l1-follower/raw-reads.types.js";
import {
  createWatcherOperationsObservability,
  WATCHER_ALERT_CODES,
} from "../../src/runtime/operations-observability.js";
import { WATCHER_OPERATIONS_READINESS_REASONS } from "../../src/runtime/operations-observability.watcher-operations-readiness-reasons.js";
import { alertRuleSelectors } from "../support/alert-rule-selectors.js";
import { supervisor } from "./operations-observability.supervisor.js";

/** The fault-proof holds the execution names through `l1Readiness`. */
const PROOF_HOLD_REASONS = [
  WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
  WATCHER_FAULT_PROOF_CONTINUATION_STALLED,
  WATCHER_FAULT_PROOF_QUARANTINED,
  WATCHER_PROVER_FUNDING_UNAVAILABLE,
];
/** An objective held on a refused L1 read, named
 * `<WATCHER_FAULT_PROOF_L1_REFUSED>:<refusal reason>`: a rule matches the
 * family with this pattern. */
const PROOF_L1_REFUSED = `${WATCHER_FAULT_PROOF_L1_REFUSED}:.*`;
const L1_REASONS = [L1_USER_EVENT_REFUSED];

/** The label values the watcher names, per family and label. */
const NAMED: Readonly<
  Record<string, Readonly<Record<string, readonly string[]>>>
> = {
  midgard_watcher_active_alerts: { code: WATCHER_ALERT_CODES },
  midgard_watcher_replay_refusals_total: {
    phase: WATCHER_REPLAY_REFUSAL_PHASES,
    kind: WATCHER_REPLAY_REFUSAL_KINDS,
  },
  midgard_watcher_readiness_reason: {
    reason: [
      ...WATCHER_OPERATIONS_READINESS_REASONS,
      WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
      ...PROOF_HOLD_REASONS,
      PROOF_L1_REFUSED,
      ...L1_REASONS,
    ],
  },
  midgard_watcher_l1_readiness: {
    reason: [...PROOF_HOLD_REASONS, PROOF_L1_REFUSED, ...L1_REASONS],
  },
  midgard_watcher_l1_degradation: {
    reason: [L1_USER_EVENT_REFUSED, L1_EVENT_REFUSALS_UNREADABLE],
  },
};

/** The metric families a healthy watcher's `/metrics` declares. */
const servedFamilies = async (): Promise<ReadonlySet<string>> => {
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: "11".repeat(32),
    supervisor: supervisor().runtime,
    launchScopeStatus: () => ({
      installedCategoryCount: 32,
      requiredCategoryCount: 32,
    }),
    // The supervisor fixture reports one queued job.
    durableProofQueueStatus: () => ({
      queuedJobCount: 1,
      oldestQueuedAtMs: "97000",
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    replayRefusals: createWatcherReplayRefusals({ log: () => undefined })
      .status,
    nowMs: () => 100_000n,
  });
  const response = await operations.handleHttpRequest(
    new Request("http://127.0.0.1/metrics"),
  );
  expect(response.status).toBe(200);
  return new Set(
    (await response.text())
      .split("\n")
      .filter((line) => line.startsWith("# TYPE "))
      .map((line) => line.split(" ")[2]!),
  );
};

describe("the watcher's alert rules", () => {
  it("select only metric families /metrics serves, and only label values the watcher names", async () => {
    const families = await servedFamilies();
    const selectors = alertRuleSelectors(
      await readFile(new URL("../../alert-rules.yml", import.meta.url), "utf8"),
    );
    expect(new Set(selectors.map(({ alert }) => alert)).size).toBe(7);
    for (const { alert, metric, matchers } of selectors) {
      expect(families, `${alert} selects ${metric}`).toContain(metric);
      for (const { label, values } of matchers) {
        const named = NAMED[metric]?.[label];
        expect(
          named,
          `${alert}: ${metric} has no named ${label}`,
        ).toBeDefined();
        for (const value of values)
          expect(named, `${alert}: ${metric}{${label}}`).toContain(value);
      }
    }
  });

  it("page every fault-proof hold the execution names", async () => {
    const selectors = alertRuleSelectors(
      await readFile(new URL("../../alert-rules.yml", import.meta.url), "utf8"),
    );
    const proofHeld = selectors.find(
      ({ alert }) => alert === "MidgardWatcherProofHeld",
    );
    const matcher = proofHeld?.matchers.find(
      ({ label, operator }) => label === "reason" && operator === "=~",
    );
    expect(matcher).toBeDefined();
    // Prometheus anchors a regex matcher at both ends.
    const pages = new RegExp(`^(?:${matcher!.values.join("|")})$`, "u");
    const refusal: RawReadRefusal = "point_beyond_retention";
    for (const reason of [
      ...PROOF_HOLD_REASONS,
      WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
      `${WATCHER_FAULT_PROOF_L1_REFUSED}:${refusal}`,
    ])
      expect(reason, `MidgardWatcherProofHeld pages ${reason}`).toMatch(pages);
  });
});
