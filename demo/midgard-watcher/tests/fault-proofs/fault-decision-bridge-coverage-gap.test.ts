import {
  FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID,
  type HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import { watcherProofFamilyCoverageGapAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  decision,
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  operations,
  records,
} from "./fault-decision-bridge.released-fixture.js";

/** What the classifier seals for a forced leaf rejected for a coordinate its
 * transaction does not have: no installed family proves it. */
const absentClaim = ({
  category: _category,
  ...fresh
}: ReturnType<typeof decision>): HeaderDecision => ({
  ...fresh,
  decision: "unprovable",
  reason: "unregistered_violation",
  violationId: FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID,
  detectionId: `${FORCED_CLAIM_COORDINATE_ABSENT_VIOLATION_ID}:forced:0:${"aa".repeat(32)}`,
  position: "0",
});

const coverageGaps = (observability: ReturnType<typeof operations>) =>
  observability.api
    .status()
    .activeAlerts.filter(({ code }) => code === "proof_family_coverage_gap")
    .map(({ subjectDigest }) => subjectDigest);

describe("fault decision bridge on a finding no installed family proves", () => {
  it("alerts on the header, keeps deciding later headers, and clears once the header leaves the queue", async () => {
    const [first, second] = [headerFixture("01"), headerFixture("02")];
    const unprovable = observation([first]);
    const later = observation([first, second]);
    const moved = observation([second]);
    const unprovableHash = later.finalizedHeaders[0]!.headerHash;
    const faultyHash = later.finalizedHeaders[1]!.headerHash;
    const observability = operations();
    const h = harness({
      current: unprovable,
      categoryByHeader: {
        [unprovableHash]: "transitionTrace",
        [faultyHash]: "doubleSpend",
      },
      operationsSink: observability.sink,
      classifyOverride: (fresh) =>
        fresh.headerHash === unprovableHash ? absentClaim(fresh) : fresh,
    });
    h.admitted.add(later);
    h.admitted.add(moved);

    // The pass completes with nothing targeted; the alert holds readiness.
    const held = await h.bridge.reconcileAndDispatch(unprovable);
    expect(held.target).toBeNull();
    expect(h.bridge.readiness()).toEqual([]);
    expect(coverageGaps(observability)).toEqual([
      watcherProofFamilyCoverageGapAlertSubject(unprovableHash),
    ]);
    expect(observability.api.status().readinessReasons).toContain(
      "active_alert",
    );

    // A later header with a provable fault is decided and targeted while
    // the unprovable one stays alerted.
    const targeted = await h.bridge.reconcileAndDispatch(later);
    expect(targeted.target).toMatchObject({
      headerHash: faultyHash,
      category: "doubleSpend",
    });
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faultyHash,
    ]);
    expect(
      records(observability).map(({ headerHash, outcome }) => ({
        headerHash,
        outcome,
      })),
    ).toEqual(
      expect.arrayContaining([
        { headerHash: unprovableHash, outcome: "unprovable_gap" },
        { headerHash: faultyHash, outcome: "fault_detected" },
      ]),
    );
    expect(coverageGaps(observability)).toEqual([
      watcherProofFamilyCoverageGapAlertSubject(unprovableHash),
    ]);

    // Once the unprovable header is no longer queued, its alert clears
    // without an operator, and so does the readiness hold.
    await h.bridge.reconcileAndDispatch(moved);
    expect(coverageGaps(observability)).toEqual([]);
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
  });

  it("clears the alert when the header is later decided otherwise", async () => {
    const current = observation([headerFixture("01")]);
    const headerHash = current.finalizedHeaders[0]!.headerHash;
    const observability = operations();
    let reclassified = false;
    const h = harness({
      current,
      categoryByHeader: { [headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      classifyOverride: (fresh) => (reclassified ? fresh : absentClaim(fresh)),
    });
    await h.bridge.reconcileAndDispatch(current);
    expect(coverageGaps(observability)).toEqual([
      watcherProofFamilyCoverageGapAlertSubject(headerHash),
    ]);
    reclassified = true;
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(headerHash);
    expect(coverageGaps(observability)).toEqual([]);
  });

  it("raises no alert for a header waiting on its predecessor's context", async () => {
    const current = observation([headerFixture("01")]);
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [current.finalizedHeaders[0]!.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      classifyOverride: (fresh) => ({
        ...absentClaim(fresh),
        reason: "predecessor_context_unavailable",
      }),
    });
    await h.bridge.reconcileAndDispatch(current);
    expect(coverageGaps(observability)).toEqual([]);
  });
});
