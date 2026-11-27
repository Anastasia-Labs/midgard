/**
 * A header that matured at the follower's L1 tip before it merged: no fraud
 * proof against it can land any more, so classification skips it
 * (`fault-decision-bridge.matured-headers.ts`). What the watcher records,
 * warns and holds for it depends on whether it decided on the header first.
 */
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { FAULT_TARGET_CHANGED } from "../../src/fault-proofs/fault-decision-bridge.selected-target.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
  OBSERVATION_DIGEST,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  operations,
  proof,
  records,
} from "./fault-decision-bridge.released-fixture.js";

describe("fault decision bridge over a matured header", () => {
  it("records a header that matured unverified past its horizon and warns once across passes, and its merge unverified", async () => {
    const { maturityMs } = MIDGARD_RETENTION_WINDOW;
    const current = observation(
      [headerFixture("01"), headerFixture("02")],
      "Idle",
      [ATTESTED, ATTESTED],
      [1_000_000n, 1_000_001n],
    );
    const [matured, live] = current.finalizedHeaders;
    const observability = operations();
    const warnings: { event: string; headerHash: string }[] = [];
    let mergedOnL1 = false;
    const h = harness({
      current,
      categoryByHeader: {
        [matured!.headerHash]: "doubleSpend",
        [live!.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      warn: ({ event, headerHash }) => warnings.push({ event, headerHash }),
      mergedHeaders: async () =>
        new Map(
          mergedOnL1 ? [[matured!.headerHash, proof(matured!.headerHash)]] : [],
        ),
      l1TipTimeMs: async () => 1_000_000 + maturityMs,
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    await h.bridge.reconcileAndDispatch(current);
    await h.bridge.reconcileAndDispatch(current);
    const ofMatured = () =>
      records(observability)
        .filter(({ headerHash }) => headerHash === matured!.headerHash)
        .map(({ outcome }) => outcome);
    expect(ofMatured()).toEqual(["unverified_past_horizon"]);
    expect(
      records(observability).find(
        ({ headerHash }) => headerHash === matured!.headerHash,
      ),
    ).toMatchObject({ subjectDigest: OBSERVATION_DIGEST });
    expect(warnings).toEqual([
      { event: "unverified_matured", headerHash: matured!.headerHash },
    ]);
    // Its merge is still unverified: reported as a past-horizon header's is.
    mergedOnL1 = true;
    await h.bridge.reconcileAndDispatch(current);
    expect(ofMatured()).toEqual([
      "unverified_past_horizon",
      "unverified_merged",
    ]);
  });

  it("records nothing and warns nothing when a header it verified matures", async () => {
    const { maturityMs } = MIDGARD_RETENTION_WINDOW;
    const current = observation(
      [headerFixture("01"), headerFixture("02")],
      "Idle",
      [ATTESTED, ATTESTED],
      [1_000_000n, 1_000_001n],
    );
    const [header, later] = current.finalizedHeaders;
    const observability = operations();
    const warnings: string[] = [];
    // One millisecond before the header matures, then at its maturity.
    let tipMs = 1_000_000 + maturityMs - 1;
    const h = harness({
      current,
      categoryByHeader: {
        [header!.headerHash]: "doubleSpend",
        [later!.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      warn: ({ event }) => warnings.push(event),
      mergedHeaders: async () => new Map(),
      l1TipTimeMs: async () => tipMs,
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    await h.bridge.reconcileAndDispatch(current);
    tipMs += 1;
    await h.bridge.reconcileAndDispatch(current);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([header!.headerHash, later!.headerHash, later!.headerHash]);
    expect(
      records(observability)
        .filter(({ headerHash }) => headerHash === header!.headerHash)
        .map(({ outcome }) => outcome),
    ).toEqual(["verified"]);
    expect(warnings).toEqual([]);
  });

  it("revokes a detected fault's submission authority once its header matures, holds nothing for it and reports its merge unverified", async () => {
    const { maturityMs } = MIDGARD_RETENTION_WINDOW;
    const current = observation(
      [headerFixture("01"), headerFixture("02")],
      "Idle",
      [ATTESTED, ATTESTED],
      [1_000_000n, 1_000_001n],
    );
    const [faulty, later] = current.finalizedHeaders;
    const observability = operations();
    const warnings: string[] = [];
    // One millisecond before the faulty header matures, then at its maturity.
    let tipMs = 1_000_000 + maturityMs - 1;
    let mergedOnL1 = false;
    const h = harness({
      current,
      categoryByHeader: {
        [faulty!.headerHash]: "doubleSpend",
        [later!.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      warn: ({ event }) => warnings.push(event),
      mergedHeaders: async () =>
        new Map(
          mergedOnL1 ? [[faulty!.headerHash, proof(faulty!.headerHash)]] : [],
        ),
      l1TipTimeMs: async () => tipMs,
      classifyOverride: (fresh) =>
        fresh.headerHash === faulty!.headerHash
          ? fresh
          : { ...fresh, decision: "healthy" },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toMatchObject(
      { headerHash: faulty!.headerHash },
    );
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);

    // Matured: no proof against it can land, so it is never submitted again.
    tipMs += 1;
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(h.revocations).toEqual([FAULT_TARGET_CHANGED]);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(h.bridge.readiness()).toEqual([]);
    const ofFaulty = () =>
      records(observability)
        .filter(({ headerHash }) => headerHash === faulty!.headerHash)
        .map(({ outcome }) => outcome);
    // Decided on: not recorded past its horizon.
    expect(ofFaulty()).toEqual(["fault_detected"]);
    expect(warnings).toEqual([]);

    mergedOnL1 = true;
    await h.bridge.reconcileAndDispatch(current);
    expect(ofFaulty()).toEqual(["fault_detected", "unverified_merged"]);
    expect(warnings).toEqual(["unverified_merged"]);
    expect(h.bridge.readiness()).toEqual([]);
  });
});
