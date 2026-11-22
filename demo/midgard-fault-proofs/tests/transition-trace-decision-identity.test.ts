import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKUsers from "@al-ft/midgard-sdk/users";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  createTransitionTraceEventAuthority,
  requireTransitionTraceL1Events,
} from "../src/transition-trace/l1-events.js";
import { TRANSITION_TRACE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import type { FraudProofRawL1Snapshot } from "../src/workflow/raw-l1-snapshot.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  RELEASE_L1_FINALITY_POLICY,
} from "../src/workflow/release-finality-policy.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { fraudProofL1SourceForTest } from "./support/fraud-proof-l1-source.js";
import { buildRetainedPlutusIdentityFixture } from "./support/retained-reason-classifier.build-retained-plutus-fixture.js";
import { captureRetainedPlutusIdentityOrigins } from "./support/retained-reason-classifier.capture-retained-plutus-identity-origins.js";
import { classifyRetainedReasonFixture } from "./support/retained-reason-classifier.classify-retained-reason-fixture.js";
import {
  capture,
  captureInput,
  changeDatum,
  respendOutput,
  withLaterEvent,
} from "./support/transition-l1-snapshot.js";

// A decision names the block and the content of its L1 events. Who later
// respends an event node or the hub (a list insert, a governance update)
// moves outRefs and grows unit history, and must not move the decision.

let seed: FraudProofRawL1Snapshot;
let retained: Awaited<ReturnType<typeof buildRetainedPlutusIdentityFixture>>;
beforeAll(async () => {
  retained = await buildRetainedPlutusIdentityFixture(
    { verdict: "accepted" },
    { sourceKind: "forced" },
  );
  seed = requireTransitionTraceL1Events(
    await captureRetainedPlutusIdentityOrigins(retained),
  ).snapshot;
});

/** The production classifier over a fake L1 source serving `snapshot`. */
const decide = async (snapshot: FraudProofRawL1Snapshot) => {
  const { binding, authority } = captureInput(snapshot);
  return (
    await classifyRetainedReasonFixture({
      observation: authenticatedHeaderObservation(retained.block),
      payloadEnvelopeCbor: retained.block.payloadEnvelopeCbor,
      hubOraclePolicyId: seed.hubOraclePolicyId,
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => ({
          schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
          hubOraclePolicyId: seed.hubOraclePolicyId,
          blueprintHash: seed.blueprintHash,
          policyDigest: seed.finalityPolicyDigest,
          policy: { ...RELEASE_L1_FINALITY_POLICY },
        }),
      },
      replayer: TRANSITION_TRACE_COMPLETE_CANONICAL_REPLAY,
      transitionTraceEventAuthority: createTransitionTraceEventAuthority({
        binding,
        l1: fraudProofL1SourceForTest({ authority: () => authority }),
      }),
      predecessor: {
        observation: authenticatedHeaderObservation(retained.predecessor),
        payloadEnvelopeCbor: retained.predecessor.payloadEnvelopeCbor,
      },
    })
  ).decision;
};

const forcedEvent = (snapshot: FraudProofRawL1Snapshot) =>
  snapshot.scopes.find(({ role }) => role === "forced_transaction_event")!
    .utxos[0]!;

describe("transition trace decision identity", () => {
  it("keeps the decision digest when an event node is respent and the hub UTxO moves", async () => {
    const moved = respendOutput(
      respendOutput(seed, "forced_transaction_event", "a1"),
      "hub_oracle",
      "a2",
    );
    // The fake source really serves different outRefs and longer histories.
    expect(forcedEvent(moved).outRef).not.toBe(forcedEvent(seed).outRef);
    expect((await capture(moved)).evidenceDigest).not.toBe(
      (await capture(seed)).evidenceDigest,
    );
    const before = await decide(seed);
    expect(before.decision).toBe("healthy");
    expect(await decide(moved)).toEqual(before);
  });

  it("keeps the decision digest when the hub changes a field replay never reads", async () => {
    const raw = seed.scopes.find(({ role }) => role === "hub_oracle")!
      .utxos[0]!;
    const hub = Data.from(raw.datumCbor!, SDK.HubOracleDatum);
    const governed = respendOutput(
      seed,
      "hub_oracle",
      "a3",
      Data.to({ ...hub, scheduler: "86".repeat(28) }, SDK.HubOracleDatum),
    );
    // The fake source really serves the updated hub datum.
    expect(
      governed.scopes.find(({ role }) => role === "hub_oracle")!.utxos[0]!
        .datumCbor,
    ).not.toBe(raw.datumCbor);
    const before = await decide(seed);
    expect(await decide(governed)).toEqual(before);
  });

  it("moves the decision digest when an event datum changes", async () => {
    const raw = forcedEvent(seed);
    const datum = Data.from(raw.datumCbor!, SDKUsers.TxOrderDatum);
    const changed = changeDatum(
      seed,
      Number(raw.outRef.split("#")[1]),
      Data.to({ ...datum, witness: "85".repeat(28) }, SDKUsers.TxOrderDatum),
    );
    const before = await decide(seed);
    const after = await decide(changed);
    expect(after.decision).toBe(before.decision);
    expect(after.decisionDigest).not.toBe(before.decisionDigest);
  });

  it("moves the decision digest when the set of relevant events changes", async () => {
    const before = await decide(seed);
    const after = await decide(
      withLaterEvent(seed, retained.block.header.endTime),
    );
    expect(after.decisionDigest).not.toBe(before.decisionDigest);
  });
});
