/**
 * Source verification of a validation-trace dispute
 * (`fraud_proofs/validation_trace/source_v1`) routes on
 * `committed_claim_endpoints_and_source_are_valid`, which binds a normal
 * source to an accepting descriptor (`source_binding_is_exact` in
 * `validation-claim-v1.ak`; its off-chain mirror is
 * `committedValidationClaimEndpointsAndSourceAreValid`). A normal source
 * committed under a rejecting descriptor therefore goes straight to the
 * award, whatever the replay says; an accepting one takes the open route.
 *
 * Both polarities run the installed workflow over the emulator chain, with
 * the watcher classifying from follower facts:
 *
 * - positive: an unbound-variable script committed with its replayed
 *   rejecting trace (the descriptor equals the replay) is a
 *   `validationTraceDispute` fault; the watcher's capture drives the
 *   production workflow through the award, and the block is removed;
 * - negative: the identity program committed with its replayed accepting
 *   trace is healthy, and source verification refuses to send its dispute to
 *   the award.
 */
import { createManifestBoundValidationTraceDisputeWorkflow } from "@al-ft/midgard-fault-proofs";
import {
  buildFollowerValidationDisputeFixture,
  committedFollowerValidationClaim,
  followerClaimTakesOpenRoute,
  type FollowerValidationCommitment,
  openStagedValidationDispute,
  validationTraceMaterial,
} from "@al-ft/midgard-fault-proofs/test-support/installed-validation-follower-fixture";
import {
  type InstalledValidationChallengeSupply,
  type InstalledValidationJourneyStaged,
  stageSuppliedValidationTraceDisputeJourney,
} from "@al-ft/midgard-fault-proofs/test-support/installed-validation-trace-dispute-journey";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { followJourneyValidationDecision } from "../support/follower-validation-dispute.js";

/**
 * The installed dispute journey's production workflow, bound to the
 * deployment the journey stages. `forceAwardRoute` makes the challenger's
 * off-chain mirror of source verification pick the award, so the negative
 * reaches the validator with an award output.
 */
const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  forceAwardRoute: false,
}));
vi.mock(
  "../../../midgard-fault-proofs/src/validation-dispute/workflow-binding.js",
  async (load) => ({
    ...(await load<Record<string, unknown>>()),
    bindValidationTraceDisputeWorkflowDeployment: async () => hooks.binding,
  }),
);
vi.mock(
  "../../../midgard-fault-proofs/src/workflow/family-l1-observation.js",
  async (load) => {
    const actual = await load<{
      createFraudProofFamilyRawL1ObservationPort: (input: object) => unknown;
    }>();
    return {
      ...actual,
      createFraudProofFamilyL1ObservationPort: (input: object) =>
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority,
        }),
    };
  },
);
vi.mock(
  "../../../midgard-fault-proofs/src/validation-dispute/claim-endpoints.js",
  async (load) => {
    type Valid = (...args: unknown[]) => boolean;
    const actual = await load<{
      committedValidationClaimEndpointsAndSourceAreValid: Valid;
    }>();
    const valid: Valid = (...args) =>
      !hooks.forceAwardRoute &&
      actual.committedValidationClaimEndpointsAndSourceAreValid(...args);
    return {
      ...actual,
      committedValidationClaimEndpointsAndSourceAreValid: valid,
    };
  },
);

type Fixture = Awaited<
  ReturnType<typeof buildFollowerValidationDisputeFixture>
>;
type Followed = Awaited<ReturnType<typeof followJourneyValidationDecision>>;

const cleanups: (() => Promise<void>)[] = [];
afterEach(async () => {
  hooks.forceAwardRoute = false;
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

/**
 * Stages the installed journey over the retained block the operator commits
 * as `commitment`, then lets the watcher follow the emulator chain and
 * decide on the committed header.
 */
const stageFollowedJourney = async (
  commitment: FollowerValidationCommitment,
  decide: (
    followed: Followed,
    staged: InstalledValidationJourneyStaged<Fixture>,
  ) => Promise<
    Omit<InstalledValidationChallengeSupply, "deploymentFingerprint">
  >,
) => {
  let followed: Followed | undefined;
  const journey = await stageSuppliedValidationTraceDisputeJourney(hooks, {
    fixture: async ({ operatorVkey, now }) =>
      await buildFollowerValidationDisputeFixture({
        operatorVkey,
        now,
        commitment,
      }),
    challenge: async (staged) => {
      followed = await followJourneyValidationDecision(staged);
      cleanups.push(() => followed!.follower.close());
      return {
        ...(await decide(followed, staged)),
        deploymentFingerprint:
          followed.deploymentAuthority.deploymentIdentity.manifestId,
      };
    },
  });
  cleanups.push(journey.cleanup);
  return { journey, followed: followed! };
};

describe("source verification of a normal source's committed descriptor on the installed workflow", () => {
  it("captures a normal source committed under a rejecting descriptor from follower facts and wins at source verification", async () => {
    let decisionDigest: string | undefined;
    const { journey, followed } = await stageFollowedJourney(
      "replayedRejection",
      async (watcher, staged) => {
        const decision = await watcher.classify();
        expect(decision).toMatchObject({
          decision: "fault_detected",
          category: "validationTraceDispute",
          headerHash: staged.setup.headerHash,
        });
        const capture = await watcher.capture(decision);
        decisionDigest = capture.decisionDigest;
        return {
          challenge: capture.challenge,
          decisionDigest: capture.decisionDigest,
        };
      },
    );
    const { challenge, fixture, setup } = journey;
    expect(challenge).toBeDefined();
    expect(challenge!.coordinate).toMatchObject({
      deploymentFingerprint:
        followed.deploymentAuthority.deploymentIdentity.manifestId,
      stateQueueObservationDigest: followed.observation.observationDigest,
      headerHash: setup.headerHash,
      coordinate: { domain: "transition_step" },
    });
    expect(journey.config!.decisionDigest).toBe(decisionDigest);
    // The operator committed the replayed trace: the challenger's replay
    // agrees with it, and only the descriptor's verdict decides the route.
    const material = validationTraceMaterial(challenge!);
    expect(material.claim.descriptor_membership.value).toEqual(
      validationTraceDescriptorDataFromCore(
        fixture.challengerTrace.tree.descriptor,
      ),
    );
    expect(material.challengerDescriptor).toEqual(
      material.claim.descriptor_membership.value,
    );
    expect(material.claim.descriptor_membership.value.verdict).toBe("Rejected");
    expect(material.claim.source_membership).toHaveProperty(
      "NormalValidationSource",
    );
    expect(followerClaimTakesOpenRoute(fixture, material.claim)).toBe(false);

    // Source verification awards the dispute itself: the operator is never
    // asked to move and no one-step resolution runs.
    const kinds: string[] = [];
    let { result } = await journey.runCold();
    for (let hop = 0; hop < 200 && result.kind !== "completed"; hop++) {
      kinds.push(result.kind);
      expect(result.kind).not.toBe("stalled");
      expect(result.kind).not.toBe("awaiting_counterparty");
      await journey.advance(result);
      ({ result } = await journey.runCold());
    }
    expect(result.kind).toBe("completed");
    expect(kinds.length).toBeGreaterThan(0);
    const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
      journey.config!,
    );
    expect((await workflow.deriveStage(Date.now())).kind).toBe("removed");
    expect(
      await journey.config!.lucid.utxosAtWithUnit(
        journey.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
        toUnit(
          journey.resolvedContracts.contracts.fraudProof.policyId,
          `00000006${setup.headerHash}`,
        ),
      ),
    ).toHaveLength(1);
  }, 900_000);

  it("finds an honest normal commitment healthy, and source verification refuses to award its dispute", async () => {
    const { journey, followed } = await stageFollowedJourney(
      "honest",
      async (watcher, staged) => {
        const decision = await watcher.classify();
        expect(decision).toMatchObject({
          decision: "healthy",
          headerHash: staged.setup.headerHash,
        });
        await expect(watcher.capture(decision)).rejects.toThrow(
          "Validation transcript requires this deployment's selected decision",
        );
        return {};
      },
    );
    expect(journey.challenge).toBeUndefined();
    expect(followed.header.headerHash).toBe(journey.setup.headerHash);

    const { fixture } = journey;
    const claim = await committedFollowerValidationClaim(fixture);
    expect(claim.descriptor_membership.value.verdict).toBe("Accepted");
    expect(followerClaimTakesOpenRoute(fixture, claim)).toBe(true);
    const direct = await openStagedValidationDispute(journey, {
      claim,
      challengerDescriptor: validationTraceDescriptorDataFromCore(
        fixture.challengerTrace.tree.descriptor,
      ),
    });
    hooks.forceAwardRoute = true;
    // `source_v1` admits the award output only when the committed endpoints
    // are invalid; the thread is the transaction's only script input.
    await expect(direct.verifySource()).rejects.toThrow(
      "failed script execution Spend[0]",
    );
  }, 900_000);
});
