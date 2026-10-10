import { readdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { midgardValidationDescriptorsCanDispute } from "@al-ft/midgard-core/validation-dispute";
import { createManifestBoundValidationTraceDisputeWorkflow } from "@al-ft/midgard-fault-proofs";
import {
  buildFollowerValidationDisputeFixture,
  committedFollowerValidationClaim,
  FOLLOWER_VALIDATION_CEK_CORE_STEP,
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
 * The installed dispute journey's production workflow, bound (as the
 * fault-proofs installed lifecycle binds it) to the deployment the journey
 * stages. `openIndisputable` lets the challenger's off-chain mirror of
 * source verification build a game for descriptors it would refuse, so the
 * negative reaches the validator.
 */
const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  openIndisputable: false,
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
  "../../../midgard-fault-proofs/src/validation-dispute/submit/open.js",
  async (load) => {
    type Descriptor = Readonly<{ terminalStateHash: Buffer }>;
    type Open = (
      input: Readonly<{ challengerDescriptor: Descriptor }>,
    ) => Readonly<Record<string, unknown>>;
    const actual = await load<{
      openValidationDisputeAfterSourceVerification: Open;
    }>();
    const open: Open = (input) => {
      if (!hooks.openIndisputable)
        return actual.openValidationDisputeAfterSourceVerification(input);
      const { challengerDescriptor } = input;
      // The game the mirror would open were the challenger's terminal
      // different, carrying the challenger's actual descriptor.
      return {
        ...actual.openValidationDisputeAfterSourceVerification({
          ...input,
          challengerDescriptor: {
            ...challengerDescriptor,
            terminalStateHash: Buffer.alloc(32, 0x5a),
          },
        }),
        challengerDescriptor,
        challengerHighHash: challengerDescriptor.terminalStateHash,
      };
    };
    return { ...actual, openValidationDisputeAfterSourceVerification: open };
  },
);

type Fixture = Awaited<
  ReturnType<typeof buildFollowerValidationDisputeFixture>
>;
type Followed = Awaited<ReturnType<typeof followJourneyValidationDecision>>;

const cleanups: (() => Promise<void>)[] = [];
afterEach(async () => {
  hooks.openIndisputable = false;
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

/**
 * Stages the installed journey over the retained unbound-variable block the
 * operator commits as `commitment`, then lets the watcher follow the
 * emulator chain and decide on the committed header.
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

type Journey = Awaited<ReturnType<typeof stageFollowedJourney>>["journey"];
type JourneyResult = Awaited<ReturnType<Journey["runCold"]>>["result"];

/** Plays the dispute cold, one workflow invocation per hop, to completion. */
const runJourneyToCompletion = async (
  journey: Journey,
  afterRun: (result: JourneyResult) => Promise<void> = async () => {},
) => {
  let { result } = await journey.runCold();
  for (let hop = 0; hop < 200 && result.kind !== "completed"; hop++) {
    if (result.kind === "stalled") throw new Error(result.reason);
    await afterRun(result);
    await journey.advance(result);
    if (result.kind === "awaiting_counterparty") {
      await journey.operatorResponds();
      journey.emulator.awaitBlock();
    }
    ({ result } = await journey.runCold());
  }
  if (result.kind !== "completed") throw new Error("journey did not complete");
  return result;
};

/** The submission intents of a finished journey, in journal order. */
const submissionIntents = (
  result: Extract<JourneyResult, { kind: "completed" }>,
) =>
  result.entries.flatMap(({ event }) =>
    event.kind === "submission_intent" ? [event] : [],
  );

/** Stages the forged CEK core successor and captures the watcher's challenge. */
const stageForgedCekCoreJourney = async () => {
  let decisionDigest: string | undefined;
  const staged = await stageFollowedJourney(
    "forgedCekCoreSuccessor",
    async (watcher, journey) => {
      const decision = await watcher.classify();
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "validationTraceDispute",
        headerHash: journey.setup.headerHash,
      });
      const capture = await watcher.capture(decision);
      decisionDigest = capture.decisionDigest;
      return {
        challenge: capture.challenge,
        decisionDigest: capture.decisionDigest,
      };
    },
  );
  return { ...staged, decisionDigest: decisionDigest! };
};

describe("follower-sourced validation-trace dispute on the installed workflow", () => {
  it("captures a forged CEK core successor from follower facts and wins the dispute on-chain", async () => {
    const { journey, followed, decisionDigest } =
      await stageForgedCekCoreJourney();
    const { challenge, fixture, setup } = journey;
    expect(fixture.evidence.oneStepArgument).toMatchObject(
      FOLLOWER_VALIDATION_CEK_CORE_STEP,
    );
    // The challenge is the watcher's: its coordinate names the follower's
    // deployment, the emulator-committed header and the captured transcript.
    expect(challenge).toBeDefined();
    expect(challenge!.coordinate).toMatchObject({
      deploymentFingerprint:
        followed.deploymentAuthority.deploymentIdentity.manifestId,
      stateQueueObservationDigest: followed.observation.observationDigest,
      headerHash: setup.headerHash,
      coordinate: { domain: "transition_step" },
    });
    expect(followed.header).toMatchObject({
      headerHash: setup.headerHash,
      queueOutRef: setup.fraudulentBlockOutRef,
    });
    expect(journey.config!.decisionDigest).toBe(decisionDigest);
    // The operator committed the forged trace; the challenger replays the
    // honest one, which disagrees at the terminal alone.
    const material = validationTraceMaterial(challenge!);
    expect(material.claim.descriptor_membership.value).toEqual(
      validationTraceDescriptorDataFromCore(
        fixture.operatorTrace.tree.descriptor,
      ),
    );
    expect(material.challengerDescriptor).toEqual(
      validationTraceDescriptorDataFromCore(
        fixture.challengerTrace.tree.descriptor,
      ),
    );

    // The CEK core resolution is a chain of transactions; every stage
    // reaches the pre-submit boundary, so the workflow submits it one stage
    // per move and never stalls.
    const result = await runJourneyToCompletion(journey);
    // The binder stage, then each later CEK core stage resumed against the
    // preparation the binder consumed; no route was cancelled.
    const intents = submissionIntents(result).map(
      ({ actionInput }) => actionInput,
    );
    const semantic = intents.filter(
      ({ stage }) => stage === "semantic_resolution",
    );
    expect(semantic.length).toBeGreaterThan(2);
    expect(semantic[0]!.cekPreparedResolutionCbor).toBeUndefined();
    expect(
      semantic
        .slice(1)
        .map(
          ({ cekPreparedResolutionCbor }) =>
            typeof cekPreparedResolutionCbor === "string",
        ),
    ).toEqual(semantic.slice(1).map(() => true));
    expect(
      intents.filter(({ stage }) => stage === "cancel_semantic_route"),
    ).toEqual([]);
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

  it("recomputes a corrupt journaled CEK preparation from the thread history and still wins the dispute", async () => {
    const { journey } = await stageForgedCekCoreJourney();
    const journalRoot = join(journey.directory, "journal");
    let corruption: { original: string; corrupt: string } | undefined;
    const result = await runJourneyToCompletion(journey, async () => {
      if (corruption !== undefined) return;
      // The journal's newest submission intent is the route the next move
      // resumes against; once it carries the CEK preparation, corrupt it on
      // disk as a torn write would.
      for (const workflow of await readdir(journalRoot)) {
        const names = (await readdir(join(journalRoot, workflow))).sort();
        for (const name of names.reverse()) {
          const path = join(journalRoot, workflow, name);
          const entry = JSON.parse(await readFile(path, "utf8"));
          if (entry.event.kind !== "submission_intent") continue;
          const route = entry.event.durableRecovery?.durableRouteInput;
          if (typeof route?.cekPreparedResolutionCbor !== "string") break;
          const original: string = route.cekPreparedResolutionCbor;
          const corrupt = original.slice(0, 40);
          route.cekPreparedResolutionCbor = corrupt;
          await writeFile(path, JSON.stringify(entry));
          corruption = { original, corrupt };
          break;
        }
      }
    });
    expect(corruption).toBeDefined();
    const intents = submissionIntents(result);
    // The next stage was planned against the corrupt preparation, captured
    // against the one recomputed from the thread history, and journaled that
    // one for every later stage; no route was cancelled.
    const recovered = intents.findIndex(
      ({ actionInput }) =>
        actionInput.cekPreparedResolutionCbor === corruption!.corrupt,
    );
    expect(recovered).toBeGreaterThan(0);
    expect(
      intents[recovered]!.durableRecovery?.durableRouteInput,
    ).toMatchObject({ cekPreparedResolutionCbor: corruption!.original });
    expect(
      intents
        .slice(recovered + 1)
        .filter(
          ({ actionInput }) => actionInput.stage === "semantic_resolution",
        )
        .map(({ actionInput }) => actionInput.cekPreparedResolutionCbor),
    ).not.toContain(corruption!.corrupt);
    expect(
      intents.filter(
        ({ actionInput }) => actionInput.stage === "cancel_semantic_route",
      ),
    ).toEqual([]);
    const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
      journey.config!,
    );
    expect((await workflow.deriveStage(Date.now())).kind).toBe("removed");
  }, 900_000);

  it("finds an honest commitment healthy, and source verification refuses to open a dispute of it", async () => {
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
    expect(journey.config).toBeUndefined();
    expect(followed.header.headerHash).toBe(journey.setup.headerHash);

    // The challenger's own replay is the operator's committed trace.
    const { fixture } = journey;
    const claim = await committedFollowerValidationClaim(fixture);
    const challengerDescriptor = validationTraceDescriptorDataFromCore(
      fixture.challengerTrace.tree.descriptor,
    );
    expect(claim.descriptor_membership.value).toEqual(challengerDescriptor);
    expect(followerClaimTakesOpenRoute(fixture, claim)).toBe(true);
    const direct = await openStagedValidationDispute(journey, {
      claim,
      challengerDescriptor,
    });
    await expect(direct.verifySource()).rejects.toThrow(
      "Validation trace descriptors cannot be disputed",
    );
    // `fraud_proofs/validation_trace/source_v1` takes the open route (the
    // claim's endpoints and source are valid), so it builds the game with
    // `validation_dispute_v1.open`, whose `descriptors_can_dispute` is the
    // only check the mirror's game differs on: equal descriptors fail it, the
    // terminal the mirror substitutes passes it.
    const operatorDescriptor = fixture.operatorTrace.tree.descriptor;
    const replayedDescriptor = fixture.challengerTrace.tree.descriptor;
    expect(
      midgardValidationDescriptorsCanDispute(
        operatorDescriptor,
        replayedDescriptor,
      ),
    ).toBe(false);
    expect(
      midgardValidationDescriptorsCanDispute(operatorDescriptor, {
        ...replayedDescriptor,
        terminalStateHash: Buffer.alloc(32, 0x5a),
      }),
    ).toBe(true);
    hooks.openIndisputable = true;
    // The thread is the transaction's only script input.
    await expect(direct.verifySource()).rejects.toThrow(
      "failed script execution Spend[0] the validator crashed / exited prematurely",
    );
  }, 900_000);
});
