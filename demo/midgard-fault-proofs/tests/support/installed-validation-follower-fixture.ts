import {
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  type EventKey,
  type ValidationClaimWitness,
  type ValidationTraceDescriptor,
} from "@al-ft/midgard-sdk";
import {
  buildValidationDisputeEvidenceBundle,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "@al-ft/midgard-validation";

import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import { buildRetainedValidationClaimWitness } from "../../src/transition-trace/witnesses.js";
import { committedValidationClaimEndpointsAndSourceAreValid } from "../../src/validation-dispute/claim-endpoints.js";
import {
  submitValidationDisputeOpen,
  submitValidationDisputeVerifySource,
} from "../../src/validation-dispute/submit.js";
import { network } from "./emulator/blueprints.js";
import { expectSingleUtxoWithUnit } from "./emulator/emulator-context.js";
import { replaceTerminalState } from "./emulator/validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";
import { type stageSuppliedValidationTraceDisputeJourney } from "./installed-validation-trace-dispute-journey.js";
import { submitInit } from "./legacy-submit-emulator.js";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
} from "./retained-reason-classifier.build-retained-plutus-fixture.js";

/**
 * The one-step this fixture's fault lands on: the CEK core step. A Plutus
 * execution failure is the only rejection the watcher routes to the
 * interactive validation-trace dispute; every other reason is a direct
 * catalogue route.
 */
export const FOLLOWER_VALIDATION_CEK_CORE_STEP = Object.freeze({
  resolverIndex: 11,
  semanticResolverIndex: 3,
});

/**
 * What the operator commits for a normal L2 transaction spending a PlutusV3
 * script input. `forgedCekCoreSuccessor`: the script runs an unbound
 * variable, and the operator keeps every replayed state but turns the
 * rejecting terminal, the CEK core step's successor, into an accepting one
 * (its retained work unchanged). `honest`: the script is the identity
 * program, and the operator commits the replayed trace it accepts under. A
 * normal source's committed descriptor must accept, so an honest commitment
 * of a normal transaction is an accepting one.
 */
export type FollowerValidationCommitment = "forgedCekCoreSuccessor" | "honest";

const forgeAcceptingTerminal = (
  replayed: DeterministicValidationMachineTrace,
): DeterministicValidationMachineTrace =>
  replaceTerminalState(replayed, {
    terminal: {
      ...replayed.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });

/**
 * A retained validation block and its predecessor, framed to be committed as
 * the first two blocks of an emulator state queue by `operatorVkey`: the
 * predecessor spans `[now, now + 1s]` and the challenged block starts where
 * it ends. The block's DA payload is what the watcher classifies and
 * captures; nothing here is a challenge.
 */
export const buildFollowerValidationDisputeFixture = async ({
  operatorVkey,
  now,
  commitment,
}: Readonly<{
  operatorVkey: string;
  now: number;
  commitment: FollowerValidationCommitment;
}>) => {
  let committed: DeterministicValidationMachineTrace | undefined;
  const build =
    commitment === "honest"
      ? buildRetainedPlutusIdentityFixture
      : buildRetainedPlutusUnboundVariableFixture;
  const retained = await build(
    { verdict: "accepted" },
    {
      operatorVkey,
      predecessorFrame: {
        operatorVkey,
        startTime: BigInt(now),
        endTime: BigInt(now + 1_000),
      },
      blockStartTimeMs: now + 1_000,
      blockEndTimeMs: now + 121_000,
      committedTrace: (replayed) =>
        (committed =
          commitment === "honest"
            ? replayed
            : forgeAcceptingTerminal(replayed)),
    },
  );
  const operatorTrace = committed!;
  // Measured, not assumed: the replay convicts the forged case's script
  // execution and accepts the honest one.
  const replayedRejection =
    commitment === "honest" ? null : RejectCodes.PlutusScriptInvalid;
  if (retained.replay.trace.rejectionCode !== replayedRejection)
    throw new Error(
      `Replay rejected with ${String(retained.replay.trace.rejectionCode)}, expected ${String(replayedRejection)}`,
    );
  const challengerTrace = retained.replay.trace;
  const eventKey: EventKey = {
    L2TransactionEventKey: { tx_id: retained.transaction.txId },
  };
  return {
    commitment,
    header: retained.block.header,
    headerHash: retained.block.headerHash,
    predecessorHeader: retained.predecessor.header,
    operatorTrace,
    challengerTrace,
    // An honest commitment has no fault to argue; it stages the same
    // resolution references so both polarities run one ledger.
    evidence:
      commitment === "honest"
        ? { oneStepArgument: FOLLOWER_VALIDATION_CEK_CORE_STEP }
        : buildValidationDisputeEvidenceBundle({
            operatorTrace,
            challengerTrace,
            currentTime: now + 2_000,
          }),
    eventKey,
    /** The retained classifier fixture the header and payload come from. */
    retained,
  };
};

export type FollowerValidationDisputeFixture = Awaited<
  ReturnType<typeof buildFollowerValidationDisputeFixture>
>;

/**
 * The operator's committed claim for the fixture's event, reopened from its
 * DA payload as a challenger would reopen it.
 */
export const committedFollowerValidationClaim = async (
  fixture: FollowerValidationDisputeFixture,
) =>
  (
    await buildRetainedValidationClaimWitness({
      reconstruction: await reconstructDaPayload({
        payloadEnvelopeCbor: fixture.retained.block.payloadEnvelopeCbor,
        expectedHeaderHash: fixture.headerHash,
      }),
      eventKey: fixture.eventKey,
    })
  ).claim;

type StagedFollowerJourney = Awaited<
  ReturnType<
    typeof stageSuppliedValidationTraceDisputeJourney<FollowerValidationDisputeFixture>
  >
>;

/**
 * The challenger's first moves on the staged journey's header without the
 * workflow: init, then open with `challengerDescriptor` against `claim`.
 * Source verification is returned unsubmitted.
 */
export const openStagedValidationDispute = async (
  journey: StagedFollowerJourney,
  {
    claim,
    challengerDescriptor,
  }: Readonly<{
    claim: ValidationClaimWitness;
    challengerDescriptor: ValidationTraceDescriptor;
  }>,
) => {
  const common = {
    lucid: journey.targetChallengerLucid,
    blueprint: journey.realBlueprint,
    deploymentInfo: journey.deploymentInfo,
    network,
    signer: journey.challengerSigner,
  };
  const init = await submitInit({
    ...common,
    fraudCategory: "validationTraceDispute",
    fraudulentBlockOutRef: journey.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: journey.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const firstStep = await expectSingleUtxoWithUnit(
    journey.targetChallengerLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
  const open = await submitValidationDisputeOpen({
    ...common,
    threadOutRef: outRefLabel(firstStep),
    stateQueueBlockOutRef: journey.setup.fraudulentBlockOutRef,
    claim,
    challengerDescriptor,
    validityRange: journey.validityRange(),
    awaitConfirmation: true,
  });
  return {
    open,
    /** The source-verification thread the open created. */
    verifySource: async () =>
      await submitValidationDisputeVerifySource({
        ...common,
        threadOutRef: open.nextThreadOutRef,
        sourceReferenceScriptUtxo: journey.referenceScripts.control.source,
        validityRange: journey.validityRange(),
        awaitConfirmation: true,
      }),
  };
};

/**
 * Whether source verification of `claim` against the fixture's header takes
 * the open route (endpoints and source valid) rather than the direct award.
 */
export const followerClaimTakesOpenRoute = (
  fixture: FollowerValidationDisputeFixture,
  claim: ValidationClaimWitness,
) => committedValidationClaimEndpointsAndSourceAreValid(fixture.header, claim);

export { validationTraceMaterial } from "../../src/workflow/challenge-authority.js";
