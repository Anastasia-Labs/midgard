import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  type CompleteCanonicalReplay,
  createCatalogueCompleteCanonicalReplay,
  createHeaderClassifier,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  headerDecisionCanonicalEvidence,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { unsafeCreateTransitionTraceEventAuthorityFromRawForTest } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";

import { bindJourneyEventAuthorities } from "./event-history-bindings.js";
import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import type { LocalHistoryEventStage } from "./history-event-local-staging.js";

/** Depth of the shallowest recorded transaction; one still pending is 0 deep. */
const recordedDepth = async ({
  deployment,
  recorded,
}: LocalHistoryEventStage) =>
  (
    await Promise.all(
      recorded.map(({ txHash }) =>
        deployment.emulator.getTransactionStatus(txHash),
      ),
    )
  ).reduce(
    (shallowest, status) =>
      Math.min(
        shallowest,
        status.status === "confirmed"
          ? (status.confirmation.confirmations ?? 0)
          : 0,
      ),
    Number.POSITIVE_INFINITY,
  );

/**
 * Advance the staging chain until its latest recorded transaction is `depth`
 * blocks deep: the installed classifier admits a raw L1 snapshot only at the
 * signed release depth, which a live chain reaches before a watcher acts.
 * The wait moves the clock past a header's commit window, so a case that
 * commits a header classifies it after the commit, as a watcher does.
 */
const awaitRecordedDepth = async (
  stage: LocalHistoryEventStage,
  depth: number,
) => {
  const missing = depth - (await recordedDepth(stage));
  if (missing > 0) {
    const { emulator, chain } = stage.deployment;
    emulator.awaitBlock(missing);
    // Pass the advanced clock through the chain clock callers synchronize with.
    await chain.awaitLedgerTime(emulator.now());
  }
  if ((await recordedDepth(stage)) < depth)
    throw new Error(
      "Staging chain did not reach the release observation depth",
    );
};

/**
 * Run the unmodified installed selector over an event-bearing retained block.
 * Unlike the transaction-only verifier, the deposit and withdrawal authorities
 * here read the actual published events: the raw L1 snapshot authority replays
 * the exact submitted transactions and scope outputs of the staging chain, and
 * the replay resolves the live hub oracle and event outputs through the same
 * deployment. Only local raw transport and finality attestation are synthetic.
 */
export const classifyLocalHistoryEventFixture = async (input: {
  stage: LocalHistoryEventStage;
  block: VerifiableJourneyBlock;
  predecessor: VerifiableJourneyBlock;
  history?: readonly VerifiableJourneyBlock[];
  /** Restrict the launch scope; the full installed catalogue by default. */
  replayer?: CompleteCanonicalReplay;
}) => {
  const { deployment } = input.stage;
  const retained = [input.block, input.predecessor, ...(input.history ?? [])];
  const retainedBlock = (headerHash: string) => {
    const block = retained.find((block) => block.headerHash === headerHash);
    if (block === undefined)
      throw new Error(`Missing actual retained ancestor ${headerHash}`);
    return block;
  };
  const deploymentFingerprint = deployment.manifest.manifestId;
  const { transition, history } = await bindJourneyEventAuthorities({
    manifest: deployment.manifest,
    blueprintJson: deployment.blueprintJson,
    deploymentInfo: deployment.deploymentInfo,
    headerHash: input.block.headerHash,
    proverCredential: input.stage.operatorVkey,
  });
  const releaseFinality = transition.releaseFinality;
  const policy = releaseFinality.policy;
  const hubOraclePolicyId = deployment.contracts.hubOracle.policyId;
  await awaitRecordedDepth(input.stage, policy.confirmationDepth);
  const transitionTraceEventAuthority =
    unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
      binding: transition,
      authority: input.stage.rawAuthority,
    });
  const replayer =
    input.replayer ??
    createCatalogueCompleteCanonicalReplay({
      lucid: deployment.operatorLucid,
      network: "Custom",
      hubOraclePolicyId,
      minimumConfirmationDepth: policy.confirmationDepth,
      owner: input.stage.operatorVkey,
      history,
    });
  const classifier = await createHeaderClassifier({
    deploymentFingerprint,
    replayer,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
    transitionTraceEventAuthority,
  });
  const sources: RetainedDaPayloadSource[] = [
    {
      sourceId: "retained-local",
      fetchPayloadByHeaderHash: async (headerHash) => {
        const retained = retainedBlock(headerHash);
        return {
          ok: true,
          sourceId: "retained-local",
          sourcePeerId: "local-test",
          attempts: [],
          payloadEnvelopeCbor: Buffer.from(retained.payloadEnvelopeCbor),
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "retained-local/local-test",
            grade: "security",
          },
        };
      },
    },
  ];
  const observation = authenticatedHeaderObservation(input.block);
  const decision = await classifyHeader({
    classifier,
    observation,
    predecessorObservation: authenticatedHeaderObservation(input.predecessor),
    sources,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: policy.confirmationDepth,
      }),
  });
  return {
    decision,
    evidence: await headerDecisionCanonicalEvidence(decision),
    verification: "full-installed-local-event-classification" as const,
  };
};
