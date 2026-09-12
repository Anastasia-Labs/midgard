import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  type CompleteCanonicalReplay,
  computeFraudProofReleaseFinalityPolicyDigest,
  createCatalogueCompleteCanonicalReplay,
  createHeaderClassifier,
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  headerDecisionCanonicalEvidence,
  type RetainedDaPayloadSource,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { unsafeCreateCrossBlockSettlementAuthorityFromRawForTest } from "@al-ft/midgard-fault-proofs/test-support/cross-block-settlement-authority";
import { unsafeCreateTransitionTraceEventAuthorityFromRawForTest } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";

import type { VerifiableJourneyBlock } from "./fixture-verification.js";
import type { LocalHistoryEventStage } from "./history-event-local-staging.js";

const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

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
  const releaseFinality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: deployment.manifest.artifacts.blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const hubOraclePolicyId = deployment.contracts.hubOracle.policyId;
  const directory = await mkdtemp("/var/tmp/midgard-history-event-local-");
  try {
    const historySource = createHistoricalNativeScriptHistorySource({
      providerRoster: createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint,
        providers: [
          {
            sourceId: "retained-local-a",
            authorityEndpoint: "https://retained-a.example.test",
            operatorIdentitySha256: "aa".repeat(32),
          },
          {
            sourceId: "retained-local-b",
            authorityEndpoint: "https://retained-b.example.test",
            operatorIdentitySha256: "bb".repeat(32),
          },
        ],
      }),
    });
    const checkpointStore = createSqliteHistoricalNativeScriptCheckpointStore({
      path: join(directory, "history.sqlite"),
      rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
    });
    // Only the fields raw authority admission consumes, as in the established
    // catalogue-retained classifier test. The deployment identity is the real
    // published manifest, so a foreign snapshot cannot be admitted here.
    const bindingFields = {
      deploymentFingerprint,
      blueprintHash: releaseFinality.blueprintHash,
      network: "Custom" as const,
      releaseFinality,
      resolvedContracts: { hubOraclePolicyId },
      definition: { headerHash: input.block.headerHash },
    };
    const transitionTraceEventAuthority =
      unsafeCreateTransitionTraceEventAuthorityFromRawForTest({
        binding: bindingFields as Parameters<
          typeof unsafeCreateTransitionTraceEventAuthorityFromRawForTest
        >[0]["binding"],
        authority: input.stage.rawAuthority,
      });
    const settlementAuthority =
      unsafeCreateCrossBlockSettlementAuthorityFromRawForTest({
        binding: bindingFields as Parameters<
          typeof unsafeCreateCrossBlockSettlementAuthorityFromRawForTest
        >[0]["binding"],
        raw: input.stage.rawAuthority,
        historySource,
      });
    const replayer =
      input.replayer ??
      createCatalogueCompleteCanonicalReplay({
        lucid: deployment.operatorLucid,
        network: "Custom",
        hubOraclePolicyId,
        minimumConfirmationDepth: policy.confirmationDepth,
        owner: input.stage.operatorVkey,
      });
    const classifier = await createHeaderClassifier({
      deploymentFingerprint,
      replayer,
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
      historicalReplayAuthority: { checkpointStore, historySource },
      settlementAuthority,
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
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
};
