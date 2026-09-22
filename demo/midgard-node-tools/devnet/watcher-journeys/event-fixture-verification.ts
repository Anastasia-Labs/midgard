import { mkdtemp, readFile, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  authenticatedStateQueueObservationDigest,
  bindFraudProofWorkflowDeployment,
  classifyHeader,
  createCatalogueCompleteCanonicalReplay,
  createCrossBlockSettlementAuthority,
  createHeaderClassifier,
  createHistoricalNativeScriptHistorySource,
  createHistoricalNativeScriptProviderRoster,
  createSqliteHistoricalNativeScriptCheckpointStore,
  createTransitionTraceEventAuthority,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  headerDecisionCanonicalEvidence,
  type RetainedDaPayloadSource,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  CrossBlockDuplicateEventStep02DatumSchema,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import type { JourneyContext } from "./fixture.js";
import type { VerifiableJourneyBlock } from "./fixture-verification.js";

/**
 * Local fixture selection using the installed catalogue and actual current L1
 * event authorities. The proposed payload remains uncommitted: this establishes
 * fixture selection, not automatic discovery or completed real-chain proof.
 * No transaction is constructed or submitted by this verifier.
 */
export const classifyJourneyEventFixture = async (input: {
  context: JourneyContext;
  block: VerifiableJourneyBlock;
  predecessor: VerifiableJourneyBlock;
  history?: readonly VerifiableJourneyBlock[];
  historyProviders: Parameters<
    typeof createHistoricalNativeScriptProviderRoster
  >[0]["providers"];
}) => {
  const { deployment } = input.context;
  const deploymentFingerprint = deployment.manifest.manifestId;
  const bindingInput = {
    manifest: deployment.manifest,
    blueprintJson: deployment.blueprintJson,
    deploymentInfo: deployment.deploymentInfo,
    headerHash: input.block.headerHash,
    proverCredential: "00".repeat(28),
  };
  const [transition, settlement] = await Promise.all([
    bindFraudProofWorkflowDeployment({
      ...bindingInput,
      category: "transitionTrace",
      stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
    }),
    bindFraudProofWorkflowDeployment({
      ...bindingInput,
      category: "crossBlockDuplicateEvent",
      stepDatumSchemas: [
        FraudProofComputationThreadStepDatum,
        CrossBlockDuplicateEventStep02DatumSchema,
      ],
    }),
  ]);
  if (
    transition.deploymentFingerprint !== deploymentFingerprint ||
    settlement.deploymentFingerprint !== deploymentFingerprint
  )
    throw new Error(
      "Event fixture authorities differ from its deployed identity",
    );
  const directory = await mkdtemp("/var/tmp/midgard-event-fixture-");
  try {
    const historySource = createHistoricalNativeScriptHistorySource({
      providerRoster: createHistoricalNativeScriptProviderRoster({
        deploymentFingerprint,
        providers: input.historyProviders,
      }),
    });
    const checkpointStore = createSqliteHistoricalNativeScriptCheckpointStore({
      path: join(directory, "history.sqlite"),
      rollbackAuthenticationKey: Buffer.alloc(32, 0x90),
    });
    const source = {
      sourceId: "journey-event-fixture",
      kupoHttpUrl: input.context.kupoUrl,
      ogmiosUrl: input.context.ogmiosUrl,
      timeoutMs: 120_000,
    };
    const settlementAuthority = createCrossBlockSettlementAuthority({
      binding: settlement,
      source,
      historySource,
      checkpointStore,
    });
    const transitionTraceEventAuthority = createTransitionTraceEventAuthority({
      binding: transition,
      source,
    });
    const policy = transition.releaseFinality.policy;
    const replayer = createCatalogueCompleteCanonicalReplay({
      lucid: deployment.operatorLucid,
      network: "Custom",
      hubOraclePolicyId: deployment.contracts.hubOracle.policyId,
      minimumConfirmationDepth: policy.confirmationDepth,
      owner: input.block.header.operatorVkey,
    });
    const classifier = await createHeaderClassifier({
      deploymentFingerprint,
      replayer,
      settlementAuthority,
      transitionTraceEventAuthority,
      historicalReplayAuthority: { historySource, checkpointStore },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => transition.releaseFinality,
      },
    });
    const sources: RetainedDaPayloadSource[] = [
      {
        sourceId: "fixture-retained",
        fetchPayloadByHeaderHash: async (headerHash) => {
          const retained = [
            input.block,
            input.predecessor,
            ...(input.history ?? []),
          ].find((block) => block.headerHash === headerHash);
          let payloadEnvelopeCbor: Buffer;
          if (retained !== undefined) {
            payloadEnvelopeCbor = Buffer.from(retained.payloadEnvelopeCbor);
          } else {
            if (!/^[0-9a-f]{56}$/u.test(headerHash))
              throw new Error("Event fixture requested an invalid archive key");
            const records = await Promise.all(
              ["a", "b"].map((role) =>
                readFile(
                  join(
                    input.context.runDirectory,
                    "history",
                    role,
                    "records",
                    `${headerHash}.json`,
                  ),
                  "utf8",
                ),
              ),
            );
            if (records[0] !== records[1])
              throw new Error("Retained event fixture archives disagree");
            const record = JSON.parse(records[0]!);
            if (
              record.deploymentFingerprint !== deploymentFingerprint ||
              record.headerHash !== headerHash ||
              typeof record.payloadEnvelopeCborHex !== "string" ||
              !/^(?:[0-9a-f]{2})+$/u.test(record.payloadEnvelopeCborHex)
            )
              throw new Error(
                "Retained event fixture archive identity changed",
              );
            payloadEnvelopeCbor = Buffer.from(
              record.payloadEnvelopeCborHex,
              "hex",
            );
          }
          return {
            ok: true,
            sourceId: "fixture-retained",
            sourcePeerId: "local",
            attempts: [],
            payloadEnvelopeCbor,
            provenance: {
              trustClass: "public_or_permissionless_da",
              sourceId: "fixture-retained/local",
              grade: "security",
            },
          };
        },
      },
    ];
    const observation = authenticatedHeaderObservation(input.block, {
      confirmationDepth: policy.confirmationDepth,
    });
    const decision = await classifyHeader({
      classifier,
      observation,
      predecessorObservation: authenticatedHeaderObservation(
        input.predecessor,
        { confirmationDepth: policy.confirmationDepth },
      ),
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
      verification: "local-classification-with-live-event-authorities" as const,
    };
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
};
