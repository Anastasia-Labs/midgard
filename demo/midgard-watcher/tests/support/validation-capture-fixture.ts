import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofRawL1RollbackCursor,
  createHeaderClassifier,
  createTransitionTraceEventAuthority,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofL1Source,
  type FraudProofRawL1SnapshotAuthority,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  buildRetainedPlutusIdentityFixture,
  buildRetainedPlutusUnboundVariableFixture,
  captureRetainedPlutusIdentityOrigins,
} from "@al-ft/midgard-fault-proofs/test-support/retained-reason-classifier";
import { TRANSITION_HISTORY_FIXTURE_PARAMETERS } from "@al-ft/midgard-fault-proofs/test-support/transition-history-fixture";
import { requireTransitionTraceL1Events } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "./deployment-authority-fixture.js";
import {
  followerUserEventsDeployment,
  forcedOrderTransaction,
  userEventIdOf,
} from "./follower-user-events-fixture.js";
import {
  createSyntheticStateQueueObservationFixture,
  type SyntheticStateQueueObservationCapture,
} from "./state-queue-observation-fixture.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "./user-event-forced-order-fixture.js";

/**
 * The retained classifier fixture's header, committed on a synthetic chain
 * and observed from an L1 follower store; a forced case's order is an
 * ordinary mint under the deployment's tx-order scripts, in the commit's
 * block before the commit.
 */
export const setupValidationCapture = async (kind: "normal" | "forced") => {
  const retained =
    kind === "normal"
      ? await buildRetainedPlutusUnboundVariableFixture({ verdict: "accepted" })
      : await buildRetainedPlutusIdentityFixture(
          {
            verdict: "rejected",
            reason: { PlutusExecutionFailed: { execution_index: 0n } },
          },
          { sourceKind: "forced" },
        );
  const identity = followerUserEventsDeployment().deploymentIdentity;
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: identity.manifestId,
      network: identity.network,
      blueprintHash: identity.blueprintHash,
      programCommitments: identity.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  const queue = await createSyntheticStateQueueObservationFixture({
    header: retained.block.header,
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
    ...(kind === "normal"
      ? {}
      : {
          composeCommitBlock: ({ deployment, commitTransactionCbor }) => ({
            transactions: [
              forcedOrderTransaction(
                deployment,
                userEventIdOf(retained.orderKey),
                genuineUserEventForcedPayloadForCanonicalTx(
                  encodeMidgardForcedTxCanonical(
                    decodeMidgardNativeTxFullFromCanonicalCbor(
                      retained.transaction.canonicalCbor,
                    ),
                  ),
                ),
              ),
              commitTransactionCbor,
            ],
          }),
        }),
  });
  const deployment = queue.deployment.deployment;
  const directory = await mkdtemp("/var/tmp/replay-capture-release-");
  const authorityPath = join(directory, "authority.json");
  const ruleBundlePath = join(directory, "rules.json");
  await writeFile(
    authorityPath,
    JSON.stringify({
      signedIdentity: deployment.signedIdentity,
      policy: deployment.policy,
      trustRoots: deployment.trustRoots,
      durableMarker: deployment.marker,
    }),
  );
  await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
  const loadAuthority = () =>
    loadWatcherVerifiedDeploymentAuthority({
      path: authorityPath,
      ruleBundlePath,
    });
  return {
    retained,
    queue,
    deploymentAuthority: await loadAuthority(),
    loadAuthority,
    close: async () => {
      await queue.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

export type ValidationCaptureContext = Awaited<
  ReturnType<typeof setupValidationCapture>
>;

/**
 * Classifies the retained header as observed from the follower store; the
 * retained predecessor and classifier-origin context remain the ordinary
 * classifier fixture's. The decision must select a validation-trace dispute.
 */
export const classifyValidationCapture = async (
  context: Pick<ValidationCaptureContext, "retained" | "deploymentAuthority">,
  captured: Pick<
    SyntheticStateQueueObservationCapture,
    "observation" | "header"
  >,
  deploymentAuthority = context.deploymentAuthority,
) => {
  const decision = await classifyRetainedValidationHeader(
    context,
    captured,
    deploymentAuthority,
  );
  expect(decision).toMatchObject({
    decision: "fault_detected",
    category: "validationTraceDispute",
    headerHash: captured.header.headerHash,
  });
  return decision;
};

/** The classifier's decision on the retained header, whatever it is. */
export const classifyRetainedValidationHeader = async (
  context: Pick<ValidationCaptureContext, "retained" | "deploymentAuthority">,
  captured: Pick<
    SyntheticStateQueueObservationCapture,
    "observation" | "header"
  >,
  deploymentAuthority = context.deploymentAuthority,
) => {
  const observation = authenticatedHeaderObservation(context.retained.block, {
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: captured.observation.sourceId,
      grade: "security",
    },
    chainPoint: {
      blockHash: captured.header.observedBlockHash,
      slot: BigInt(captured.header.observedSlot),
    },
    confirmationDepth: Number(captured.header.finalityDepth),
  });
  const releaseFinalityAuthority = watcherDeploymentReleaseFinalityAuthority(
    deploymentAuthority.deploymentIdentity,
  );
  let transitionTraceEventAuthority;
  if (context.retained.block.header.forcedTransactionCount > 0n) {
    const seedHandle = await captureRetainedPlutusIdentityOrigins(
      context.retained,
    );
    const seed = requireTransitionTraceL1Events(seedHandle).snapshot;
    const hub = seed.scopes.find(({ role }) => role === "hub_oracle")!;
    const raw: FraudProofRawL1SnapshotAuthority = {
      authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
      capture: async (request) => ({
        ...seed,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        cursor: {
          ...seed.cursor,
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            ...request,
            sourceId: seed.provenance.sourceId,
            pointId: seed.cursor.point.pointId,
          }),
        },
        scopes: request.scopes.map((scope) => ({
          ...scope,
          utxos:
            seed.scopes.find(({ address }) => address === scope.address)
              ?.utxos ?? [],
        })),
        historyUnits: request.historyUnits,
        history: request.historyUnits.map((unit) => {
          const history = seed.history.find((entry) => entry.unit === unit);
          if (history === undefined)
            throw new Error(
              "Ordinary retained fixture cannot supply another unit",
            );
          return history;
        }),
      }),
    };
    const releaseFinality = await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
    });
    // Only configuration fields consumed by the existing raw-port fixture are
    // supplied here. This is not a claimed deployed workflow binding.
    const binding = {
      deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
      blueprintHash: deploymentAuthority.deploymentIdentity.blueprintHash,
      network: "Preprod",
      releaseFinality,
      resolvedContracts: {
        contracts: {
          transitionTrace: { history: TRANSITION_HISTORY_FIXTURE_PARAMETERS },
        },
        hubOraclePolicyId: getAddressDetails(hub.address).paymentCredential!
          .hash,
      },
      definition: { headerHash: context.retained.block.headerHash },
    } as Parameters<typeof createTransitionTraceEventAuthority>[0]["binding"];
    transitionTraceEventAuthority = createTransitionTraceEventAuthority({
      binding,
      // The retained classifier fixture's originating-event snapshot is the
      // L1 source's inclusion-depth authority.
      l1: { snapshotAuthority: () => raw } as unknown as FraudProofL1Source,
    });
  }
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentAuthority.deploymentIdentity.manifestId,
    replayer: VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority,
    ...(transitionTraceEventAuthority === undefined
      ? {}
      : { transitionTraceEventAuthority }),
  });
  const sources = [
    {
      sourceId: "ordinary-retained-capture",
      fetchPayloadByHeaderHash: async (headerHash: string) => {
        const block = [
          context.retained.block,
          context.retained.predecessor,
        ].find((entry) => entry.headerHash === headerHash);
        if (block === undefined)
          throw new Error("Ordinary retained fixture requested another header");
        return {
          ok: true as const,
          sourceId: "ordinary-retained-capture",
          sourcePeerId: "unit",
          attempts: [],
          payloadEnvelopeCbor: block.payloadEnvelopeCbor,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "ordinary-retained-capture/unit",
            grade: "security" as const,
          },
        };
      },
    },
  ];
  const decision = await classifyHeader({
    classifier,
    observation,
    sources,
    predecessorObservation: authenticatedHeaderObservation(
      context.retained.predecessor,
    ),
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth:
          DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      }),
  });
  return decision;
};
