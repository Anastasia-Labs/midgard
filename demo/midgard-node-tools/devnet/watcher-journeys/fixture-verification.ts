import { createHash } from "node:crypto";

import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  headerDecisionCanonicalEvidence,
  requireRunnableHeaderFault,
} from "@al-ft/midgard-fault-proofs";
import { authenticatedHeaderObservation } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { classifyRetainedReasonFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-reason-classifier";
import type * as SDK from "@al-ft/midgard-sdk";

type RetainedClassifierInput = Parameters<
  typeof classifyRetainedReasonFixture
>[0];
export type VerifiableJourneyBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Uint8Array;
};

const deploymentFingerprint = "d1".repeat(32);
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};

/**
 * Diagnostic retained-byte classification with an explicitly named replay scope.
 * This establishes local classifier binding, not full-catalogue selection or a
 * runnable on-chain proof. Live readiness must additionally use the installed
 * application and its exact proof preparer against staged protocol history.
 */
export const verifyJourneyFixture = async (input: {
  category: SDK.FraudProofCatalogueCategoryName;
  replayer: RetainedClassifierInput["replayer"];
  block: VerifiableJourneyBlock;
  predecessor?: VerifiableJourneyBlock;
  history?: RetainedClassifierInput["history"];
  expected?: "fault" | "healthy";
  replayContext?: RetainedClassifierInput["replayContext"];
  transitionTraceEventAuthority?: RetainedClassifierInput["transitionTraceEventAuthority"];
}) => {
  const { block, predecessor } = input;
  const result = await classifyRetainedReasonFixture({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
    deploymentFingerprint,
    releaseFinalityAuthority,
    replayer: input.replayer,
    replayContext: input.replayContext,
    history: input.history,
    transitionTraceEventAuthority: input.transitionTraceEventAuthority,
    ...(predecessor === undefined
      ? {}
      : {
          predecessor: {
            observation: authenticatedHeaderObservation(predecessor),
            payloadEnvelopeCbor: Buffer.from(predecessor.payloadEnvelopeCbor),
          },
        }),
  });
  const decision = result.decision;
  const expected = input.expected ?? "fault";
  if (expected === "healthy") {
    if (decision.decision !== "healthy")
      throw new Error(
        `Valid ${input.category} control classified as ${JSON.stringify(decision)}`,
      );
  } else {
    const fault = requireRunnableHeaderFault(decision);
    if (fault.category !== input.category)
      throw new Error(
        `Expected ${input.category}; production classifier selected ${fault.category}`,
      );
  }
  const digest = createHash("sha256")
    .update(block.payloadEnvelopeCbor)
    .digest("hex");
  if (
    decision.headerHash !== block.headerHash ||
    decision.payloadEnvelopeSha256 !== digest
  )
    throw new Error(
      "Fixture decision is not bound to its exact header and retained bytes",
    );
  const evidence = await headerDecisionCanonicalEvidence(decision);
  return {
    ...result,
    evidence,
    verification: "scoped-retained-classification" as const,
  };
};
