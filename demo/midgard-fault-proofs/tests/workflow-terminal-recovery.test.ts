import { expect, it } from "vitest";

import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
} from "../src/workflow/family-l1-observation.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";

const hash = (byte: string) => byte.repeat(32);
const candidate: FraudProofWorkflowTerminal = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  category: "daHashPreimage",
  headerHash: "ab".repeat(28),
  proofToken: {
    unit: "11".repeat(28),
    outRef: `${hash("22")}#0`,
    createdByTxHash: hash("22"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: hash("33"),
    removedStateQueueOutRef: `${hash("44")}#0`,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: `${hash("22")}#0`,
  },
  economics: {
    operatorCredential: "55".repeat(28),
    proverCredential: "66".repeat(28),
    operatorBondInputOutRef: `${hash("77")}#0`,
    operatorBondInputLovelace: "10000000",
    slashedLovelace: "10000000",
    proverRewardOutputOutRef: `${hash("33")}#0`,
    proverRewardLovelace: "5000000",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: { slot: "4242", blockHash: hash("88"), confirmationDepth: 30 },
};
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: hash("99"),
  blueprintHash: hash("aa"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};
const verify = async (
  observed: FraudProofWorkflowTerminal,
  stored = candidate,
) => {
  const verifier = createFraudProofFamilyAuthenticatedL1TerminalVerifier({
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category: "daHashPreimage",
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async (): Promise<never> => {
        throw new Error("unused");
      },
    },
    observeHeader: async (): Promise<never> => {
      throw new Error("unused");
    },
    transactionConfirmed: async () => true,
    observe: async () => ({
      provenance: {
        trustClass: "authenticated_cardano_l1",
        sourceId: "local-kupmios",
        grade: "security",
      } as const,
      stage: { kind: "removed", terminal: observed } as const,
    }),
  });
  return verifier.verify({
    identity: {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: hash("99"),
      category: "daHashPreimage",
      target: { kind: "state_queue_header", headerHash: candidate.headerHash },
    },
    workflowId: hash("bb"),
    candidate: stored,
    releaseFinality,
    artifact: {},
    entries: [],
  });
};

it("preserves the exact durable terminal after independently reconfirming greater depth", async () => {
  const observed = {
    ...candidate,
    observedAt: { ...candidate.observedAt, confirmationDepth: 45 },
  };
  expect(await verify(observed)).toBe(candidate);
});

it.each([
  {
    ...candidate,
    observedAt: { ...candidate.observedAt, blockHash: hash("cc") },
  },
  { ...candidate, observedAt: { ...candidate.observedAt, slot: "4243" } },
  {
    ...candidate,
    economics: { ...candidate.economics, proverRewardLovelace: "5000001" },
  },
  {
    ...candidate,
    correction: { ...candidate.correction, removalTxHash: hash("dd") },
  },
])(
  "rejects altered terminal facts or removal inclusion: %j",
  async (observed) => {
    await expect(verify(observed)).rejects.toThrow(
      "differs from independent L1 observation",
    );
  },
);

it("rejects insufficient or regressed confirmation depth", async () => {
  await expect(
    verify({
      ...candidate,
      observedAt: { ...candidate.observedAt, confirmationDepth: 29 },
    }),
  ).rejects.toThrow("depth");
  const stored = {
    ...candidate,
    observedAt: { ...candidate.observedAt, confirmationDepth: 40 },
  };
  await expect(verify(candidate, stored)).rejects.toThrow("depth");
});
