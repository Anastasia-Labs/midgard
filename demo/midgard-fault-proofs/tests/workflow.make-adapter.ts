import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { vi } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { type CanonicalViolationDetection } from "../src/workflow/classification.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../src/workflow/orchestrator.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "./helpers/canonical-block-evidence-fixture.js";

export const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);

export const PROOF_TX_HASH = "a1".repeat(32);

export const REMOVAL_TX_HASH = "a2".repeat(32);

export const REFERENCE_OUT_REF = `${"b2".repeat(32)}#0`;

export const REFERENCE_SCRIPT_HASH = "c3".repeat(28);

export const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinalityAuthority = (
  overrides: Partial<{
    readonly deploymentIdentityDigest: string;
    readonly blueprintHash: string;
  }> = {},
): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest:
      overrides.deploymentIdentityDigest ?? DEPLOYMENT_FINGERPRINT,
    blueprintHash: overrides.blueprintHash ?? "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

export const canonicalEvidence = async (): Promise<CanonicalBlockEvidence> => {
  const fixture = await buildCanonicalBlockFixture({ transactions: [] });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
};

export const retainedDaSource = (
  payloadEnvelopeCbor: Buffer,
): RetainedDaPayloadSource => ({
  sourceId: "libp2p",
  fetchPayloadByHeaderHash: async () => ({
    ok: true,
    provenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
    sourceId: "libp2p",
    sourcePeerId: "peer-a",
    payloadEnvelopeCbor,
    attempts: [],
  }),
});

export const detection = (
  evidence: CanonicalBlockEvidence,
  violationId = "double-spend",
  overrides: Partial<CanonicalViolationDetection> = {},
): CanonicalViolationDetection => ({
  detectionId: `${violationId}-0`,
  headerHash: evidence.headerHash,
  violationId,
  position: 0n,
  ...overrides,
});

type AdapterControls = {
  readonly submit?: FraudProofFamilyWorkflowAdapter["submit"];
  readonly reconcile?: FraudProofFamilyWorkflowAdapter["reconcile"];
  readonly referenceScripts?: boolean;
  readonly durableRecovery?: Readonly<Record<string, string>>;
};

export const terminal = (headerHash: string): FraudProofWorkflowTerminal => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  category: "doubleSpend",
  headerHash,
  proofToken: {
    unit: "11".repeat(28),
    outRef: `${PROOF_TX_HASH}#0`,
    createdByTxHash: PROOF_TX_HASH,
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: REMOVAL_TX_HASH,
    removedStateQueueOutRef: `${"a3".repeat(32)}#0`,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: `${PROOF_TX_HASH}#0`,
  },
  economics: {
    operatorCredential: "22".repeat(28),
    proverCredential: "33".repeat(28),
    operatorBondInputOutRef: `${"a4".repeat(32)}#0`,
    operatorBondInputLovelace: "10000000",
    slashedLovelace: "10000000",
    proverRewardOutputOutRef: `${REMOVAL_TX_HASH}#0`,
    proverRewardLovelace: "5000000",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "4242",
    blockHash: "44".repeat(32),
    confirmationDepth: RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2,
  },
});

export const terminalVerifier: FraudProofWorkflowTerminalVerifier = {
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  verify: async ({ candidate }) => candidate,
};

export const makeAdapter = (
  controls: AdapterControls = {},
): FraudProofFamilyWorkflowAdapter => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
  category: "doubleSpend",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY,
  prepare: vi.fn(
    async ({
      evidence,
    }: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0]) => ({
      headerHash: evidence.headerHash,
      txIds: [],
    }),
  ),
  observe: vi.fn(
    async ({
      artifact,
      entries,
    }: Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0]) =>
      entries.some(
        (entry) =>
          entry.event.kind === "confirmed" &&
          entry.event.txHash === REMOVAL_TX_HASH,
      )
        ? {
            kind: "completed" as const,
            terminal: terminal(String(artifact.headerHash)),
          }
        : entries.some(
              (entry) =>
                entry.event.kind === "confirmed" &&
                entry.event.txHash === PROOF_TX_HASH,
            )
          ? {
              kind: "action_required" as const,
              action: { actionId: "remove", input: { step: 1 } },
            }
          : {
              kind: "action_required" as const,
              action: { actionId: "prove", input: { step: 0 } },
            },
  ),
  preflight: vi.fn(
    async ({
      action,
    }: Parameters<FraudProofFamilyWorkflowAdapter["preflight"]>[0]) => ({
      actionId: action.actionId,
      txHash: action.actionId === "prove" ? PROOF_TX_HASH : REMOVAL_TX_HASH,
      scriptExecution: "reference_scripts" as const,
      localUplcEvaluation: { status: "passed" as const, evaluator: "uplc-v1" },
      referenceScripts:
        controls.referenceScripts === false
          ? ([] as unknown as [
              {
                readonly role: string;
                readonly outRef: string;
                readonly scriptHash: string;
              },
            ])
          : ([
              {
                role: "family-step",
                outRef: REFERENCE_OUT_REF,
                scriptHash: REFERENCE_SCRIPT_HASH,
              },
            ] as const),
      ...(controls.durableRecovery === undefined
        ? {}
        : { durableRecovery: controls.durableRecovery }),
    }),
  ),
  submit: vi.fn(
    controls.submit ??
      (async ({ preflight }) => ({
        kind: "submitted" as const,
        txHash: preflight.txHash,
      })),
  ),
  reconcile: vi.fn(
    controls.reconcile ??
      (async ({ txHash }) =>
        txHash === undefined
          ? { kind: "conflict" as const, reason: "missing intended hash" }
          : { kind: "confirmed" as const, txHash }),
  ),
});

export const run = async ({
  evidence,
  adapter,
  journal,
  verifier = terminalVerifier,
  finalityAuthority = releaseFinalityAuthority(),
  now = () => new Date("2026-08-29T00:00:00.000Z"),
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly verifier?: FraudProofWorkflowTerminalVerifier;
  readonly finalityAuthority?: FraudProofReleaseFinalityAuthority;
  readonly now?: () => Date;
}) =>
  await runFraudProofWorkflow({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    evidence,
    detections: [detection(evidence)],
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: ["doubleSpend"],
    }),
    journal,
    terminalVerifier: verifier,
    releaseFinalityAuthority: finalityAuthority,
    now,
  });
