import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { parseArgs } from "../src/bin.js";
import {
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  type CanonicalBlockEvidenceV1,
} from "../src/evidence/canonical-block-evidence-v1.js";
import { sealManifestBoundNetworkIdRuntimeV1 } from "../src/network-id/workflow-adapter-v1.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import {
  type CanonicalViolationDetectionV1,
  classifyCanonicalBlockViolationsV1,
  FRAUD_PROOF_CLASSIFICATION_RULES_V1,
} from "../src/workflow/classification-v1.js";
import {
  productionWorkflowReadinessReportV1,
  runProductionFraudProofWorkflowCliV1,
} from "../src/workflow/cli-v1.js";
import {
  COMPLETE_CANONICAL_REPLAY_V1,
  type CompleteCanonicalReplayV1,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
  DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../src/workflow/deployment-manifest-binding-v1.js";
import {
  computeFraudProofWorkflowIdV1,
  ConcurrentFraudProofWorkflowWriteErrorV1,
  DirectoryFraudProofWorkflowJournalStoreV1,
  FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_V1_SCHEMA_VERSION,
  type FraudProofWorkflowIdentityV1,
  type FraudProofWorkflowJournalEntryV1,
  type FraudProofWorkflowJournalStoreV1,
  type FraudProofWorkflowTerminalV1,
  journalJsonDigestV1,
  MemoryFraudProofWorkflowJournalStoreV1,
  normalizeJournalJsonV1,
  validateFraudProofWorkflowJournalV1,
} from "../src/workflow/journal-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
  runFraudProofWorkflowV1,
} from "../src/workflow/orchestrator-v1.js";
import { PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1 } from "../src/workflow/production-actuation-permit-v1.js";
import {
  MissingProductionWorkflowAdaptersErrorV1,
  PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1,
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
  validateProductionWorkflowAdapterCoverageV1,
} from "../src/workflow/production-adapters-v1.js";
import { PRODUCTION_WORKFLOW_FUNDING_RESERVATION_PERMIT_V1 } from "../src/workflow/production-funding-reservation-permit-v1.js";
import { createDoubleSpendProductionWorkflowRunnerV1 } from "../src/workflow/production-runtime-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigestV1,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthorityV1,
} from "../src/workflow/release-finality-policy-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);
const PROOF_TX_HASH = "a1".repeat(32);
const REMOVAL_TX_HASH = "a2".repeat(32);
const REFERENCE_OUT_REF = `${"b2".repeat(32)}#0`;
const REFERENCE_SCRIPT_HASH = "c3".repeat(28);
const RELEASE_FINALITY_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinalityAuthority = (
  overrides: Partial<{
    readonly deploymentIdentityDigest: string;
    readonly releaseIdentityDigest: string;
  }> = {},
): FraudProofReleaseFinalityAuthorityV1 => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest:
      overrides.deploymentIdentityDigest ?? DEPLOYMENT_FINGERPRINT,
    releaseIdentityDigest: overrides.releaseIdentityDigest ?? "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const canonicalEvidence = async (): Promise<CanonicalBlockEvidenceV1> => {
  const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
};

const retainedDaSource = (
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

const detection = (
  evidence: CanonicalBlockEvidenceV1,
  violationId = "double-spend",
  overrides: Partial<CanonicalViolationDetectionV1> = {},
): CanonicalViolationDetectionV1 => ({
  detectionId: `${violationId}-0`,
  headerHash: evidence.headerHash,
  violationId,
  position: 0n,
  ...overrides,
});

type AdapterControls = {
  readonly submit?: FraudProofFamilyWorkflowAdapterV1["submit"];
  readonly reconcile?: FraudProofFamilyWorkflowAdapterV1["reconcile"];
  readonly referenceScripts?: boolean;
  readonly durableRecovery?: Readonly<Record<string, string>>;
};

const terminal = (headerHash: string): FraudProofWorkflowTerminalV1 => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_V1_SCHEMA_VERSION,
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
    confirmationDepth: 30,
  },
});

const terminalVerifier: FraudProofWorkflowTerminalVerifierV1 = {
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  verify: async ({ candidate }) => candidate,
};

const makeAdapter = (
  controls: AdapterControls = {},
): FraudProofFamilyWorkflowAdapterV1 => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  category: "doubleSpend",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  prepare: vi.fn(
    async ({
      evidence,
    }: Parameters<FraudProofFamilyWorkflowAdapterV1["prepare"]>[0]) => ({
      headerHash: evidence.headerHash,
      txIds: [],
    }),
  ),
  observe: vi.fn(
    async ({
      artifact,
      entries,
    }: Parameters<FraudProofFamilyWorkflowAdapterV1["observe"]>[0]) =>
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
    }: Parameters<FraudProofFamilyWorkflowAdapterV1["preflight"]>[0]) => ({
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

const run = async ({
  evidence,
  adapter,
  journal,
  verifier = terminalVerifier,
  finalityAuthority = releaseFinalityAuthority(),
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly adapter: FraudProofFamilyWorkflowAdapterV1;
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly verifier?: FraudProofWorkflowTerminalVerifierV1;
  readonly finalityAuthority?: FraudProofReleaseFinalityAuthorityV1;
}) =>
  await runFraudProofWorkflowV1({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    evidence,
    detections: [detection(evidence)],
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [adapter],
      launchScope: ["doubleSpend"],
    }),
    journal,
    terminalVerifier: verifier,
    releaseFinalityAuthority: finalityAuthority,
    now: () => new Date("2026-08-29T00:00:00.000Z"),
  });

describe("Q55/W-O6 deterministic violation classification", () => {
  it("covers every registered family exactly once in catalogue order", () => {
    expect(
      FRAUD_PROOF_CLASSIFICATION_RULES_V1.map((rule) => rule.category),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    const identifiers = FRAUD_PROOF_CLASSIFICATION_RULES_V1.flatMap((rule) => [
      ...rule.violationIds,
    ]);
    expect(new Set(identifiers).size).toBe(identifiers.length);
  });

  it("selects the earliest position, then stable family order", async () => {
    const evidence = await canonicalEvidence();
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: [
        detection(evidence, "mint-authorization", {
          detectionId: "mint-late",
          position: 9n,
        }),
        detection(evidence, "invalid-range", {
          detectionId: "range-first",
          position: 2n,
        }),
        detection(evidence, "double-spend", {
          detectionId: "double-first",
          position: 2n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "doubleSpend",
      selected: { detectionId: "double-first" },
    });
  });

  it("maps unknown earliest violations to unprovable_gap, never verified", async () => {
    const evidence = await canonicalEvidence();
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: [
        detection(evidence, "unknown-launch-fault", {
          detectionId: "gap",
          position: 0n,
        }),
        detection(evidence, "double-spend", {
          detectionId: "later-proof",
          position: 1n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "unprovable_gap",
      selected: {
        detectionId: "gap",
        reason: "unregistered_violation",
      },
    });
  });

  it("does not promote an empty partial detector result to verified", async () => {
    const evidence = await canonicalEvidence();
    await expect(
      classifyCanonicalBlockViolationsV1({ evidence, detections: [] }),
    ).resolves.toMatchObject({ decision: "no_fault_detected" });
  });

  it("rejects duplicate detector identities and cross-header detections", async () => {
    const evidence = await canonicalEvidence();
    const duplicate = detection(evidence);
    await expect(
      classifyCanonicalBlockViolationsV1({
        evidence,
        detections: [duplicate, duplicate],
      }),
    ).rejects.toThrow("duplicate canonical violation detectionId");
    await expect(
      classifyCanonicalBlockViolationsV1({
        evidence,
        detections: [
          detection(evidence, "double-spend", { headerHash: h32(9) }),
        ],
      }),
    ).rejects.toThrow("targets header");
  });
});

describe("Q51/W-O4 resumable workflow", () => {
  it("runs from authenticated L1 plus public retained DA with no private evidence input", async () => {
    const sharedInput = outRefCbor(61, 0n);
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 1n }),
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 2n }),
      ],
    });
    const adapter = makeAdapter();
    const result = await runFraudProofWorkflowFromRetainedDaV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      observation: authenticatedHeaderObservationV1(fixture),
      sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      registry: createFraudProofWorkflowRegistryV1({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
      terminalVerifier,
      releaseFinalityAuthority: releaseFinalityAuthority(),
      now: () => new Date("2026-08-29T00:00:00.000Z"),
    });
    expect(result.kind).toBe("completed");
  });

  it("rejects a caller-authored partial detector disguised as complete replay", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const forgedReplayer = {
      replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
      launchScope: ["doubleSpend"] as const,
      replay: async () => ({
        replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
        launchScope: ["doubleSpend"] as const,
        headerHash: fixture.headerHash,
        payloadEnvelopeSha256: "00".repeat(32),
        payloadSha256: "00".repeat(32),
        context: null,
        detections: [],
      }),
    } satisfies CompleteCanonicalReplayV1;
    await expect(
      runFraudProofWorkflowFromRetainedDaV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservationV1(fixture),
        sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
        replayer: forgedReplayer,
        registry: createFraudProofWorkflowRegistryV1({
          adapters: [makeAdapter()],
          launchScope: ["doubleSpend"],
        }),
        journal: new MemoryFraudProofWorkflowJournalStoreV1(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      }),
    ).rejects.toThrow("closed canonical replay bundle");
  });

  it("completely replays network-id faults from canonical retained DA", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        buildFixtureTransactionV1({
          spendInputs: [],
          fee: 1n,
          networkId: 1n,
        }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
    });
    const decision =
      await NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    expect(decision.detections).toEqual([
      expect.objectContaining({
        headerHash: evidence.headerHash,
        violationId: "network-id",
        position: 0n,
      }),
    ]);
    await expect(
      classifyCanonicalBlockViolationsV1({
        evidence,
        detections: decision.detections,
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "networkId",
    });
  });

  it("completely replays invalid-range and zero-input faults from canonical retained DA", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        // The invalid-range violation is now stated against the committed
        // block slot — the transaction's normalized range must contain it —
        // rather than against the header's time window. The fixture header
        // commits slot 0, so a range opening at 10 is the exact
        // `starts-after-block-slot` fault.
        buildFixtureTransactionV1({
          spendInputs: [outRefCbor(23, 0n)],
          fee: 1n,
          validityIntervalStart: 10n,
          validityIntervalEnd: 30n,
        }),
        buildFixtureTransactionV1({ spendInputs: [], fee: 1n }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-linear",
        grade: "security",
      },
    });
    await expect(
      INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence),
    ).resolves.toMatchObject({
      launchScope: ["invalidRange"],
      detections: [{ violationId: "invalid-range", position: 0n }],
    });
    await expect(
      ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence),
    ).resolves.toMatchObject({
      launchScope: ["zeroInput"],
      detections: [{ violationId: "zero-input", position: 1n }],
    });
  });

  it("reports no_fault_detected, never verified, after a complete empty family replay", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const result = await runFraudProofWorkflowFromRetainedDaV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      observation: authenticatedHeaderObservationV1(fixture),
      sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      registry: createFraudProofWorkflowRegistryV1({
        adapters: [makeAdapter()],
        launchScope: ["doubleSpend"],
      }),
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
      terminalVerifier,
      releaseFinalityAuthority: releaseFinalityAuthority(),
    });
    expect(result).toMatchObject({ kind: "no_fault_detected" });
  });

  it("rejects narrow, broader, or reordered replay/adapter compositions before retained-DA I/O", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const doubleSpend = makeAdapter();
    const networkId = {
      ...makeAdapter(),
      category: "networkId" as const,
    };
    const invoke = (
      replayer: CompleteCanonicalReplayV1,
      registry: ReturnType<typeof createFraudProofWorkflowRegistryV1>,
    ) =>
      runFraudProofWorkflowFromRetainedDaV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservationV1(fixture),
        sources: [],
        replayer,
        registry,
        journal: new MemoryFraudProofWorkflowJournalStoreV1(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      });

    const both = createFraudProofWorkflowRegistryV1({
      adapters: [doubleSpend, networkId],
      launchScope: ["doubleSpend", "networkId"],
    });
    await expect(
      invoke(DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1, both),
    ).rejects.toThrow("differs from exact workflow registry order");

    const onlyDoubleSpend = createFraudProofWorkflowRegistryV1({
      adapters: [doubleSpend],
      launchScope: ["doubleSpend"],
    });
    await expect(
      invoke(
        DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
        onlyDoubleSpend,
      ),
    ).rejects.toThrow("differs from exact workflow registry order");

    const reordered = createFraudProofWorkflowRegistryV1({
      adapters: [networkId, doubleSpend],
      launchScope: ["doubleSpend", "networkId"],
    });
    await expect(
      invoke(DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1, reordered),
    ).rejects.toThrow("differs from exact workflow registry order");
  });

  it("journals prepare, intent, submit, reconcile, confirmation, and completion", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
    });
    expect(result.kind).toBe("completed");
    if (result.kind !== "completed") return;
    expect(result.entries.map((entry) => entry.event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
      "completed",
    ]);
    expect(result.identity).toMatchObject({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: evidence.headerHash },
    });
  });

  it("refuses a terminal whose removal references a substituted proof token", async () => {
    const evidence = await canonicalEvidence();
    const verifier: FraudProofWorkflowTerminalVerifierV1 = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
      verify: async ({ candidate }) => ({
        ...candidate,
        correction: {
          ...candidate.correction,
          referencedProofTokenOutRef: `${"ee".repeat(32)}#1`,
        },
      }),
    };
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
      verifier,
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "removal did not reference the retained proof token",
      ),
    });
  });

  it("refuses a terminal that claims the permanent proof token was consumed", async () => {
    const evidence = await canonicalEvidence();
    const verifier: FraudProofWorkflowTerminalVerifierV1 = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
      verify: async ({ candidate }) =>
        ({
          ...candidate,
          proofToken: {
            ...candidate.proofToken,
            spentByTxHash: REMOVAL_TX_HASH,
          },
          correction: {
            ...candidate.correction,
            proofTokenSpent: true,
          },
        }) as unknown as FraudProofWorkflowTerminalV1,
    };
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
      verifier,
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("permanent proof token remains unspent"),
    });
  });

  it("binds terminal verification to the configured release confirmation depth", async () => {
    const evidence = await canonicalEvidence();
    const observedPolicies: string[] = [];
    const verifier: FraudProofWorkflowTerminalVerifierV1 = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
      verify: async ({ candidate, releaseFinality }) => {
        observedPolicies.push(releaseFinality.policyDigest);
        return {
          ...candidate,
          observedAt: { ...candidate.observedAt, confirmationDepth: 29 },
        };
      },
    };
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
      verifier,
    });
    expect(observedPolicies).toEqual([
      computeFraudProofReleaseFinalityPolicyDigestV1(RELEASE_FINALITY_POLICY),
    ]);
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "confirmation depth is below the release threshold: required=30 actual=29",
      ),
    });
  });

  it("rejects a release-finality authority bound to another deployment", async () => {
    const evidence = await canonicalEvidence();
    await expect(
      run({
        evidence,
        adapter: makeAdapter(),
        journal: new MemoryFraudProofWorkflowJournalStoreV1(),
        finalityAuthority: releaseFinalityAuthority({
          deploymentIdentityDigest: "f1".repeat(32),
        }),
      }),
    ).rejects.toThrow("returned a different deployment identity");
  });

  it("rejects a release-finality identity change across journal resume", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStoreV1();
    await expect(
      run({ evidence, adapter: makeAdapter(), journal }),
    ).resolves.toMatchObject({ kind: "completed" });
    await expect(
      run({
        evidence,
        adapter: makeAdapter(),
        journal,
        finalityAuthority: releaseFinalityAuthority({
          releaseIdentityDigest: "f2".repeat(32),
        }),
      }),
    ).rejects.toThrow("release-finality identity does not match");
  });

  it("reconciles an ambiguous submit before retrying it", async () => {
    const evidence = await canonicalEvidence();
    let submitCalls = 0;
    let reconcileCalls = 0;
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        submitCalls += 1;
        return submitCalls === 1
          ? { kind: "ambiguous", detail: "connection reset" }
          : { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({ txHash }) => {
        reconcileCalls += 1;
        return reconcileCalls === 1
          ? { kind: "not_found" }
          : txHash === undefined
            ? { kind: "conflict", reason: "missing intended hash" }
            : { kind: "confirmed", txHash };
      },
    });
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
    });
    expect(result.kind).toBe("completed");
    if (result.kind !== "completed") return;
    const events = result.entries.map((entry) => entry.event.kind);
    expect(events).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "submission_ambiguous",
      "reconciled",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
      "completed",
    ]);
    expect(events.indexOf("reconciled")).toBeLessThan(
      events.lastIndexOf("submission_intent"),
    );
  });

  it("resumes unresolved intent through an intervening stalled diagnostic", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStoreV1();
    const firstAdapter = makeAdapter({
      submit: async () => ({
        kind: "ambiguous",
        detail: "socket closed after body transmission",
      }),
      reconcile: async () => {
        throw new Error("temporary L1 provider outage");
      },
    });
    const first = await run({ evidence, adapter: firstAdapter, journal });
    expect(first).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("temporary L1 provider outage"),
    });
    if (first.kind !== "stalled") return;
    const forgedRetry: FraudProofWorkflowJournalEntryV1 = {
      ...first.entries[0]!,
      sequence: first.entries.length,
      event: {
        kind: "preflight_passed",
        actionId: "prove",
        txHash: PROOF_TX_HASH,
        localEvaluator: "uplc-v1",
        referenceScripts: [
          {
            role: "family-step",
            outRef: REFERENCE_OUT_REF,
            scriptHash: REFERENCE_SCRIPT_HASH,
          },
        ],
      },
    };
    expect(() =>
      validateFraudProofWorkflowJournalV1({
        workflowId: first.workflowId,
        entries: [...first.entries, forgedRetry],
      }),
    ).toThrow("before reconciling the unresolved intent");

    let resumedReconciliations = 0;
    const resumedAdapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        resumedReconciliations += 1;
        return resumedReconciliations === 1
          ? { kind: "not_found" }
          : txHash === undefined
            ? { kind: "conflict", reason: "missing intended hash" }
            : { kind: "confirmed", txHash };
      },
    });
    const resumed = await run({
      evidence,
      adapter: resumedAdapter,
      journal,
    });
    expect(resumed.kind).toBe("completed");
    if (resumed.kind !== "completed") return;
    const events = resumed.entries.map((entry) => entry.event.kind);
    const stalledIndex = events.indexOf("stalled");
    const reconciliationIndex = events.indexOf("reconciled", stalledIndex);
    const retryIndex = events.indexOf("submission_intent", stalledIndex);
    expect(stalledIndex).toBeGreaterThan(-1);
    expect(reconciliationIndex).toBeGreaterThan(stalledIndex);
    expect(retryIndex).toBeGreaterThan(reconciliationIndex);
    expect(resumedAdapter.prepare).not.toHaveBeenCalled();
  });

  it.each([
    {
      boundary: "after durable intent",
      crashEvent: "submission_ambiguous" as const,
      firstReconciliation: "not_found" as const,
      submit: async () => {
        throw new Error("simulated process loss before provider response");
      },
    },
    {
      boundary: "after network submit",
      crashEvent: "submitted" as const,
      firstReconciliation: "confirmed" as const,
      submit: async ({
        preflight,
      }: Parameters<FraudProofFamilyWorkflowAdapterV1["submit"]>[0]) => ({
        kind: "submitted" as const,
        txHash: preflight.txHash,
      }),
    },
  ])(
    "recovers journal-safe coordinator state with a fresh adapter $boundary",
    async ({ crashEvent, firstReconciliation, submit }) => {
      const evidence = await canonicalEvidence();
      const backing = new MemoryFraudProofWorkflowJournalStoreV1();
      let armed = true;
      const journal: FraudProofWorkflowJournalStoreV1 = {
        load: async (workflowId) => await backing.load(workflowId),
        append: async (entry, expectedSequence) => {
          if (armed && entry.event.kind === crashEvent) {
            armed = false;
            throw new Error(`simulated crash at ${crashEvent}`);
          }
          await backing.append(entry, expectedSequence);
        },
      };
      const durableRecovery = {
        coordinator: "state-queue-lease-v1",
        source: "http://midgard-node.test",
        token: "lease-fence-7",
      };
      await expect(
        run({
          evidence,
          adapter: makeAdapter({ durableRecovery, submit }),
          journal,
        }),
      ).rejects.toThrow(`simulated crash at ${crashEvent}`);

      let reconciliations = 0;
      const observedRecoveries: unknown[] = [];
      const resumedAdapter = makeAdapter({
        durableRecovery,
        reconcile: async ({ txHash, durableRecovery: recovered }) => {
          observedRecoveries.push(recovered);
          reconciliations += 1;
          if (reconciliations === 1 && firstReconciliation === "not_found") {
            return { kind: "not_found" as const };
          }
          return txHash === undefined
            ? { kind: "conflict" as const, reason: "missing intended hash" }
            : { kind: "confirmed" as const, txHash };
        },
      });
      const resumed = await run({
        evidence,
        adapter: resumedAdapter,
        journal,
      });
      expect(resumed.kind).toBe("completed");
      expect(observedRecoveries[0]).toEqual(durableRecovery);
      expect(resumedAdapter.prepare).not.toHaveBeenCalled();
    },
  );

  it("refuses an adapter that returns a hash different from durable intent", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter({
      submit: async () => ({
        kind: "submitted",
        txHash: "d4".repeat(32),
      }),
    });
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("durable intent permits only"),
    });
    expect(adapter.reconcile).not.toHaveBeenCalled();
  });

  it("resumes a pending submission without preparing or submitting twice", async () => {
    const evidence = await canonicalEvidence();
    let reconciliation = 0;
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        reconciliation += 1;
        return reconciliation === 1
          ? { kind: "pending", txHash }
          : txHash === undefined
            ? { kind: "conflict", reason: "missing intended hash" }
            : { kind: "confirmed", txHash };
      },
    });
    const journal = new MemoryFraudProofWorkflowJournalStoreV1();
    const first = await run({ evidence, adapter, journal });
    expect(first.kind).toBe("pending");
    if (first.kind !== "pending") return;
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
        workflowId: first.workflowId,
        identity: first.identity,
        sequence: first.entries.length,
        recordedAt: "2026-08-29T00:00:00.000Z",
        event: {
          kind: "stalled",
          reason: "operator stopped after observing pending state",
        },
      },
      first.entries.length,
    );
    const second = await run({ evidence, adapter, journal });
    expect(second.kind).toBe("completed");
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(adapter.reconcile).toHaveBeenCalledTimes(3);
  });

  it("refuses submission without passed reference-script preflight", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter({ referenceScripts: false });
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStoreV1(),
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("requires reference scripts"),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("rejects a journal whose deployment/category/target identity changed", async () => {
    const evidence = await canonicalEvidence();
    const expectedIdentity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: evidence.headerHash },
    };
    const workflowId = computeFraudProofWorkflowIdV1(expectedIdentity);
    const foreignIdentity: FraudProofWorkflowIdentityV1 = {
      ...expectedIdentity,
      deploymentFingerprint: "e2".repeat(32),
    };
    const poisoned: FraudProofWorkflowJournalEntryV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity: foreignIdentity,
      sequence: 0,
      recordedAt: "2026-08-29T00:00:00.000Z",
      event: { kind: "started" },
    };
    const journal: FraudProofWorkflowJournalStoreV1 = {
      load: async () => [poisoned],
      append: async () => undefined,
    };
    await expect(
      run({ evidence, adapter: makeAdapter(), journal }),
    ).rejects.toThrow("identity does not derive workflowId");
  });

  it("rejects a journal intent that changes the locally evaluated body hash", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowIdV1(identity);
    const artifact = { headerHash };
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    const entries: readonly FraudProofWorkflowJournalEntryV1[] = [
      { ...base, sequence: 0, event: { kind: "started" } },
      {
        ...base,
        sequence: 1,
        event: {
          kind: "prepared",
          artifact,
          artifactDigest: journalJsonDigestV1(artifact),
        },
      },
      {
        ...base,
        sequence: 2,
        event: {
          kind: "preflight_passed",
          actionId: "prove",
          txHash: PROOF_TX_HASH,
          localEvaluator: "uplc-v1",
          referenceScripts: [
            {
              role: "double-spend-step-04",
              outRef: REFERENCE_OUT_REF,
              scriptHash: REFERENCE_SCRIPT_HASH,
            },
          ],
        },
      },
      {
        ...base,
        sequence: 3,
        event: {
          kind: "submission_intent",
          actionId: "prove",
          actionInput: { step: 0 },
          attempt: 1,
          txHash: REMOVAL_TX_HASH,
        },
      },
    ];
    expect(() =>
      validateFraudProofWorkflowJournalV1({ workflowId, entries }),
    ).toThrow("lacks a matching exact-body preflight");
  });

  it("rejects unknown events and duplicate lifecycle roots from loaded JSON", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowIdV1(identity);
    const artifact = { headerHash };
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    const started: FraudProofWorkflowJournalEntryV1 = {
      ...base,
      sequence: 0,
      event: { kind: "started" },
    };
    const prepared: FraudProofWorkflowJournalEntryV1 = {
      ...base,
      sequence: 1,
      event: {
        kind: "prepared",
        artifact,
        artifactDigest: journalJsonDigestV1(artifact),
      },
    };
    expect(() =>
      validateFraudProofWorkflowJournalV1({
        workflowId,
        entries: [
          started,
          prepared,
          {
            ...base,
            sequence: 2,
            event: { kind: "forged-success" },
          } as unknown as FraudProofWorkflowJournalEntryV1,
        ],
      }),
    ).toThrow("unknown event kind");
    expect(() =>
      validateFraudProofWorkflowJournalV1({
        workflowId,
        entries: [started, { ...started, sequence: 1 }],
      }),
    ).toThrow("duplicate started event");
    expect(() =>
      validateFraudProofWorkflowJournalV1({
        workflowId,
        entries: [started, prepared, { ...prepared, sequence: 2 }],
      }),
    ).toThrow("duplicate prepared artifact");
  });

  it("rejects a malformed terminal even when its digest matches", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowIdV1(identity);
    const artifact = { headerHash };
    const malformed = {
      ...terminal(headerHash),
      proofToken: {
        ...terminal(headerHash).proofToken,
        unit: "not-hex",
      },
    };
    const normalized = normalizeJournalJsonV1(malformed);
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    expect(() =>
      validateFraudProofWorkflowJournalV1({
        workflowId,
        entries: [
          { ...base, sequence: 0, event: { kind: "started" } },
          {
            ...base,
            sequence: 1,
            event: {
              kind: "prepared",
              artifact,
              artifactDigest: journalJsonDigestV1(artifact),
            },
          },
          {
            ...base,
            sequence: 2,
            event: {
              kind: "completed",
              terminal: malformed,
              terminalDigest: journalJsonDigestV1(normalized),
            },
          },
        ] as readonly FraudProofWorkflowJournalEntryV1[],
      }),
    ).toThrow("malformed proof-token unit");
  });

  it("binds workflow identity independently to deployment, category, target, and decision", () => {
    const base: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: "f4".repeat(28) },
    };
    const identities: FraudProofWorkflowIdentityV1[] = [
      base,
      { ...base, deploymentFingerprint: "e5".repeat(32) },
      { ...base, category: "invalidRange" },
      {
        ...base,
        target: { kind: "state_queue_header", headerHash: "f6".repeat(28) },
      },
      { ...base, target: { kind: "settlement_claim", claimId: "claim-7" } },
      { ...base, decisionDigest: "a7".repeat(32) },
      { ...base, decisionDigest: "a8".repeat(32) },
    ];
    expect(new Set(identities.map(computeFraudProofWorkflowIdV1)).size).toBe(
      identities.length,
    );
  });

  it("fails competing journal writers at the same expected sequence", async () => {
    const store = new MemoryFraudProofWorkflowJournalStoreV1();
    const identity: FraudProofWorkflowIdentityV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: "f4".repeat(28) },
    };
    const workflowId = computeFraudProofWorkflowIdV1(identity);
    const entry: FraudProofWorkflowJournalEntryV1 = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: 0,
      recordedAt: "2026-08-29T00:00:00.000Z",
      event: { kind: "started" },
    };
    await store.append(entry, 0);
    await expect(store.append(entry, 0)).rejects.toBeInstanceOf(
      ConcurrentFraudProofWorkflowWriteErrorV1,
    );
  });

  it("recovers fsynced immutable journal entries through a fresh store", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-fp-journal-"));
    try {
      const identity: FraudProofWorkflowIdentityV1 = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_V1_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "doubleSpend",
        target: { kind: "state_queue_header", headerHash: "f7".repeat(28) },
      };
      const workflowId = computeFraudProofWorkflowIdV1(identity);
      const entry: FraudProofWorkflowJournalEntryV1 = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_V1_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: 0,
        recordedAt: "2026-08-29T00:00:00.000Z",
        event: { kind: "started" },
      };
      await new DirectoryFraudProofWorkflowJournalStoreV1(directory).append(
        entry,
        0,
      );
      await expect(
        new DirectoryFraudProofWorkflowJournalStoreV1(directory).load(
          workflowId,
        ),
      ).resolves.toEqual([entry]);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects incomplete or downgraded adapter registration", () => {
    const adapter = makeAdapter();
    expect(
      createFraudProofWorkflowRegistryV1({
        adapters: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => ({
          ...makeAdapter(),
          category,
        })),
      }).size,
    ).toBe(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length);
    expect(() =>
      createFraudProofWorkflowRegistryV1({
        adapters: [adapter],
        launchScope: ["doubleSpend", "invalidRange"],
      }),
    ).toThrow("missing launch-scope workflow adapters: invalidRange");
    expect(() =>
      createFraudProofWorkflowRegistryV1({
        adapters: [
          {
            ...adapter,
            safety: {
              ...FRAUD_PROOF_WORKFLOW_SAFETY_V1,
              scriptCarriage: "inline" as "reference-script-only",
            },
          },
        ],
        launchScope: ["doubleSpend"],
      }),
    ).toThrow("does not enforce canonical evidence");
  });
});

describe("compiled production workflow boundary", () => {
  it("rejects substituted signer and reference-script identities", () => {
    const paymentKeyHash = "a7".repeat(28);
    const address = credentialToAddress("Preview", {
      type: "Key",
      hash: paymentKeyHash,
    });
    expect(() =>
      assertManifestBoundWorkflowSignerV1({
        network: "Preview",
        address,
        paymentKeyHash,
      }),
    ).not.toThrow();
    expect(() =>
      assertManifestBoundWorkflowSignerV1({
        network: "Mainnet",
        address,
        paymentKeyHash,
      }),
    ).toThrow("manifest-network enterprise address");

    const scriptRef = {
      type: "Native" as const,
      script: `8200581c${"b9".repeat(28)}`,
    };
    const exact = {
      txHash: "a8".repeat(32),
      outputIndex: 2,
      address,
      assets: { lovelace: 2_000_000n },
      scriptRef,
    } satisfies UTxO;
    const binding = {
      referenceScriptsByContract: {
        fraudProofNetworkId: {
          outRef: `${exact.txHash}#2`,
          scriptHash: validatorToScriptHash(scriptRef),
        },
      },
    };
    expect(() =>
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: exact,
      }),
    ).not.toThrow();
    expect(() =>
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: { ...exact, outputIndex: 3 },
      }),
    ).toThrow("differs from finalized manifest identity");
    expect(() =>
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "unpublishedSharedWitness",
        utxo: exact,
      }),
    ).toThrow("has no published reference-script identity");
  });

  it("seals every network-id runtime reference role and overrides hostile inline removal", () => {
    const paymentKeyHash = "a7".repeat(28);
    const address = credentialToAddress("Preview", {
      type: "Key",
      hash: paymentKeyHash,
    });
    const scriptRef = {
      type: "Native" as const,
      script: `8200581c${"b9".repeat(28)}`,
    };
    const roleNames = [
      "fraudProofNetworkId",
      "fraudProofNetworkIdStep02",
      "fieldPreimageCertificateMint",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "chunkedVerifyWithdraw",
      "pexcludesWithdraw",
    ] as const;
    const references = Object.fromEntries(
      roleNames.map((role, index) => [
        role,
        {
          txHash: (index + 1).toString(16).padStart(64, "0"),
          outputIndex: index,
          address,
          assets: { lovelace: 2_000_000n },
          scriptRef,
        } satisfies UTxO,
      ]),
    ) as unknown as Record<(typeof roleNames)[number], UTxO>;
    const binding = {
      network: "Preview" as const,
      referenceScriptsByContract: Object.fromEntries(
        roleNames.map((role) => [
          role,
          {
            outRef: `${references[role].txHash}#${references[role].outputIndex.toString()}`,
            scriptHash: validatorToScriptHash(scriptRef),
          },
        ]),
      ),
    };
    const base = {
      binding,
      signer: {
        source: "test",
        address,
        paymentKeyHash,
        selectWallet: () => {},
      },
      stepReferenceScripts: [
        references.fraudProofNetworkId,
        references.fraudProofNetworkIdStep02,
      ] as const,
      fieldPreimageCertificateReferenceScript:
        references.fieldPreimageCertificateMint,
      witnessReferenceScripts: {
        computationThreadMint: references.computationThreadMint,
        fraudProofMint: references.fraudProofMint,
        phasMembershipWithdraw: references.phasMembershipWithdraw,
        chunkedVerifyWithdraw: references.chunkedVerifyWithdraw,
        pexcludesWithdraw: references.pexcludesWithdraw,
      },
      removal: {
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => {
            throw new Error("not called by pure runtime seal");
          },
        },
      },
    };
    const hostileRemoval = {
      ...base.removal,
      requireReferenceScripts: false,
    } as typeof base.removal;
    const sealed = sealManifestBoundNetworkIdRuntimeV1({
      ...base,
      removal: hostileRemoval,
    });
    expect(sealed.removal.requireReferenceScripts).toBe(true);
    expect([
      ...sealed.stepReferenceScripts,
      sealed.fieldPreimageCertificateReferenceScript,
      sealed.witnessReferenceScripts.computationThreadMint,
      sealed.witnessReferenceScripts.fraudProofMint,
      sealed.witnessReferenceScripts.phasMembershipWithdraw,
      sealed.witnessReferenceScripts.chunkedVerifyWithdraw,
      sealed.witnessReferenceScripts.pexcludesWithdraw,
    ]).toEqual(roleNames.map((role) => references[role]));

    const substituted = {
      ...references.fraudProofNetworkId,
      outputIndex: 99,
    };
    const hostileInputs = [
      {
        ...base,
        stepReferenceScripts: [
          substituted,
          base.stepReferenceScripts[1],
        ] as const,
      },
      {
        ...base,
        stepReferenceScripts: [
          base.stepReferenceScripts[0],
          substituted,
        ] as const,
      },
      { ...base, fieldPreimageCertificateReferenceScript: substituted },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          computationThreadMint: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          fraudProofMint: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          phasMembershipWithdraw: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          chunkedVerifyWithdraw: substituted,
        },
      },
      {
        ...base,
        witnessReferenceScripts: {
          ...base.witnessReferenceScripts,
          pexcludesWithdraw: substituted,
        },
      },
    ] as const;
    for (const hostile of hostileInputs) {
      expect(() => sealManifestBoundNetworkIdRuntimeV1(hostile)).toThrow(
        "differs from finalized manifest identity",
      );
    }
  });

  it("rejects omitted, duplicate, and unknown production registrations", () => {
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1(
        PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.slice(0, -1),
      ),
    ).toThrow("cardinality mismatch");
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1([
        ...PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.slice(0, -1),
        PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1[0]!,
      ]),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1([
        ...PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.slice(0, -1),
        { category: "forgedFamily" },
      ]),
    ).toThrow("actual=forgedFamily");
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1([
        {
          ...PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1[0]!,
          status: "ready",
        },
        ...PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.slice(1),
      ]),
    ).toThrow("has no compiled executable runner");
  });

  it("seals registry keys and adapter methods against post-construction mutation", () => {
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistryV1({
      adapters: [original],
      launchScope: ["doubleSpend"],
    });
    const admitted = registry.get("doubleSpend")!;
    expect(Object.isFrozen(registry)).toBe(true);
    expect(Object.isFrozen(admitted)).toBe(true);
    expect(Object.isFrozen(admitted.safety)).toBe(true);
    expect("set" in registry).toBe(false);
    expect("delete" in registry).toBe(false);

    const substitutedObserve = vi.fn(async () => ({
      kind: "conflict" as const,
      reason: "substituted",
    }));
    expect(Reflect.set(original, "category", "networkId")).toBe(true);
    expect(Reflect.set(original, "observe", substitutedObserve)).toBe(true);
    expect([...registry.keys()]).toEqual(["doubleSpend"]);
    expect(admitted.category).toBe("doubleSpend");
    expect(admitted.observe).not.toBe(substitutedObserve);
    expect(Reflect.set(admitted, "category", "networkId")).toBe(false);
  });

  it("cannot mutate the workflow registry during awaited retained-DA fetch", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistryV1({
      adapters: [original],
      launchScope: ["doubleSpend"],
    });
    const before = registry.get("doubleSpend")!;
    let mutationAttempted = false;
    const source: RetainedDaPayloadSource = {
      sourceId: "libp2p-hostile",
      fetchPayloadByHeaderHash: async () => {
        mutationAttempted = true;
        expect(() =>
          (
            registry as unknown as Map<
              SDK.FraudProofCatalogueCategoryName,
              FraudProofFamilyWorkflowAdapterV1
            >
          ).set("networkId", {
            ...makeAdapter(),
            category: "networkId",
          }),
        ).toThrow();
        Reflect.set(original, "observe", async () => ({
          kind: "conflict" as const,
          reason: "substituted during fetch",
        }));
        return {
          ok: true,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-hostile/peer-hostile",
            grade: "security" as const,
          },
          sourceId: "libp2p-hostile",
          sourcePeerId: "peer-hostile",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        };
      },
    };
    await expect(
      runFraudProofWorkflowFromRetainedDaV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservationV1(fixture),
        sources: [source],
        replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
        registry,
        journal: new MemoryFraudProofWorkflowJournalStoreV1(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      }),
    ).resolves.toMatchObject({ kind: "no_fault_detected" });
    expect(mutationAttempted).toBe(true);
    expect(registry.get("doubleSpend")).toBe(before);
  });

  it("deep-freezes registry rows and rejects forged or cross-category runners", () => {
    const first = PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1[0]!;
    expect(Object.isFrozen(first)).toBe(true);
    expect(Reflect.set(first, "status", "ready")).toBe(false);
    expect(first.status).toBe("missing");
    expect(Object.isFrozen(first.existingSurface)).toBe(true);
    expect(Reflect.set(first.existingSurface, 0, "forged-surface")).toBe(false);

    const forgedRunner = {
      runnerVersion: PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
      runOrResume: async () => "forged",
    };
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1(
        PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: forgedRunner }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");

    const admittedDoubleSpend = createDoubleSpendProductionWorkflowRunnerV1(
      async () => {
        throw new Error("runner loader is not invoked during admission");
      },
    );
    expect(() =>
      validateProductionWorkflowAdapterCoverageV1(
        PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.map((registration) =>
          registration.category === "networkId"
            ? {
                ...registration,
                status: "ready",
                runner: admittedDoubleSpend,
              }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");
  });

  it("enumerates every registered category with an exact missing-adapter reason", () => {
    expect(
      PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.map(
        (registration) => registration.category,
      ),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(productionWorkflowReadinessReportV1()).toMatchObject({
      registeredCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      requestedCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      readyCategoryCount: 0,
      missingCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    });
    expect(
      PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.find(
        ({ category }) => category === "doubleSpend",
      ),
    ).toMatchObject({
      status: "missing",
      reason: "constrained_adapter_is_not_launch_scope_complete",
    });
    expect(
      PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.find(
        ({ category }) => category === "nativeScriptDecoding",
      ),
    ).toMatchObject({
      reason: "one_shot_prover_has_no_pre_submit_journal_hook",
    });
    expect(
      PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.find(
        ({ category }) => category === "networkId",
      ),
    ).toMatchObject({
      status: "missing",
      reason: "constrained_adapter_is_not_launch_scope_complete",
    });
  });

  it("parses run/resume journal identity flags in the compiled CLI", () => {
    const parsed = parseArgs([
      "node",
      "midgard-fault-proofs",
      "resume-workflow",
      "--fraud-category",
      "doubleSpend",
      "--deployment-fingerprint",
      DEPLOYMENT_FINGERPRINT,
      "--header-hash",
      "f8".repeat(28),
      "--workflow-journal-dir",
      "/tmp/midgard-workflow-test",
      "--workflow-runtime-config",
      "/etc/midgard/fraud-proof-runtime-v1.json",
    ]);
    expect(parsed).toMatchObject({
      command: "resume-workflow",
      fraudCategory: "doubleSpend",
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash: "f8".repeat(28),
      workflowJournalDir: "/tmp/midgard-workflow-test",
      workflowRuntimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
    });
  });

  it("fails closed before opening a journal or accepting evidence", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-fp-cli-"));
    const journalDirectory = join(root, "must-not-be-created");
    try {
      await expect(
        runProductionFraudProofWorkflowCliV1({
          mode: "run",
          category: "invalidRange",
          deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
          headerHash: "f8".repeat(28),
          decisionDigest: "f9".repeat(32),
          actuationPermit: {
            permitVersion: PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1,
          },
          // The CLI now demands the funding-reservation permit alongside the
          // actuation permit before it looks at adapters, so both are present
          // here: this test is about the missing-adapter refusal, not about
          // the permit refusal that precedes it.
          fundingReservationPermit: {
            permitVersion: PRODUCTION_WORKFLOW_FUNDING_RESERVATION_PERMIT_V1,
          },
          journalDirectory,
          runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        }),
      ).rejects.toBeInstanceOf(MissingProductionWorkflowAdaptersErrorV1);
      await expect(
        import("node:fs/promises").then(({ stat }) => stat(journalDirectory)),
      ).rejects.toMatchObject({ code: "ENOENT" });
    } finally {
      await rm(root, { recursive: true, force: true });
    }
  });
});
