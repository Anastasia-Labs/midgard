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
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import {
  createNetworkIdAuthenticatedL1TerminalVerifier,
  type ManifestBoundNetworkIdWorkflow,
  runOrResumeManifestBoundNetworkIdWorkflow,
  sealManifestBoundNetworkIdRuntime,
} from "../src/network-id/workflow-adapter.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import * as actuationAuthority from "../src/workflow/actuation-permit.js";
import { WORKFLOW_ACTUATION_PERMIT } from "../src/workflow/actuation-permit.js";
import {
  MissingWorkflowAdaptersError,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_RUNNER,
} from "../src/workflow/adapters.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
  FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "../src/workflow/classification.js";
import {
  runFraudProofWorkflowCli,
  workflowReadinessReport,
} from "../src/workflow/cli.js";
import {
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  requireManifestBoundReferenceScriptUtxo,
} from "../src/workflow/deployment-manifest-binding.js";
import {
  createDoubleSpendConstrainedWorkflowAdapter,
  type ManifestBoundDoubleSpendWorkflow,
  runOrResumeManifestBoundDoubleSpendWorkflow,
} from "../src/workflow/double-spend-adapter.js";
import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import {
  reconcileWorkflowFundingSubmissionHandoff,
  WORKFLOW_FUNDING_RESERVATION_PERMIT,
} from "../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  ConcurrentFraudProofWorkflowWriteError,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "../src/workflow/journal.js";
import { LocalKupmiosCheckpointChangedError } from "../src/workflow/local-kupmios-raw-l1-authority.js";
import {
  createFraudProofWorkflowRegistry,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../src/workflow/orchestrator.js";
import { StateQueueHeaderNotLiveError } from "../src/workflow/raw-l1-family-derivation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy.js";
import { WORKFLOW_RUNNER_FACTORIES } from "../src/workflow/runtime.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

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

const canonicalEvidence = async (): Promise<CanonicalBlockEvidence> => {
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

const terminal = (headerHash: string): FraudProofWorkflowTerminal => ({
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
    confirmationDepth: 30,
  },
});

const terminalVerifier: FraudProofWorkflowTerminalVerifier = {
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  verify: async ({ candidate }) => candidate,
};

const makeAdapter = (
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

const run = async ({
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

describe("Q55/W-O6 deterministic violation classification", () => {
  it("covers every registered family exactly once in catalogue order", () => {
    expect(
      FRAUD_PROOF_CLASSIFICATION_RULES.map((rule) => rule.category),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    const identifiers = FRAUD_PROOF_CLASSIFICATION_RULES.flatMap((rule) => [
      ...rule.violationIds,
    ]);
    expect(new Set(identifiers).size).toBe(identifiers.length);
  });

  it("ranks crossBlockDuplicateEvent ahead of doubleWithdraw per decision 0008", () => {
    expect([...FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE].sort()).toEqual(
      [...SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER].sort(),
    );
    const promoted = FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.indexOf(
      "crossBlockDuplicateEvent",
    );
    expect(promoted).toBe(
      FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.indexOf("doubleWithdraw") -
        1,
    );
    expect(
      FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.slice(0, promoted),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(0, promoted));
  });

  it("selects the earliest position, then stable family order", async () => {
    const evidence = await canonicalEvidence();
    const classification = await classifyCanonicalBlockViolations({
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
    const classification = await classifyCanonicalBlockViolations({
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
      classifyCanonicalBlockViolations({ evidence, detections: [] }),
    ).resolves.toMatchObject({ decision: "no_fault_detected" });
  });

  it("rejects duplicate detector identities and cross-header detections", async () => {
    const evidence = await canonicalEvidence();
    const duplicate = detection(evidence);
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [duplicate, duplicate],
      }),
    ).rejects.toThrow("duplicate canonical violation detectionId");
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [
          detection(evidence, "double-spend", { headerHash: h32(9) }),
        ],
      }),
    ).rejects.toThrow("targets header");
  });
});

describe("Q51/W-O4 resumable workflow", () => {
  it("revalidates recorded family material before any resumed live observation", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const observe = vi.fn(async () => ({
      kind: "pending" as const,
      reason: "awaiting chain",
    }));
    const validatePreparedArtifact = vi.fn(
      async ({
        artifact,
      }: Parameters<
        NonNullable<FraudProofFamilyWorkflowAdapter["validatePreparedArtifact"]>
      >[0]) => {
        if (artifact.headerHash !== evidence.headerHash)
          throw new Error("changed typed material");
      },
    );
    const adapter = { ...makeAdapter(), observe, validatePreparedArtifact };
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(validatePreparedArtifact).not.toHaveBeenCalled();
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(validatePreparedArtifact).toHaveBeenCalledOnce();
    expect(validatePreparedArtifact.mock.calls[0]![0].evidence).toBe(evidence);
    validatePreparedArtifact.mockRejectedValueOnce(
      new Error("changed typed material"),
    );
    await expect(run({ evidence, adapter, journal })).rejects.toThrow(
      "changed typed material",
    );
    expect(observe).toHaveBeenCalledTimes(2);
    expect(adapter.prepare).toHaveBeenCalledOnce();
  });

  it.each(["doubleSpend", "networkId"] as const)(
    "resumes %s terminal tracking after its header was removed",
    async (category) => {
      const sharedInput = outRefCbor(61, 0n);
      const fixture = await buildCanonicalBlockFixture({
        transactions:
          category === "doubleSpend"
            ? [
                buildFixtureTransaction({
                  spendInputs: [sharedInput],
                  fee: 1n,
                }),
                buildFixtureTransaction({
                  spendInputs: [sharedInput],
                  fee: 2n,
                }),
              ]
            : [
                buildFixtureTransaction({
                  spendInputs: [],
                  fee: 1n,
                  networkId: 1n,
                }),
              ],
      });
      const observation = authenticatedHeaderObservation(fixture);
      let depth = 1;
      const observeHeader = vi.fn(async () => {
        throw new StateQueueHeaderNotLiveError();
      });
      const observeRetainedHeader = vi.fn(async () => observation);
      const observe = vi.fn(async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1" as const,
          sourceId: "local-node-test",
          grade: "security" as const,
        },
        stage: {
          kind: "removed" as const,
          terminal: {
            ...terminal(observation.headerHash),
            category,
            observedAt: {
              ...terminal(observation.headerHash).observedAt,
              confirmationDepth: depth,
            },
          },
        },
      }));
      const workflow = {
        binding: {
          deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
          definition: { headerHash: observation.headerHash },
        },
        adapterConfig: {
          l1: {
            observeHeader,
            observeRetainedHeader,
            observe,
            transactionConfirmed: async ({ txHash }: { txHash: string }) =>
              txHash === REMOVAL_TX_HASH,
          },
        },
        releaseFinalityAuthority: releaseFinalityAuthority(),
      } as unknown as ManifestBoundDoubleSpendWorkflow;
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const invocation = {
        workflow,
        sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
        journal,
      };
      const removalAction = {
        actionId: `remove:${terminal(observation.headerHash).correction.removedStateQueueOutRef}`,
        input: { stage: "remove", requiresMutationLease: false },
      };
      const pendingAdapter = {
        ...makeAdapter({
          reconcile: async ({ txHash }) => ({
            kind: txHash === PROOF_TX_HASH ? "confirmed" : "pending",
            txHash: txHash!,
          }),
        }),
        category,
        prepare:
          category === "doubleSpend"
            ? createDoubleSpendConstrainedWorkflowAdapter(
                workflow.adapterConfig,
              ).prepare
            : makeAdapter().prepare,
        observe: async ({
          entries,
        }: Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0]) => ({
          kind: "action_required" as const,
          action: entries.some(
            ({ event }) =>
              event.kind === "confirmed" && event.txHash === PROOF_TX_HASH,
          )
            ? removalAction
            : { actionId: "prove", input: { stage: "step_04" } },
        }),
      };
      const rawL1 = {
        observeHeader,
        observeRetainedHeader,
        observe: async () => (await observe()).stage,
      };
      const networkWorkflow = {
        binding: workflow.binding,
        adapterConfig: { rawL1 },
        adapter: {
          ...pendingAdapter,
          observe: async () => ({
            kind: "completed",
            terminal: (await observe()).stage.terminal,
          }),
          reconcile: async () => ({
            kind: "confirmed",
            txHash: REMOVAL_TX_HASH,
          }),
        },
        releaseFinalityAuthority: workflow.releaseFinalityAuthority,
        terminalVerifier: createNetworkIdAuthenticatedL1TerminalVerifier(rawL1),
      } as unknown as ManifestBoundNetworkIdWorkflow;
      const resume = async () =>
        category === "doubleSpend"
          ? await runOrResumeManifestBoundDoubleSpendWorkflow(invocation)
          : await runOrResumeManifestBoundNetworkIdWorkflow({
              ...invocation,
              workflow: networkWorkflow,
            });
      const pending = await runFraudProofWorkflowFromRetainedDa({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation,
        sources: invocation.sources,
        replayer:
          category === "doubleSpend"
            ? DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY
            : NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
        registry: createFraudProofWorkflowRegistry({
          adapters: [pendingAdapter],
          launchScope: [category],
        }),
        journal,
        terminalVerifier,
        releaseFinalityAuthority: workflow.releaseFinalityAuthority,
      });
      expect(pending.kind).toBe("pending");
      const included = await resume();
      expect(included.kind).toBe("terminal_included");
      depth = 30;
      const completed = await resume();
      expect(completed.kind).toBe("completed");
      expect(observeRetainedHeader).toHaveBeenCalledTimes(2);
      expect(observe.mock.calls.length).toBeGreaterThanOrEqual(4);
      if (completed.kind !== "completed")
        throw new Error("expected terminal completion");
      const entries = await journal.load(completed.workflowId);
      expect(
        entries.filter(({ event }) => event.kind === "submission_intent"),
      ).toHaveLength(2);
      expect(
        entries.some(
          ({ event }) =>
            event.kind === "confirmed" && event.txHash === REMOVAL_TX_HASH,
        ),
      ).toBe(true);
      expect(entries.at(-1)?.event.kind).toBe("completed");
      observeHeader.mockRejectedValueOnce(new Error("provider unavailable"));
      await expect(resume()).rejects.toThrow("provider unavailable");
      expect(observeRetainedHeader).toHaveBeenCalledTimes(2);
    },
  );

  it("runs from authenticated L1 plus public retained DA with no private evidence input", async () => {
    const sharedInput = outRefCbor(61, 0n);
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
        buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
      ],
    });
    const adapter = makeAdapter();
    const replayContext = {};
    const resolveReplayContext = vi.fn(
      async (_evidence: CanonicalBlockEvidence) => replayContext,
    );
    const result = await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      observation: authenticatedHeaderObservation(fixture),
      sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      resolveReplayContext,
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal: new MemoryFraudProofWorkflowJournalStore(),
      terminalVerifier,
      releaseFinalityAuthority: releaseFinalityAuthority(),
      now: () => new Date("2026-08-29T00:00:00.000Z"),
    });
    expect(result.kind).toBe("completed");
    expect(resolveReplayContext).toHaveBeenCalledOnce();
    expect(adapter.prepare).toHaveBeenCalledWith(
      expect.objectContaining({
        evidence: resolveReplayContext.mock.calls[0]![0],
        replayContext,
      }),
    );
  });

  it("rejects a caller-authored partial detector disguised as complete replay", async () => {
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const forgedReplayer = {
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      launchScope: ["doubleSpend"] as const,
      replay: async () => ({
        replayVersion: COMPLETE_CANONICAL_REPLAY,
        launchScope: ["doubleSpend"] as const,
        headerHash: fixture.headerHash,
        payloadEnvelopeSha256: "00".repeat(32),
        payloadSha256: "00".repeat(32),
        context: null,
        detections: [],
      }),
    } satisfies CompleteCanonicalReplay;
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservation(fixture),
        sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
        replayer: forgedReplayer,
        registry: createFraudProofWorkflowRegistry({
          adapters: [makeAdapter()],
          launchScope: ["doubleSpend"],
        }),
        journal: new MemoryFraudProofWorkflowJournalStore(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      }),
    ).rejects.toThrow("closed canonical replay bundle");
  });

  it("completely replays network-id faults from canonical retained DA", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({
          spendInputs: [],
          fee: 1n,
          networkId: 1n,
        }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
    });
    const decision =
      await NETWORK_ID_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toEqual([
      expect.objectContaining({
        headerHash: evidence.headerHash,
        violationId: "network-id",
        position: 0n,
      }),
    ]);
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "networkId",
    });
  });

  it("completely replays invalid-range and zero-input faults from canonical retained DA", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        // The invalid-range violation is now stated against the committed
        // block slot — the transaction's normalized range must contain it —
        // rather than against the header's time window. The fixture header
        // commits slot 0, so a range opening at 10 is the exact
        // `starts-after-block-slot` fault.
        buildFixtureTransaction({
          spendInputs: [outRefCbor(23, 0n)],
          fee: 1n,
          validityIntervalStart: 10n,
          validityIntervalEnd: 30n,
        }),
        buildFixtureTransaction({ spendInputs: [], fee: 1n }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-linear",
        grade: "security",
      },
    });
    await expect(
      INVALID_RANGE_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).resolves.toMatchObject({
      launchScope: ["invalidRange"],
      detections: [{ violationId: "invalid-range", position: 0n }],
    });
    await expect(
      ZERO_INPUT_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).resolves.toMatchObject({
      launchScope: ["zeroInput"],
      detections: [{ violationId: "zero-input", position: 1n }],
    });
  });

  it("reports no_fault_detected, never verified, after a complete empty family replay", async () => {
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const result = await runFraudProofWorkflowFromRetainedDa({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      observation: authenticatedHeaderObservation(fixture),
      sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      registry: createFraudProofWorkflowRegistry({
        adapters: [makeAdapter()],
        launchScope: ["doubleSpend"],
      }),
      journal: new MemoryFraudProofWorkflowJournalStore(),
      terminalVerifier,
      releaseFinalityAuthority: releaseFinalityAuthority(),
    });
    expect(result).toMatchObject({ kind: "no_fault_detected" });
  });

  it("rejects narrow, broader, or reordered replay/adapter compositions before retained-DA I/O", async () => {
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const doubleSpend = makeAdapter();
    const networkId = {
      ...makeAdapter(),
      category: "networkId" as const,
    };
    const invoke = (
      replayer: CompleteCanonicalReplay,
      registry: ReturnType<typeof createFraudProofWorkflowRegistry>,
    ) =>
      runFraudProofWorkflowFromRetainedDa({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservation(fixture),
        sources: [],
        replayer,
        registry,
        journal: new MemoryFraudProofWorkflowJournalStore(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      });

    const both = createFraudProofWorkflowRegistry({
      adapters: [doubleSpend, networkId],
      launchScope: ["doubleSpend", "networkId"],
    });
    await expect(
      invoke(DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY, both),
    ).rejects.toThrow("differs from exact workflow registry order");

    const onlyDoubleSpend = createFraudProofWorkflowRegistry({
      adapters: [doubleSpend],
      launchScope: ["doubleSpend"],
    });
    await expect(
      invoke(
        DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
        onlyDoubleSpend,
      ),
    ).rejects.toThrow("differs from exact workflow registry order");

    const reordered = createFraudProofWorkflowRegistry({
      adapters: [networkId, doubleSpend],
      launchScope: ["doubleSpend", "networkId"],
    });
    await expect(
      invoke(DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY, reordered),
    ).rejects.toThrow("differs from exact workflow registry order");
  });

  it("journals prepare, intent, submit, reconcile, confirmation, and completion", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStore(),
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
    const verifier: FraudProofWorkflowTerminalVerifier = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
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
      journal: new MemoryFraudProofWorkflowJournalStore(),
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
    const verifier: FraudProofWorkflowTerminalVerifier = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
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
        }) as unknown as FraudProofWorkflowTerminal,
    };
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      journal: new MemoryFraudProofWorkflowJournalStore(),
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
    const verifier: FraudProofWorkflowTerminalVerifier = {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
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
      journal: new MemoryFraudProofWorkflowJournalStore(),
      verifier,
    });
    expect(observedPolicies).toEqual([
      computeFraudProofReleaseFinalityPolicyDigest(RELEASE_FINALITY_POLICY),
    ]);
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        "confirmation depth is below the release threshold: required=30 actual=29",
      ),
    });
  });

  it("advances on inclusion and anchors the same execution after restart", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    const observe = adapter.observe;
    let depth = 1;
    adapter.observe = async (context) => {
      const result = await observe(context);
      return result.kind === "completed"
        ? {
            ...result,
            terminal: {
              ...result.terminal,
              observedAt: {
                ...result.terminal.observedAt,
                confirmationDepth: depth,
              },
            },
          }
        : result;
    };
    const verify = vi.fn(
      async ({
        candidate,
      }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0]) =>
        candidate,
    );
    const verifyIncluded = vi.fn(
      async ({
        candidate,
      }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0]) =>
        candidate,
    );
    const verifier = { ...terminalVerifier, verify, verifyIncluded };
    const included = await run({ evidence, adapter, journal, verifier });
    expect(included.kind).toBe("terminal_included");
    expect(verify).not.toHaveBeenCalled();
    expect(verifyIncluded).toHaveBeenCalledOnce();
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    depth = 30;
    const anchored = await run({ evidence, adapter, journal, verifier });
    expect(anchored.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(verify).toHaveBeenCalledOnce();
  });

  it("reobserves an included terminal after rollback and reconciles prior actions", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const onChain = new Set<string>();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        onChain.add(txHash!);
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () =>
      onChain.has(REMOVAL_TX_HASH)
        ? {
            kind: "completed",
            terminal: {
              ...terminal(evidence.headerHash),
              observedAt: {
                ...terminal(evidence.headerHash).observedAt,
                confirmationDepth: 1,
              },
            },
          }
        : {
            kind: "action_required",
            action: onChain.has(PROOF_TX_HASH)
              ? { actionId: "remove", input: { step: 1 } }
              : { actionId: "prove", input: { step: 0 } },
          };
    const verifier = {
      ...terminalVerifier,
      verifyIncluded: terminalVerifier.verify,
    };
    expect((await run({ evidence, adapter, journal, verifier })).kind).toBe(
      "terminal_included",
    );
    onChain.clear();
    const resumed = await run({ evidence, adapter, journal, verifier });
    expect(resumed.kind).toBe("terminal_included");
    if (resumed.kind !== "terminal_included") return;
    expect(
      resumed.entries
        .filter(({ event }) => event.kind === "reobserved")
        .map(({ event }) => "actionId" in event && event.actionId),
    ).toEqual(["prove", "remove"]);
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("rejects a release-finality authority bound to another deployment", async () => {
    const evidence = await canonicalEvidence();
    await expect(
      run({
        evidence,
        adapter: makeAdapter(),
        journal: new MemoryFraudProofWorkflowJournalStore(),
        finalityAuthority: releaseFinalityAuthority({
          deploymentIdentityDigest: "f1".repeat(32),
        }),
      }),
    ).rejects.toThrow("returned a different deployment identity");
  });

  it("rejects a release-finality identity change across journal resume", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    await expect(
      run({ evidence, adapter: makeAdapter(), journal }),
    ).resolves.toMatchObject({ kind: "completed" });
    await expect(
      run({
        evidence,
        adapter: makeAdapter(),
        journal,
        finalityAuthority: releaseFinalityAuthority({
          blueprintHash: "f2".repeat(32),
        }),
      }),
    ).rejects.toThrow("release-finality identity does not match");
  });

  it("retains a thrown submission through a moving reconciliation boundary without restarting", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let reconciliations = 0;
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        if (preflight.txHash === PROOF_TX_HASH)
          throw new Error(
            "All inputs are spent. Transaction has probably already been included",
          );
        return { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({ txHash }) => {
        reconciliations += 1;
        if (reconciliations === 1)
          throw new LocalKupmiosCheckpointChangedError(
            "Kupo advanced during canonical capture",
          );
        return { kind: "confirmed", txHash: txHash! };
      },
    });

    const pending = await run({ evidence, adapter, journal });
    expect(pending).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringContaining("Kupo advanced"),
    });
    if (pending.kind !== "pending")
      throw new Error("expected reconciliation yield");
    expect(pending.entries.at(-1)?.event).toMatchObject({
      kind: "submission_ambiguous",
      txHash: PROOF_TX_HASH,
    });
    expect(adapter.preflight).toHaveBeenCalledTimes(1);
    expect(adapter.submit).toHaveBeenCalledTimes(1);

    // A fresh admitted observation reuses this objective, adapter and journal;
    // the accepted proof is reconciled before constructing only its removal.
    const completed = await run({ evidence, adapter, journal });
    expect(completed.kind).toBe("completed");
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    if (completed.kind !== "completed") throw new Error("expected completion");
    expect(
      completed.entries.filter(({ event }) => event.kind === "stalled"),
    ).toEqual([]);
    expect(
      completed.entries.filter(
        ({ event }) =>
          event.kind === "submission_intent" && event.txHash === PROOF_TX_HASH,
      ),
    ).toHaveLength(1);
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
      journal: new MemoryFraudProofWorkflowJournalStore(),
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
    const journal = new MemoryFraudProofWorkflowJournalStore();
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
    const forgedRetry: FraudProofWorkflowJournalEntry = {
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
      validateFraudProofWorkflowJournal({
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
      }: Parameters<FraudProofFamilyWorkflowAdapter["submit"]>[0]) => ({
        kind: "submitted" as const,
        txHash: preflight.txHash,
      }),
    },
  ])(
    "recovers journal-safe coordinator state with a fresh adapter $boundary",
    async ({ crashEvent, firstReconciliation, submit }) => {
      const evidence = await canonicalEvidence();
      const backing = new MemoryFraudProofWorkflowJournalStore();
      let armed = true;
      const journal: FraudProofWorkflowJournalStore = {
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
      journal: new MemoryFraudProofWorkflowJournalStore(),
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
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const first = await run({ evidence, adapter, journal });
    expect(first.kind).toBe("pending");
    if (first.kind !== "pending") return;
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
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

  it("marks an adapter preflight failure as resumable once L1 observation catches up", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    vi.mocked(adapter.preflight).mockRejectedValueOnce(
      new Error("Expected exactly one fraudulent block UTxO, found 0."),
    );
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const result = await run({ evidence, adapter, journal });
    expect(result).toMatchObject({
      kind: "stalled",
      phase: "preflight",
      reason: expect.stringContaining(
        "preflight failed for prove: Error: Expected exactly one fraudulent block UTxO, found 0.",
      ),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
    // A later resume re-observes the family and completes normally.
    const resumed = await run({ evidence, adapter, journal });
    expect(resumed).toMatchObject({ kind: "completed" });
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("refuses submission without passed reference-script preflight", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter({ referenceScripts: false });
    const result = await run({
      evidence,
      adapter,
      journal: new MemoryFraudProofWorkflowJournalStore(),
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("requires reference scripts"),
    });
    expect(result).not.toHaveProperty("phase");
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("rejects a journal whose deployment/category/target identity changed", async () => {
    const evidence = await canonicalEvidence();
    const expectedIdentity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: evidence.headerHash },
    };
    const workflowId = computeFraudProofWorkflowId(expectedIdentity);
    const foreignIdentity: FraudProofWorkflowIdentity = {
      ...expectedIdentity,
      deploymentFingerprint: "e2".repeat(32),
    };
    const poisoned: FraudProofWorkflowJournalEntry = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity: foreignIdentity,
      sequence: 0,
      recordedAt: "2026-08-29T00:00:00.000Z",
      event: { kind: "started" },
    };
    const journal: FraudProofWorkflowJournalStore = {
      load: async () => [poisoned],
      append: async () => undefined,
    };
    await expect(
      run({ evidence, adapter: makeAdapter(), journal }),
    ).rejects.toThrow("identity does not derive workflowId");
  });

  it("rejects a journal intent that changes the locally evaluated body hash", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const artifact = { headerHash };
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    const entries: readonly FraudProofWorkflowJournalEntry[] = [
      { ...base, sequence: 0, event: { kind: "started" } },
      {
        ...base,
        sequence: 1,
        event: {
          kind: "prepared",
          artifact,
          artifactDigest: journalJsonDigest(artifact),
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
      validateFraudProofWorkflowJournal({ workflowId, entries }),
    ).toThrow("lacks a matching exact-body preflight");
  });

  it("rejects unknown events and duplicate lifecycle roots from loaded JSON", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const artifact = { headerHash };
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    const started: FraudProofWorkflowJournalEntry = {
      ...base,
      sequence: 0,
      event: { kind: "started" },
    };
    const prepared: FraudProofWorkflowJournalEntry = {
      ...base,
      sequence: 1,
      event: {
        kind: "prepared",
        artifact,
        artifactDigest: journalJsonDigest(artifact),
      },
    };
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [
          started,
          prepared,
          {
            ...base,
            sequence: 2,
            event: { kind: "forged-success" },
          } as unknown as FraudProofWorkflowJournalEntry,
        ],
      }),
    ).toThrow("unknown event kind");
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [started, { ...started, sequence: 1 }],
      }),
    ).toThrow("duplicate started event");
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [started, prepared, { ...prepared, sequence: 2 }],
      }),
    ).toThrow("duplicate prepared artifact");
  });

  it("rejects a malformed terminal even when its digest matches", () => {
    const headerHash = "f4".repeat(28);
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: {
        kind: "state_queue_header",
        headerHash,
      },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const artifact = { headerHash };
    const malformed = {
      ...terminal(headerHash),
      proofToken: {
        ...terminal(headerHash).proofToken,
        unit: "not-hex",
      },
    };
    const normalized = normalizeJournalJson(malformed);
    const base = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      recordedAt: "2026-08-29T00:00:00.000Z",
    } as const;
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [
          { ...base, sequence: 0, event: { kind: "started" } },
          {
            ...base,
            sequence: 1,
            event: {
              kind: "prepared",
              artifact,
              artifactDigest: journalJsonDigest(artifact),
            },
          },
          {
            ...base,
            sequence: 2,
            event: {
              kind: "completed",
              terminal: malformed,
              terminalDigest: journalJsonDigest(normalized),
            },
          },
        ] as readonly FraudProofWorkflowJournalEntry[],
      }),
    ).toThrow("malformed proof-token unit");
  });

  it("binds workflow identity independently to deployment, category, target, and decision", () => {
    const base: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: "f4".repeat(28) },
    };
    const identities: FraudProofWorkflowIdentity[] = [
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
    expect(new Set(identities.map(computeFraudProofWorkflowId)).size).toBe(
      identities.length,
    );
  });

  it("fails competing journal writers at the same expected sequence", async () => {
    const store = new MemoryFraudProofWorkflowJournalStore();
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: "f4".repeat(28) },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const entry: FraudProofWorkflowJournalEntry = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: 0,
      recordedAt: "2026-08-29T00:00:00.000Z",
      event: { kind: "started" },
    };
    await store.append(entry, 0);
    await expect(store.append(entry, 0)).rejects.toBeInstanceOf(
      ConcurrentFraudProofWorkflowWriteError,
    );
  });

  it("recovers fsynced immutable journal entries through a fresh store", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-fp-journal-"));
    try {
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "doubleSpend",
        target: { kind: "state_queue_header", headerHash: "f7".repeat(28) },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const entry: FraudProofWorkflowJournalEntry = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: 0,
        recordedAt: "2026-08-29T00:00:00.000Z",
        event: { kind: "started" },
      };
      await new DirectoryFraudProofWorkflowJournalStore(directory).append(
        entry,
        0,
      );
      await expect(
        new DirectoryFraudProofWorkflowJournalStore(directory).load(workflowId),
      ).resolves.toEqual([entry]);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects incomplete or downgraded adapter registration", () => {
    const adapter = makeAdapter();
    expect(
      createFraudProofWorkflowRegistry({
        adapters: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => ({
          ...makeAdapter(),
          category,
        })),
      }).size,
    ).toBe(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length);
    expect(() =>
      createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend", "invalidRange"],
      }),
    ).toThrow("missing launch-scope workflow adapters: invalidRange");
    expect(() =>
      createFraudProofWorkflowRegistry({
        adapters: [
          {
            ...adapter,
            safety: {
              ...FRAUD_PROOF_WORKFLOW_SAFETY,
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
      assertManifestBoundWorkflowSigner({
        network: "Preview",
        address,
        paymentKeyHash,
      }),
    ).not.toThrow();
    expect(() =>
      assertManifestBoundWorkflowSigner({
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
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: exact,
      }),
    ).not.toThrow();
    expect(() =>
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofNetworkId",
        utxo: { ...exact, outputIndex: 3 },
      }),
    ).toThrow("differs from finalized manifest identity");
    expect(() =>
      requireManifestBoundReferenceScriptUtxo({
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
    const sealed = sealManifestBoundNetworkIdRuntime({
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
      expect(() => sealManifestBoundNetworkIdRuntime(hostile)).toThrow(
        "differs from finalized manifest identity",
      );
    }
  });

  it("rejects omitted, duplicate, and unknown production registrations", () => {
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
      ),
    ).toThrow("cardinality mismatch");
    expect(() =>
      validateWorkflowAdapterCoverage([
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
        WORKFLOW_ADAPTER_REGISTRATIONS[0]!,
      ]),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      validateWorkflowAdapterCoverage([
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(0, -1),
        { category: "forgedFamily" },
      ]),
    ).toThrow("actual=forgedFamily");
    expect(() =>
      validateWorkflowAdapterCoverage([
        {
          ...WORKFLOW_ADAPTER_REGISTRATIONS[0]!,
          status: "ready",
        },
        ...WORKFLOW_ADAPTER_REGISTRATIONS.slice(1),
      ]),
    ).toThrow("has no compiled executable runner");
  });

  it("seals registry keys and adapter methods against post-construction mutation", () => {
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistry({
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
    const fixture = await buildCanonicalBlockFixture({ transactions: [] });
    const original = makeAdapter();
    const registry = createFraudProofWorkflowRegistry({
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
              FraudProofFamilyWorkflowAdapter
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
      runFraudProofWorkflowFromRetainedDa({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        observation: authenticatedHeaderObservation(fixture),
        sources: [source],
        replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
        registry,
        journal: new MemoryFraudProofWorkflowJournalStore(),
        terminalVerifier,
        releaseFinalityAuthority: releaseFinalityAuthority(),
      }),
    ).resolves.toMatchObject({ kind: "no_fault_detected" });
    expect(mutationAttempted).toBe(true);
    expect(registry.get("doubleSpend")).toBe(before);
  });

  it("deep-freezes registry rows and rejects forged or cross-category runners", () => {
    const first = WORKFLOW_ADAPTER_REGISTRATIONS[0]!;
    expect(Object.isFrozen(first)).toBe(true);
    expect(Reflect.set(first, "status", "ready")).toBe(false);
    expect(first.status).toBe("missing");
    expect(Object.isFrozen(first.existingSurface)).toBe(true);
    expect(Reflect.set(first.existingSurface, 0, "forged-surface")).toBe(false);

    const forgedRunner = {
      runnerVersion: WORKFLOW_ADAPTER_RUNNER,
      runOrResume: async () => "forged",
    };
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: forgedRunner }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");

    const admittedDoubleSpend = WORKFLOW_RUNNER_FACTORIES.doubleSpend(
      async () => {
        throw new Error("runner loader is not invoked during admission");
      },
    );
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
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
      WORKFLOW_ADAPTER_REGISTRATIONS.map(
        (registration) => registration.category,
      ),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    expect(workflowReadinessReport()).toMatchObject({
      registeredCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      requestedCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
      readyCategoryCount: 0,
      missingCategoryCount: SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length,
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        ({ category }) => category === "doubleSpend",
      ),
    ).toMatchObject({
      status: "missing",
      reason: "constrained_adapter_is_not_launch_scope_complete",
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        ({ category }) => category === "nativeScriptDecoding",
      ),
    ).toMatchObject({
      reason: "manual_step_chain_has_no_atomic_driver",
    });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
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
        runFraudProofWorkflowCli({
          mode: "run",
          category: "invalidRange",
          deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
          headerHash: "f8".repeat(28),
          decisionDigest: "f9".repeat(32),
          actuationPermit: {
            permitVersion: WORKFLOW_ACTUATION_PERMIT,
          },
          // The CLI now demands the funding-reservation permit alongside the
          // actuation permit before it looks at adapters, so both are present
          // here: this test is about the missing-adapter refusal, not about
          // the permit refusal that precedes it.
          fundingReservationPermit: {
            permitVersion: WORKFLOW_FUNDING_RESERVATION_PERMIT,
          },
          journalDirectory,
          runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        }),
      ).rejects.toBeInstanceOf(MissingWorkflowAdaptersError);
      await expect(
        import("node:fs/promises").then(({ stat }) => stat(journalDirectory)),
      ).rejects.toMatchObject({ code: "ENOENT" });
    } finally {
      await rm(root, { recursive: true, force: true });
    }
  });
});

describe("canonical reobservation with pending descendants", () => {
  it("backs off exact rebroadcasts without exhausting the proof objective", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let time = Date.parse("2026-08-29T00:00:00.000Z");
    let finish = false;
    let rebroadcasts = 0;
    let transition: fundingAuthority.WorkflowFundingPreparedTransition | null =
      null;
    const recovery = vi
      .spyOn(fundingAuthority, "readWorkflowFundingRecovery")
      .mockImplementation(async () => ({
        transition,
        submissionHandoff: null,
        completionHandoff: null,
        abandonmentHandoff: null,
      }));
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        transition = {
          actionKind: "prove",
          transactionHash: preflight.txHash,
          signedTransactionCborHex: "80",
          transactionBodySha256: "00".repeat(32),
          consumedOutRefs: [],
          producedInputs: [],
        };
        return { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({
        txHash,
        authorizeResubmission,
        signedTransactionCborHex,
      }) => {
        if (finish) {
          transition = null;
          return { kind: "confirmed", txHash: txHash! };
        }
        if (
          authorizeResubmission === undefined ||
          signedTransactionCborHex === undefined
        )
          throw new Error("expected exact signed recovery authority");
        try {
          await authorizeResubmission({
            transactionHash: txHash!,
            signedTransactionCborHex,
          });
        } catch (cause) {
          return { kind: "unknown", reason: String(cause) };
        }
        rebroadcasts += 1;
        return { kind: "pending", txHash };
      },
    });
    const invoke = () =>
      run({ evidence, adapter, journal, now: () => new Date(time) });
    try {
      expect(await invoke()).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      for (let index = 1; index <= 4; index += 1) {
        time += 29_999;
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index - 1);
        time += 1;
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index);
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index);
      }
      expect(adapter.submit).toHaveBeenCalledTimes(1);
      finish = true;
      const result = await invoke();
      expect(result.kind).toBe("completed");
      if (result.kind !== "completed")
        throw new Error("expected objective completion");
      expect(
        result.entries.flatMap(({ event }) =>
          event.kind === "rebroadcast_intent" ? [event.attempt] : [],
        ),
      ).toEqual([2, 3, 4, 5]);
      expect(adapter.prepare).toHaveBeenCalledTimes(1);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    } finally {
      recovery.mockRestore();
    }
  });

  it.each(["action changed", "funding unavailable"])(
    "resumes an unsigned %s without recording a transaction attempt",
    async (cause) => {
      const evidence = await canonicalEvidence();
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const adapter = makeAdapter();
      const original = adapter.preflight;
      adapter.preflight = async () => {
        throw cause === "action changed"
          ? new WorkflowActionChangedError("authenticated action changed")
          : new fundingAuthority.WorkflowFundingReservationUnavailableError();
      };
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      if (pending.kind !== "pending") throw new Error("expected safe yield");
      expect(
        pending.entries.some(({ event }) => event.kind === "submission_intent"),
      ).toBe(false);
      expect(adapter.submit).not.toHaveBeenCalled();
      adapter.preflight = original;
      expect((await run({ evidence, adapter, journal })).kind).toBe(
        "completed",
      );
      expect(adapter.prepare).toHaveBeenCalledTimes(1);
    },
  );

  it("keeps deterministic preflight failures explicit", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    adapter.preflight = async () => {
      throw new Error("local validator rejected the proof witness");
    };
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("validator rejected"),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("preserves a signed attempt when its funding disappears before submit", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    const readiness = vi
      .spyOn(fundingAuthority, "assertWorkflowFundingReservationReadyToSubmit")
      .mockRejectedValueOnce(
        new fundingAuthority.WorkflowFundingReservationUnavailableError(),
      );
    try {
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        reason: expect.stringContaining(PROOF_TX_HASH),
      });
      if (pending.kind !== "pending")
        throw new Error("expected signed recovery");
      expect(pending.entries.at(-1)?.event).toMatchObject({
        kind: "submission_intent",
        txHash: PROOF_TX_HASH,
      });
      expect(adapter.submit).not.toHaveBeenCalled();
      // The exact intent must be reconciled before another body can be built.
      adapter.reconcile = async ({ txHash }) => ({ kind: "pending", txHash });
      expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
      expect(adapter.preflight).toHaveBeenCalledTimes(1);
      expect(adapter.submit).not.toHaveBeenCalled();
    } finally {
      readiness.mockRestore();
    }
  });

  it("keeps the objective resumable after a bounded batch of retired attempts", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let proofAttempts = 0;
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        txHash === PROOF_TX_HASH || txHash === REMOVAL_TX_HASH
          ? { kind: "confirmed", txHash }
          : { kind: "not_found" },
    });
    const preflight = adapter.preflight;
    adapter.preflight = async (context) => {
      const prepared = await preflight(context);
      if (context.action.actionId !== "prove") return prepared;
      proofAttempts += 1;
      return {
        ...prepared,
        txHash:
          proofAttempts === 5
            ? PROOF_TX_HASH
            : proofAttempts.toString(16).padStart(64, "0"),
      };
    };

    const first = await run({ evidence, adapter, journal });
    expect(first).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
    });
    expect(proofAttempts).toBe(3);
    const resumed = await run({ evidence, adapter, journal });
    expect(resumed.kind).toBe("completed");
    if (first.kind !== "pending" || resumed.kind !== "completed")
      throw new Error("expected bounded continuation of the same objective");
    expect(resumed.workflowId).toBe(first.workflowId);
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
    expect(
      resumed.entries.flatMap(({ event }) =>
        event.kind === "submission_intent" && event.actionId === "prove"
          ? [event.attempt]
          : [],
      ),
    ).toEqual([1, 2, 3, 4, 5]);
    expect(resumed.entries.some(({ event }) => event.kind === "stalled")).toBe(
      false,
    );
  });

  it("yields uncertain signed recovery and later confirms the same attempt", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let available = false;
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        available
          ? { kind: "confirmed", txHash: txHash! }
          : { kind: "unknown", reason: "canonical history is catching up" },
    });
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
    });
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    available = true;
    const result = await run({ evidence, adapter, journal });
    expect(result.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
  });

  it("yields an admitted ownership collision without changing the submitted journal", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const onChain = new Set<string>();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        if (txHash === REMOVAL_TX_HASH) return { kind: "pending", txHash };
        onChain.add(txHash!);
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () => ({
      kind: "action_required",
      action: onChain.has(PROOF_TX_HASH)
        ? { actionId: "remove", input: { step: 1 } }
        : { actionId: "prove", input: { step: 0 } },
    });
    const first = await run({ evidence, adapter, journal });
    expect(first.kind).toBe("pending");
    onChain.clear();
    const unavailable = vi
      .spyOn(fundingAuthority, "reobserveWorkflowFundingReservationTransaction")
      .mockResolvedValue(false);
    try {
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      if (first.kind !== "pending" || pending.kind !== "pending")
        throw new Error("expected pending workflow");
      expect(pending.entries).toEqual(first.entries);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    } finally {
      unavailable.mockRestore();
    }
  });

  it("keeps unknown read-only reconciliation pending instead of blocking execution", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    adapter.reconcile = async () => ({
      kind: "unknown",
      reason: "Exact recorded transaction requires live rebroadcast authority",
    });
    const readonly = vi
      .spyOn(actuationAuthority, "workflowJournalIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, journal })).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining("live rebroadcast authority"),
      });
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("waits for fresh authority before replacing an expired read-only attempt", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    adapter.reconcile = async () => ({ kind: "not_found" });
    const readonly = vi
      .spyOn(actuationAuthority, "workflowJournalIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, journal })).toMatchObject({
        kind: "pending",
        reason: "Canonical workflow requires fresh submission authority",
      });
      expect(adapter.preflight).toHaveBeenCalledTimes(1);
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("reconciles a rolled-back parent before its already submitted descendant", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const onChain = new Set<string>();
    let finishChild = false;
    const reconciled: string[] = [];
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        reconciled.push(txHash!);
        if (txHash === REMOVAL_TX_HASH && !finishChild)
          return { kind: "pending", txHash };
        onChain.add(txHash!);
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () =>
      onChain.has(REMOVAL_TX_HASH)
        ? { kind: "completed", terminal: terminal(evidence.headerHash) }
        : {
            kind: "action_required",
            action: onChain.has(PROOF_TX_HASH)
              ? { actionId: "remove", input: { step: 1 } }
              : { actionId: "prove", input: { step: 0 } },
          };
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    onChain.clear();
    finishChild = true;
    reconciled.length = 0;
    expect((await run({ evidence, adapter, journal })).kind).toBe("completed");
    expect(reconciled).toEqual([PROOF_TX_HASH, REMOVAL_TX_HASH]);
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("rebuilds a stably expired parent and derives a fresh child identity", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const replacementParent = "b1".repeat(32);
    const replacementChild = "b2".repeat(32);
    let parentOnChain: string | undefined;
    let childOnChain = false;
    let expiredParent = false;
    const reconciled: string[] = [];
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        reconciled.push(txHash!);
        // Signed recovery supplies authenticated stable expiry, not simple absence.
        if (txHash === PROOF_TX_HASH && expiredParent)
          return { kind: "not_found" };
        if (txHash === PROOF_TX_HASH || txHash === replacementParent) {
          parentOnChain = txHash;
          return { kind: "confirmed", txHash };
        }
        if (txHash === REMOVAL_TX_HASH) return { kind: "pending", txHash };
        childOnChain = true;
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    const preflight = adapter.preflight;
    adapter.preflight = vi.fn(async (context) => ({
      ...(await preflight(context)),
      txHash:
        context.action.actionId === "prove"
          ? expiredParent
            ? replacementParent
            : PROOF_TX_HASH
          : parentOnChain === replacementParent
            ? replacementChild
            : REMOVAL_TX_HASH,
    }));
    adapter.observe = async (): Promise<
      Awaited<ReturnType<FraudProofFamilyWorkflowAdapter["observe"]>>
    > => {
      if (childOnChain) {
        const value = terminal(evidence.headerHash);
        return {
          kind: "completed",
          terminal: {
            ...value,
            proofToken: {
              ...value.proofToken,
              outRef: `${replacementParent}#0`,
              createdByTxHash: replacementParent,
            },
            correction: {
              ...value.correction,
              removalTxHash: replacementChild,
              referencedProofTokenOutRef: `${replacementParent}#0`,
            },
            economics: {
              ...value.economics,
              proverRewardOutputOutRef: `${replacementChild}#0`,
            },
          },
        };
      }
      return {
        kind: "action_required",
        action:
          parentOnChain === undefined
            ? { actionId: "prove", input: { step: 0 } }
            : {
                actionId: `remove:${parentOnChain}`,
                input: { step: 1, parent: parentOnChain },
              },
      };
    };
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    parentOnChain = undefined;
    expiredParent = true;
    reconciled.length = 0;
    expect((await run({ evidence, adapter, journal })).kind).toBe("completed");
    expect(reconciled).toEqual([
      PROOF_TX_HASH,
      replacementParent,
      replacementChild,
    ]);
    expect(adapter.submit).toHaveBeenCalledTimes(4);
    expect(
      vi
        .mocked(adapter.submit)
        .mock.calls.map(([input]) => input.action.actionId),
    ).toEqual([
      "prove",
      `remove:${PROOF_TX_HASH}`,
      "prove",
      `remove:${replacementParent}`,
    ]);
  });
});

it.each(["prove", "remove", "superseded"])(
  "reconciles a restored funding cursor after a journal-write crash: %s",
  async (scenario) => {
    const actionId = scenario === "remove" ? "remove" : "prove";
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        txHash === REMOVAL_TX_HASH
          ? { kind: "pending", txHash }
          : { kind: "confirmed", txHash: txHash! },
    });
    const first = await run({ evidence, adapter, journal });
    if (first.kind !== "pending")
      throw new Error("fixture did not leave its child pending");
    const entries = [...first.entries];
    const append = (event: FraudProofWorkflowJournalEvent) =>
      entries.push({
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: first.workflowId,
        identity: first.identity,
        sequence: entries.length,
        recordedAt: "2026-08-29T00:00:00.000Z",
        event,
      });
    if (actionId === "remove") {
      // The parent has already been recovered; the pending child is being selected again.
      append({ kind: "reobserved", actionId: "prove", txHash: PROOF_TX_HASH });
      append({
        kind: "reconciled",
        actionId: "prove",
        outcome: "confirmed",
        txHash: PROOF_TX_HASH,
      });
      append({ kind: "confirmed", actionId: "prove", txHash: PROOF_TX_HASH });
    }
    const prepared = entries[1]!.event;
    const index = entries.findIndex(
      ({ event }) =>
        event.kind === "preflight_passed" && event.actionId === actionId,
    );
    const preflight = entries[index]!.event;
    const submissionIntent = entries[index + 1]!.event;
    if (
      prepared.kind !== "prepared" ||
      preflight.kind !== "preflight_passed" ||
      submissionIntent.kind !== "submission_intent"
    )
      throw new Error("fixture lacks its exact prepared submission");
    if (scenario === "superseded") {
      const replacementHash = "b3".repeat(32);
      append({ kind: "reobserved", actionId: "prove", txHash: PROOF_TX_HASH });
      append({
        kind: "reconciled",
        actionId: "prove",
        outcome: "not_found",
        txHash: PROOF_TX_HASH,
      });
      append({ ...preflight, txHash: replacementHash });
      append({ ...submissionIntent, attempt: 2, txHash: replacementHash });
    }
    const recover = () =>
      reconcileWorkflowFundingSubmissionHandoff({
        handoff: {
          workflowId: first.workflowId,
          identity: first.identity,
          preparedArtifactDigest: prepared.artifactDigest,
          expectedJournalSequence: index,
          preflight,
          submissionIntent,
        },
        entries,
      });
    if (scenario === "superseded") {
      expect(recover).toThrow("superseded by a later submission intent");
      return;
    }
    const recovered = recover();
    expect(recovered).toEqual([
      { kind: "reobserved", actionId, txHash: submissionIntent.txHash },
    ]);
    for (const event of recovered) append(event);
    expect(() =>
      validateFraudProofWorkflowJournal({
        workflowId: first.workflowId,
        entries,
        expectedIdentity: first.identity,
      }),
    ).not.toThrow();
  },
);

it("reuses proof material after the same authenticated commitment is re-included at a new L1 point", async () => {
  const fixture = await buildCanonicalBlockFixture({ transactions: [] });
  const evidenceAt = (slot: bigint, blockHash: string) =>
    canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture, {
        chainPoint: { slot, blockHash },
      }),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
    });
  const original = await evidenceAt(4242n, "31".repeat(32));
  const relocated = await evidenceAt(4300n, "32".repeat(32));
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const adapter = makeAdapter({
    reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
  });
  const validate = vi.fn(async () => undefined);
  adapter.validatePreparedArtifact = validate;
  const first = await run({ evidence: original, adapter, journal });
  expect(first.kind).toBe("pending");
  if (first.kind !== "pending")
    throw new Error("expected pending original intent");
  const prepared = first.entries.find(
    ({ event }) => event.kind === "prepared",
  )!;
  const resumed = await run({ evidence: relocated, adapter, journal });
  expect(resumed.kind).toBe("pending");
  expect(validate).toHaveBeenCalledWith(
    expect.objectContaining({ evidence: relocated }),
  );
  expect(adapter.prepare).toHaveBeenCalledOnce();
  expect(adapter.submit).toHaveBeenCalledOnce();
  expect(
    (await journal.load(first.workflowId)).find(
      ({ event }) => event.kind === "prepared",
    ),
  ).toEqual(prepared);
  // The original source locator remains in the immutable artifact for audit.
  expect(prepared.event).toMatchObject({
    artifact: {
      evidenceBinding: { l1Slot: "4242", l1BlockHash: "31".repeat(32) },
    },
  });
});

it.each(["headerHash", "payloadEnvelopeSha256", "payloadSha256"])(
  "rejects persisted %s substitution when reusing relocated evidence",
  async (field) => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    const first = await run({ evidence, adapter, journal });
    if (first.kind !== "pending")
      throw new Error("expected pending original intent");
    const forged = structuredClone(first.entries);
    const prepared = forged.find(
      ({ event }) => event.kind === "prepared",
    )!.event;
    if (prepared.kind !== "prepared")
      throw new Error("expected prepared evidence");
    const originalBinding = prepared.artifact.evidenceBinding;
    if (
      typeof originalBinding !== "object" ||
      originalBinding === null ||
      Array.isArray(originalBinding)
    )
      throw new Error("missing evidence binding");
    const replacement = {
      ...prepared.artifact,
      evidenceBinding: {
        ...originalBinding,
        [field]: "ef".repeat(field === "headerHash" ? 28 : 32),
      },
    };
    const entries = forged.map((entry) =>
      entry.event.kind === "prepared"
        ? {
            ...entry,
            event: {
              ...entry.event,
              artifact: replacement,
              artifactDigest: journalJsonDigest(replacement),
            },
          }
        : entry,
    );
    const hostileJournal: FraudProofWorkflowJournalStore = {
      load: async () => entries,
      append: async () => {
        throw new Error("must not append");
      },
    };
    await expect(
      run({ evidence, adapter, journal: hostileJournal }),
    ).rejects.toThrow("does not match the proof-critical artifact");
    expect(adapter.submit).toHaveBeenCalledOnce();
  },
);
