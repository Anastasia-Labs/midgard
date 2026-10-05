import "./workflow.provisional-completion.js";
import "./workflow.q55-w-o6-deterministic-violation-classification.js";

import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import {
  createNetworkIdAuthenticatedL1TerminalVerifier,
  type ManifestBoundNetworkIdWorkflow,
  runOrResumeManifestBoundNetworkIdWorkflow,
} from "../src/network-id/workflow-adapter.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
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
  createDoubleSpendConstrainedWorkflowAdapter,
  type ManifestBoundDoubleSpendWorkflow,
  runOrResumeManifestBoundDoubleSpendWorkflow,
} from "../src/workflow/double-spend-adapter.js";
import {
  computeFraudProofWorkflowId,
  ConcurrentFraudProofWorkflowWriteError,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
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
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../src/workflow/orchestrator.js";
import { StateQueueHeaderNotLiveError } from "../src/workflow/raw-l1-family-derivation.js";
import { computeFraudProofReleaseFinalityPolicyDigest } from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  canonicalEvidence,
  DEPLOYMENT_FINGERPRINT,
  makeAdapter,
  PROOF_TX_HASH,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  REMOVAL_TX_HASH,
  retainedDaSource,
  run,
  terminal,
  terminalVerifier,
} from "./workflow.make-adapter.js";

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
      depth = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
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
          observedAt: {
            ...candidate.observedAt,
            confirmationDepth: RELEASE_FINALITY_POLICY.confirmationDepth - 1,
          },
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
        `confirmation depth is below the release threshold: required=${RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2} actual=${RELEASE_FINALITY_POLICY.confirmationDepth - 1}`,
      ),
    });
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
