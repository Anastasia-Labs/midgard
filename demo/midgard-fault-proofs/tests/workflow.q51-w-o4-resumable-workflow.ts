import "./workflow.provisional-completion.js";
import "./workflow.q55-w-o6-deterministic-violation-classification.js";

import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it, vi } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import { runOrResumeManifestBoundNetworkIdWorkflow } from "../src/network-id/workflow-adapter.create-manifest-bound-network-id-workflow.js";
import { createNetworkIdAuthenticatedL1TerminalVerifier } from "../src/network-id/workflow-adapter.create-network-id-raw-l1-observation-port.js";
import type { ManifestBoundNetworkIdWorkflow } from "../src/network-id/workflow-adapter.seal-manifest-bound-network-id-runtime.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import {
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.detect-double-withdraws.js";
import {
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
} from "../src/workflow/complete-replay.replay-context-identity.js";
import { createDoubleSpendConstrainedWorkflowAdapter } from "../src/workflow/double-spend-adapter.create-double-spend-constrained-workflow-adapter.js";
import { runOrResumeManifestBoundDoubleSpendWorkflow } from "../src/workflow/double-spend-adapter.create-manifest-bound-double-spend-workflow.js";
import type { ManifestBoundDoubleSpendWorkflow } from "../src/workflow/double-spend-adapter.preflight-of.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/identity.js";
import { FRAUD_PROOF_WORKFLOW_SAFETY } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflowFromRetainedDa } from "../src/workflow/orchestrator.run-fraud-proof-workflow-from-retained-da.js";
import { StateQueueHeaderNotLiveError } from "../src/workflow/raw-l1-family-derivation.state-queue-topology.js";
import { computeFraudProofReleaseFinalityPolicyDigest } from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  chainOf,
  createFixtureChain,
  DEPLOYMENT_FINGERPRINT,
  makeAdapter,
  PROOF_TX_HASH,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  REMOVAL_TX_HASH,
  retainedDaSource,
  run,
  terminal,
  terminalVerifier,
} from "./workflow.make-adapter.js";

/** A retained-DA run of `fixture`'s block in a fresh process. */
const retainedDaRun = (
  fixture: Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
) => ({
  deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
  observation: authenticatedHeaderObservation(fixture),
  sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
  run: createTestWorkflowProcess().run({
    category: "doubleSpend",
    headerHash: fixture.headerHash,
  }),
  terminalVerifier,
  releaseFinalityAuthority: releaseFinalityAuthority(),
});

describe("Q51/W-O4 resumable workflow", () => {
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
      // One process across every run: the first sends both transactions,
      // the later ones read their effect on the chain.
      const process = createTestWorkflowProcess();
      const objective = { category, headerHash: observation.headerHash };
      const invocation = {
        workflow,
        sources: [retainedDaSource(fixture.payloadEnvelopeCbor)],
      };
      const removalAction = {
        actionId: `remove:${terminal(observation.headerHash).correction.removedStateQueueOutRef}`,
        input: { stage: "remove", requiresMutationLease: false },
      };
      // The proof lands; the removal waits in the mempool.
      const chain = createFixtureChain();
      const base = makeAdapter({
        chain,
        submit: async ({ preflight }) => {
          if (preflight.txHash === PROOF_TX_HASH) chain.land(preflight.txHash);
          else chain.accept(preflight.txHash);
          return { kind: "submitted", txHash: preflight.txHash };
        },
      });
      const pendingAdapter = {
        ...base,
        category,
        prepare:
          category === "doubleSpend"
            ? createDoubleSpendConstrainedWorkflowAdapter(
                workflow.adapterConfig,
              ).prepare
            : makeAdapter().prepare,
        observe: async () =>
          chain.seen(REMOVAL_TX_HASH)
            ? {
                kind: "pending" as const,
                reason: "the removal awaits inclusion",
              }
            : {
                kind: "action_required" as const,
                action: chain.landed(PROOF_TX_HASH)
                  ? removalAction
                  : { actionId: "prove", input: { stage: "step_04" } },
              },
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
        },
        releaseFinalityAuthority: workflow.releaseFinalityAuthority,
        terminalVerifier: createNetworkIdAuthenticatedL1TerminalVerifier(rawL1),
      } as unknown as ManifestBoundNetworkIdWorkflow;
      const resume = async () =>
        category === "doubleSpend"
          ? await runOrResumeManifestBoundDoubleSpendWorkflow({
              ...invocation,
              run: process.run(objective),
            })
          : await runOrResumeManifestBoundNetworkIdWorkflow({
              ...invocation,
              run: process.run(objective),
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
          adapters: [process.recording(pendingAdapter)],
          launchScope: [category],
        }),
        run: process.run(objective),
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
      // The proof and the removal were each sent once, by the first run.
      expect(process.sent().map(({ actionId }) => actionId)).toEqual([
        "prove",
        removalAction.actionId,
      ]);
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
      ...retainedDaRun(fixture),
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      resolveReplayContext,
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
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
    const adapter = makeAdapter();
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        ...retainedDaRun(fixture),
        replayer: forgedReplayer,
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: ["doubleSpend"],
        }),
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
    const adapter = makeAdapter();
    const result = await runFraudProofWorkflowFromRetainedDa({
      ...retainedDaRun(fixture),
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
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
        ...retainedDaRun(fixture),
        sources: [],
        replayer,
        registry,
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

  it("sends the proof, follows it onto the chain, then sends and follows the removal to completion", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    const process = createTestWorkflowProcess();
    const result = await run({ evidence, adapter, process });
    expect(result.kind).toBe("completed");
    if (result.kind !== "completed") return;
    expect(
      process.sent().map(({ actionId, txHash }) => [actionId, txHash]),
    ).toEqual([
      ["prove", PROOF_TX_HASH],
      ["remove", REMOVAL_TX_HASH],
    ]);
    expect(result.identity).toMatchObject({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: evidence.headerHash },
    });
  });

  it("refuses a terminal whose removal references a substituted proof token", async () => {
    const evidence = await canonicalEvidence();
    const verifier = terminalVerifierOf(async ({ candidate }) => ({
      ...candidate,
      correction: {
        ...candidate.correction,
        referencedProofTokenOutRef: `${"ee".repeat(32)}#1`,
      },
    }));
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      process: createTestWorkflowProcess(),
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
    const verifier = terminalVerifierOf(
      async ({ candidate }) =>
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
    );
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      process: createTestWorkflowProcess(),
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
    const verifier = terminalVerifierOf(
      async ({ candidate, releaseFinality }) => {
        observedPolicies.push(releaseFinality.policyDigest);
        return {
          ...candidate,
          observedAt: {
            ...candidate.observedAt,
            confirmationDepth: RELEASE_FINALITY_POLICY.confirmationDepth - 1,
          },
        };
      },
    );
    const result = await run({
      evidence,
      adapter: makeAdapter(),
      process: createTestWorkflowProcess(),
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
        process: createTestWorkflowProcess(),
        finalityAuthority: releaseFinalityAuthority({
          deploymentIdentityDigest: "f1".repeat(32),
        }),
      }),
    ).rejects.toThrow("returned a different deployment identity");
  });

  it("builds an ambiguous submission the chain does not show again", async () => {
    const evidence = await canonicalEvidence();
    const chain = createFixtureChain();
    let submitCalls = 0;
    const adapter = makeAdapter({
      chain,
      submit: async ({ preflight }) => {
        submitCalls += 1;
        if (submitCalls === 1) {
          // The first attempt never reaches the chain.
          return { kind: "ambiguous", detail: "connection reset" };
        }
        chain.land(preflight.txHash);
        return { kind: "submitted", txHash: preflight.txHash };
      },
    });
    const process = createTestWorkflowProcess();
    const result = await run({ evidence, adapter, process });
    expect(result.kind).toBe("completed");
    // The next state shows no first attempt, so the proof is built again.
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      "prove",
      "prove",
      "remove",
    ]);
    expect(adapter.submit).toHaveBeenCalledTimes(3);
  });

  it("refuses an adapter that reports a hash different from the one its preflight built", async () => {
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
      process: createTestWorkflowProcess(),
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining(
        `reported ${"d4".repeat(32)}, but its preflight built ${PROOF_TX_HASH}`,
      ),
    });
    expect(adapter.submit).toHaveBeenCalledOnce();
  });

  it("resumes a submission waiting in the mempool without submitting it twice", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter({ lands: false });
    const chain = chainOf(adapter);
    const process = createTestWorkflowProcess();
    // The proof waits in the mempool; the removal is built over it.
    const first = await run({ evidence, adapter, process });
    expect(first).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(`${REMOVAL_TX_HASH} awaits inclusion`),
    });
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    const again = await run({ evidence, adapter, process });
    expect(again.kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    chain.land(PROOF_TX_HASH);
    chain.land(REMOVAL_TX_HASH);
    const second = await run({ evidence, adapter, process });
    expect(second.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      "prove",
      "remove",
    ]);
  });

  it("marks an adapter preflight failure as resumable once L1 observation catches up", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    vi.mocked(adapter.preflight).mockRejectedValueOnce(
      new Error("Expected exactly one fraudulent block UTxO, found 0."),
    );
    const process = createTestWorkflowProcess();
    const result = await run({ evidence, adapter, process });
    expect(result).toMatchObject({
      kind: "stalled",
      phase: "preflight",
      reason: expect.stringContaining(
        "preflight failed for prove: Error: Expected exactly one fraudulent block UTxO, found 0.",
      ),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
    // A later resume re-observes the family and completes normally.
    const resumed = await run({ evidence, adapter, process });
    expect(resumed).toMatchObject({ kind: "completed" });
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("refuses submission without passed reference-script preflight", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter({ referenceScripts: false });
    const result = await run({
      evidence,
      adapter,
      process: createTestWorkflowProcess(),
    });
    expect(result).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("requires reference scripts"),
    });
    expect(result).not.toHaveProperty("phase");
    expect(adapter.submit).not.toHaveBeenCalled();
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
