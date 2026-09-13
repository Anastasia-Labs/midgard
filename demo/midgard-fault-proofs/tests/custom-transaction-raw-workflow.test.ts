import { expect, it, vi } from "vitest";

import { prepareMintDeclaredAssetLimitAcceptedArtifact } from "../src/mint-declared-asset-limit/replay.js";
import { executeManifestBoundMintDeclaredAssetLimitWorkflow } from "../src/mint-declared-asset-limit/v1.js";
import { prepareObserversForbiddenAcceptedArtifact } from "../src/observers-forbidden-on-untagged-network/replay.js";
import { executeManifestBoundObserversForbiddenWorkflow } from "../src/observers-forbidden-on-untagged-network/v1.js";
import {
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
  runFraudProofWorkflowFromRetainedDa,
  type WorkflowRawFamilyEvidence,
} from "../src/workflow/orchestrator.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const categories = [
  "mintDeclaredAssetLimit",
  "observersForbiddenOnUntaggedNetwork",
] as const;
type Category = (typeof categories)[number];
const deploymentFingerprint = "22".repeat(32);
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
    blueprintHash: "44".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};
const prepare = async (
  category: Category,
  routed: WorkflowRawFamilyEvidence,
): Promise<JournalJsonObject> => {
  if (
    category === "mintDeclaredAssetLimit" &&
    routed.kind === "mint_declared_asset_limit"
  )
    return await prepareMintDeclaredAssetLimitAcceptedArtifact(routed.evidence);
  if (
    category === "observersForbiddenOnUntaggedNetwork" &&
    routed.kind === "observers_forbidden_on_untagged_network"
  )
    return await prepareObserversForbiddenAcceptedArtifact(routed.evidence);
  throw new Error("raw category changed");
};
const fixtureFor = async (category: Category) => {
  const transaction = buildFixtureTransaction({
    spendInputs: [outRefCbor(7, 0n)],
    fee: 1n,
    ...(category === "mintDeclaredAssetLimit"
      ? {
          mintPolicyItems: [
            Buffer.concat([
              Buffer.from("82581c", "hex"),
              Buffer.alloc(28, 3),
              Buffer.from("b9400100", "hex"),
            ]),
          ],
        }
      : {
          requiredObservers: [Buffer.alloc(28, 1)],
          scriptIntegrityHash: Buffer.alloc(32, 2),
        }),
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const sources = [
    {
      sourceId: "raw-workflow-fixture",
      fetchPayloadByHeaderHash: async () => ({
        ok: true as const,
        sourceId: "raw-workflow-fixture",
        sourcePeerId: "fixture-peer",
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        attempts: [],
        provenance: {
          trustClass: "public_or_permissionless_da" as const,
          sourceId: "raw-workflow-fixture/fixture-peer",
          grade: "security" as const,
        },
      }),
    },
  ];
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error(
      "raw preparation unexpectedly entered canonical preparation or submission",
    );
  });
  const prepareRaw = vi.fn(
    async (routed: WorkflowRawFamilyEvidence) =>
      await prepare(category, routed),
  );
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: forbidden,
    prepareRaw,
    validatePreparedRawArtifact: async ({ routed, artifact }) => {
      if (
        journalJsonDigest(await prepare(category, routed)) !==
        journalJsonDigest(artifact)
      )
        throw new Error("raw saved artifact changed");
    },
    observe: async () => ({
      kind: "pending",
      reason: "local raw preparation boundary",
    }),
    preflight: forbidden,
    submit: forbidden,
    reconcile: forbidden,
  };
  const workflow = {
    binding: {
      deploymentFingerprint,
      definition: { headerHash: fixture.headerHash },
    },
    decisionDigest: undefined,
    l1: { observeHeader: async () => observation },
    adapter,
    terminalVerifier: {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
      verify: forbidden,
    },
    releaseFinalityAuthority,
  };
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const execute = () =>
    category === "mintDeclaredAssetLimit"
      ? executeManifestBoundMintDeclaredAssetLimitWorkflow({
          workflow,
          sources,
          journal,
        } as unknown as Parameters<
          typeof executeManifestBoundMintDeclaredAssetLimitWorkflow
        >[0])
      : executeManifestBoundObserversForbiddenWorkflow({
          workflow,
          sources,
          journal,
        } as unknown as Parameters<
          typeof executeManifestBoundObserversForbiddenWorkflow
        >[0]);
  return {
    workflow,
    observation,
    sources,
    journal,
    execute,
    prepareRaw,
    forbidden,
  };
};

it.each(categories)(
  "%s admits the exact raw retained route and revalidates its prepared artifact",
  async (category) => {
    const fixture = await fixtureFor(category);
    const result = await fixture.execute();
    expect(result.kind).toBe("pending");
    if (result.kind !== "pending")
      throw new Error("raw fixture did not enter common workflow");
    const entries = await fixture.journal.load(result.workflowId);
    expect(entries[1]?.event.kind).toBe("prepared");
    expect(fixture.prepareRaw).toHaveBeenCalledOnce();
    expect(fixture.forbidden).not.toHaveBeenCalled();
    expect((await fixture.execute()).kind).toBe("pending");
    expect(fixture.prepareRaw).toHaveBeenCalledOnce();
    const changed = new MemoryFraudProofWorkflowJournalStore();
    for (const entry of entries) {
      if (entry.event.kind !== "prepared") {
        await changed.append(entry, entry.sequence);
        continue;
      }
      const family = entry.event.artifact.familyArtifact as JournalJsonObject;
      const artifact = {
        ...entry.event.artifact,
        familyArtifact: { ...family, detectionId: "changed" },
      };
      await changed.append(
        {
          ...entry,
          event: {
            kind: "prepared",
            artifact,
            artifactDigest: journalJsonDigest(artifact),
          },
        },
        entry.sequence,
      );
    }
    const executeChanged =
      category === "mintDeclaredAssetLimit"
        ? executeManifestBoundMintDeclaredAssetLimitWorkflow
        : executeManifestBoundObserversForbiddenWorkflow;
    await expect(
      executeChanged({
        ...fixture,
        workflow: fixture.workflow,
        journal: changed,
      } as unknown as Parameters<
        typeof executeManifestBoundMintDeclaredAssetLimitWorkflow
      >[0] &
        Parameters<typeof executeManifestBoundObserversForbiddenWorkflow>[0]),
    ).rejects.toThrow("raw saved artifact changed");
  },
);

it.each(categories)(
  "%s refuses another family's raw route before preparing",
  async (category) => {
    const fixture = await fixtureFor(category);
    const other =
      category === "mintDeclaredAssetLimit"
        ? "observersForbiddenOnUntaggedNetwork"
        : "mintDeclaredAssetLimit";
    const adapter: FraudProofFamilyWorkflowAdapter = {
      ...fixture.workflow.adapter,
      category: other,
    };
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        deploymentFingerprint,
        observation: fixture.observation,
        sources: fixture.sources,
        replayer:
          other === "mintDeclaredAssetLimit"
            ? MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY
            : OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
        registry: createFraudProofWorkflowRegistry({
          adapters: [adapter],
          launchScope: [other],
        }),
        journal: fixture.journal,
        terminalVerifier: fixture.workflow.terminalVerifier,
        releaseFinalityAuthority,
      }),
    ).rejects.toThrow(/exact|registry|route/);
    expect(fixture.prepareRaw).not.toHaveBeenCalled();
  },
);
