import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, it, vi } from "vitest";

import { prepareMintDeclaredAssetLimitAcceptedArtifact } from "../src/mint-declared-asset-limit/replay.js";
import { executeManifestBoundMintDeclaredAssetLimitWorkflow } from "../src/mint-declared-asset-limit/v1.js";
import { prepareObserversForbiddenAcceptedArtifact } from "../src/observers-forbidden-on-untagged-network/replay.js";
import { executeManifestBoundObserversForbiddenWorkflow } from "../src/observers-forbidden-on-untagged-network/v1.js";
import {
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import type { JournalJsonObject } from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type WorkflowRawFamilyEvidence,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflowFromRetainedDa } from "../src/workflow/orchestrator.run-fraud-proof-workflow-from-retained-da.js";
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
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";

const categories = [
  "mintDeclaredAssetLimit",
  "observersForbiddenOnUntaggedNetwork",
] as const;
type Category = (typeof categories)[number];
const deploymentFingerprint = "22".repeat(32);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
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
  const observe = vi.fn<FraudProofFamilyWorkflowAdapter["observe"]>(
    async () => ({
      kind: "pending",
      reason: "local raw preparation boundary",
    }),
  );
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: forbidden,
    prepareRaw,
    observe,
    preflight: forbidden,
    submit: forbidden,
  };
  const workflow = {
    binding: {
      deploymentFingerprint,
      definition: { headerHash: fixture.headerHash },
    },
    decisionDigest: undefined,
    l1: { observeHeader: async () => observation },
    adapter,
    terminalVerifier: terminalVerifierOf(forbidden),
    releaseFinalityAuthority,
  };
  // Each call is a fresh process: a restart holds nothing in memory.
  const execute = () => {
    const run = createTestWorkflowProcess().run({
      category,
      headerHash: fixture.headerHash,
    });
    return category === "mintDeclaredAssetLimit"
      ? executeManifestBoundMintDeclaredAssetLimitWorkflow({
          workflow,
          sources,
          run,
        } as unknown as Parameters<
          typeof executeManifestBoundMintDeclaredAssetLimitWorkflow
        >[0])
      : executeManifestBoundObserversForbiddenWorkflow({
          workflow,
          sources,
          run,
        } as unknown as Parameters<
          typeof executeManifestBoundObserversForbiddenWorkflow
        >[0]);
  };
  return {
    workflow,
    observation,
    sources,
    execute,
    prepareRaw,
    observe,
    forbidden,
    headerHash: fixture.headerHash,
  };
};

it.each(categories)(
  "%s admits the exact raw retained route and prepares it again after a restart",
  async (category) => {
    const fixture = await fixtureFor(category);
    const result = await fixture.execute();
    expect(result.kind).toBe("pending");
    if (result.kind !== "pending")
      throw new Error("raw fixture did not enter common workflow");
    expect(fixture.prepareRaw).toHaveBeenCalledOnce();
    const artifact = fixture.observe.mock.calls[0]![0].artifact;
    expect(artifact).toEqual(await fixture.prepareRaw.mock.results[0]!.value);
    expect(fixture.observe.mock.calls[0]![0].identity).toMatchObject({
      category,
      target: { kind: "state_queue_header", headerHash: fixture.headerHash },
    });
    expect(fixture.forbidden).not.toHaveBeenCalled();
    expect((await fixture.execute()).kind).toBe("pending");
    expect(fixture.prepareRaw).toHaveBeenCalledTimes(2);
    expect(fixture.observe.mock.calls.at(-1)![0].artifact).toEqual(artifact);
    expect(fixture.forbidden).not.toHaveBeenCalled();
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
        run: createTestWorkflowProcess().run({
          category: other,
          headerHash: fixture.headerHash,
        }),
        terminalVerifier: fixture.workflow.terminalVerifier,
        releaseFinalityAuthority,
      }),
    ).rejects.toThrow(/exact|registry|route/);
    expect(fixture.prepareRaw).not.toHaveBeenCalled();
  },
);
