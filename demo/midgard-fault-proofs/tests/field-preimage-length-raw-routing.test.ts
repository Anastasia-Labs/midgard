import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import {
  normalizeWorkflowJson,
  type WorkflowJsonObject,
} from "../src/workflow/identity.js";
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

const category = "fieldPreimageLengthMismatch";
const hubOraclePolicyId = "22".repeat(28);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    hubOraclePolicyId,
    blueprintHash: "44".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};

const fixtureFor = async () => {
  const transaction = buildFixtureTransaction({
    spendInputs: [outRefCbor(7, 0n)],
    fee: 1n,
  });
  const lengths = [
    ...decodeMidgardNativeTxProofFieldLengths(
      Buffer.from(transaction.source.source.field_preimage_lengths_cbor, "hex"),
    ),
  ];
  lengths[0] = lengths[0]! + 1;
  const source = {
    ...transaction.source,
    source: {
      ...transaction.source.source,
      field_preimage_lengths_cbor:
        encodeMidgardNativeTxProofFieldLengths(lengths).toString("hex"),
    },
  };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      {
        ...transaction,
        source,
        sourceValueBytes: Buffer.from(
          Data.to(source, SDK.L2TransactionSource),
          "hex",
        ),
      },
    ],
  });
  const material = (routed: WorkflowRawFamilyEvidence) => {
    if (routed.kind !== "field_preimage_length_mismatch")
      throw new Error("wrong raw family");
    // This adapter only probes routing and the prepared artifact. The installed
    // family adapter owns the reversible typed encoding of proof material.
    return normalizeWorkflowJson(
      JSON.parse(
        JSON.stringify(
          {
            prepared: routed.evidence.prepared,
            fieldMaterial: routed.evidence.fieldMaterial,
            stageEvidence: routed.evidence.stageEvidence,
          },
          (_, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      ),
    ) as WorkflowJsonObject;
  };
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("raw route entered canonical preparation or submission");
  });
  const prepareRaw = vi.fn(async (routed: WorkflowRawFamilyEvidence) =>
    material(routed),
  );
  const observe = vi.fn<FraudProofFamilyWorkflowAdapter["observe"]>(
    async () => ({ kind: "pending", reason: "routing boundary" }),
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
  const input = {
    hubOraclePolicyId,
    observation: authenticatedHeaderObservation(fixture),
    sources: [
      {
        sourceId: "field-length-route-fixture",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          sourceId: "field-length-route-fixture",
          sourcePeerId: "fixture-peer",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "field-length-route-fixture/fixture-peer",
            grade: "security" as const,
          },
        }),
      },
    ],
    replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: [category],
    }),
    run: createTestWorkflowProcess().run({
      category,
      headerHash: fixture.headerHash,
    }),
    terminalVerifier: terminalVerifierOf(forbidden),
    releaseFinalityAuthority,
  };
  return {
    input,
    adapter,
    prepareRaw,
    observe,
    forbidden,
    headerHash: fixture.headerHash,
  };
};

it("routes authenticated raw length mismatch into raw preparation, preparing it again after a restart", async () => {
  const fixture = await fixtureFor();
  const result = await runFraudProofWorkflowFromRetainedDa(fixture.input);
  expect(result.kind).toBe("pending");
  const artifact = fixture.observe.mock.calls[0]![0].artifact;
  expect(artifact).toMatchObject({
    prepared: {
      direction: "wrongfulAcceptance",
      headerHash: fixture.headerHash,
    },
  });
  expect(fixture.observe.mock.calls[0]![0].identity.target).toEqual({
    kind: "state_queue_header",
    headerHash: fixture.headerHash,
  });
  // A restart holds nothing: the same evidence prepares the same artifact.
  expect(
    (
      await runFraudProofWorkflowFromRetainedDa({
        ...fixture.input,
        run: createTestWorkflowProcess().run({
          category,
          headerHash: fixture.headerHash,
        }),
      })
    ).kind,
  ).toBe("pending");
  expect(fixture.prepareRaw).toHaveBeenCalledTimes(2);
  expect(fixture.observe.mock.calls.at(-1)![0].artifact).toEqual(artifact);
  expect(fixture.forbidden).not.toHaveBeenCalled();
});

it("rejects a different registry before raw length preparation", async () => {
  const fixture = await fixtureFor();
  await expect(
    runFraudProofWorkflowFromRetainedDa({
      ...fixture.input,
      replayer: MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
      registry: createFraudProofWorkflowRegistry({
        adapters: [{ ...fixture.adapter, category: "mintDeclaredAssetLimit" }],
        launchScope: ["mintDeclaredAssetLimit"],
      }),
    }),
  ).rejects.toThrow("exact fieldPreimageLengthMismatch registry");
  expect(fixture.prepareRaw).not.toHaveBeenCalled();
  expect(fixture.forbidden).not.toHaveBeenCalled();
});
