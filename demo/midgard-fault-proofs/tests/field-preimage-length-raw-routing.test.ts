import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import {
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
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

const category = "fieldPreimageLengthMismatch";
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

const fixtureFor = async (directory: string) => {
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
    // This adapter only probes routing and durable envelopes. The installed
    // family adapter owns the reversible typed encoding of proof material.
    return normalizeJournalJson(
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
    ) as JournalJsonObject;
  };
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("raw route entered canonical preparation or submission");
  });
  const prepareRaw = vi.fn(async (routed: WorkflowRawFamilyEvidence) =>
    material(routed),
  );
  const validateRaw = vi.fn(
    async ({
      routed,
      artifact,
    }: {
      routed: WorkflowRawFamilyEvidence;
      artifact: JournalJsonObject;
    }) => {
      if (journalJsonDigest(material(routed)) !== journalJsonDigest(artifact))
        throw new Error("raw saved material changed");
    },
  );
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: forbidden,
    prepareRaw,
    validatePreparedRawArtifact: validateRaw,
    observe: async () => ({ kind: "pending", reason: "routing boundary" }),
    preflight: forbidden,
    submit: forbidden,
    reconcile: forbidden,
  };
  const input = {
    deploymentFingerprint,
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
    journal: new DirectoryFraudProofWorkflowJournalStore(directory),
    terminalVerifier: {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
      verify: forbidden,
    },
    releaseFinalityAuthority,
  };
  return {
    input,
    adapter,
    prepareRaw,
    validateRaw,
    forbidden,
    headerHash: fixture.headerHash,
  };
};

it("routes authenticated raw length mismatch into a restartable directory envelope", async () => {
  const directory = await mkdtemp(join(tmpdir(), "field-length-raw-"));
  try {
    const fixture = await fixtureFor(directory);
    const result = await runFraudProofWorkflowFromRetainedDa(fixture.input);
    expect(result.kind).toBe("pending");
    if (result.kind !== "pending") throw new Error("expected routing boundary");
    const reopened = new DirectoryFraudProofWorkflowJournalStore(directory);
    const entries = await reopened.load(result.workflowId);
    expect(entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
    ]);
    expect(entries[0]?.identity.target).toEqual({
      kind: "state_queue_header",
      headerHash: fixture.headerHash,
    });
    const prepared = entries[1]!.event;
    if (prepared.kind !== "prepared")
      throw new Error("prepared envelope missing");
    expect(prepared.artifact).toMatchObject({
      evidenceBinding: {
        route: "authenticated_raw_family",
        category,
        headerHash: fixture.headerHash,
      },
      releaseFinality: {
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
      },
      familyArtifact: {
        prepared: {
          direction: "wrongfulAcceptance",
          headerHash: fixture.headerHash,
        },
      },
    });
    expect(
      (
        await runFraudProofWorkflowFromRetainedDa({
          ...fixture.input,
          journal: reopened,
        })
      ).kind,
    ).toBe("pending");
    expect(fixture.prepareRaw).toHaveBeenCalledOnce();
    expect(fixture.validateRaw).toHaveBeenCalledOnce();
    fixture.validateRaw.mockRejectedValueOnce(
      new Error("raw saved material changed"),
    );
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        ...fixture.input,
        journal: reopened,
      }),
    ).rejects.toThrow("raw saved material changed");
    expect(fixture.forbidden).not.toHaveBeenCalled();
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

it("rejects a different registry before raw length preparation or journal creation", async () => {
  const directory = await mkdtemp(join(tmpdir(), "field-length-route-scope-"));
  try {
    const fixture = await fixtureFor(directory);
    await expect(
      runFraudProofWorkflowFromRetainedDa({
        ...fixture.input,
        replayer: MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY,
        registry: createFraudProofWorkflowRegistry({
          adapters: [
            { ...fixture.adapter, category: "mintDeclaredAssetLimit" },
          ],
          launchScope: ["mintDeclaredAssetLimit"],
        }),
      }),
    ).rejects.toThrow("exact fieldPreimageLengthMismatch registry");
    expect(fixture.prepareRaw).not.toHaveBeenCalled();
    expect(fixture.forbidden).not.toHaveBeenCalled();
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
