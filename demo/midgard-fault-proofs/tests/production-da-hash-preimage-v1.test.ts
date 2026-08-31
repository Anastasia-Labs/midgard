import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { type EvidenceProvenanceV1, ROOT_DOMAINS } from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { prepareDaHashPreimageFromCommittedLeavesV1 } from "../src/prepare-da-hash-preimage.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../src/workflow/family-l1-observation-v1.js";
import {
  type FraudProofWorkflowIdentityV1,
  MemoryFraudProofWorkflowJournalStoreV1,
} from "../src/workflow/journal-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
  FRAUD_PROOF_WORKFLOW_SAFETY_V1,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
  type FraudProofFamilyWorkflowAdapterV1,
  runDaHashPreimageWorkflowFromRetainedDaV1,
} from "../src/workflow/orchestrator-v1.js";
import {
  admitProductionDaHashPreimageArtifactV1,
  productionDaHashPreimageArtifactV1,
  unsafeCreateDaHashPreimageTransactionPortForTest,
} from "../src/workflow/production-da-hash-preimage-v1.js";
import { createProductionLinearFamilyWorkflowAdapterV1 } from "../src/workflow/production-linear-family-adapter-v1.js";
import type { FraudProofRawL1FamilyStageV1 } from "../src/workflow/raw-l1-family-derivation-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigestV1,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy-v1.js";
import type { LocallyEvaluatedTransactionV1 } from "../src/workflow/transaction-boundary-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const committedTxId = hash("99");
const committedLeafValueCbor = "deadbeef";
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");

const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const plan = async () => {
  const counted = await buildCountedRoot(ROOT_DOMAINS.transactionsV1, [
    {
      key: Buffer.from(committedTxId, "hex"),
      value: Buffer.from(committedLeafValueCbor, "hex"),
    },
  ]);
  return await prepareDaHashPreimageFromCommittedLeavesV1({
    headerHash,
    committedTransactionsRoot: counted.root,
    l2TransactionCount: 1n,
    entries: [[committedTxId, committedLeafValueCbor]],
  });
};

const routedQ44Fixture = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    transactionsRootMode: "payloadSource",
  });
  const sourceValue = fixture.transactions[0]!.sourceValueBytes;
  const counted = await buildCountedRoot(ROOT_DOMAINS.transactionsV1, [
    { key: Buffer.from(committedTxId, "hex"), value: sourceValue },
  ]);
  const header: SDK.HeaderV1 = {
    ...fixture.header,
    transactionsRoot: counted.root,
  };
  const observedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeaderV1(header),
  );
  const payload: SDK.DaPayloadV1 = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: observedHeaderHash,
      transactions: [[committedTxId, sourceValue.toString("hex")]],
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayloadV1(
    SDK.encodeDaPayloadV1(payload),
    { mode: "identity" },
  );
  const source: RetainedDaPayloadSource = {
    sourceId: "retained-da-peer",
    fetchPayloadByHeaderHash: async () => ({
      ok: true,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-da-peer/peer-a",
        grade: "security",
      },
      sourceId: "retained-da-peer",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    }),
  };
  return {
    observation: authenticatedHeaderObservationV1({
      ...fixture,
      header,
      headerHash: observedHeaderHash,
    }),
    source,
    headerHash: observedHeaderHash,
  };
};

const signed = (): LocallyEvaluatedTransactionV1["signed"] => {
  const [referenceTxHash, referenceIndex] = referenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => txHash,
    toTransaction: () => ({
      // Reference-script-only admission reads the signed witness set, so the
      // fake must answer it with an empty one rather than omit the accessor.
      witness_set: () => ({
        native_scripts: () => undefined,
        plutus_v1_scripts: () => undefined,
        plutus_v2_scripts: () => undefined,
        plutus_v3_scripts: () => undefined,
      }),
      body: () => ({
        reference_inputs: () => ({
          len: () => 1,
          get: () => ({
            transaction_id: () => ({ to_hex: () => referenceTxHash! }),
            index: () => BigInt(referenceIndex!),
          }),
        }),
      }),
    }),
  } as unknown as LocallyEvaluatedTransactionV1["signed"];
};

const transaction = (): LocallyEvaluatedTransactionV1 => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 fraud-proof da-hash-preimage step-01",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
});

const l1 = (stage: {
  value: FraudProofRawL1FamilyStageV1;
}): FraudProofFamilyL1ObservationPortV1<"daHashPreimage"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  category: "daHashPreimage",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("not used in focused Q44 builder test");
  },
  transactionConfirmed: async ({ txHash: observed }) => observed === txHash,
  observe: async () => ({ provenance, stage: stage.value }),
});

const identity: FraudProofWorkflowIdentityV1 = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "daHashPreimage",
  target: { kind: "state_queue_header", headerHash },
};

describe("production da-hash-preimage workflow V1", () => {
  it("persists only raw leaves and re-derives the exact Q44 proof on reload", async () => {
    const prepared = await plan();
    const artifact = await productionDaHashPreimageArtifactV1(prepared);
    expect(Object.keys(artifact).sort()).toEqual(
      [
        "committedTransactionsRoot",
        "committedTxId",
        "entries",
        "headerHash",
        "l2TransactionCount",
        "schemaVersion",
      ].sort(),
    );
    const admitted = await admitProductionDaHashPreimageArtifactV1(artifact);
    expect(admitted.txInclusion).toEqual(prepared.txInclusion);
    expect(admitted.step02State).toEqual({ verdict: "MalformedSource" });
  });

  it("rejects forged roots, extra claim fields, and file-backed plans", async () => {
    const prepared = await plan();
    const artifact = await productionDaHashPreimageArtifactV1(prepared);
    await expect(
      admitProductionDaHashPreimageArtifactV1({
        ...artifact,
        committedTransactionsRoot: hash("88"),
      }),
    ).rejects.toThrow("transactions_root_mismatch");
    await expect(
      admitProductionDaHashPreimageArtifactV1({
        ...artifact,
        claimedVerdict: "MalformedSource",
      }),
    ).rejects.toThrow("unknown or missing fields");
    await expect(
      productionDaHashPreimageArtifactV1({
        ...prepared,
        files: { txInclusionPath: "/tmp/proof", planPath: "/tmp/plan" },
      }),
    ).rejects.toThrow("not an in-memory authenticated raw-leaf plan");
  });

  it("captures the real step boundary, journals the exact body, and reconciles from raw L1", async () => {
    const artifact = await productionDaHashPreimageArtifactV1(await plan());
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStageV1,
    };
    const step01 = vi.fn(async (input: { preSubmitBoundary?: Function }) => {
      await input.preSubmitBoundary?.(transaction());
      throw new Error("pre-submit boundary did not stop provider I/O");
    });
    const transactions = unsafeCreateDaHashPreimageTransactionPortForTest({
      config: {
        lucid: {} as never,
        blueprint: {},
        deploymentInfo: {},
        network: "Custom" as never,
        signer: {
          source: "test",
          address: "addr_test",
          paymentKeyHash: "77".repeat(28),
          selectWallet: () => undefined,
        },
        headerHash,
        referenceScripts: {
          steps: [{} as never, {} as never],
          witnesses: {
            computationThreadMint: {} as never,
            fraudProofMint: {} as never,
            phasMembershipWithdraw: {} as never,
          },
        },
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => {
            throw new Error("no removal lease expected");
          },
        },
        fraudProverRewardLovelace: 1n,
      },
      builders: {
        init: vi.fn() as never,
        step01: step01 as never,
        step02: vi.fn() as never,
        remove: vi.fn() as never,
      },
    });
    const adapter = createProductionLinearFamilyWorkflowAdapterV1({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("no removal lease expected");
        },
      },
    });
    const context = {
      identity,
      workflowId: hash("bb"),
      artifact,
      entries: [],
    } as const;
    const observed = await adapter.observe(context);
    if (observed.kind !== "action_required") {
      throw new Error("expected Q44 step action");
    }
    const preflight = await adapter.preflight({
      ...context,
      action: observed.action,
    });
    await expect(
      adapter.submit({
        ...context,
        action: observed.action,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(step01).toHaveBeenCalledWith(
      expect.objectContaining({
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
        awaitConfirmation: false,
      }),
    );
    stage.value = {
      kind: "step",
      step: 2,
      threadOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: outRef("10"),
    };
    await expect(
      adapter.reconcile({
        ...context,
        action: observed.action,
        txHash,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
  });

  it("keeps Q44 out of the canonical-only prepare route", async () => {
    const transactions = unsafeCreateDaHashPreimageTransactionPortForTest({
      config: {} as never,
      builders: {} as never,
    });
    await expect(transactions.prepare({} as never)).rejects.toThrow(
      "authenticated raw-source-leaf evidence route",
    );
  });

  it("admits only the dedicated raw-leaf route into the shared durable journal loop", async () => {
    const routed = await routedQ44Fixture();
    const deploymentFingerprint = hash("d1");
    const txHashes = [hash("a1"), hash("a2"), hash("a3"), hash("a4")];
    const actionIds = ["init", "step_01", "step_02", "remove"];
    const terminal = {
      schemaVersion: "midgard-fraud-proof-workflow-terminal-v1" as const,
      category: "daHashPreimage" as const,
      headerHash: routed.headerHash,
      proofToken: {
        unit: "11".repeat(56),
        outRef: `${txHashes[2]}#0`,
        createdByTxHash: txHashes[2]!,
        retainedAtFinalState: true as const,
      },
      correction: {
        removalTxHash: txHashes[3]!,
        removedStateQueueOutRef: outRef("33"),
        fraudulentHeaderAbsent: true as const,
        referencedProofTokenOutRef: `${txHashes[2]}#0`,
      },
      economics: {
        operatorCredential: "22".repeat(28),
        proverCredential: "33".repeat(28),
        operatorBondInputOutRef: outRef("88"),
        operatorBondInputLovelace: "10000000",
        slashedLovelace: "10000000",
        proverRewardOutputOutRef: `${txHashes[3]}#0`,
        proverRewardLovelace: "5000000",
        removalFeeLovelace: "200000",
        duplicateRewardAbsent: true as const,
      },
      observedAt: {
        slot: "4242",
        blockHash: hash("44"),
        confirmationDepth: 30,
      },
    };
    const prepare = vi.fn(async () => {
      throw new Error("canonical adapter prepare must not be called for Q44");
    });
    const adapter: FraudProofFamilyWorkflowAdapterV1 = {
      adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER_V1,
      category: "daHashPreimage",
      safety: FRAUD_PROOF_WORKFLOW_SAFETY_V1,
      prepare,
      observe: async ({ artifact, entries }) => {
        expect(artifact.schemaVersion).toBe(
          "midgard-production-da-hash-preimage-artifact-v1",
        );
        const confirmed = entries.filter(
          (entry) => entry.event.kind === "confirmed",
        ).length;
        return confirmed === actionIds.length
          ? { kind: "completed", terminal }
          : {
              kind: "action_required",
              action: {
                actionId: actionIds[confirmed]!,
                input: { sequence: confirmed },
              },
            };
      },
      preflight: async ({ action }) => ({
        actionId: action.actionId,
        txHash: txHashes[actionIds.indexOf(action.actionId)]!,
        scriptExecution: "reference_scripts",
        localUplcEvaluation: {
          status: "passed",
          evaluator: "lucid-evolution.complete(localUPLCEval=true)",
        },
        referenceScripts: [
          {
            role: `Q44 ${action.actionId}`,
            outRef: referenceOutRef,
            scriptHash: "66".repeat(28),
          },
        ],
      }),
      submit: async ({ preflight }) => ({
        kind: "submitted",
        txHash: preflight.txHash,
      }),
      reconcile: async ({ txHash: submitted }) => ({
        kind: "confirmed",
        txHash: submitted!,
      }),
    };
    const finalityPolicy = {
      confirmationDepth: 30,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: "automated_rewind_replay_incident-v1",
    } as const;
    const journal = new MemoryFraudProofWorkflowJournalStoreV1();
    const result = await runDaHashPreimageWorkflowFromRetainedDaV1({
      deploymentFingerprint,
      observation: routed.observation,
      sources: [routed.source],
      registry: createFraudProofWorkflowRegistryV1({
        adapters: [adapter],
        launchScope: ["daHashPreimage"],
      }),
      journal,
      terminalVerifier: {
        verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER_V1,
        verify: async ({ candidate }) => candidate,
      },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
        verifyForWorkflow: async () => ({
          schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
          deploymentIdentityDigest: deploymentFingerprint,
          releaseIdentityDigest: hash("e1"),
          policyDigest:
            computeFraudProofReleaseFinalityPolicyDigestV1(finalityPolicy),
          policy: finalityPolicy,
        }),
      },
    });
    expect(result.kind).toBe("completed");
    expect(prepare).not.toHaveBeenCalled();
    if (result.kind !== "completed") throw new Error("Q44 did not complete");
    const prepared = result.entries.find(
      (entry) => entry.event.kind === "prepared",
    );
    expect(prepared?.event).toMatchObject({
      kind: "prepared",
      artifact: {
        evidenceBinding: {
          route: "authenticated_source_leaf",
          headerHash: routed.headerHash,
          committedTxId,
        },
      },
    });
    expect(
      result.entries.filter((entry) => entry.event.kind === "confirmed"),
    ).toHaveLength(4);
  });
});
