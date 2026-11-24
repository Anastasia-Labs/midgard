import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/core";
import { ROOT_DOMAINS } from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import type { EvidenceProvenance } from "@al-ft/midgard-sdk/fraud-proof";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { prepareDaHashPreimageFromCommittedLeaves } from "../src/prepare-da-hash-preimage.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  admitDaHashPreimageArtifact,
  daHashPreimageArtifact,
} from "../src/workflow/da-hash-preimage.artifact-input.js";
import { unsafeCreateDaHashPreimageTransactionPortForTest } from "../src/workflow/da-hash-preimage.create-bound-da-hash-preimage-transaction-port.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import type { FraudProofWorkflowIdentity } from "../src/workflow/identity.js";
import { createLinearFamilyWorkflowAdapter } from "../src/workflow/linear-family-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runDaHashPreimageWorkflowFromRetainedDa } from "../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  bindWorkflowPreflightTransaction,
  type LocallyEvaluatedTransaction,
} from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { signedWorkflowTransactionFixture } from "./support/signed-workflow-transaction.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import { createFixtureChain } from "./workflow.make-adapter.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const committedTxId = hash("99");
const committedLeafValueCbor = "deadbeef";
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");

const provenance: EvidenceProvenance = {
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
  return await prepareDaHashPreimageFromCommittedLeaves({
    headerHash,
    committedTransactionsRoot: counted.root,
    l2TransactionCount: 1n,
    entries: [[committedTxId, committedLeafValueCbor]],
  });
};

const routedQ44Fixture = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
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
  const header: SDK.Header = {
    ...fixture.header,
    transactionsRoot: counted.root,
  };
  const observedHeaderHash = await Effect.runPromise(
    SDK.hashBlockHeader(header),
  );
  const payload: SDKDa.DaPayload = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: observedHeaderHash,
      transactions: [[committedTxId, sourceValue.toString("hex")]],
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDKDa.encodeDaPayload(payload),
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
    observation: authenticatedHeaderObservation({
      ...fixture,
      header,
      headerHash: observedHeaderHash,
    }),
    source,
    headerHash: observedHeaderHash,
  };
};

const signed = (): LocallyEvaluatedTransaction["signed"] => {
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
  } as unknown as LocallyEvaluatedTransaction["signed"];
};

const transaction = (): LocallyEvaluatedTransaction => ({
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
  value: FraudProofRawL1FamilyStage;
}): FraudProofFamilyL1ObservationPort<"daHashPreimage"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  category: "daHashPreimage",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("not used in focused da-hash-preimage builder test");
  },
  transactionConfirmed: async ({ txHash: observed }) => observed === txHash,
  observe: async () => ({ provenance, stage: stage.value }),
});

const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  hubOraclePolicyId: "aa".repeat(28),
  category: "daHashPreimage",
  target: { kind: "state_queue_header", headerHash },
};

describe("production da-hash-preimage workflow V1", () => {
  it("persists only raw leaves and re-derives the exact da-hash-preimage proof on reload", async () => {
    const prepared = await plan();
    const artifact = await daHashPreimageArtifact(prepared);
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
    const admitted = await admitDaHashPreimageArtifact(artifact);
    expect(admitted.txInclusion).toEqual(prepared.txInclusion);
    expect(admitted.step02State).toEqual({ verdict: "MalformedSource" });
  });

  it("rejects forged roots, extra claim fields, and file-backed plans", async () => {
    const prepared = await plan();
    const artifact = await daHashPreimageArtifact(prepared);
    await expect(
      admitDaHashPreimageArtifact({
        ...artifact,
        committedTransactionsRoot: hash("88"),
      }),
    ).rejects.toThrow("transactions_root_mismatch");
    await expect(
      admitDaHashPreimageArtifact({
        ...artifact,
        claimedVerdict: "MalformedSource",
      }),
    ).rejects.toThrow("unknown or missing fields");
    await expect(
      daHashPreimageArtifact({
        ...prepared,
        files: { txInclusionPath: "/tmp/proof", planPath: "/tmp/plan" },
      }),
    ).rejects.toThrow("not an in-memory authenticated raw-leaf plan");
  });

  it("captures the real step boundary, submits the exact body, and reads the next step from raw L1", async () => {
    const artifact = await daHashPreimageArtifact(await plan());
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const step01 = vi.fn(async () => ({ transaction: transaction() }));
    const transactions = unsafeCreateDaHashPreimageTransactionPortForTest({
      config: {
        lucid: {} as never,
        blueprint: {},
        deploymentInfo: {},
        network: "Custom" as never,
        signer: {
          source: "test",
          address: "addr_test",
          proofMaterialsAddress: "addr_test_proof_materials",
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
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions,
    });
    const context = {
      identity,
      workflowId: hash("bb"),
      artifact,
    } as const;
    const observed = await adapter.observe(context);
    if (observed.kind !== "action_required") {
      throw new Error("expected da-hash-preimage step action");
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
      }),
    );
    stage.value = {
      kind: "step",
      step: 2,
      threadOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: outRef("10"),
    };
    await expect(adapter.observe(context)).resolves.toMatchObject({
      kind: "action_required",
      action: {
        input: { stage: "step_02", threadOutRef: `${txHash}#0` },
      },
    });
  });

  it("keeps da-hash-preimage out of the canonical-only prepare route", async () => {
    const transactions = unsafeCreateDaHashPreimageTransactionPortForTest({
      config: {} as never,
      builders: {} as never,
    });
    await expect(transactions.prepare({} as never)).rejects.toThrow(
      "authenticated raw-source-leaf evidence route",
    );
  });

  it("admits only the dedicated raw-leaf route into the shared workflow loop", async () => {
    const routed = await routedQ44Fixture();
    const hubOraclePolicyId = "d1".repeat(28);
    const actionIds = ["init", "step_01", "step_02", "remove"];
    const signedActions = await Promise.all(
      actionIds.map(async () => await signedWorkflowTransactionFixture()),
    );
    const txHashes = signedActions.map(({ input }) => input.transactionHash);
    // The L1 the family reads: each action's transaction lands once sent.
    const chain = createFixtureChain();
    const observedArtifacts: unknown[] = [];
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
      throw new Error(
        "canonical adapter prepare must not be called for da-hash-preimage",
      );
    });
    const adapter: FraudProofFamilyWorkflowAdapter = {
      adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
      category: "daHashPreimage",
      safety: FRAUD_PROOF_WORKFLOW_SAFETY,
      prepare,
      observe: async ({ artifact }) => {
        expect(artifact.schemaVersion).toBe(
          "midgard-production-da-hash-preimage-artifact-v1",
        );
        observedArtifacts.push(artifact);
        const landed = txHashes.filter((sent) => chain.landed(sent)).length;
        return landed === actionIds.length
          ? { kind: "completed", terminal: structuredClone(terminal) }
          : {
              kind: "action_required",
              action: {
                actionId: actionIds[landed]!,
                input: { sequence: landed },
              },
            };
      },
      preflight: async ({ action }) =>
        bindWorkflowPreflightTransaction(
          {
            actionId: action.actionId,
            txHash: txHashes[actionIds.indexOf(action.actionId)]!,
            scriptExecution: "reference_scripts",
            localUplcEvaluation: {
              status: "passed",
              evaluator: "lucid-evolution.complete(localUPLCEval=true)",
            },
            referenceScripts: [
              {
                role: `da-hash-preimage ${action.actionId}`,
                outRef: referenceOutRef,
                scriptHash: "66".repeat(28),
              },
            ],
          },
          signedActions[actionIds.indexOf(action.actionId)]!.signed,
        ),
      submit: async ({ preflight }) => {
        chain.land(preflight.txHash);
        return { kind: "submitted", txHash: preflight.txHash };
      },
    };
    const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
    const process = createTestWorkflowProcess();
    const run = () =>
      runDaHashPreimageWorkflowFromRetainedDa({
        hubOraclePolicyId,
        observation: routed.observation,
        sources: [routed.source],
        registry: createFraudProofWorkflowRegistry({
          adapters: [process.recording(adapter)],
          launchScope: ["daHashPreimage"],
        }),
        run: process.run({
          category: "daHashPreimage",
          headerHash: routed.headerHash,
        }),
        terminalVerifier: terminalVerifierOf(
          async ({ candidate }) => candidate,
        ),
        releaseFinalityAuthority: {
          authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
          verifyForWorkflow: async () => ({
            schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
            hubOraclePolicyId,
            blueprintHash: hash("e1"),
            policyDigest:
              computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
            policy: finalityPolicy,
          }),
        },
      });
    const result = await run();
    expect(result.kind).toBe("terminal_included");
    expect(prepare).not.toHaveBeenCalled();
    if (result.kind !== "terminal_included")
      throw new Error("da-hash-preimage did not include");
    expect(observedArtifacts[0]).toMatchObject({
      schemaVersion: "midgard-production-da-hash-preimage-artifact-v1",
    });
    expect(JSON.stringify(observedArtifacts[0])).toContain(committedTxId);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual(actionIds);
    expect(process.sent().map(({ txHash: sent }) => sent)).toEqual(txHashes);
    terminal.observedAt.confirmationDepth = 2161;
    expect((await run()).kind).toBe("terminal_included");
    terminal.observedAt.confirmationDepth = 2162;
    expect((await run()).kind).toBe("completed");
    expect(prepare).not.toHaveBeenCalled();
    // The later runs read every action landed and send nothing more.
    expect(process.sent()).toHaveLength(actionIds.length);
  });
});
