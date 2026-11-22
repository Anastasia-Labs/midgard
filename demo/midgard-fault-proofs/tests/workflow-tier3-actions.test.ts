import {
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.plan-fault-proof-field-opening.js";
import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
} from "../src/network-id/prepare.js";
import type { NetworkIdWorkflowAdapterConfig } from "../src/network-id/workflow-adapter.create-network-id-raw-l1-observation-port.js";
import { createNetworkIdWorkflowAdapter } from "../src/network-id/workflow-adapter.create-network-id-workflow-adapter.js";
import { deriveL2TransactionSourceCbor } from "../src/prepare-double-spend.encode-l2-transaction-source-value.js";
import { buildInit } from "../src/submit-init.js";
import { createDoubleSpendConstrainedWorkflowAdapter } from "../src/workflow/double-spend-adapter.create-double-spend-constrained-workflow-adapter.js";
import type { DoubleSpendConstrainedWorkflowAdapterConfig } from "../src/workflow/double-spend-adapter.preflight-of.js";
import type { JournalJsonObject } from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import {
  type LocallyEvaluatedTransaction,
  workflowPreflightTransaction,
} from "../src/workflow/transaction-boundary.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  certificateFor,
  HEADER_HASH,
  ledgerReads,
  observe,
  POLICY_ID,
  publicationsFor,
  SCRIPT,
  SIGNER,
  stageObservation,
  STATE_QUEUE_OUT_REF,
  THREAD_OUT_REF,
  utxo,
  workflowIdentity,
} from "./support/tier3-workflow-fixtures.js";

// The double-spend preflight must reach a real builder's signed transaction:
// only the init builder is replaced, so all the adapter does stays under test.
vi.mock("../src/submit-init.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/submit-init.js")>()),
  buildInit: vi.fn(),
}));

describe("Q38 tier-3 workflow action chains", () => {
  it("doubleSpend heals publications and certification by content before the proof step", async () => {
    const spendInputs = Array.from({ length: 440 }, (_, index) =>
      outRefCbor((index % 200) + 1, BigInt(Math.floor(index / 200))),
    );
    const transaction = buildFixtureTransaction({
      spendInputs,
      fee: 1n,
    });
    const plan = planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
      anchorTxId: transaction.txId,
      nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
      itemCbors: spendInputs,
      owner: SIGNER.paymentKeyHash,
      label: "double-spend tier-3 workflow test",
    });
    expect(plan.plan.tier).toBe("Certified");

    let walletUtxos: readonly UTxO[] = [];
    let certificateUtxos: readonly UTxO[] = [];
    const observeExact = vi.fn(
      async (input: { readonly expectedOutRef: string }) => ({
        kind: "confirmed" as const,
        outRef: input.expectedOutRef,
      }),
    );
    const lucid = {
      ...ledgerReads(() => [walletUtxos, certificateUtxos]),
    } as unknown as LucidEvolution;
    const config = {
      lucid,
      blueprint: {},
      deploymentInfo: {},
      network: "Preview",
      signer: SIGNER,
      referenceScripts: {
        steps:
          [] as unknown as DoubleSpendConstrainedWorkflowAdapterConfig["referenceScripts"]["steps"],
        witnesses:
          {} as DoubleSpendConstrainedWorkflowAdapterConfig["referenceScripts"]["witnesses"],
      },
      fieldPreimageCertificate: {
        policyId: POLICY_ID,
        mintingScript: SCRIPT,
        referenceScriptUtxo: utxo({
          txHash: h32(0x10),
          outputIndex: 0,
          scriptRef: SCRIPT,
        }),
      },
      l1: {
        publications: {
          observerVersion:
            "midgard-fraud-proof-authenticated-publication-observer-v1",
          observeExact,
        },
        observe: async () =>
          stageObservation({
            kind: "step_03",
            threadOutRef: THREAD_OUT_REF,
            stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
          }),
      },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("removal is outside this test");
        },
      },
    } satisfies DoubleSpendConstrainedWorkflowAdapterConfig;
    const adapter = createDoubleSpendConstrainedWorkflowAdapter(config);
    const txArtifact = {
      inclusion: {},
      nativeTxId: transaction.txId,
      nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
      spendInputCbors: spendInputs.map((input) => input.toString("hex")),
      doubleSpentInputIndex: 0,
    };
    const artifact = {
      headerHash: HEADER_HASH,
      tx1: txArtifact,
      tx2: txArtifact,
    } as unknown as JournalJsonObject;

    const publication = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
    });
    expect(publication).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "publish-field", proofFor: "step_03" } },
    });
    if (publication.kind !== "action_required") return;
    const publicationTxHash = h32(0x6a);
    walletUtxos = [
      utxo({
        txHash: publicationTxHash,
        outputIndex: 0,
        datum: String(publication.action.input.publicationDatumCbor),
      }),
    ];
    // The published chunk is found by content at the proof-materials
    // address: the next chunk's publication is next.
    const next = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
    });
    expect(next).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "publish-field", proofFor: "step_03" } },
    });
    expect(next).not.toEqual(publication);
    // The chunk gone again: the same publication heals it.
    walletUtxos = [];
    await expect(
      observe({ adapter, category: "doubleSpend", artifact }),
    ).resolves.toEqual(publication);

    walletUtxos = publicationsFor(plan);
    const certification = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
    });
    expect(certification).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "certify-field", proofFor: "step_03" } },
    });
    if (certification.kind !== "action_required") return;

    const certificate = certificateFor({ plan, network: "Preview" });
    certificateUtxos = [certificate];
    await expect(
      observe({
        adapter,
        category: "doubleSpend",
        artifact,
      }),
    ).resolves.toMatchObject({
      kind: "action_required",
      action: {
        input: {
          stage: "step_03",
          certificateOutRef: `${certificate.txHash}#0`,
        },
      },
    });
    // The certificate gone again: the same certification heals it.
    certificateUtxos = [];
    await expect(
      observe({ adapter, category: "doubleSpend", artifact }),
    ).resolves.toEqual(certification);
  });

  it("networkId heals publications and certification by content before step-02", async () => {
    const output: MidgardTxOutput = {
      address: Buffer.concat([Buffer.from([0x61]), Buffer.alloc(28, 0x55)]),
      value: { lovelace: 1n, assets: new Map() },
    };
    const outputs = Array.from({ length: 440 }, () =>
      encodeMidgardTxOutput(output),
    );
    const transaction = buildFixtureTransaction({
      spendInputs: [],
      outputs,
      fee: 1n,
      networkId: 0n,
    });
    const prepared = {
      headerHash: HEADER_HASH,
      expectedNetworkId: 0n,
      badTxId: transaction.txId,
      nativeTxCanonicalCbor: transaction.canonicalCbor.toString("hex"),
      nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
      outputsItemCbors: outputs.map((item) => item.toString("hex")),
      faultClaim: { kind: "output-network", outputIndex: 0n },
      fault: { OutputNetwork: { output_index: 0n } },
      txInclusion: {
        nativeTxId: transaction.txId,
        nativeTx: {} as never,
        nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
        l2TransactionSourceCbor: deriveL2TransactionSourceCbor(
          transaction.canonicalCbor,
        ),
        transactionsPhasRoot: h32(0x31),
        txMembershipProofCbor: "d87980",
      },
    } satisfies PreparedNetworkIdProof;
    const plan = planNetworkIdOutputsOpening({
      prepared,
      owner: SIGNER.paymentKeyHash,
    });
    expect(plan.plan.tier).toBe("Certified");

    let walletUtxos: readonly UTxO[] = [];
    let certificateUtxos: readonly UTxO[] = [];
    const observeExact = vi.fn(
      async (input: { readonly expectedOutRef: string }) => ({
        kind: "confirmed" as const,
        outRef: input.expectedOutRef,
      }),
    );
    const step02Utxo = utxo({
      txHash: THREAD_OUT_REF.slice(0, 64),
      outputIndex: 0,
      address: "network-step-02",
    });
    const stateQueueUtxo = utxo({
      txHash: STATE_QUEUE_OUT_REF.slice(0, 64),
      outputIndex: 0,
      address: "state-queue",
    });
    const reads = ledgerReads(() => [walletUtxos, certificateUtxos]);
    const lucid = {
      ...reads,
      utxosAtWithUnit: async (address: string, unit: string) =>
        address === "network-step-02"
          ? [step02Utxo]
          : address === "state-queue"
            ? [stateQueueUtxo]
            : await reads.utxosAtWithUnit(address, unit),
    } as unknown as LucidEvolution;
    const config = {
      lucid,
      blueprint: {},
      network: "Preview",
      contracts: {
        steps: [
          {
            spendingScript: SCRIPT,
            spendingScriptHash: h28(0x11),
            spendingScriptAddress: "network-step-01",
          },
          {
            spendingScript: SCRIPT,
            spendingScriptHash: h28(0x12),
            spendingScriptAddress: "network-step-02",
          },
        ],
        expectedNetworkId: 0n,
        computationThread: { policyId: h28(0x13), mintingScript: SCRIPT },
        fraudProof: {
          policyId: h28(0x14),
          mintingScript: SCRIPT,
          spendingScriptAddress: "proof",
        },
        hubOraclePolicyId: h28(0x15),
        stateQueuePolicyId: h28(0x16),
        fieldPreimageCertificatePolicyId: POLICY_ID,
        fieldPreimageCertificateMintingScript: SCRIPT,
      },
      stateQueueAddress: "state-queue",
      category: {
        categoryId: "0000001c",
        label: "network-id",
      },
      catalogue: {
        policyId: h28(0x17),
        spendingScriptAddress: "catalogue",
        root: h32(0x18),
      },
      signer: SIGNER,
      stepReferenceScripts: [step02Utxo, step02Utxo],
      fieldPreimageCertificateReferenceScript: utxo({
        txHash: h32(0x19),
        outputIndex: 0,
        scriptRef: SCRIPT,
      }),
      witnessReferenceScripts: {},
      removal: {
        deploymentInfo: {},
        category: "networkId",
        isCurrentHead: async () => true,
      },
      rawL1: {
        publications: {
          observerVersion:
            "midgard-fraud-proof-authenticated-publication-observer-v1",
          observeExact,
        },
        transactionConfirmed: async () => true,
        observe: async () => ({
          kind: "step" as const,
          step: 2 as const,
          threadOutRef: THREAD_OUT_REF,
          stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
        }),
      },
    } as unknown as NetworkIdWorkflowAdapterConfig;
    const adapter = createNetworkIdWorkflowAdapter(config);
    const artifact = {
      schemaVersion: "midgard-network-id-workflow-artifact-v1",
      headerHash: HEADER_HASH,
      expectedNetworkId: "0",
      badTxId: transaction.txId,
      nativeTxCanonicalCbor: transaction.canonicalCbor.toString("hex"),
      nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
      l2TransactionSourceCbor: deriveL2TransactionSourceCbor(
        transaction.canonicalCbor,
      ),
      outputsItemCbors: outputs.map((item) => item.toString("hex")),
      faultKind: "output-network",
      outputIndex: "0",
      transactionsPhasRoot: h32(0x31),
      txMembershipProofCbor: "d87980",
    } as JournalJsonObject;

    const publication = await observe({
      adapter,
      category: "networkId",
      artifact,
    });
    expect(publication).toMatchObject({
      kind: "action_required",
      action: { input: { kind: "publish_field" } },
    });
    if (publication.kind !== "action_required") return;
    const publicationTxHash = h32(0x6c);
    walletUtxos = [
      utxo({
        txHash: publicationTxHash,
        outputIndex: 0,
        datum: String(publication.action.input.publicationDatumCbor),
      }),
    ];
    // The published chunk is found by content at the proof-materials
    // address: the next chunk's publication is next.
    const next = await observe({ adapter, category: "networkId", artifact });
    expect(next).toMatchObject({
      kind: "action_required",
      action: { input: { kind: "publish_field" } },
    });
    expect(next).not.toEqual(publication);
    // The chunk gone again: the same publication heals it.
    walletUtxos = [];
    await expect(
      observe({ adapter, category: "networkId", artifact }),
    ).resolves.toEqual(publication);

    walletUtxos = publicationsFor(plan);
    const certification = await observe({
      adapter,
      category: "networkId",
      artifact,
    });
    expect(certification).toMatchObject({
      kind: "action_required",
      action: { input: { kind: "certify_field" } },
    });
    if (certification.kind !== "action_required") return;

    const certificate = certificateFor({ plan, network: "Preview" });
    certificateUtxos = [certificate];
    await expect(
      observe({ adapter, category: "networkId", artifact }),
    ).resolves.toMatchObject({
      kind: "action_required",
      action: {
        input: {
          kind: "step02",
          certificateOutRef: `${certificate.txHash}#0`,
        },
      },
    });
    // The certificate gone again: the same certification heals it.
    certificateUtxos = [];
    await expect(
      observe({ adapter, category: "networkId", artifact }),
    ).resolves.toEqual(certification);
  }, 30_000);

  // Regression: the runner reads the signed body back off the in-memory
  // preflight to send it, so a preflight that forgets to bind its captured
  // transaction fails the whole run.
  it("doubleSpend binds the captured signed transaction to the preflight it returns", async () => {
    const capturedTxHash = h32(0x7e);
    const capturedSigned = {
      toHash: () => capturedTxHash,
      submit: async () => capturedTxHash,
      toTransaction: () => ({
        witness_set: () => ({
          native_scripts: () => undefined,
          plutus_v1_scripts: () => undefined,
          plutus_v2_scripts: () => undefined,
          plutus_v3_scripts: () => undefined,
        }),
        body: () => ({
          inputs: { len: () => 0, get: () => undefined },
          reference_inputs: () => undefined,
        }),
      }),
    } as unknown as LocallyEvaluatedTransaction["signed"];
    vi.mocked(buildInit).mockImplementation(
      async () =>
        ({
          transaction: {
            txHash: capturedTxHash,
            signed: capturedSigned,
            referenceScripts: [
              {
                role: "V1 fraud-proof double-spend init",
                outRef: `${h32(0x19)}#0`,
                scriptHash: h28(0x11),
              },
            ],
          },
        }) as never,
    );

    const adapter = createDoubleSpendConstrainedWorkflowAdapter({
      signer: SIGNER,
      referenceScripts: { steps: [], witnesses: {} },
      l1: {
        transactionConfirmed: async () => false,
        observe: async () =>
          stageObservation({
            kind: "not_started",
            stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
          }),
      },
    } as unknown as DoubleSpendConstrainedWorkflowAdapterConfig);
    const artifact = {
      headerHash: HEADER_HASH,
    } as unknown as JournalJsonObject;
    const observation = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
    });
    expect(observation).toMatchObject({
      kind: "action_required",
      action: { input: { stage: "init" } },
    });
    if (observation.kind !== "action_required") return;
    const preflight = await adapter.preflight({
      identity: workflowIdentity("doubleSpend"),
      workflowId: h32(0x51),
      artifact,
      action: observation.action,
    });
    expect(workflowPreflightTransaction(preflight)).toBeDefined();
    expect(workflowPreflightTransaction(preflight)).toBe(capturedSigned);
    expect(preflight).toMatchObject({
      actionId: observation.action.actionId,
      txHash: capturedTxHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed" },
    });
  });
});
