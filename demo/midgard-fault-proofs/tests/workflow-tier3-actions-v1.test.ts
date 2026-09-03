import {
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import type {
  LucidEvolution,
  MintingPolicy,
  UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  fieldPreimageCertificateAddress,
  planFaultProofFieldOpening,
} from "../src/field-opening-v1.js";
import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdProof,
} from "../src/network-id/prepare-v1.js";
import {
  createNetworkIdWorkflowAdapter,
  type NetworkIdWorkflowAdapterConfig,
} from "../src/network-id/workflow-adapter-v1.js";
import { deriveL2TransactionSourceCbor } from "../src/prepare-double-spend.js";
import type { ResolvedProverSigner } from "../src/runtime.js";
import {
  createDoubleSpendConstrainedWorkflowAdapter,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
} from "../src/workflow/double-spend-adapter-v1.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEntry,
  type JournalJsonObject,
} from "../src/workflow/journal-v1.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator-v1.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const POLICY_ID = "91".repeat(28);
const HEADER_HASH = h28(0x71);
const THREAD_OUT_REF = `${h32(0x72)}#0`;
const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;
const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
const SIGNER: ResolvedProverSigner = {
  source: "tier3-test",
  address: "addr_test1_tier3_test",
  paymentKeyHash: "81".repeat(28),
  selectWallet: () => undefined,
};

const utxo = ({
  txHash,
  outputIndex,
  address = SIGNER.address,
  datum,
  assets = { lovelace: 5_000_000n },
  scriptRef,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address?: string;
  readonly datum?: string;
  readonly assets?: Readonly<Record<string, bigint>>;
  readonly scriptRef?: MintingPolicy;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets,
  ...(datum === undefined ? {} : { datum }),
  ...(scriptRef === undefined ? {} : { scriptRef }),
});

const publicationsFor = (
  plan: ReturnType<typeof planFaultProofFieldOpening>,
): readonly UTxO[] =>
  plan.plan.publications.map((publication, index) =>
    utxo({
      txHash: h32(0x20 + index),
      outputIndex: 0,
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );

const certificateFor = ({
  plan,
  network,
}: {
  readonly plan: ReturnType<typeof planFaultProofFieldOpening>;
  readonly network: "Preview";
}): UTxO => {
  const certification = deriveFieldPreimageCertification(plan.plan);
  const unit = `${POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  return utxo({
    txHash: h32(0x61),
    outputIndex: 0,
    address: fieldPreimageCertificateAddress({
      network,
      certificatePolicyId: POLICY_ID,
    }),
    datum: certification.datumCbor,
    assets: { lovelace: 5_000_000n, [unit]: 1n },
  });
};

const confirmedEntry = (actionId: string): FraudProofWorkflowJournalEntry =>
  ({
    event: { kind: "confirmed", actionId, txHash: h32(0x41) },
  }) as FraudProofWorkflowJournalEntry;

const workflowIdentity = (
  category: "doubleSpend" | "networkId",
): FraudProofWorkflowIdentity => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "d1".repeat(32),
  category,
  target: { kind: "state_queue_header", headerHash: HEADER_HASH },
});

const observe = async ({
  adapter,
  category,
  artifact,
  entries = [],
}: {
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly category: "doubleSpend" | "networkId";
  readonly artifact: JournalJsonObject;
  readonly entries?: readonly FraudProofWorkflowJournalEntry[];
}) => {
  return await adapter.observe({
    identity: workflowIdentity(category),
    workflowId: h32(0x51),
    artifact,
    entries,
  });
};

describe("Q38 tier-3 workflow action chains", () => {
  it("doubleSpend journals healing publications and certification before the proof step", async () => {
    const spendInputs = Array.from({ length: 440 }, (_, index) =>
      outRefCbor((index % 200) + 1, BigInt(Math.floor(index / 200))),
    );
    const transaction = buildFixtureTransaction({
      spendInputs,
      fee: 1n,
    });
    const plan = planFaultProofFieldOpening({
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
      utxosAt: async (address: string) =>
        address === SIGNER.address ? walletUtxos : certificateUtxos,
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
        observe: async () => ({
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "local-node-test",
            grade: "security",
          },
          stage: {
            kind: "step_03" as const,
            threadOutRef: THREAD_OUT_REF,
            stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
          },
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
    await expect(
      adapter.reconcile({
        identity: workflowIdentity("doubleSpend"),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: publication.action,
        txHash: publicationTxHash,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: publicationTxHash });
    expect(observeExact).toHaveBeenCalledWith(
      expect.objectContaining({
        expectedOutRef: `${publicationTxHash}#0`,
        expectedDatumCbor: publication.action.input.publicationDatumCbor,
      }),
    );
    observeExact.mockClear();
    await expect(
      adapter.reconcile({
        identity: workflowIdentity("doubleSpend"),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: publication.action,
        txHash: h32(0x6b),
      }),
    ).resolves.toEqual({ kind: "not_found" });
    expect(observeExact).not.toHaveBeenCalled();
    walletUtxos = [];
    const healedPublication = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
      entries: [confirmedEntry(publication.action.actionId)],
    });
    expect(healedPublication).toMatchObject({
      kind: "action_required",
      action: { actionId: `${publication.action.actionId}:heal:1` },
    });

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
    const healedCertification = await observe({
      adapter,
      category: "doubleSpend",
      artifact,
      entries: [confirmedEntry(certification.action.actionId)],
    });
    expect(healedCertification).toMatchObject({
      kind: "action_required",
      action: { actionId: `${certification.action.actionId}:heal:1` },
    });

    const certificate = certificateFor({ plan, network: "Preview" });
    certificateUtxos = [certificate];
    await expect(
      observe({ adapter, category: "doubleSpend", artifact }),
    ).resolves.toMatchObject({
      kind: "action_required",
      action: {
        input: {
          stage: "step_03",
          certificateOutRef: `${certificate.txHash}#0`,
        },
      },
    });
  });

  it("networkId journals healing publications and certification before step-02", async () => {
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
    const lucid = {
      utxosAtWithUnit: async (address: string) =>
        address === "network-step-02"
          ? [step02Utxo]
          : address === "state-queue"
            ? [stateQueueUtxo]
            : [],
      utxosAt: async (address: string) =>
        address === SIGNER.address ? walletUtxos : certificateUtxos,
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
      terminalFacts: async () => {
        throw new Error("terminal state is outside this test");
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
    await expect(
      adapter.reconcile({
        identity: workflowIdentity("networkId"),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: publication.action,
        txHash: publicationTxHash,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: publicationTxHash });
    expect(observeExact).toHaveBeenCalledWith(
      expect.objectContaining({
        expectedOutRef: `${publicationTxHash}#0`,
        expectedDatumCbor: publication.action.input.publicationDatumCbor,
      }),
    );
    observeExact.mockClear();
    await expect(
      adapter.reconcile({
        identity: workflowIdentity("networkId"),
        workflowId: h32(0x51),
        artifact,
        entries: [],
        action: publication.action,
        txHash: h32(0x6d),
      }),
    ).resolves.toEqual({ kind: "not_found" });
    expect(observeExact).not.toHaveBeenCalled();
    walletUtxos = [];
    await expect(
      observe({
        adapter,
        category: "networkId",
        artifact,
        entries: [confirmedEntry(publication.action.actionId)],
      }),
    ).resolves.toMatchObject({
      kind: "action_required",
      action: { actionId: `${publication.action.actionId}:heal:1` },
    });

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
    await expect(
      observe({
        adapter,
        category: "networkId",
        artifact,
        entries: [confirmedEntry(certification.action.actionId)],
      }),
    ).resolves.toMatchObject({
      kind: "action_required",
      action: { actionId: `${certification.action.actionId}:heal:1` },
    });

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
  }, 30_000);

  it("doubleSpend resumes the exact descendant-removal fencing lease in a fresh adapter", async () => {
    const txHash = h32(0x7a);
    const renew = vi.fn(async () => undefined);
    const resume = vi.fn(async ({ token, source }) => ({
      token,
      source,
      renew,
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    }));
    const transactionStatus = vi.fn(async () => ({
      status: "pending" as const,
      txHash,
    }));
    const config = {
      lucid: { transactionStatus },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("fresh reconciliation must not acquire a new lease");
        },
        resume,
      },
    } as unknown as DoubleSpendConstrainedWorkflowAdapterConfig;
    const adapter = createDoubleSpendConstrainedWorkflowAdapter(config);
    const identity: FraudProofWorkflowIdentity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: "d1".repeat(32),
      category: "doubleSpend",
      target: { kind: "state_queue_header", headerHash: HEADER_HASH },
    };
    const context = {
      identity,
      workflowId: h32(0x7b),
      artifact: {},
      entries: [],
      action: {
        actionId: `remove:${STATE_QUEUE_OUT_REF}`,
        input: {
          stage: "remove",
          requiresMutationLease: true,
          fraudProofOutRef: `${h32(0x7c)}#0`,
          nextRemovalOutRef: STATE_QUEUE_OUT_REF,
        },
      },
      txHash,
    } as const;
    await expect(
      adapter.reconcile({
        ...context,
        durableRecovery: {
          stateQueueMutationLease: {
            token: "lease-fence-17",
            source: "http://midgard-node.test",
          },
        },
      }),
    ).resolves.toEqual({ kind: "pending", txHash });
    expect(resume).toHaveBeenCalledWith({
      token: "lease-fence-17",
      source: "http://midgard-node.test",
    });
    expect(renew).toHaveBeenCalledTimes(1);

    const secondAdapter = createDoubleSpendConstrainedWorkflowAdapter(config);
    await expect(secondAdapter.reconcile(context)).resolves.toMatchObject({
      kind: "conflict",
      reason: expect.stringContaining("omitted its durable mutation-lease"),
    });
  });
});
