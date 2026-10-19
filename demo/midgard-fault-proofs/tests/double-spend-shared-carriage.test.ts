/**
 * The double-spend workflow adapter when the prover's carriage objects are not
 * the only ones of their kind on chain. The adapter settles its own
 * transactions by their own outputs and treats a changed selection as a
 * changed action.
 */
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
  MIDGARD_FIELD_INDEX,
  Proof,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fieldPreimageCertificateAddress,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import { splitProofIntoChunkDatums } from "../src/publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../src/runtime.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  createDoubleSpendConstrainedWorkflowAdapter,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  type DoubleSpendWorkflowStage,
} from "../src/workflow/double-spend-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type JournalJsonObject,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const POLICY_ID = "91".repeat(28);
const HEADER_HASH = h28(0x71);
const THREAD_OUT_REF = `${h32(0x72)}#0`;
const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;
const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
const SIGNER: ResolvedProverSigner = {
  source: "double-spend-shared-carriage-test",
  address: "addr_test1_double_spend_shared_carriage",
  paymentKeyHash: "81".repeat(28),
  selectWallet: () => undefined,
};
const CERTIFICATE_ADDRESS = fieldPreimageCertificateAddress({
  network: "Preview",
  certificatePolicyId: POLICY_ID,
});
const CERTIFICATE_UNIT = `${POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
/** Sorts before every hash the adapter or the fixture produces. */
const LOWEST_TX_HASH = "00".repeat(32);

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** A Certified-tier spend-inputs opening (440 inputs) and its artifact. */
const certifiedOpening = (proofCbor = "80") => {
  const spendInputs = Array.from({ length: 440 }, (_, index) =>
    outRefCbor((index % 200) + 1, BigInt(Math.floor(index / 200))),
  );
  const transaction = buildFixtureTransaction({ spendInputs, fee: 1n });
  const plan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: transaction.txId,
    nativeTxCompactCbor: transaction.compactCbor.toString("hex"),
    itemCbors: spendInputs,
    owner: SIGNER.paymentKeyHash,
    label: "double-spend shared carriage test",
  });
  expect(plan.plan.tier).toBe("Certified");
  const txArtifact = {
    inclusion: { txMembershipProofCbor: proofCbor },
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
  const certificate = (txHash: string): UTxO => ({
    txHash,
    outputIndex: 0,
    address: CERTIFICATE_ADDRESS,
    assets: { lovelace: 5_000_000n, [CERTIFICATE_UNIT]: 1n },
    datum: deriveFieldPreimageCertification(plan.plan).datumCbor,
  });
  const publications = plan.plan.publications.map(
    (publication, index): UTxO => ({
      txHash: h32(0x20 + index),
      outputIndex: 0,
      address: SIGNER.address,
      assets: { lovelace: 5_000_000n },
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );
  return { artifact, certificate, publications };
};

/**
 * The adapter over a chain that holds exactly `walletUtxos` and
 * `certificateUtxos`; the authenticated observer confirms an output only when
 * that exact out-ref carries the expected datum (and unit) at the address.
 */
const doubleSpendHarness = ({
  artifact,
  stage,
  walletUtxos,
  certificateUtxos,
}: {
  readonly artifact: JournalJsonObject;
  readonly stage: DoubleSpendWorkflowStage;
  readonly walletUtxos: readonly UTxO[];
  readonly certificateUtxos: readonly UTxO[];
}) => {
  const at = (address: string) =>
    address === SIGNER.address
      ? walletUtxos
      : address === CERTIFICATE_ADDRESS
        ? certificateUtxos
        : [];
  const config = {
    lucid: {
      utxosAt: async (address: string) => [...at(address)],
    } as unknown as LucidEvolution,
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
      referenceScriptUtxo: {
        txHash: h32(0x10),
        outputIndex: 0,
        address: SIGNER.address,
        assets: { lovelace: 5_000_000n },
        scriptRef: SCRIPT,
      },
    },
    l1: {
      publications: {
        observerVersion:
          "midgard-fraud-proof-authenticated-publication-observer-v1",
        observeExact: async (input: {
          readonly address: string;
          readonly expectedOutRef: string;
          readonly expectedDatumCbor: string;
          readonly expectedUnit?: string;
        }) =>
          at(input.address).some(
            (candidate) =>
              outRef(candidate) === input.expectedOutRef &&
              candidate.datum === input.expectedDatumCbor &&
              (input.expectedUnit === undefined ||
                candidate.assets[input.expectedUnit] === 1n),
          )
            ? { kind: "confirmed" as const, outRef: input.expectedOutRef }
            : { kind: "not_found" as const },
      },
      observe: async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "local-node-test",
          grade: "security",
        } as const,
        stage,
      }),
    },
    stateQueueMutationLeaseCoordinator: {
      acquire: async () => {
        throw new Error("removal is outside this test");
      },
    },
  } satisfies DoubleSpendConstrainedWorkflowAdapterConfig;
  const adapter = createDoubleSpendConstrainedWorkflowAdapter(config);
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "d1".repeat(32),
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash: HEADER_HASH },
  } as const;
  const next = async (): Promise<FraudProofWorkflowAction> => {
    const observed = await adapter.observe({
      identity,
      workflowId: h32(0x51),
      artifact,
      entries: [],
    });
    if (observed.kind !== "action_required")
      throw new Error(`expected an action, observed ${observed.kind}`);
    return observed.action;
  };
  const reconcile = async (action: FraudProofWorkflowAction, txHash: string) =>
    await adapter.reconcile({
      identity,
      workflowId: h32(0x51),
      artifact,
      entries: [],
      action,
      txHash,
    });
  const preflight = async (action: FraudProofWorkflowAction) =>
    await adapter.preflight({
      identity,
      workflowId: h32(0x51),
      artifact,
      entries: [],
      action,
    });
  return { next, reconcile, preflight };
};

describe("double-spend carriage shared with other on-chain objects", () => {
  it("confirms its own certificate transaction by its exact output, whatever else sits at the certificate address", async () => {
    const { artifact, certificate, publications } = certifiedOpening();
    const certificates: UTxO[] = [];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: publications,
      certificateUtxos: certificates,
    });
    const certify = await harness.next();
    expect(certify.input.stage).toBe("certify-field");
    const ownTxHash = h32(0x6c);
    const confirmed = { kind: "confirmed", txHash: ownTxHash };

    certificates.push(certificate(ownTxHash));
    await expect(harness.reconcile(certify, ownTxHash)).resolves.toEqual(
      confirmed,
    );

    // Another valid certificate for the same opening, ordered first.
    const other = certificate(LOWEST_TX_HASH);
    certificates.unshift(other);
    await expect(harness.reconcile(certify, ownTxHash)).resolves.toEqual(
      confirmed,
    );

    // Without its own output the transaction is not confirmed by the other.
    certificates.splice(0, certificates.length, other);
    await expect(harness.reconcile(certify, ownTxHash)).resolves.toEqual({
      kind: "pending",
      txHash: ownTxHash,
    });
  });

  it("confirms its own proof-chunk publication by its own outputs, whatever else sits at the prover's address", async () => {
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [chunkDatum] = splitProofIntoChunkDatums(proofCbor);
    const chunk = (txHash: string): UTxO => ({
      txHash,
      outputIndex: 0,
      address: SIGNER.address,
      assets: { lovelace: 2_000_000n },
      datum: chunkDatum!,
    });
    const { artifact } = certifiedOpening(proofCbor);
    const wallet: UTxO[] = [];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_01",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const publish = await harness.next();
    expect(publish.input.stage).toBe("publish-proof");
    const ownTxHash = h32(0x6d);
    const confirmed = { kind: "confirmed", txHash: ownTxHash };

    wallet.push(chunk(ownTxHash));
    await expect(harness.reconcile(publish, ownTxHash)).resolves.toEqual(
      confirmed,
    );

    // Another output with the same chunk datum, ordered first.
    const other = chunk(LOWEST_TX_HASH);
    wallet.unshift(other);
    await expect(harness.reconcile(publish, ownTxHash)).resolves.toEqual(
      confirmed,
    );

    // Without its own outputs the transaction is not confirmed by the other.
    wallet.splice(0, wallet.length, other);
    await expect(harness.reconcile(publish, ownTxHash)).resolves.toEqual({
      kind: "pending",
      txHash: ownTxHash,
    });
  });

  it("treats a certificate other than the scheduled one as a changed proof-step action", async () => {
    const { artifact, certificate, publications } = certifiedOpening();
    const certificates: UTxO[] = [certificate(h32(0x61))];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: publications,
      certificateUtxos: certificates,
    });
    const step = await harness.next();
    expect(step.input).toMatchObject({
      stage: "step_03",
      certificateOutRef: `${h32(0x61)}#0`,
    });

    // Another valid certificate for the same opening, ordered first.
    certificates.unshift(certificate(LOWEST_TX_HASH));
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    certificates.splice(0, certificates.length);
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });
});
