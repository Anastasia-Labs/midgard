/**
 * Mock-free fixtures for the double-spend shared-carriage suite: a
 * Certified-tier spend-inputs opening and the production double-spend adapter
 * over a stub chain whose authenticated observer refuses an output the way the
 * production observer does.
 */
import {
  deriveFieldPreimageCertification,
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  fieldPreimagePublicationDatumCbor,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  fieldPreimageCertificateAddress,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  createDoubleSpendConstrainedWorkflowAdapter,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  type DoubleSpendWorkflowStage,
} from "../../src/workflow/double-spend-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type JournalJsonObject,
} from "../../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../../src/workflow/orchestrator.js";
import {
  buildFixtureTransaction,
  h28,
  h32,
  outRefCbor,
} from "../helpers/canonical-block-evidence-fixture.js";

const POLICY_ID = "91".repeat(28);
const HEADER_HASH = h28(0x71);
export const THREAD_OUT_REF = `${h32(0x72)}#0`;
export const STATE_QUEUE_OUT_REF = `${h32(0x73)}#0`;
const SCRIPT: MintingPolicy = { type: "Native", script: "8200" };
export const SIGNER: ResolvedProverSigner = {
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
export const LOWEST_TX_HASH = "00".repeat(32);
const OTHER_UNIT = `${"92".repeat(28)}6f74686572`;

/** The same output under another out-ref, with one more asset on it. */
export const withOtherAsset = (utxo: UTxO, txHash: string): UTxO => ({
  ...utxo,
  txHash,
  assets: { ...utxo.assets, [OTHER_UNIT]: 1n },
});
const CONTENT_IDENTITY_REFUSAL =
  "authenticated publication output differs from its journaled content identity";

export const outRef = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * A Certified-tier spend-inputs opening (440 inputs unless `spendInputs` names
 * others) and its artifact.
 */
export const certifiedOpening = (
  proofCbor = "80",
  spendInputs: readonly Buffer[] = Array.from({ length: 440 }, (_, index) =>
    outRefCbor((index % 200) + 1, BigInt(Math.floor(index / 200))),
  ),
) => {
  const transaction = buildFixtureTransaction({
    spendInputs: [...spendInputs],
    fee: 1n,
  });
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
  return { artifact, certificate, publications, plan };
};

/**
 * The adapter over a chain that holds exactly `walletUtxos` and
 * `certificateUtxos`. The authenticated observer finds an output only by its
 * exact out-ref at the address and, like the production observer, refuses one
 * whose content differs from the expected datum and value shape.
 */
export const doubleSpendHarness = ({
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
  const authenticated: string[] = [];
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
        }) => {
          const candidate = at(input.address).find(
            (utxo) => outRef(utxo) === input.expectedOutRef,
          );
          if (candidate === undefined) return { kind: "not_found" as const };
          if (
            candidate.datum !== input.expectedDatumCbor ||
            candidate.datumHash != null ||
            candidate.scriptRef != null ||
            (input.expectedUnit !== undefined &&
              candidate.assets[input.expectedUnit] !== 1n) ||
            Object.entries(candidate.assets).some(
              ([unit, quantity]) =>
                unit !== "lovelace" &&
                unit !== input.expectedUnit &&
                quantity !== 0n,
            )
          ) {
            throw new Error(CONTENT_IDENTITY_REFUSAL);
          }
          authenticated.push(input.expectedOutRef);
          return { kind: "confirmed" as const, outRef: input.expectedOutRef };
        },
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
  /** Runs preflight and returns the out-refs it authenticated. */
  const authenticatedBy = async (action: FraudProofWorkflowAction) => {
    authenticated.splice(0, authenticated.length);
    // Past authentication, preflight builds the transaction, which this chain
    // cannot do; only an authentication refusal matters here.
    await expect(preflight(action)).rejects.not.toThrow(
      CONTENT_IDENTITY_REFUSAL,
    );
    return [...authenticated];
  };
  return { next, reconcile, preflight, authenticatedBy };
};
