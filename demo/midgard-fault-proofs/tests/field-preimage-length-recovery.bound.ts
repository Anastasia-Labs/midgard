import {
  decodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxProofFieldLengths,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, Lucid, type TxSigned } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import { type ManifestBoundFieldPreimageLengthWorkflow } from "../src/field-preimage-length-mismatch/authenticated-workflow.js";
import { createFieldPreimageLengthRecoveryPorts } from "../src/field-preimage-length-mismatch/recovery.js";
import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "../src/field-preimage-length-mismatch/workflow-spec.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import { cursorFamilyObservation } from "../src/workflow/cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
} from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  RELEASE_L1_FINALITY_POLICY,
} from "../src/workflow/release-finality-policy.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

// These fixtures provide an authenticated workflow directly; keep their adapter
// wiring local so the production constructor is assembled from its definition.
export const createFieldPreimageLengthRecoveryAdapter = (
  workflow: Parameters<typeof createFieldPreimageLengthRecoveryPorts>[0],
) => {
  const { transactions, requirementForAction } =
    createFieldPreimageLengthRecoveryPorts(workflow);
  return {
    transactions,
    adapter: withFieldCarriagePrerequisite({
      category: "fieldPreimageLengthMismatch",
      base: createCursorFamilyWorkflowAdapter({
        spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
        l1: workflow.l1,
        transactions,
        stateQueueMutationLeaseCoordinator:
          workflow.stateQueueMutationLeaseCoordinator,
      }),
      prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
        category: "fieldPreimageLengthMismatch",
        lucid: workflow.config.lucid,
        network: workflow.binding.network,
        signer: workflow.config.signer,
        publications: workflow.l1.publications,
        transactionConfirmed: async (input) =>
          await workflow.l1.transactionConfirmed(input),
        requirementForAction,
      }),
    }),
  };
};

export const category = "fieldPreimageLengthMismatch";

export const hash = (value: string) => value.repeat(32);

export const outRef = (value: string) => `${hash(value)}#0`;

export const deploymentFingerprint = hash("aa");

const provenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "scripted-local-observation",
  grade: "security",
} as const;

const policy = { ...RELEASE_L1_FINALITY_POLICY };

export const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
  blueprintHash: hash("bb"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

export const forbidden = (): never => {
  throw new Error("unexpected builder or live-header call");
};

export const rawFixture = async () => {
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
  return fixture;
};

export const material = async (
  provided?: Awaited<ReturnType<typeof rawFixture>>,
) => {
  const fixture = provided ?? (await rawFixture());
  const routed = await fetchFraudProofEvidence({
    observation: authenticatedHeaderObservation(fixture),
    sources: [
      {
        sourceId: "recovery-fixture",
        fetchPayloadByHeaderHash: async () => ({
          ok: true,
          sourceId: "recovery-fixture",
          sourcePeerId: "peer",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "recovery-fixture/peer",
            grade: "security",
          },
        }),
      },
    ],
  });
  if (routed.kind !== "field_preimage_length_mismatch")
    throw new Error("expected real malformed-length route");
  return routed;
};

export const bound = async (
  headerHash: string,
  stage: { value: FraudProofRawL1FamilyStage },
) => {
  const observeHeader =
    vi.fn<FraudProofFamilyL1ObservationPort<typeof category>["observeHeader"]>(
      forbidden,
    );
  const l1: FraudProofFamilyL1ObservationPort<typeof category> = {
    portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
    category,
    observeHeader,
    transactionConfirmed: async () => true,
    observe: async () => ({ provenance, stage: stage.value }),
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async () => {
        throw new Error("no non-inline prerequisite expected");
      },
    },
  };
  const binding = {
    deploymentFingerprint,
    definition: { category, headerHash },
    network: "Custom",
    releaseFinality,
    releaseEconomics: { policy: { fraudProverRewardLovelace: "2000000" } },
  } as ManifestBoundFieldPreimageLengthWorkflow["binding"];
  const lucid = await Lucid(undefined, "Custom", {
    slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1000 },
  });
  vi.spyOn(lucid, "utxosAt").mockResolvedValue([]);
  const config: ManifestBoundFieldPreimageLengthWorkflow["config"] = {
    schemaVersion:
      "midgard-field-preimage-length-mismatch-production-config-v1",
    binding,
    signer: {
      source: "test",
      address: "test-address",
      paymentKeyHash: "11".repeat(28),
      selectWallet: forbidden,
    },
    lucid,
    contracts: {
      fieldPreimageCertificate: {
        policyId: "22".repeat(28),
        mintingScript: { type: "PlutusV3", script: "00" },
        mintingScriptCBOR: "00",
      },
      get computationThread() {
        return forbidden();
      },
      get fraudProof() {
        return forbidden();
      },
      get fieldPreimageLengthMismatch() {
        return forbidden();
      },
    },
    referenceScripts: {
      get step01() {
        return forbidden();
      },
      get step02Accepted() {
        return forbidden();
      },
      get step02Forced() {
        return forbidden();
      },
      get step03() {
        return forbidden();
      },
      get fieldPreimageCertificateMint() {
        return forbidden();
      },
      get witnesses() {
        return forbidden();
      },
    },
  };
  return {
    workflow: {
      binding,
      config,
      l1,
      decisionDigest: hash("33"),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("no lease expected");
        },
      },
    },
    observeHeader,
  };
};

export const context = (
  headerHash: string,
  artifact: Parameters<
    ReturnType<
      typeof createFieldPreimageLengthRecoveryAdapter
    >["adapter"]["observe"]
  >[0]["artifact"],
) => ({
  identity: {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: { kind: "state_queue_header" as const, headerHash },
  } satisfies FraudProofWorkflowIdentity,
  workflowId: hash("44"),
  artifact,
  entries: [],
});

export const required = (
  headerHash: string,
  stage: FraudProofRawL1FamilyStage,
) => {
  const result = cursorFamilyObservation({
    spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
    headerHash,
    provenance,
    stage,
  });
  if (result.kind !== "action_required")
    throw new Error("expected selected cursor action");
  return result.action;
};

export const captured = (): LocallyEvaluatedTransaction => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("55")), 0n),
  );
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("66")), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  body.set_reference_inputs(references);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const signed = {
    toHash: () => txHash,
    toTransaction: () => transaction,
  } as TxSigned;
  return {
    txHash,
    signed,
    referenceScripts: [
      {
        role: "fixture reference",
        outRef: outRef("66"),
        scriptHash: "77".repeat(28),
      },
    ],
  };
};
