import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../src/workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowIdentityV1 } from "../src/workflow/journal-v1.js";
import {
  createProductionMissingSignatureWorkflowAdapterV1,
  PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1,
  type ProductionMissingSignatureTransactionPortV1,
} from "../src/workflow/production-missing-signature-adapter-v1.js";
import type { FraudProofRawL1FamilyStageV1 } from "../src/workflow/raw-l1-family-derivation-v1.js";
import type { LocallyEvaluatedTransactionV1 } from "../src/workflow/transaction-boundary-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const referenceOutRef = outRef("55");
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const identity: FraudProofWorkflowIdentityV1 = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "missingSignature",
  target: { kind: "state_queue_header", headerHash },
};

const signed = ({
  txHash,
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScript = false,
}: {
  readonly txHash: string;
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScript?: boolean;
}): LocallyEvaluatedTransactionV1["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => submittedHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () => (inlineScript ? { len: () => 1 } : undefined),
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

const transaction = (
  txHash: string,
  overrides: Partial<LocallyEvaluatedTransactionV1> = {},
): LocallyEvaluatedTransactionV1 => ({
  txHash,
  signed: signed({ txHash }),
  referenceScripts: [
    {
      role: "V1 fraud-proof missing-signature step-04",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

const l1 = (
  stageRef: { value: FraudProofRawL1FamilyStageV1 },
  confirmed: Set<string>,
): FraudProofFamilyL1ObservationPortV1<"missingSignature"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  category: "missingSignature",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("unused in focused adapter test");
  },
  transactionConfirmed: async ({ txHash }) => confirmed.has(txHash),
  observe: async () => ({ provenance, stage: stageRef.value }),
});

const port = (
  capture: ProductionMissingSignatureTransactionPortV1["capture"],
): ProductionMissingSignatureTransactionPortV1 => ({
  portVersion: PRODUCTION_MISSING_SIGNATURE_TRANSACTION_PORT_V1,
  category: "missingSignature",
  prepare: async () => ({ prepared: true }),
  capture,
});

const leaseCoordinator = {
  acquire: async () => {
    throw new Error("no mutation lease expected in this focused scan test");
  },
};

const context = {
  identity,
  workflowId: hash("bb"),
  artifact: { prepared: true },
  entries: [],
} as const;

const step04 = (threadOutRef: string): FraudProofRawL1FamilyStageV1 => ({
  kind: "step",
  step: 4,
  threadOutRef,
  stateQueueBlockOutRef: outRef("10"),
});

describe("production missing-signature adapter V1", () => {
  it("survives restart after each of multiple step-04 scan batches", async () => {
    const stage = { value: step04(outRef("41")) };
    const confirmed = new Set<string>();
    const txHashes = [hash("42"), hash("43")];
    let captureIndex = 0;
    const capture = vi.fn(async () => ({
      transaction: transaction(txHashes[captureIndex++]!),
    }));

    const firstProcess = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    const firstObservation = await firstProcess.observe(context);
    if (firstObservation.kind !== "action_required") {
      throw new Error("expected first scan action");
    }
    const firstPreflight = await firstProcess.preflight({
      ...context,
      action: firstObservation.action,
    });
    await expect(
      firstProcess.submit({
        ...context,
        action: firstObservation.action,
        preflight: firstPreflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash: txHashes[0] });
    confirmed.add(txHashes[0]!);
    stage.value = step04(`${txHashes[0]}#0`);

    // Fresh adapter: no captured body or in-memory cursor survives the crash.
    const secondProcess = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      secondProcess.reconcile({
        ...context,
        action: firstObservation.action,
        txHash: txHashes[0],
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: txHashes[0] });
    const secondObservation = await secondProcess.observe(context);
    if (secondObservation.kind !== "action_required") {
      throw new Error("expected second scan action");
    }
    expect(secondObservation.action.actionId).not.toBe(
      firstObservation.action.actionId,
    );
    const secondPreflight = await secondProcess.preflight({
      ...context,
      action: secondObservation.action,
    });
    await expect(
      secondProcess.submit({
        ...context,
        action: secondObservation.action,
        preflight: secondPreflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash: txHashes[1] });
    confirmed.add(txHashes[1]!);
    stage.value = step04(`${txHashes[1]}#1`);

    const thirdProcess = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      thirdProcess.reconcile({
        ...context,
        action: secondObservation.action,
        txHash: txHashes[1],
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: txHashes[1] });
    expect(capture).toHaveBeenCalledTimes(2);
  });

  it("refuses stale actions, inline witnesses, and forged reference claims", async () => {
    const stage = { value: step04(outRef("41")) };
    const confirmed = new Set<string>();
    const capture = vi.fn(async () => ({
      transaction: transaction(hash("42")),
    }));
    const adapter = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    const observation = await adapter.observe(context);
    if (observation.kind !== "action_required")
      throw new Error("missing action");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...observation.action,
          input: { ...observation.action.input, threadOutRef: outRef("99") },
        },
      }),
    ).rejects.toThrow("differs from authenticated current L1 state");
    expect(capture).not.toHaveBeenCalled();

    const inline = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(async () => ({
        transaction: transaction(hash("42"), {
          signed: signed({ txHash: hash("42"), inlineScript: true }),
        }),
      })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      inline.preflight({ ...context, action: observation.action }),
    ).rejects.toThrow("embeds inline script witnesses");

    const forged = createProductionMissingSignatureWorkflowAdapterV1({
      l1: l1(stage, confirmed),
      transactions: port(async () => ({
        transaction: transaction(hash("42"), {
          signed: signed({
            txHash: hash("42"),
            includedReferenceOutRef: outRef("56"),
          }),
        }),
      })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      forged.preflight({ ...context, action: observation.action }),
    ).rejects.toThrow("claimed a reference script absent from the signed body");
  });

  it("rejects substituted port identities", () => {
    const stage = { value: step04(outRef("41")) };
    const confirmed = new Set<string>();
    expect(() =>
      createProductionMissingSignatureWorkflowAdapterV1({
        l1: { ...l1(stage, confirmed), category: "invalidSignature" } as never,
        transactions: port(async () => ({
          transaction: transaction(hash("42")),
        })),
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      }),
    ).toThrow("ports changed identity");
  });
});
