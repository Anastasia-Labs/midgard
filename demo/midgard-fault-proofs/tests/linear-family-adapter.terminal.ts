import type { EvidenceProvenance } from "@al-ft/midgard-sdk";

import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "../src/workflow/linear-family-adapter.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { type LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

const hash = (byte: string): string => byte.repeat(32);

export const headerHash = "ab".repeat(28);

export const outRef = (byte: string, index = 0): string =>
  `${hash(byte)}#${index}`;

export const txHash = hash("44");

const referenceOutRef = outRef("55");

export const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

export const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "daHashPreimage",
  target: { kind: "state_queue_header", headerHash },
};

export const signed = ({
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScriptKind,
}: {
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScriptKind?: "native" | "plutusV1" | "plutusV2" | "plutusV3";
} = {}): LocallyEvaluatedTransaction["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => submittedHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () =>
          inlineScriptKind === "native" ? { len: () => 1 } : undefined,
        plutus_v1_scripts: () =>
          inlineScriptKind === "plutusV1" ? { len: () => 1 } : undefined,
        plutus_v2_scripts: () =>
          inlineScriptKind === "plutusV2" ? { len: () => 1 } : undefined,
        plutus_v3_scripts: () =>
          inlineScriptKind === "plutusV3" ? { len: () => 1 } : undefined,
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

export const transaction = (
  overrides: Partial<LocallyEvaluatedTransaction> = {},
): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 fraud-proof da-hash-preimage step-01",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

export const l1 = (
  stageRef: { value: FraudProofRawL1FamilyStage },
  confirmed = async (_txHash: string) => false,
): FraudProofFamilyL1ObservationPort<"daHashPreimage"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  category: "daHashPreimage",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("unused in focused adapter test");
  },
  transactionConfirmed: async ({ txHash: requested }) =>
    await confirmed(requested),
  observe: async () => ({ provenance, stage: stageRef.value }),
});

export const port = (
  capture: LinearFamilyTransactionPort<"daHashPreimage">["capture"],
): LinearFamilyTransactionPort<"daHashPreimage"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "daHashPreimage",
  prepare: async () => ({ prepared: true }),
  capture,
});

export const leaseCoordinator = {
  acquire: async () => {
    throw new Error("no mutation lease expected in this focused step test");
  },
};

export const terminal = ({
  removalTxHash = txHash,
  removedOutRef = outRef("33"),
  proofOutRef = outRef("22"),
}: {
  readonly removalTxHash?: string;
  readonly removedOutRef?: string;
  readonly proofOutRef?: string;
} = {}): FraudProofWorkflowTerminal => ({
  schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
  category: "daHashPreimage",
  headerHash,
  proofToken: {
    unit: "11".repeat(28) + "22".repeat(28),
    outRef: proofOutRef,
    createdByTxHash: hash("22"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash,
    removedStateQueueOutRef: removedOutRef,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proofOutRef,
  },
  economics: {
    operatorCredential: "66".repeat(28),
    proverCredential: "77".repeat(28),
    operatorBondInputOutRef: outRef("88"),
    operatorBondInputLovelace: "900000000",
    slashedLovelace: "500000000",
    proverRewardOutputOutRef: outRef("99"),
    proverRewardLovelace: "100000000",
    removalFeeLovelace: "500000000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "1000",
    blockHash: hash("aa"),
    confirmationDepth: 30,
  },
});

export const context = {
  identity,
  workflowId: hash("bb"),
  artifact: { prepared: true },
  entries: [],
} as const;
