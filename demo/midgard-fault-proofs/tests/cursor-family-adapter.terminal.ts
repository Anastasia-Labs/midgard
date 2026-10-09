import type { EvidenceProvenance } from "@al-ft/midgard-sdk";

import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../src/execution-native-script-invalid/workflow-spec.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../src/workflow/cursor-family-adapter.js";
import { cursorFamilyObservation } from "../src/workflow/cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import type {
  FraudProofWorkflowIdentity,
  FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { type LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

export const hash = (byte: string): string => byte.repeat(32);

const headerHash = "ab".repeat(28);

export const outRef = (byte: string, index = 0): string =>
  `${hash(byte)}#${index}`;

export const txHash = hash("44");

const referenceOutRef = outRef("55");

const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

export const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "executionNativeScriptInvalid",
  target: { kind: "state_queue_header", headerHash },
};

export const signed = ({
  bodyHash = txHash,
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScript = false,
  submit,
}: {
  readonly bodyHash?: string;
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScript?: boolean;
  readonly submit?: () => Promise<string>;
} = {}): LocallyEvaluatedTransaction["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => bodyHash,
    submit: submit ?? (async () => submittedHash),
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () => undefined,
        plutus_v1_scripts: () => undefined,
        plutus_v2_scripts: () => undefined,
        plutus_v3_scripts: () => (inlineScript ? { len: () => 1 } : undefined),
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
      role: "V1 execution-native-script-invalid step",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

export const l1 = (
  stageRef: { value: FraudProofRawL1FamilyStage },
  confirmed = async (_txHash: string) => false,
): FraudProofFamilyL1ObservationPort<"executionNativeScriptInvalid"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  category: "executionNativeScriptInvalid",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("unused in focused cursor adapter test");
  },
  transactionConfirmed: async ({ txHash: requested }) =>
    await confirmed(requested),
  observe: async () => ({ provenance, stage: stageRef.value }),
});

export const port = (
  capture: CursorFamilyTransactionPort<"executionNativeScriptInvalid">["capture"],
): CursorFamilyTransactionPort<"executionNativeScriptInvalid"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "executionNativeScriptInvalid",
  prepare: async () => ({ prepared: true }),
  capture,
});

export const noLeaseCoordinator = {
  acquire: async () => {
    throw new Error("no mutation lease expected");
  },
};

export const context = {
  identity,
  workflowId: hash("bb"),
  artifact: { prepared: true },
  entries: [],
} as const;

export const required = (stage: FraudProofRawL1FamilyStage) => {
  const observation = cursorFamilyObservation({
    spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    headerHash,
    provenance,
    stage,
  });
  if (observation.kind !== "action_required") {
    throw new Error("fixture has no required action");
  }
  return observation.action;
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
  category: "executionNativeScriptInvalid",
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
