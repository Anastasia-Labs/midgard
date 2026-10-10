import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { type JournalJsonObject } from "../workflow/journal.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { type FraudProofRawL1Utxo } from "../workflow/raw-l1-snapshot.js";
import type { ConservationPosition } from "./union-plan.js";

export type ValueConservationReferences = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  union: Readonly<Record<ConservationPosition, UTxO>>;
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: Readonly<
    Record<
      | "correctionLockSpend"
      | "stateQueueSpend"
      | "stateQueueMint"
      | "stateQueueFraudRemovalWithdraw"
      | "activeOperatorsSpend"
      | "activeOperatorsMint"
      | "retiredOperatorsSpend"
      | "retiredOperatorsMint"
      | "schedulerSpend",
      UTxO
    >
  >;
}>;

export type ManifestBoundValueConservationWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: ValueConservationReferences;
  l1Source: FraudProofL1Source;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export const fromRaw = (raw: FraudProofRawL1Utxo): UTxO => {
  const [txHash, outputIndex] = raw.outRef.split("#");
  return {
    txHash: txHash!,
    outputIndex: Number(outputIndex),
    ...coreToTxOutput(CML.TransactionOutput.from_cbor_hex(raw.outputCbor)),
  };
};

export const text = (input: JournalJsonObject, field: string): string => {
  const value = input[field];
  if (typeof value !== "string" || value.length === 0)
    throw new Error(`value conservation: missing ${field}`);
  return value;
};
