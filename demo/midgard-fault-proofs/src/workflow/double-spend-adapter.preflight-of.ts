import type {
  LucidEvolution,
  MintingPolicy,
  Network,
  UTxO,
} from "@lucid-evolution/lucid";

import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { type FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type DoubleSpendL1ObservationPort } from "./double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import type {
  FraudProofWorkflowJournalEntry,
  JournalJsonObject,
  JournalJsonValue,
} from "./journal.js";
import type { FraudProofL1Source } from "./l1-source.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
  FraudProofWorkflowPreflight,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  bindWorkflowPreflightTransaction,
  LOCAL_UPLC_EVALUATOR,
  type LocallyEvaluatedTransaction,
  requireReferenceOnlyScriptWitnesses,
} from "./transaction-boundary.js";

export type DoubleSpendWorkflowReferenceScripts = {
  readonly steps: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
};

export type DoubleSpendConstrainedWorkflowAdapterConfig = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly referenceScripts: DoubleSpendWorkflowReferenceScripts;
  readonly fieldPreimageCertificate: {
    readonly policyId: string;
    readonly mintingScript: MintingPolicy;
    readonly referenceScriptUtxo: UTxO;
  };
  readonly l1: DoubleSpendL1ObservationPort;
  /** Coordination only; never used as proof evidence. */
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  readonly fraudProverRewardLovelace?: bigint;
};

export type ManifestBoundDoubleSpendWorkflowConfig = Omit<
  DoubleSpendConstrainedWorkflowAdapterConfig,
  | "blueprint"
  | "deploymentInfo"
  | "network"
  | "fieldPreimageCertificate"
  | "l1"
  | "fraudProverRewardLovelace"
> & {
  readonly manifest: unknown;
  readonly blueprintJson: string;
  readonly deploymentInfo: unknown;
  readonly headerHash: string;
  readonly l1Source: FraudProofL1Source;
  readonly fieldPreimageCertificateReferenceScript: UTxO;
};

export type ManifestBoundDoubleSpendWorkflow = {
  readonly binding: FraudProofWorkflowDeploymentBinding<"doubleSpend">;
  readonly adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfig;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
};

export type DoubleSpendArtifact = JournalJsonObject & {
  readonly headerHash: string;
  readonly tx1: JournalJsonObject & {
    readonly inclusion: JournalJsonObject;
    readonly nativeTxId: string;
    readonly nativeTxCompactCbor: string;
    readonly spendInputCbors: readonly string[];
    readonly doubleSpentInputIndex: number;
  };
  readonly tx2: JournalJsonObject & {
    readonly inclusion: JournalJsonObject;
    readonly nativeTxId: string;
    readonly nativeTxCompactCbor: string;
    readonly spendInputCbors: readonly string[];
    readonly doubleSpentInputIndex: number;
  };
};

export const journalValue = (value: unknown): JournalJsonValue => {
  if (typeof value === "bigint") return value.toString();
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean" ||
    typeof value === "number"
  ) {
    return value;
  }
  if (Array.isArray(value)) return value.map(journalValue);
  if (typeof value !== "object") {
    throw new Error("double-spend artifact contains a non-JSON value");
  }
  return Object.fromEntries(
    Object.entries(value as Readonly<Record<string, unknown>>).map(
      ([key, child]) => [key, journalValue(child)],
    ),
  );
};

export const artifactFrom = (value: JournalJsonObject): DoubleSpendArtifact =>
  value as DoubleSpendArtifact;

export const requireJournalString = (
  value: JournalJsonValue | undefined,
  label: string,
): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a string`);
  }
  return value;
};

export const confirmed = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  actionId: string,
): boolean =>
  entries.some(
    (entry) =>
      entry.event.kind === "confirmed" && entry.event.actionId === actionId,
  );

export const contentActionId = ({
  base,
  entries,
}: {
  readonly base: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): string => {
  const priorConfirmations = entries.filter(
    (entry) =>
      entry.event.kind === "confirmed" &&
      (entry.event.actionId === base ||
        entry.event.actionId.startsWith(`${base}:heal:`)),
  ).length;
  return priorConfirmations === 0
    ? base
    : `${base}:heal:${priorConfirmations.toString()}`;
};

export const action = (
  actionId: string,
  input: JournalJsonObject,
): FraudProofWorkflowAction => ({ actionId, input });

export const preflightOf = (
  actionId: string,
  transaction: LocallyEvaluatedTransaction,
  durableRecovery?: JournalJsonObject,
): FraudProofWorkflowPreflight => {
  requireReferenceOnlyScriptWitnesses({
    transaction,
    label: "double-spend production transaction",
  });
  // The production funding reservation permit reads the signed body back from
  // the in-memory preflight to reconcile the reserved inputs it spends.
  return bindWorkflowPreflightTransaction(
    {
      actionId,
      txHash: transaction.txHash,
      scriptExecution:
        transaction.referenceScripts.length === 0
          ? "none"
          : "reference_scripts",
      localUplcEvaluation: {
        status: "passed",
        evaluator: LOCAL_UPLC_EVALUATOR,
      },
      referenceScripts: transaction.referenceScripts,
      ...(durableRecovery === undefined ? {} : { durableRecovery }),
    },
    transaction.signed,
  );
};

export const mutationLeaseRecovery = (
  lease: StateQueueMutationLease,
): JournalJsonObject => ({
  stateQueueMutationLease: {
    token: lease.token,
    source: lease.source,
  },
});

export const parseMutationLeaseRecovery = (
  recovery: JournalJsonObject | undefined,
): { readonly token: string; readonly source: string } | undefined => {
  if (recovery === undefined) return undefined;
  const keys = Object.keys(recovery);
  const value = recovery.stateQueueMutationLease;
  if (
    keys.length !== 1 ||
    keys[0] !== "stateQueueMutationLease" ||
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value)
  ) {
    throw new Error("durable recovery has an invalid mutation-lease shape");
  }
  const record = value as Readonly<Record<string, JournalJsonValue>>;
  if (
    Object.keys(record).sort().join(",") !== "source,token" ||
    typeof record.token !== "string" ||
    record.token.trim().length === 0 ||
    record.token.trim() !== record.token ||
    typeof record.source !== "string" ||
    record.source.trim().length === 0 ||
    record.source.trim() !== record.source
  ) {
    throw new Error("durable recovery mutation-lease identity is malformed");
  }
  return { token: record.token, source: record.source };
};
