import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { type FamilyAssemblyContext } from "../workflow/family-definition.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
} from "../workflow/historical-native-script-corpus.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { type TransitionDepositOpening } from "./history-opening.js";
import { captureTransitionTraceL1Events } from "./l1-events.js";
import { type TransitionProofInput } from "./proof-material.js";
import { TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES } from "./submit.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

export const TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS = Object.freeze([
  SDK.FraudProofComputationThreadStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceProofCommitmentDatum,
  SDK.TransitionTraceProofCommitmentDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
] as const);

export const TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES = Object.freeze(
  [
    "fraudProofTransitionTrace",
    ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
    ...Object.values(TRANSITION_TRACE_YIELD_REFERENCES).map(
      (entry) => entry.entry,
    ),
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "stateQueueSpend",
  ],
);

export type Prepared = Readonly<{
  proof: TransitionProofInput;
  artifact: JournalJsonObject;
  references: readonly UTxO[];
  depositPolicyId: string;
  depositOpening?: TransitionDepositOpening;
}>;

export type ReplayCell = {
  evidence?: CanonicalBlockEvidence;
  corpus?: HistoricalNativeScriptCorpus;
  l1Events?: Awaited<ReturnType<typeof captureTransitionTraceL1Events>>;
  prepared?: ReadonlyMap<string, Prepared>;
};

export const cells = new WeakMap<object, ReplayCell>();

export type ManifestBoundTransitionTraceWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  /** Every family step/yield and shared mint witness is published before Init. */
  referenceScripts: Readonly<Record<string, UTxO>>;
  l1Source: FraudProofL1Source;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type TransitionRuntime = Readonly<{
  config: ManifestBoundTransitionTraceWorkflowConfig;
  cell: ReplayCell;
}>;

export type TransitionContext = FamilyAssemblyContext<
  "transitionTrace",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  false,
  9,
  TransitionRuntime
>;
