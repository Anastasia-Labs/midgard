import { type Network } from "@lucid-evolution/lucid";
import {
  type NodeUtxo,
  type ResolvedWalletSeedPhrase,
} from "midgard-node/commands/command-utils";
import {
  type SubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "midgard-node/commands/submit-l2-transfer";

import { GroundTruthMetrics } from "../stress-db-metrics.js";
import { EnvironmentFingerprint } from "../stress-environment-fingerprint.js";
import {
  type NoOpCalibrationSummary,
  type OpenLoopCorpusPlan,
  type OpenLoopCorpusShape,
  type OpenLoopPlacementProof,
  type OpenLoopSubmitSummary,
  type OpenLoopWorkloadProfile,
} from "../stress-open-loop.js";
import {
  type StressFullFinalityDrainProof,
  type StressMetrics,
  type StressStageMetricDbSources,
} from "../stress-stage-metrics.js";
import { E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION } from "./constants.js";

export type E2EL2StressMode = "serial-chain" | "parallel-fanout";
export type E2EL2StressLoadModel =
  | "closed-loop-smoke"
  | "open-loop-upper-bound";
export type E2EL2StressClassification =
  | "closed_loop_smoke"
  | "full_pipeline_sustained"
  | "ingress_ok_commit_failed"
  | "admission_bottleneck"
  | "validation_bottleneck"
  | "commit_planner_bottleneck"
  | "da_bottleneck"
  | "merge_bottleneck"
  | "provider_bottleneck"
  | "observer_overloaded"
  | "client_overloaded"
  | "corpus_exhausted"
  | "duplicate_submission";
export type E2EL2StressMeasurementPolicy = {
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly syntheticVsProduction:
    | "synthetic_admission_diagnostic"
    | "production_end_user_path";
  readonly advanceOn: "accepted" | "scheduled_submit";
  readonly primaryStageMetric:
    | "metrics.l2Admission.perSecond"
    | "metrics.durableAdmission.perSecond";
  readonly finalityObservation: "post-submit-bounded" | "aggregate-window";
  readonly submissionWindowExcludesCommitDrain: true;
  readonly fullFinalityRequiresDrainProof: true;
};
export type E2EL2StressRateSemantics =
  | "burst_cycle_rate"
  | "offered_tps_uncalibrated";

export type E2EL2StressSubmissionState = {
  readonly status: "submitted" | "failed";
  readonly submittedAt: string | null;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressAcceptanceState = {
  readonly status:
    | "accepted"
    | "rejected"
    | "timeout"
    | "not_observed"
    | "not_submitted";
  readonly acceptedAt?: string;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressFinalityState = {
  readonly status: "committed" | "rejected" | "timeout" | "not_observed";
  readonly committedAt?: string;
  readonly durationMs?: number;
  readonly error?: string;
};

export type E2EL2StressTransaction = {
  readonly index: number;
  readonly phase: "stress";
  readonly txHash: string | null;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly string[];
  readonly submission: E2EL2StressSubmissionState;
  readonly acceptance: E2EL2StressAcceptanceState;
  readonly finality: E2EL2StressFinalityState;
  readonly workerIndex: number;
  readonly walletSeedSource: string;
};

export type E2EL2StressFinalityObserverSummary = {
  readonly mode: "post-submit-bounded" | "aggregate-window";
  readonly maxConcurrentRequests: number;
  readonly maxObservedConcurrentRequests: number;
  readonly observedTransactionCount: number;
  readonly pollRequestCount: number;
  readonly batchCount: number;
  readonly errorCount: number;
};

export type E2EL2StressSummary = {
  readonly schemaVersion: typeof E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION;
  readonly runId: string;
  readonly status: "completed" | "interrupted";
  readonly interruptedReason?: string;
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly corpusShape?: OpenLoopCorpusShape;
  readonly classification: E2EL2StressClassification;
  readonly rateSemantics: E2EL2StressRateSemantics;
  readonly burstCycleRatePerSecond: number | null;
  readonly mode: E2EL2StressMode;
  readonly measurementPolicy: E2EL2StressMeasurementPolicy;
  readonly openLoop?: {
    readonly targetRateTps: number;
    readonly durationMs: number;
    readonly maxInFlight: number;
    readonly corpus: OpenLoopCorpusPlan;
    readonly submission: OpenLoopSubmitSummary;
    readonly calibration?: NoOpCalibrationSummary;
    readonly placement: OpenLoopPlacementProof;
  };
  readonly requestedCount: number;
  readonly notStartedCount: number;
  readonly submittedCount: number;
  readonly submissionFailedCount: number;
  readonly acceptedCount: number;
  readonly acceptanceNotObservedCount: number;
  readonly acceptanceTimedOutCount: number;
  readonly finalityTimedOutCount: number;
  readonly observedCommittedCount: number;
  readonly unknownFinalityCount: number;
  readonly rejectedCount: number;
  readonly concurrency: number;
  readonly finalityObserver: E2EL2StressFinalityObserverSummary;
  readonly startedAt: string;
  readonly submissionFinishedAt: string;
  readonly finishedAt: string;
  readonly submissionDurationMs: number;
  readonly durationMs: number;
  readonly metrics: StressMetrics;
  readonly groundTruth?: GroundTruthMetrics;
  readonly fingerprint?: EnvironmentFingerprint;
  readonly latencyMs: {
    readonly submitP50: number;
    readonly submitP95: number;
    readonly acceptanceP50: number;
    readonly acceptanceP95: number;
    readonly commitP50: number;
    readonly commitP95: number;
  };
  readonly artifactPaths: {
    readonly configJson: string;
    readonly eventsNdjson: string;
    readonly summaryJson: string;
    readonly summaryMarkdown: string;
    readonly engineReportJson?: string;
    readonly engineEventsNdjson?: string;
    readonly submitRecordsNdjson?: string;
    readonly noOpCalibrationJson?: string;
  };
  readonly transactions: readonly E2EL2StressTransaction[];
};

export type E2EL2StressRunResult = {
  readonly summary: E2EL2StressSummary;
  readonly configJsonPath: string;
  readonly eventsNdjsonPath: string;
  readonly summaryJsonPath: string;
  readonly summaryMarkdownPath: string;
};

export type StressSubmitTransferRequest = {
  readonly index: number;
  readonly phase: "stress";
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly walletAddress: string;
  readonly destinationAddress: string;
  readonly walletSeedSource: string;
};

export type StressSubmitTransfer = (
  request: StressSubmitTransferRequest,
) => Promise<SubmitL2TransferResult>;

export type StressWallet = {
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly address: string;
};

export type E2EL2StressConfig = {
  readonly runId: string;
  readonly loadModel: E2EL2StressLoadModel;
  readonly workloadProfile: OpenLoopWorkloadProfile;
  readonly mode: E2EL2StressMode;
  readonly corpusShape: OpenLoopCorpusShape;
  readonly count: number;
  readonly concurrency: number;
  readonly lovelace: bigint;
  readonly feeHeadroomLovelace: bigint;
  readonly nodeEndpoint: string;
  readonly corpusPath?: string;
  readonly corpusSliceId: string;
  readonly targetRateTps: number;
  readonly openLoopDurationMs: number;
  readonly openLoopWarmupCount: number;
  readonly openLoopCooldownCount: number;
  readonly openLoopMaxInFlight: number;
  readonly noOpCalibrationEndpoint?: string;
  readonly requireNoOpCalibration: boolean;
  readonly noOpCalibrationDurationMs: number;
  readonly aggregateObserverIntervalMs: number;
  readonly destinationAddress?: string;
  readonly pollIntervalMs: number | undefined;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
  readonly submitRequestTimeoutMs: number;
  readonly acceptanceTimeoutMs: number;
  readonly commitObservationTimeoutMs: number;
  readonly finalityObserverMaxConcurrentRequests: number;
  readonly maxSubmissionFailures: number;
  readonly outDir: string;
  readonly network: Network;
  readonly allowUnsafeBounds: boolean;
  readonly primaryWallet?: StressWallet;
  readonly stressWallets: readonly StressWallet[];
};

export type ParseE2EL2StressOptions = {
  readonly endpoint?: string;
  readonly loadModel?: string;
  readonly workloadProfile?: string;
  readonly mode?: string;
  readonly corpusShape?: string;
  readonly corpusPath?: string;
  readonly corpusSliceId?: string;
  readonly targetRateTps?: string;
  readonly openLoopDurationMs?: string;
  readonly openLoopWarmupCount?: string;
  readonly openLoopCooldownCount?: string;
  readonly openLoopMaxInFlight?: string;
  readonly noOpCalibrationEndpoint?: string;
  readonly requireNoOpCalibration?: boolean;
  readonly noOpCalibrationDurationMs?: string;
  readonly aggregateObserverIntervalMs?: string;
  readonly count?: string;
  readonly concurrency?: string;
  readonly lovelace?: string;
  readonly feeHeadroomLovelace?: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly stressWalletSeedPhraseEnvs?: readonly string[];
  readonly l2Address?: string;
  readonly runId?: string;
  readonly outDir?: string;
  readonly pollIntervalMs?: string;
  readonly pollInitialIntervalMs?: string;
  readonly pollMaxIntervalMs?: string;
  readonly submitRequestTimeoutMs?: string;
  readonly acceptanceTimeoutMs?: string;
  readonly commitObservationTimeoutMs?: string;
  readonly finalityObserverMaxConcurrentRequests?: string;
  readonly maxSubmissionFailures?: string;
  readonly network?: Network;
  readonly env?: NodeJS.ProcessEnv;
  readonly allowUnsafeBounds?: boolean;
};

export type E2EL2StressRuntime = {
  readonly submitTransfer: StressSubmitTransfer;
  readonly fetchUtxos?: (
    nodeEndpoint: string,
    address: string,
  ) => Promise<readonly NodeUtxo[]>;
  readonly fetch?: typeof fetch;
  readonly sleep?: (ms: number) => Promise<void>;
  readonly now?: () => Date;
  readonly abortSignal?: AbortSignal;
  readonly collectStageMetricSources?: (input: {
    readonly txHashes: readonly string[];
  }) => Promise<StressStageMetricDbSources>;
  readonly collectGroundTruthMetrics?: (input: {
    readonly windowStart: string;
    readonly windowEnd: string;
    readonly txHashSample: readonly string[];
    readonly offeredCount: number;
    readonly calibrationProofRef?: string | null;
  }) => Promise<GroundTruthMetrics>;
  readonly collectEnvironmentFingerprint?: (input: {
    readonly calibrationProofRef?: string | null;
  }) => Promise<EnvironmentFingerprint>;
  readonly collectAggregateObserverSample?: (input: {
    readonly at: string;
    readonly runId: string;
    readonly loadModel: E2EL2StressLoadModel;
  }) => Promise<unknown>;
  readonly fullFinalityDrainProof?: StressFullFinalityDrainProof;
  readonly runCanonicalEngine?: (input: {
    readonly config: E2EL2StressConfig;
    readonly paths: CanonicalEngineArtifactPaths;
    readonly signal?: AbortSignal;
  }) => Promise<CanonicalEngineRunResult>;
};

export type CanonicalEngineArtifactPaths = {
  readonly engineReportJson: string;
  readonly engineEventsNdjson: string;
  readonly submitRecordsNdjson: string;
  readonly noopCalibrationJson: string;
  readonly stdoutLog: string;
  readonly stderrLog: string;
};

export type CanonicalEngineRunResult = {
  readonly exitCode: number;
  readonly signal: NodeJS.Signals | null;
};
