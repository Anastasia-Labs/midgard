import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";

import { ForeignTipReconciliationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { ContractDeploymentIdentity } from "../services/index.js";
import { stateQueueBaseHeaderHash } from "../workers/commit-block-header/state-queue.js";
import {
  deserializeStateQueueUTxO,
  type SpeculativeCommitWorkerInstruction,
  type WorkerOutput,
} from "../workers/utils/commit-block-header.js";
import { type CommitWorkerMessage } from "./block-commitment.js";
import { type SpeculativeCandidateSummary } from "./speculative-commit-state.js";

export const speculativeBuildDuration = Metric.timer(
  "speculative_build_duration_ms",
  "Memory-only speculative candidate build duration",
);

export const speculationHitCounter = Metric.counter("speculation_hit_total", {
  description: "Speculative candidates submitted without rebuilding",
});

export const speculationInvalidationCounter = Metric.counter(
  "speculation_invalidations_total",
  { description: "Speculative candidates invalidated by T1-T7 reason" },
);

export const speculationOverlapGauge = Metric.gauge(
  "speculation_overlap_efficiency",
  {
    description: "Fraction of confirmation wait overlapped by candidate build",
  },
);

export const submitAfterConfirmTimer = Metric.timer(
  "submit_after_confirm_ms",
  "Confirmation wake to speculative candidate submission completion",
);

export const commitCadenceTimer = Metric.timer(
  "commit_cadence_ms",
  "Time between consecutive block submissions",
);

export const l1ConfirmationWaitTimer = Metric.timer(
  "l1_confirmation_wait_ms",
  "Previous block submission to confirmation observation latency",
);

export const speculativeCommitBlockNumTxGauge = Metric.gauge(
  "commit_block_num_tx_count",
  {
    description:
      "Current number of L2 transactions in the submitted commit block",
    bigint: true,
  },
);

export const speculativeCommitBlockCounter = Metric.counter(
  "commit_block_count",
  {
    description: "Number of submitted commit blocks",
    bigint: true,
    incremental: true,
  },
);

export const speculativeCommitBlockTxCounter = Metric.counter(
  "commit_block_tx_count",
  {
    description: "Number of L2 transactions in submitted commit blocks",
    bigint: true,
    incremental: true,
  },
);

export type ActiveSpeculativeWorkerSession = {
  readonly generation: number;
  readonly worker: SpeculativeCommitWorkerPort;
  readonly candidate: SpeculativeCandidateSummary;
  readonly finalOutput: Promise<WorkerOutput>;
  readonly terminate: () => Promise<number>;
  localFinalizationRecovery?: Extract<
    WorkerOutput,
    { readonly type: "SuccessfulLocalFinalizationRecoveryOutput" }
  >;
};

export type SpeculativeCommitWorkerPort = {
  on(event: "message", listener: (message: CommitWorkerMessage) => void): void;
  on(event: "error", listener: (error: Error) => void): void;
  on(event: "exit", listener: (code: number) => void): void;
  postMessage(instruction: SpeculativeCommitWorkerInstruction): void;
  terminate(): Promise<number>;
};

export type BuildingSpeculativeWorkerSession = {
  readonly generation: number;
  readonly worker: SpeculativeCommitWorkerPort;
  cancelled: boolean;
  readonly terminate: () => Promise<number>;
};

export type FinishedSpeculativeWorkerSession = {
  readonly output: WorkerOutput;
  readonly candidate: SpeculativeCandidateSummary;
  readonly localFinalizationRecovery?: ActiveSpeculativeWorkerSession["localFinalizationRecovery"];
};

type SubmitSpeculativeCandidateInstruction = Extract<
  SpeculativeCommitWorkerInstruction,
  { readonly type: "SubmitSpeculativeCandidate" }
>;

const authenticateForeignTipEvidence = (
  liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"],
  _consensusProfile: MidgardConsensusProfile,
) =>
  Effect.gen(function* () {
    const tail = yield* deserializeStateQueueUTxO(liveTail);
    const headerHash = yield* stateQueueBaseHeaderHash(tail);
    if (headerHash === undefined) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Foreign T2 tip has no committed header hash",
          cause: "missing_header_hash",
        }),
      );
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(tail.datum);
    const recomputedHeaderHash = yield* SDK.hashBlockHeader(header);
    if (recomputedHeaderHash !== headerHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Foreign T2 tip header evidence is not self-consistent",
          cause: `tip=${headerHash},recomputed=${recomputedHeaderHash}`,
        }),
      );
    }
    return { headerHash, header } as const;
  });

/**
 * Authenticates the canonical tip carried across the confirmation boundary and
 * persists its immutable T2 evidence when it replaced the candidate base.
 * Returns false only when the supplied tip is still the expected base.
 */
export const persistAuthenticatedForeignTipMismatch = ({
  expectedHeaderHash,
  liveTail,
  assertedForeignHeaderHash,
  consensusProfile,
}: {
  readonly expectedHeaderHash: string;
  readonly liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly assertedForeignHeaderHash?: string;
  readonly consensusProfile: MidgardConsensusProfile;
}) =>
  Effect.gen(function* () {
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    if (
      deploymentIdentity.deploymentMarker === undefined ||
      deploymentIdentity.consensusProfile.profileId !==
        consensusProfile.profileId
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Foreign-tip persistence requires the exact active deployment marker and consensus profile",
          cause: "missing_or_mismatched_deployment_identity",
        }),
      );
    }
    const evidence = yield* authenticateForeignTipEvidence(
      liveTail,
      consensusProfile,
    );
    if (
      assertedForeignHeaderHash !== undefined &&
      evidence.headerHash !== assertedForeignHeaderHash
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Confirmation wake does not match its canonical tip evidence",
          cause: `wake=${assertedForeignHeaderHash},tip=${evidence.headerHash}`,
        }),
      );
    }
    if (evidence.headerHash === expectedHeaderHash) return false;
    yield* ForeignTipReconciliationsDB.recordMismatch({
      foreignHeaderHash: evidence.headerHash,
      replacedBaseHeaderHash: expectedHeaderHash,
      foreignHeader: evidence.header,
      consensusProfile,
      deploymentMarker: deploymentIdentity.deploymentMarker,
    });
    return true;
  });

export const recordForeignTipMismatchBeforeInvalidation = <E, R>({
  expectedHeaderHash,
  confirmedHeaderHash,
  confirmedTip,
  consensusProfile,
  invalidateCandidate,
}: {
  readonly expectedHeaderHash: string;
  readonly confirmedHeaderHash: string;
  readonly confirmedTip: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly consensusProfile: MidgardConsensusProfile;
  readonly invalidateCandidate: Effect.Effect<void, E, R>;
}) =>
  Effect.gen(function* () {
    const recorded = yield* persistAuthenticatedForeignTipMismatch({
      expectedHeaderHash,
      liveTail: confirmedTip,
      assertedForeignHeaderHash: confirmedHeaderHash,
      consensusProfile,
    });
    if (!recorded) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Confirmation mismatch branch received the unchanged candidate base",
          cause: expectedHeaderHash,
        }),
      );
    }
    yield* invalidateCandidate;
  });

/**
 * Production pre-resume decision seam. A foreign/reorged live tail must be
 * rejected before the parked speculative MPFs are handed back to the worker.
 */
export const decideSpeculativeInstructionForLiveTip = ({
  expectedHeaderHash,
  liveTail,
  submitInstruction,
  consensusProfile,
}: {
  readonly expectedHeaderHash: string;
  readonly liveTail: SubmitSpeculativeCandidateInstruction["confirmedBlock"];
  readonly submitInstruction: SubmitSpeculativeCandidateInstruction;
  readonly consensusProfile: MidgardConsensusProfile;
}) =>
  Effect.gen(function* () {
    const mismatchRecorded = yield* persistAuthenticatedForeignTipMismatch({
      expectedHeaderHash,
      liveTail,
      consensusProfile,
    });
    if (!mismatchRecorded) return submitInstruction;
    return {
      type: "InvalidateSpeculativeCandidate",
      reason: "T2",
    } as const;
  });

export const terminateWorkerSession = (
  session: BuildingSpeculativeWorkerSession | ActiveSpeculativeWorkerSession,
): Promise<number> => session.terminate();
