import { unlink } from "node:fs/promises";
import { isAbsolute } from "node:path";

import { Effect } from "effect";

export const PIPELINED_COMMIT_E2E_HARNESS_MODE = "pipelined-commit-process-v1";

export const PIPELINED_COMMIT_CRASH_CHECKPOINTS = [
  "speculative_mid_build",
  "candidate_ready_unconfirmed",
  "confirmation_wake_before_journal",
  "journal_prepared_before_submit",
] as const;

export type PipelinedCommitCrashCheckpoint =
  (typeof PIPELINED_COMMIT_CRASH_CHECKPOINTS)[number];

export const pipelinedCommitCrashCheckpointMarker = (
  checkpoint: PipelinedCommitCrashCheckpoint,
): string =>
  `pipeline_trace phase=e2e_crash_checkpoint checkpoint=${checkpoint}`;

const isCheckpoint = (value: string): value is PipelinedCommitCrashCheckpoint =>
  PIPELINED_COMMIT_CRASH_CHECKPOINTS.some((checkpoint) => checkpoint === value);

const consumeArmFile = (path: string): Promise<boolean> =>
  unlink(path).then(
    () => true,
    (error: unknown) => {
      if (
        typeof error === "object" &&
        error !== null &&
        "code" in error &&
        error.code === "ENOENT"
      ) {
        return false;
      }
      throw error;
    },
  );

/**
 * Deterministic e2e-only pause point. Production behavior is a zero-cost no-op
 * unless an exact checkpoint is configured. The harness must create a one-shot
 * arm file; the first process to reach the checkpoint atomically consumes it,
 * emits a marker, and waits for the external supervisor to SIGKILL its process
 * group. A restarted process sees no arm file and follows the normal path.
 */
export const reachPipelinedCommitCrashCheckpoint = (
  checkpoint: PipelinedCommitCrashCheckpoint,
): Effect.Effect<void, never> =>
  Effect.gen(function* () {
    const configured =
      process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_CHECKPOINT;
    if (configured === undefined || configured.trim().length === 0) {
      return;
    }
    if (!isCheckpoint(configured)) {
      return yield* Effect.die(
        new Error(
          `Invalid MIDGARD_E2E_PIPELINED_COMMIT_CRASH_CHECKPOINT: ${configured}`,
        ),
      );
    }
    if (configured !== checkpoint) {
      return;
    }
    if (
      process.env.MIDGARD_E2E_PIPELINED_COMMIT_HARNESS !==
        PIPELINED_COMMIT_E2E_HARNESS_MODE ||
      process.env.NODE_ENV !== "emulator"
    ) {
      return yield* Effect.die(
        new Error(
          "Pipelined-commit crash checkpoints require the explicit emulator process harness",
        ),
      );
    }
    const armFile = process.env.MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE;
    if (armFile === undefined || !isAbsolute(armFile)) {
      return yield* Effect.die(
        new Error(
          "MIDGARD_E2E_PIPELINED_COMMIT_CRASH_ARM_FILE must be an absolute path",
        ),
      );
    }
    const armed = yield* Effect.tryPromise(() => consumeArmFile(armFile)).pipe(
      Effect.orDie,
    );
    if (!armed) {
      return;
    }
    yield* Effect.logWarning(
      `${pipelinedCommitCrashCheckpointMarker(checkpoint)} pid=${process.pid.toString()}`,
    );
    // The child never releases itself. A bounded supervisor timeout remains a
    // second fail-closed guard if the expected marker is not externally killed.
    yield* Effect.never;
  });
