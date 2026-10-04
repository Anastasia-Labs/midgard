import {
  mkdir,
  open,
  readFile,
  realpath,
  rename,
  unlink,
} from "node:fs/promises";
import { join } from "node:path";

import { WATCHER_ROLLBACK_BOUNDS } from "../l1/rollback-engine/types.js";
import {
  watcherCanonicalJson,
  watcherSha256CanonicalJson,
} from "../storage/durable-store.js";
import {
  stagedRecordPath,
  syncDirectory,
} from "../storage/exclusive-record-file.js";
import {
  readWatcherProofExecution,
  type WatcherProofExecution,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";

export const WATCHER_PROOF_COMPLETION_MARKER =
  "midgard-watcher-fault-proof-completion-marker-v1" as const;

const MAXIMUM_MARKER_BYTES = 4 * 1024;

type CompletionMarker = Readonly<{
  schemaVersion: typeof WATCHER_PROOF_COMPLETION_MARKER;
  deploymentFingerprint: string;
  category: string;
  headerHash: string;
  workflowId: string;
  journalDigest: string;
  confirmationDepth: number;
  recoveryDepth: string;
}>;

/** A completion deeper than the watcher's rollback recovery reach cannot be
 * undone by any rollback the watcher recovers from automatically. A rollback
 * of depth d removes the block at confirmation depth c exactly when d >= c. */
export const isBeyondWatcherRollbackRecovery = (
  confirmationDepth: number,
): boolean =>
  Number.isSafeInteger(confirmationDepth) &&
  BigInt(confirmationDepth) > WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth;

const markerDirectory = async (
  journalRoot: string,
  category: string,
): Promise<string> => {
  const root = join(journalRoot, "fault-proof-completions-v1");
  const directory = join(root, category);
  await mkdir(directory, { recursive: true, mode: 0o700 });
  if (
    (await realpath(root)) !== root ||
    (await realpath(directory)) !== directory
  )
    throw new Error("proof completion marker traverses a symlink");
  return directory;
};

const expectedMarker = (input: {
  readonly deploymentFingerprint: string;
  readonly objective: WatcherProofObjective;
  readonly execution: WatcherProofExecution;
}) => ({
  schemaVersion: WATCHER_PROOF_COMPLETION_MARKER,
  deploymentFingerprint: input.deploymentFingerprint,
  category: input.objective.category,
  headerHash: input.objective.headerHash,
  workflowId: input.execution.workflowId,
  journalDigest: watcherSha256CanonicalJson(input.execution.entries),
});

/** Records that this exact completed journal was canonically verified beyond
 * rollback recovery, so a restart skips it. A completion within recovery
 * reach is left unmarked and is verified again on the next start. */
export const markWatcherProofCompletionBeyondRecovery = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly objective: WatcherProofObjective;
  readonly execution: WatcherProofExecution;
  readonly confirmationDepth: number;
}): Promise<void> => {
  if (
    input.execution.entries.at(-1)?.event.kind !== "completed" ||
    !isBeyondWatcherRollbackRecovery(input.confirmationDepth)
  )
    return;
  const marker: CompletionMarker = {
    ...expectedMarker(input),
    confirmationDepth: input.confirmationDepth,
    recoveryDepth: WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth.toString(),
  };
  // The marker only spares a later start one verification. Failing to write
  // it leaves the objective unmarked, so the next start verifies it again;
  // it must never stop the supervisor.
  await writeMarker(input, marker).catch(() => undefined);
};

/** Replacing by rename keeps a crash from exposing partial marker bytes. */
const writeMarker = async (
  input: {
    readonly journalRoot: string;
    readonly objective: WatcherProofObjective;
  },
  marker: CompletionMarker,
): Promise<void> => {
  const directory = await markerDirectory(
    input.journalRoot,
    input.objective.category,
  );
  const staging = stagedRecordPath(directory);
  const handle = await open(staging, "wx", 0o600);
  try {
    try {
      await handle.writeFile(`${watcherCanonicalJson(marker)}\n`, "utf8");
      await handle.sync();
    } finally {
      await handle.close();
    }
    await rename(
      staging,
      join(directory, `${input.objective.headerHash}.json`),
    );
  } catch (error) {
    await unlink(staging).catch(() => undefined);
    throw error;
  }
  await syncDirectory(directory);
};

/** True only for a marker bound to this exact completed journal. A missing,
 * torn, foreign or stale marker leaves the objective to canonical
 * re-verification, which rewrites the marker when it qualifies. The marker
 * is never acceptance evidence for anything else. */
export const isWatcherProofCompletionMarked = async (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly target: Readonly<{ category: string; headerHash: string }>;
}): Promise<boolean> => {
  const path = join(
    input.journalRoot,
    "fault-proof-completions-v1",
    input.target.category,
    `${input.target.headerHash}.json`,
  );
  // Any unreadable marker only costs a verification, never a start.
  const bytes = await readFile(path).catch(() => undefined);
  const marker = bytes === undefined ? undefined : parseMarker(bytes);
  if (
    marker === undefined ||
    typeof marker.confirmationDepth !== "number" ||
    !isBeyondWatcherRollbackRecovery(marker.confirmationDepth)
  )
    return false;
  const objective = input.target as WatcherProofObjective;
  const execution = await readWatcherProofExecution({ ...input, objective });
  if (execution?.entries.at(-1)?.event.kind !== "completed") return false;
  const expected = expectedMarker({ ...input, objective, execution });
  return (Object.keys(expected) as (keyof typeof expected)[]).every(
    (key) => marker[key] === expected[key],
  );
};

const parseMarker = (
  bytes: Buffer,
): Partial<Record<keyof CompletionMarker, unknown>> | undefined => {
  if (bytes.byteLength > MAXIMUM_MARKER_BYTES) return undefined;
  try {
    const parsed: unknown = JSON.parse(bytes.toString("utf8"));
    return typeof parsed === "object" && parsed !== null
      ? (parsed as Partial<Record<keyof CompletionMarker, unknown>>)
      : undefined;
  } catch {
    return undefined;
  }
};
