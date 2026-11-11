import { existsSync } from "node:fs";
import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import {
  isSqliteMutexBusy,
  SqliteProcessMutex,
} from "@al-ft/midgard-core/sqlite-process-mutex";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";

/** What a journey replay store follows: one deployment from one L1 origin. */
export type JourneyReplayStoreIdentity = Readonly<{
  deploymentManifestId: string;
  blueprintHash: string;
  origin: L1Origin | null;
}>;

/** A journey replay store this process holds until `release`. */
export type JourneyReplayStore = Readonly<{
  directory: string;
  release(): void;
}>;

/** The follower store `openJourneyReplayedLedger` keeps in its directory. */
const FOLLOWER_STORE = "replayed-ledger-follower.sqlite";

const describeOrigin = (origin: L1Origin | null) =>
  origin === null
    ? "no configured origin"
    : `slot ${origin.slot.toString()} block ${origin.blockHash}`;

const differences = (
  recorded: JourneyReplayStoreIdentity,
  wanted: JourneyReplayStoreIdentity,
) =>
  [
    recorded.deploymentManifestId === wanted.deploymentManifestId
      ? undefined
      : `deployment ${recorded.deploymentManifestId}, not ${wanted.deploymentManifestId}`,
    recorded.blueprintHash === wanted.blueprintHash
      ? undefined
      : `blueprint ${recorded.blueprintHash}, not ${wanted.blueprintHash}`,
    recorded.origin?.slot === wanted.origin?.slot &&
    recorded.origin?.blockHash === wanted.origin?.blockHash
      ? undefined
      : `L1 origin ${describeOrigin(recorded.origin)}, not ${describeOrigin(wanted.origin)}`,
  ].filter((difference) => difference !== undefined);

/**
 * The run's one journey replay store, `<runDirectory>/work/journeys/replay`:
 * the follower store and replayed-ledger checkpoint every family of the run
 * reopens, so only the first walks L1 from the stack's origin and each later
 * one follows on from where the last stopped. One process holds it at a
 * time, under a kernel lock that the process's death releases, as the
 * follower's own writer lease is (`sqlite-backend.ts`). A store recorded for
 * another deployment, blueprint or L1 origin is refused, never reused.
 */
export const openJourneyReplayStore = async (
  runDirectory: string,
  identity: JourneyReplayStoreIdentity,
): Promise<JourneyReplayStore> => {
  const directory = join(runDirectory, "work/journeys/replay");
  await mkdir(directory, { recursive: true, mode: 0o700 });
  let lock: SqliteProcessMutex;
  try {
    lock = SqliteProcessMutex.acquire(join(directory, "replay-store.lock"));
  } catch (error) {
    if (!isSqliteMutexBusy(error)) throw error;
    throw new Error(
      `Cannot open the run's replay store ${directory}: another journey process holds the replay store, and a run's families take it one process at a time`,
    );
  }
  try {
    const identityPath = join(directory, "replay-store.json");
    if (existsSync(identityPath)) {
      const recorded =
        await readJourneyArtifact<JourneyReplayStoreIdentity>(identityPath);
      const differs = differences(recorded, identity);
      if (differs.length > 0)
        throw new Error(
          `The run's replay store ${directory} follows ${differs.join("; ")}; move it aside to replay this deployment`,
        );
    } else if (existsSync(join(directory, FOLLOWER_STORE)))
      throw new Error(
        `The run's replay store ${directory} holds a follower store with no recorded deployment or origin; move it aside`,
      );
    else await writeJourneyArtifact(identityPath, identity);
    return { directory, release: () => lock.close() };
  } catch (error) {
    lock.close();
    throw error;
  }
};
