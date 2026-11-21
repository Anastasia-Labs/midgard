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

/** Where the replay store keeps what it follows. */
const IDENTITY = "replay-store.json";

/**
 * The command that fills a run's replay store before its drill, run in
 * `demo/midgard-node-tools` with the drill's stack configuration and run id.
 */
export const JOURNEY_REPLAY_WARM_UP_COMMAND = "pnpm run journey-replay:warm-up";

const replayDirectory = (runDirectory: string) =>
  join(runDirectory, "work/journeys/replay");

const notWarmed = (directory: string, missing: string) =>
  new Error(
    `The run's replay store ${directory} holds no ${missing}: run \`${JOURNEY_REPLAY_WARM_UP_COMMAND}\` in demo/midgard-node-tools with this drill's MIDGARD_WATCHER_JOURNEY_STACK_CONFIG and MIDGARD_WATCHER_JOURNEY_STACK_RUN_ID before the drill; the drill does not walk L1 from the stack's origin itself`,
  );

const describeOrigin = (origin: L1Origin | null) =>
  origin === null
    ? "no configured origin"
    : `slot ${origin.slot.toString()} block ${origin.blockHash}`;

const deploymentDifferences = (
  recorded: JourneyReplayStoreIdentity,
  wanted: Pick<
    JourneyReplayStoreIdentity,
    "deploymentManifestId" | "blueprintHash"
  >,
) =>
  [
    recorded.deploymentManifestId === wanted.deploymentManifestId
      ? undefined
      : `deployment ${recorded.deploymentManifestId}, not ${wanted.deploymentManifestId}`,
    recorded.blueprintHash === wanted.blueprintHash
      ? undefined
      : `blueprint ${recorded.blueprintHash}, not ${wanted.blueprintHash}`,
  ].filter((difference) => difference !== undefined);

const differences = (
  recorded: JourneyReplayStoreIdentity,
  wanted: JourneyReplayStoreIdentity,
) =>
  [
    ...deploymentDifferences(recorded, wanted),
    recorded.origin?.slot === wanted.origin?.slot &&
    recorded.origin?.blockHash === wanted.origin?.blockHash
      ? undefined
      : `L1 origin ${describeOrigin(recorded.origin)}, not ${describeOrigin(wanted.origin)}`,
  ].filter((difference) => difference !== undefined);

const mismatch = (directory: string, differs: readonly string[]) =>
  new Error(
    `The run's replay store ${directory} follows ${differs.join("; ")}; move it aside to replay this deployment`,
  );

/** The identity the store records, or undefined when it records none. */
const recordedIdentity = (directory: string) => {
  const path = join(directory, IDENTITY);
  return existsSync(path)
    ? readJourneyArtifact<JourneyReplayStoreIdentity>(path)
    : undefined;
};

/**
 * Takes the run's replay store, `<runDirectory>/work/journeys/replay`, for
 * one process, under a kernel lock that the process's death releases, as the
 * follower's own writer lease is (`sqlite-backend.ts`). A store recorded for
 * another deployment, blueprint or L1 origin is refused, never reused.
 */
const claimJourneyReplayStore = async (
  runDirectory: string,
  identity: JourneyReplayStoreIdentity,
  mode: "fill" | "open",
): Promise<JourneyReplayStore> => {
  const directory = replayDirectory(runDirectory);
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
    const recorded = await recordedIdentity(directory);
    const followed = existsSync(join(directory, FOLLOWER_STORE));
    if (recorded !== undefined) {
      const differs = differences(recorded, identity);
      if (differs.length > 0) throw mismatch(directory, differs);
      if (mode === "open" && !followed)
        throw notWarmed(directory, "follower store");
    } else if (followed)
      throw new Error(
        `The run's replay store ${directory} holds a follower store with no recorded deployment or origin; move it aside`,
      );
    else if (mode === "open") throw notWarmed(directory, "recorded deployment");
    else await writeJourneyArtifact(join(directory, IDENTITY), identity);
    return { directory, release: () => lock.close() };
  } catch (error) {
    lock.close();
    throw error;
  }
};

/**
 * The warm-up's claim on the run's replay store: it records the deployment
 * and origin of a new store, and reopens one it filled before.
 */
export const fillJourneyReplayStore = (
  runDirectory: string,
  identity: JourneyReplayStoreIdentity,
) => claimJourneyReplayStore(runDirectory, identity, "fill");

/**
 * A drill family's claim on the run's replay store, which the warm-up
 * filled: every family of the run reopens it and follows on from where the
 * last stopped. A store the warm-up never filled is refused by the warm-up
 * command's name, so the drill never walks L1 from the stack's origin.
 */
export const openJourneyReplayStore = (
  runDirectory: string,
  identity: JourneyReplayStoreIdentity,
) => claimJourneyReplayStore(runDirectory, identity, "open");

/**
 * Refuses at drill start, before any L1 work and without the lock, a run
 * whose replay store the warm-up has not filled for this deployment and
 * blueprint. The origin is checked when the store is first opened.
 */
export const requireJourneyReplayStore = async (
  runDirectory: string,
  deployment: Pick<
    JourneyReplayStoreIdentity,
    "deploymentManifestId" | "blueprintHash"
  >,
): Promise<void> => {
  const directory = replayDirectory(runDirectory);
  const recorded = await recordedIdentity(directory);
  if (recorded === undefined) throw notWarmed(directory, "recorded deployment");
  const differs = deploymentDifferences(recorded, deployment);
  if (differs.length > 0) throw mismatch(directory, differs);
  if (!existsSync(join(directory, FOLLOWER_STORE)))
    throw notWarmed(directory, "follower store");
};
