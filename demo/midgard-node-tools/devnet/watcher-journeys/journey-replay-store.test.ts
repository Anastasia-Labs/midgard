import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  fillJourneyReplayStore,
  type JourneyReplayStoreIdentity,
  openJourneyReplayStore,
  requireJourneyReplayStore,
} from "./journey-replay-store.js";

// A stack deployment, followed from twin4's L1 origin slot.
const twin4: JourneyReplayStoreIdentity = {
  deploymentManifestId: "ffd9e6c8".repeat(8),
  blueprintHash: "b1".repeat(32),
  origin: { slot: 138_606_953, blockHash: "0f".repeat(32) },
};

const runs: string[] = [];
afterEach(() =>
  Promise.all(
    runs.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  ),
);
const runDirectory = async () => {
  const path = await mkdtemp(join(tmpdir(), "journey-replay-store-"));
  runs.push(path);
  return path;
};

const FOLLOWER_STORE = "replayed-ledger-follower.sqlite";

/** A run whose replay store the warm-up filled: its identity and its follower store. */
const warmedRun = async (identity = twin4) => {
  const run = await runDirectory();
  const store = await fillJourneyReplayStore(run, identity);
  await writeFile(join(store.directory, FOLLOWER_STORE), "cursor at 5398120");
  store.release();
  return { run, directory: store.directory };
};

it("opens for the drill the store the warm-up filled, with what it followed", async () => {
  const { run, directory } = await warmedRun();
  await requireJourneyReplayStore(run, twin4);
  const drill = await openJourneyReplayStore(run, twin4);
  try {
    expect(drill.directory).toBe(directory);
    await expect(
      readFile(join(drill.directory, FOLLOWER_STORE), "utf8"),
    ).resolves.toBe("cursor at 5398120");
  } finally {
    drill.release();
  }
});

it("refuses the drill a run whose replay store was never filled, naming the warm-up command", async () => {
  const run = await runDirectory();
  await expect(requireJourneyReplayStore(run, twin4)).rejects.toThrow(
    /pnpm run journey-replay:warm-up/u,
  );
  await expect(openJourneyReplayStore(run, twin4)).rejects.toThrow(
    /pnpm run journey-replay:warm-up/u,
  );
});

it("refuses the drill a store whose warm-up stopped before its follower opened", async () => {
  const run = await runDirectory();
  (await fillJourneyReplayStore(run, twin4)).release();
  await expect(requireJourneyReplayStore(run, twin4)).rejects.toThrow(
    /pnpm run journey-replay:warm-up/u,
  );
  await expect(openJourneyReplayStore(run, twin4)).rejects.toThrow(
    /pnpm run journey-replay:warm-up/u,
  );
});

it("refuses a second process while one holds the run's store, and lets it in once released", async () => {
  const { run } = await warmedRun();
  const holder = await openJourneyReplayStore(run, twin4);
  await expect(openJourneyReplayStore(run, twin4)).rejects.toThrow(
    /another journey process holds the replay store/u,
  );
  await expect(fillJourneyReplayStore(run, twin4)).rejects.toThrow(
    /another journey process holds the replay store/u,
  );
  holder.release();
  const next = await openJourneyReplayStore(run, twin4);
  next.release();
});

it.each([
  [
    "another deployment",
    { ...twin4, deploymentManifestId: "aa".repeat(32) },
    /deployment/u,
  ],
  [
    "another blueprint",
    { ...twin4, blueprintHash: "cc".repeat(32) },
    /blueprint/u,
  ],
  [
    "another L1 origin",
    { ...twin4, origin: { slot: 138_682_730, blockHash: "1e".repeat(32) } },
    /origin/u,
  ],
] as const)(
  "refuses the run's store for %s, and leaves it to the next claim",
  async (_name, other, names) => {
    const { run } = await warmedRun();
    await expect(openJourneyReplayStore(run, other)).rejects.toThrow(names);
    await expect(fillJourneyReplayStore(run, other)).rejects.toThrow(names);
    const again = await openJourneyReplayStore(run, twin4);
    again.release();
  },
);

it("refuses at drill start a store filled for another deployment or blueprint", async () => {
  const { run } = await warmedRun();
  await expect(
    requireJourneyReplayStore(run, {
      ...twin4,
      deploymentManifestId: "aa".repeat(32),
    }),
  ).rejects.toThrow(/deployment/u);
  await expect(
    requireJourneyReplayStore(run, {
      ...twin4,
      blueprintHash: "cc".repeat(32),
    }),
  ).rejects.toThrow(/blueprint/u);
});
