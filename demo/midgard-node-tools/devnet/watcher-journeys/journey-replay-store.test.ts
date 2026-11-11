import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  type JourneyReplayStoreIdentity,
  openJourneyReplayStore,
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

it("reopens the store a finished family of the same run left, with what it followed", async () => {
  const run = await runDirectory();
  const first = await openJourneyReplayStore(run, twin4);
  await writeFile(
    join(first.directory, "replayed-ledger-follower.sqlite"),
    "cursor at 5398120",
  );
  first.release();
  const next = await openJourneyReplayStore(run, twin4);
  try {
    expect(next.directory).toBe(first.directory);
    await expect(
      readFile(join(next.directory, "replayed-ledger-follower.sqlite"), "utf8"),
    ).resolves.toBe("cursor at 5398120");
  } finally {
    next.release();
  }
});

it("refuses a second family process while one holds the run's store, and lets it in once released", async () => {
  const run = await runDirectory();
  const holder = await openJourneyReplayStore(run, twin4);
  await expect(openJourneyReplayStore(run, twin4)).rejects.toThrow(
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
    const run = await runDirectory();
    (await openJourneyReplayStore(run, twin4)).release();
    await expect(openJourneyReplayStore(run, other)).rejects.toThrow(names);
    const again = await openJourneyReplayStore(run, twin4);
    again.release();
  },
);
