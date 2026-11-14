import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  type JourneyReplayStoreIdentity,
  openJourneyReplayStore,
  requireJourneyReplayStore,
} from "./journey-replay-store.js";
import {
  describeJourneyReplayWarmth,
  warmJourneyReplayStore,
} from "./journey-replay-warm-up.js";
import {
  type JourneyReplayWarmFollowerView,
  warmWhenJourneyReplayed,
} from "./replayed-ledger.read-when-replayed.js";

const STALL_MS = 400;
const options = { stallMs: STALL_MS, pollMs: 10 };

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
  const path = await mkdtemp(join(tmpdir(), "journey-replay-warm-up-"));
  runs.push(path);
  return path;
};

/**
 * A follower opened at a public stack's L1 origin, the block after it at
 * `from`: its cursor rises one block every `blockMs`, so the walk takes
 * several stall allowances, and it reports itself catching up until its
 * cursor reaches the node's tip.
 */
const catchingUpFollower = (
  input: Readonly<{ from: number; blocks: number; blockMs: number }>,
): JourneyReplayWarmFollowerView => {
  const openedAt = performance.now();
  const walked = () =>
    Math.min(
      input.blocks,
      Math.floor((performance.now() - openedAt) / input.blockMs),
    );
  const atTip = () => walked() >= input.blocks;
  return {
    isClosed: () => false,
    ready: atTip,
    cursor: () => ({ height: input.from + walked(), generation: 0 }),
    tip: () => ({ height: input.from + input.blocks }),
    pending: async () =>
      atTip()
        ? []
        : [
            {
              reason: "l1_follower_catching_up",
              detail: "the cursor is not known to be at the node's tip",
            },
          ],
  };
};

it("drives a follower from the stack's origin to the node's tip past several stall allowances, and reports the walk", async () => {
  let advanced = 0;
  const warmth = await warmWhenJourneyReplayed(
    catchingUpFollower({ from: 5_394_699, blocks: 12, blockMs: STALL_MS / 4 }),
    async () => {
      advanced += 1;
    },
    options,
  );
  expect(warmth).toEqual({
    fromHeight: 5_394_699,
    cursorHeight: 5_394_711,
    tipHeight: 5_394_711,
  });
  expect(advanced).toBe(1);
});

it("fails a warm-up whose follower stops moving for the stall allowance, naming its readiness", async () => {
  await expect(
    warmWhenJourneyReplayed(
      {
        isClosed: () => false,
        ready: () => false,
        cursor: () => ({ height: 5_396_001, generation: 0 }),
        tip: () => ({ height: 5_398_120 }),
        pending: async () => [
          { reason: "l1_node_behind", detail: "the node's tip is stale" },
        ],
      },
      async () => undefined,
      options,
    ),
  ).rejects.toThrow(/Journey replay follower is pending: .*l1_node_behind/u);
});

it("fills the run's replay store from the stack's follower, and the drill then opens it", async () => {
  const run = await runDirectory();
  await expect(requireJourneyReplayStore(run, twin4)).rejects.toThrow(
    /journey-replay:warm-up/u,
  );
  const closed: string[] = [];
  const warmed = await warmJourneyReplayStore({
    runDirectory: run,
    identity: twin4,
    // The stack's follower creates its store where it is opened, and walks
    // from the block after the origin to the tip.
    openLedger: async (directory) => {
      await writeFile(join(directory, "replayed-ledger-follower.sqlite"), "");
      return {
        warm: () =>
          warmWhenJourneyReplayed(
            catchingUpFollower({
              from: 5_394_699,
              blocks: 3_421,
              blockMs: STALL_MS / 2_000,
            }),
            async () => undefined,
            options,
          ),
        close: async () => {
          closed.push(directory);
        },
      };
    },
  });
  expect(warmed).toEqual({
    directory: join(run, "work/journeys/replay"),
    fromHeight: 5_394_699,
    cursorHeight: 5_398_120,
    tipHeight: 5_398_120,
  });
  expect(closed).toEqual([warmed.directory]);
  const report = describeJourneyReplayWarmth(warmed);
  expect(report).toContain("walked 3421 blocks");
  expect(report).toContain("0 blocks behind the node's tip");
  await requireJourneyReplayStore(run, twin4);
  const drill = await openJourneyReplayStore(run, twin4);
  expect(drill.directory).toBe(warmed.directory);
  drill.release();
});
