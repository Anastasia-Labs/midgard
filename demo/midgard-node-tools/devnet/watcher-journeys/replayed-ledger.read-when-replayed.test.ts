import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
  RetainedDaPayloadUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { expect, it } from "vitest";

import {
  type JourneyReplayFollowerView,
  readWhenJourneyReplayed,
} from "./replayed-ledger.read-when-replayed.js";

const STALL_MS = 400;
const POLL_MS = 10;
const options = { stallMs: STALL_MS, pollMs: POLL_MS };

/**
 * A follower opened at a public stack's L1 origin, walking the blocks since:
 * its cursor rises one block at a time, each block well inside the stall
 * allowance, while the whole walk takes several allowances. Until it reaches
 * the tip it reports itself catching up.
 */
const catchingUpFollower = (
  input: Readonly<{ blocks: number; blockMs: number }>,
): JourneyReplayFollowerView & { atTip(): boolean } => {
  const openedAt = performance.now();
  const height = () =>
    Math.min(
      input.blocks,
      Math.floor((performance.now() - openedAt) / input.blockMs),
    );
  const atTip = () => height() >= input.blocks;
  return {
    atTip,
    isClosed: () => false,
    ready: atTip,
    cursor: () => ({ height: 5_394_699 + height(), generation: 0 }),
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

const readyFollower = (): JourneyReplayFollowerView => ({
  isClosed: () => false,
  ready: () => true,
  cursor: () => ({ height: 5_397_332, generation: 0 }),
  pending: async () => [],
});

it("waits for a follower that keeps advancing from the stack's origin past one stall allowance", async () => {
  const follower = catchingUpFollower({
    blocks: 12,
    blockMs: STALL_MS / 4,
  });
  const ledger = await readWhenJourneyReplayed(
    follower,
    async () => {
      expect(follower.atTip()).toBe(true);
      return ["ledger"];
    },
    options,
  );
  expect(ledger).toEqual(["ledger"]);
});

it("fails a follower whose cursor stops for the stall allowance, naming its readiness", async () => {
  const startedAt = performance.now();
  await expect(
    readWhenJourneyReplayed(
      {
        isClosed: () => false,
        ready: () => false,
        cursor: () => ({ height: 5_394_699, generation: 0 }),
        pending: async () => [
          { reason: "l1_node_behind", detail: "the node's tip is stale" },
        ],
      },
      async () => "unreachable",
      options,
    ),
  ).rejects.toThrow(/Journey replay follower is pending: .*l1_node_behind/u);
  expect(performance.now() - startedAt).toBeGreaterThanOrEqual(STALL_MS - 5);
});

it.each([
  [
    "a tip rollback under the read",
    () =>
      new FraudProofL1CheckpointChangedError(
        "the follower changed during ledger replay",
      ),
  ],
  [
    "a follower not yet able to answer",
    () =>
      new FraudProofL1UnavailableError(
        "header's authenticated admission is unavailable",
      ),
  ],
  [
    "a retained-DA peer that has not served the payload yet",
    () =>
      new RetainedDaPayloadUnavailableError(
        "ab".repeat(32),
        "no retained-DA source served the payload",
        "unreachable",
      ),
  ],
])("reads again after %s", async (_name, failure) => {
  let reads = 0;
  const ledger = await readWhenJourneyReplayed(
    readyFollower(),
    async () => {
      reads += 1;
      if (reads === 1) throw failure();
      return ["ledger"];
    },
    options,
  );
  expect(ledger).toEqual(["ledger"]);
  expect(reads).toBe(2);
});

it("fails at once on a read failure that is not a wait", async () => {
  let reads = 0;
  await expect(
    readWhenJourneyReplayed(
      readyFollower(),
      async () => {
        reads += 1;
        throw new Error("retained replay header differs from its admission");
      },
      options,
    ),
  ).rejects.toThrow("retained replay header differs");
  expect(reads).toBe(1);
});

it("fails a read still waiting once the stall allowance passes without L1 progress", async () => {
  await expect(
    readWhenJourneyReplayed(
      readyFollower(),
      async () => {
        throw new FraudProofL1UnavailableError("still unavailable");
      },
      options,
    ),
  ).rejects.toThrow("still unavailable");
});

it("fails a ready follower's read that keeps waiting within the stall allowance, though its cursor advances", async () => {
  const openedAt = performance.now();
  const liveBlocks = 24;
  const startedAt = performance.now();
  await expect(
    readWhenJourneyReplayed(
      {
        isClosed: () => false,
        ready: () => true,
        cursor: () => ({
          height:
            5_397_332 +
            Math.min(
              liveBlocks,
              Math.floor((performance.now() - openedAt) / (STALL_MS / 4)),
            ),
          generation: 0,
        }),
        pending: async () => [],
      },
      async () => {
        throw new RetainedDaPayloadUnavailableError(
          "cd".repeat(32),
          "no retained-DA source served the payload",
          "unreachable",
        );
      },
      options,
    ),
  ).rejects.toThrow("no retained-DA source served the payload");
  expect(performance.now() - startedAt).toBeLessThan(STALL_MS * 2);
});

it("gives a read a fresh allowance once its follower falls back to catching up and returns", async () => {
  const openedAt = performance.now();
  const elapsed = () => performance.now() - openedAt;
  const catchingUp = () =>
    elapsed() >= STALL_MS * 0.75 && elapsed() < STALL_MS * 1.5;
  const ledger = await readWhenJourneyReplayed(
    {
      isClosed: () => false,
      ready: () => !catchingUp(),
      cursor: () => ({
        height: 5_397_332 + Math.floor(elapsed() / (STALL_MS / 4)),
        generation: 0,
      }),
      pending: async () => [],
    },
    async () => {
      if (elapsed() < STALL_MS * 2)
        throw new FraudProofL1UnavailableError("not yet admitted");
      return ["ledger"];
    },
    options,
  );
  expect(ledger).toEqual(["ledger"]);
});
