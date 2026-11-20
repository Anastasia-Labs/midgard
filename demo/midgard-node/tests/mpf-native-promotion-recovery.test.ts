import { describe, expect, it, vi } from "vitest";

import {
  recoverNativeMpfAfterCommitWorkerFailure,
  recoverNativeMpfFromSubmittedJournal,
} from "../src/fibers/block-commitment.js";
import type {
  NativeMpfGenerationHandle,
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "../src/services/mpf-native-owner/index.js";

const baseRoot = "11".repeat(32);
const candidateRoot = "22".repeat(32);
const handle: NativeMpfGenerationHandle = {
  ownerEpoch: Buffer.alloc(16, 1),
  generationId: Buffer.alloc(16, 2),
  baseRoot,
};
const replay: PersistedNativeMpfReplay = {
  schema: 1,
  ownerBinarySha256: "33".repeat(32),
  baseRoot,
  candidateRoot,
  eventLog: Buffer.alloc(92),
  eventLogDigest: "44".repeat(32),
  eventRoots: Buffer.alloc(32),
  eventCount: 1,
};

describe("Architecture G post-submit promotion recovery", () => {
  it("replays the exact submitted journal after the owner epoch changes", async () => {
    const recover = vi.fn().mockResolvedValue(undefined);
    const owner = { recover } as unknown as NativeMpfOwnerService;
    await recoverNativeMpfFromSubmittedJournal({
      owner,
      handle,
      submitted: true,
      replay,
    });
    expect(recover).toHaveBeenCalledOnce();
    expect(recover).toHaveBeenCalledWith(replay);
  });

  it("fails closed for an unsubmitted or different-base journal", async () => {
    const recover = vi.fn().mockResolvedValue(undefined);
    const owner = { recover } as unknown as NativeMpfOwnerService;
    await expect(
      recoverNativeMpfFromSubmittedJournal({
        owner,
        handle,
        submitted: false,
        replay,
      }),
    ).rejects.toThrow(/journal mismatch/);
    await expect(
      recoverNativeMpfFromSubmittedJournal({
        owner,
        handle,
        submitted: true,
        replay: { ...replay, baseRoot: "55".repeat(32) },
      }),
    ).rejects.toThrow(/journal mismatch/);
    expect(recover).not.toHaveBeenCalled();
  });

  it("recovers in the live parent when the worker crashes after durable submission", async () => {
    const recover = vi.fn().mockResolvedValue(undefined);
    const owner = { recover } as unknown as NativeMpfOwnerService;

    await expect(
      recoverNativeMpfAfterCommitWorkerFailure({
        owner,
        submitted: true,
        replay,
      }),
    ).resolves.toBe(true);
    expect(recover).toHaveBeenCalledOnce();
    expect(recover).toHaveBeenCalledWith(replay);

    recover.mockClear();
    await expect(
      recoverNativeMpfAfterCommitWorkerFailure({
        owner,
        submitted: false,
        replay,
      }),
    ).resolves.toBe(false);
    expect(recover).not.toHaveBeenCalled();

    await expect(
      recoverNativeMpfAfterCommitWorkerFailure({
        owner,
        submitted: true,
        replay: undefined,
      }),
    ).rejects.toThrow(/missing native replay data/);
    expect(recover).not.toHaveBeenCalled();
  });
});
