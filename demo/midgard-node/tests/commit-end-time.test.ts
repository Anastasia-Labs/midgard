import { describe, expect, it } from "vitest";
import {
  Emulator,
  Lucid,
  generateEmulatorAccount,
} from "@lucid-evolution/lucid";
import {
  resolveAlignedCommitEndTime,
} from "@/workers/utils/commit-end-time.js";

const makeLucid = async () => {
  const operator = generateEmulatorAccount({ lovelace: 50_000_000n });
  const emulator = new Emulator([operator]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  return lucid;
};

describe("commit end-time resolver", () => {
  it("forces end-time to advance when candidate is stale", async () => {
    const lucid = await makeLucid();
    const provider = lucid.config().provider as { time: number; slot: number };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;

    const { alignedCandidateEndTime, minimumMonotonicEndTime, resolvedEndTime } =
      resolveAlignedCommitEndTime({
        lucid,
        latestEndTime,
        candidateEndTime: latestEndTime - 5_000,
      });

    expect(alignedCandidateEndTime).toBeLessThanOrEqual(latestEndTime);
    expect(minimumMonotonicEndTime).toBeGreaterThan(latestEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(minimumMonotonicEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(alignedCandidateEndTime);
  });

  it("keeps a forward candidate end-time when already monotonic", async () => {
    const lucid = await makeLucid();
    const provider = lucid.config().provider as { time: number; slot: number };
    const zeroTime = provider.time - provider.slot * 1000;
    const latestEndTime = zeroTime + provider.slot * 1000;
    const candidateEndTime = latestEndTime + 2_500;

    const { alignedCandidateEndTime, minimumMonotonicEndTime, resolvedEndTime } =
      resolveAlignedCommitEndTime({
        lucid,
        latestEndTime,
        candidateEndTime,
      });

    expect(alignedCandidateEndTime).toBe(latestEndTime + 2_000);
    expect(minimumMonotonicEndTime).toBeGreaterThan(latestEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(alignedCandidateEndTime);
    expect(resolvedEndTime).toBeGreaterThanOrEqual(minimumMonotonicEndTime);
  });
});
