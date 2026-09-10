import { describe, expect, it } from "vitest";

import { startWatcherNativeChainSync } from "../../src/l1/native-chain-sync.js";
import { parseWatcherConfig } from "../../src/runtime/config.js";
import { readWatcherNativeRecoveryBoundary } from "../../src/runtime/watcher-runtime.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Exercise the startup guard with authority acquired from the native helper
// protocol. This is not a deployed-node acceptance claim.
const openBoundary = async (depth = 30, grow = false) => {
  const fixture = await createSyntheticUserEventOriginFixture({
    nativeTipMode: "controlled",
  });
  try {
    const point = fixture.emptySuccessorBlock.point;
    const tip = {
      blockHash: "ed".repeat(32),
      blockNo: (BigInt(point.blockNo) + BigInt(depth)).toString(),
      slot: (BigInt(point.slot) + 600n).toString(),
    };
    await fixture.setNativeTip(tip);
    const recovery = {
      replayIntersection: { ...point, chainPointId: "ab".repeat(32) },
      catchupBoundary: {
        ...point,
        chainPointId: "ab".repeat(32),
        finalityDepth: "30",
        ogmiosTipBlockNo: tip.blockNo,
      },
    };
    if (grow) await fixture.growNativeTip();
    const native = await startWatcherNativeChainSync({
      binaryPath: fixture.nativeChainSyncBinaryPath,
      watcherConfig: parseWatcherConfig(fixture.watcherConfig),
      intersection: {
        kind: "point",
        blockHash: point.blockHash,
        slot: point.slot,
      },
      startupTimeoutMs: 10_000,
      onEvent: async () => undefined,
    });
    return {
      recovery,
      native,
      close: async () => {
        await native.close();
        await fixture.close();
      },
    };
  } catch (error) {
    await fixture.close();
    throw error;
  }
};

describe("watcher native recovery startup boundary", () => {
  it("admits actual native tip growth after the recovery snapshot", async () => {
    const owner = await openBoundary(30, true);
    try {
      const admitted = readWatcherNativeRecoveryBoundary({
        nativeAuthority: owner.native.authority,
        recovery: owner.recovery,
      });
      expect(BigInt(admitted.currentTip.blockNo)).toBe(
        BigInt(owner.recovery.catchupBoundary.ogmiosTipBlockNo) + 1n,
      );
      expect(admitted.selectedIntersection).toEqual({
        kind: "point",
        blockHash: owner.recovery.replayIntersection.blockHash,
        slot: owner.recovery.replayIntersection.slot,
      });
    } finally {
      await owner.close();
    }
  });

  it.each([2_160, 2_161])(
    "enforces the recovery cap at depth %i",
    async (depth) => {
      const owner = await openBoundary(depth);
      try {
        const admit = () =>
          readWatcherNativeRecoveryBoundary({
            nativeAuthority: owner.native.authority,
            recovery: owner.recovery,
          });
        if (depth === 2_160) expect(admit).not.toThrow();
        else expect(admit).toThrow("admitted state-queue recovery bound");
      } finally {
        await owner.close();
      }
    },
  );

  it("refuses a native tip older than the admitted Ogmios snapshot", async () => {
    const owner = await openBoundary();
    try {
      expect(() =>
        readWatcherNativeRecoveryBoundary({
          nativeAuthority: owner.native.authority,
          recovery: {
            ...owner.recovery,
            catchupBoundary: {
              ...owner.recovery.catchupBoundary,
              ogmiosTipBlockNo: (
                BigInt(owner.recovery.catchupBoundary.ogmiosTipBlockNo) + 1n
              ).toString(),
            },
          },
        }),
      ).toThrow("admitted state-queue recovery bound");
    } finally {
      await owner.close();
    }
  });

  it.each(["blockHash", "slot"] as const)(
    "refuses intersection %s substitution",
    async (field) => {
      const owner = await openBoundary();
      try {
        expect(() =>
          readWatcherNativeRecoveryBoundary({
            nativeAuthority: owner.native.authority,
            recovery: {
              ...owner.recovery,
              replayIntersection: {
                ...owner.recovery.replayIntersection,
                [field]: field === "blockHash" ? "fe".repeat(32) : "1",
              },
            },
          }),
        ).toThrow("outside state-queue restore authority");
      } finally {
        await owner.close();
      }
    },
  );

  it("refuses an expired native authority", async () => {
    const owner = await openBoundary();
    try {
      await owner.native.close();
      expect(() =>
        readWatcherNativeRecoveryBoundary({
          nativeAuthority: owner.native.authority,
          recovery: owner.recovery,
        }),
      ).toThrow("authority expired");
    } finally {
      await owner.close();
    }
  });
});
