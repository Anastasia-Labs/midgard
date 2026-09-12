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
    const admittedIntersections = Object.freeze([
      Object.freeze({
        blockHash: point.blockHash,
        blockNo: point.blockNo,
        slot: point.slot,
      }),
    ]);
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
      admittedIntersections,
      tip,
      point,
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
        admittedIntersections: owner.admittedIntersections,
      });
      expect(BigInt(admitted.currentTip.blockNo)).toBe(
        BigInt(owner.tip.blockNo) + 1n,
      );
      expect(admitted.selectedIntersection).toEqual({
        kind: "point",
        blockHash: owner.point.blockHash,
        slot: owner.point.slot,
      });
      expect(admitted.selectedBlockNo).toBe(owner.point.blockNo);
    } finally {
      await owner.close();
    }
  });

  // Replay depth is unbounded: a watcher resuming after a long L1 gap
  // replays every block it missed rather than failing closed at k.
  it.each([2_160, 2_161])(
    "admits a resume point %i blocks behind the native tip",
    async (depth) => {
      const owner = await openBoundary(depth);
      try {
        expect(() =>
          readWatcherNativeRecoveryBoundary({
            nativeAuthority: owner.native.authority,
            admittedIntersections: owner.admittedIntersections,
          }),
        ).not.toThrow();
      } finally {
        await owner.close();
      }
    },
  );

  it("refuses a native tip behind the selected resume point", async () => {
    const owner = await openBoundary();
    try {
      expect(() =>
        readWatcherNativeRecoveryBoundary({
          nativeAuthority: owner.native.authority,
          admittedIntersections: [
            {
              ...owner.admittedIntersections[0]!,
              blockNo: (BigInt(owner.tip.blockNo) + 1n).toString(),
            },
          ],
        }),
      ).toThrow("behind the selected resume point");
    } finally {
      await owner.close();
    }
  });

  it.each(["blockHash", "slot"] as const)(
    "refuses a selection outside the recorded history (%s substitution)",
    async (field) => {
      const owner = await openBoundary();
      try {
        expect(() =>
          readWatcherNativeRecoveryBoundary({
            nativeAuthority: owner.native.authority,
            admittedIntersections: [
              {
                ...owner.admittedIntersections[0]!,
                [field]: field === "blockHash" ? "fe".repeat(32) : "1",
              },
            ],
          }),
        ).toThrow("outside the recorded resume history");
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
          admittedIntersections: owner.admittedIntersections,
        }),
      ).toThrow("authority expired");
    } finally {
      await owner.close();
    }
  });
});
