import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { stepDownCommitEventWindowToDaFrame } from "../src/workers/utils/commit-block-planner.event-frame.js";
export const registerEventPrefixCases = () => {
  describe("DA step-down across complete event instants", () => {
    const groups = [10, 20, 30].map((inclusionTimeMs) => ({
      inclusionTimeMs,
      deposits: 1,
      forcedTransactions: 0,
      withdrawals: 0,
    }));
    const initialWindow = {
      mode: "normal" as const,
      endTimeMs: 100,
      counts: { deposits: 3, forcedTransactions: 0, withdrawals: 0 },
      includedInstantCount: 3,
    };
    const run = async (sizes: ReadonlyMap<number, number>, drift = false) => {
      const builds: number[] = [];
      let rebases = 0;
      const selected = await Effect.runPromise(
        stepDownCommitEventWindowToDaFrame({
          initialWindow,
          groups,
          maxInnerBytes: 50,
          run: (window) =>
            Effect.sync(() => {
              builds.push(window.endTimeMs);
              return {
                bytes: sizes.get(window.endTimeMs)!,
                digest: `${window.endTimeMs}:${drift && builds.filter((time) => time === window.endTimeMs).length > 1 ? "changed" : "stable"}`,
              };
            }),
          measure: (built) => ({
            innerBytesUpperBound: built.bytes,
            materialDigest: built.digest,
          }),
          rebase: Effect.sync(() => {
            rebases += 1;
          }),
        }),
      );
      return { ...selected, builds, rebases };
    };
    it("drops normal transactions before lowering coverage to a complete fitting instant", async () => {
      expect(
        await run(
          new Map([
            [100, 90],
            [30, 80],
            [20, 40],
            [10, 30],
          ]),
        ),
      ).toMatchObject({
        window: { mode: "catch_up", endTimeMs: 20, includedInstantCount: 2 },
        builds: [100, 30, 20],
        rebases: 2,
      });
    });
    it("searches nonmonotonic withdrawal effects and preserves the first largest fitting prefix", async () => {
      expect(
        await run(
          new Map([
            [100, 90],
            [30, 80],
            [20, 70],
            [10, 40],
          ]),
        ),
      ).toMatchObject({
        window: { endTimeMs: 10 },
        builds: [100, 30, 20, 10],
        rebases: 3,
      });
    });
    it("rebuilds the smallest measured candidate when every conservative estimate exceeds the frame", async () => {
      expect(
        await run(
          new Map([
            [100, 90],
            [30, 60],
            [20, 80],
            [10, 70],
          ]),
        ),
      ).toMatchObject({
        window: { endTimeMs: 30 },
        builds: [100, 30, 20, 10, 30],
        rebases: 4,
      });
    });
    it("refuses changed rebuilt material before signing", async () => {
      await expect(
        run(
          new Map([
            [100, 90],
            [30, 60],
            [20, 80],
            [10, 70],
          ]),
          true,
        ),
      ).rejects.toThrow("event prefix changed while rebuilding");
    });
  });
};
