import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  COMMIT_HORIZON_LAG_SOURCE,
  COMMIT_HORIZON_LAG_UNAVAILABLE,
  publishCommitHorizonLagReadiness,
} from "../src/fibers/block-commitment.commit-horizon-lag-readiness.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  FOLLOWER_GENERATION,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const makeGlobals = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

/** One commitment tick's readiness publish at horizon lag `lagBlocks`. */
const publishAt = (globals: Globals, lagBlocks: number) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const config = yield* NodeConfig;
        yield* publishCommitHorizonLagReadiness.pipe(
          Effect.provideService(NodeConfig, {
            ...config,
            HISTORY_COMMIT_HORIZON_LAG_BLOCKS: lagBlocks,
          }),
          Effect.provideService(Globals, globals),
        );
      }),
    ),
  );

const follow = (...heights: readonly number[]) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        // Each write moves the cursor; the last height is the covered tip.
        for (const height of heights)
          yield* writeFollowerTip(height, FOLLOWER_GENERATION, height);
      }),
    ),
  );

/** The commit horizon lag reason as /readyz lists it, if raised. */
const readyzReason = async (globals: Globals) =>
  (await Effect.runPromise(activeLivenessReasons(globals))).find(
    (reason) => reason.source === COMMIT_HORIZON_LAG_SOURCE,
  )?.reason;

describe("commit horizon lag readiness", () => {
  it("names the hold on /readyz while the follower has no cursor, and clears once the lagged block is there", async () => {
    const globals = await makeGlobals();
    await follow();
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBe(COMMIT_HORIZON_LAG_UNAVAILABLE);

    await follow(100, 101);
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBeUndefined();
  });

  it("names the hold while the lagged block is not held (catching up or pruned), and clears on its own when it is", async () => {
    const globals = await makeGlobals();
    // Covered tip at height 101; the block 2 below it (height 99) is absent.
    await follow(100, 101);
    await publishAt(globals, 2);
    expect(await readyzReason(globals)).toBe(COMMIT_HORIZON_LAG_UNAVAILABLE);
    const raised = await Effect.runPromise(Ref.get(globals.LIVENESS_REASONS));
    expect(raised.get(COMMIT_HORIZON_LAG_SOURCE)).toBe(
      COMMIT_HORIZON_LAG_UNAVAILABLE,
    );

    // The follower now holds height 99: the next tick clears, no operator step.
    await follow(99, 100, 101);
    await publishAt(globals, 2);
    expect(await readyzReason(globals)).toBeUndefined();
  });

  it("clears at no lag, whatever the follower holds", async () => {
    const globals = await makeGlobals();
    await follow();
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBe(COMMIT_HORIZON_LAG_UNAVAILABLE);
    await publishAt(globals, 0);
    expect(await readyzReason(globals)).toBeUndefined();
  });
});
