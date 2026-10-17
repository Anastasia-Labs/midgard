import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  COMMIT_ANCHOR_SOURCE,
  COMMIT_ANCHOR_UNAVAILABLE,
  publishCommitAnchorReadiness,
} from "../src/fibers/block-commitment.commit-anchor-readiness.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  FOLLOWER_GENERATION,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const makeGlobals = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

/** One commitment tick's readiness publish at commit-event depth `depth`. */
const publishAt = (globals: Globals, depth: number) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const config = yield* NodeConfig;
        yield* publishCommitAnchorReadiness.pipe(
          Effect.provideService(NodeConfig, {
            ...config,
            COMMIT_EVENT_DEPTH: depth,
          }),
          Effect.provideService(Globals, globals),
        );
      }),
    ),
  );

/**
 * The follower holds `heights` (slot = height); the driver applied the
 * view at the last one, unless `applied` is false.
 */
const follow = (heights: readonly number[], applied = true) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        let view = null;
        for (const height of heights)
          view = yield* writeFollowerTip(height, FOLLOWER_GENERATION, height);
        if (applied && view !== null) yield* openFollowerWriteGateAt(view);
      }),
    ),
  );

/** The commit anchor reason as /readyz lists it, if raised. */
const readyzReason = async (globals: Globals) =>
  (await Effect.runPromise(activeLivenessReasons(globals))).find(
    (reason) => reason.source === COMMIT_ANCHOR_SOURCE,
  )?.reason;

describe("commit anchor readiness", () => {
  it("names the hold on /readyz while the block d below the applied view is not held, and clears on its own when it is", async () => {
    const globals = await makeGlobals();
    // Applied view at height 101; the block 2 below it (height 99) is absent.
    await follow([100, 101]);
    await publishAt(globals, 2);
    expect(await readyzReason(globals)).toBe(COMMIT_ANCHOR_UNAVAILABLE);
    const raised = await Effect.runPromise(Ref.get(globals.LIVENESS_REASONS));
    expect(raised.get(COMMIT_ANCHOR_SOURCE)).toBe(COMMIT_ANCHOR_UNAVAILABLE);

    // The follower now holds height 99: the next tick clears, no operator step.
    await follow([99, 100, 101]);
    await publishAt(globals, 2);
    expect(await readyzReason(globals)).toBeUndefined();
  });

  it("clears at d = 0, where the anchor is the applied view itself", async () => {
    const globals = await makeGlobals();
    await follow([101]);
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBe(COMMIT_ANCHOR_UNAVAILABLE);
    await publishAt(globals, 0);
    expect(await readyzReason(globals)).toBeUndefined();
  });

  it("leaves the hold to the write gate's own reasons with no applied view or one a rewind removed", async () => {
    const globals = await makeGlobals();
    await follow([101]);
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBe(COMMIT_ANCHOR_UNAVAILABLE);
    // No applied view.
    await follow([101], false);
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBeUndefined();
    // The applied view at 101 left the chain.
    await follow([101]);
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBe(COMMIT_ANCHOR_UNAVAILABLE);
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM l1_blocks WHERE height = 101`;
          yield* writeFollowerTip(100, FOLLOWER_GENERATION + 1, 100);
        }),
      ),
    );
    await publishAt(globals, 1);
    expect(await readyzReason(globals)).toBeUndefined();
  });
});
