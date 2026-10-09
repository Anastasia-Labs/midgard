import "./utils.js";

import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { blockCommitmentAction } from "../src/fibers/block-commitment.block-commitment-action.js";
import {
  COMMIT_ANCHOR_SOURCE,
  COMMIT_ANCHOR_UNAVAILABLE,
} from "../src/fibers/block-commitment.commit-anchor-readiness.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import {
  FOLLOWER_GENERATION,
  writeFollowerTip,
} from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/** One commitment tick that does nothing past its readiness publication: a
 * reset in progress skips the rest of the tick. */
const tickReasons = (depth: number) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        // The driver applied a view at a lone follower block: there is no
        // block d > 0 below it.
        const view = yield* writeFollowerTip(100, FOLLOWER_GENERATION, 100);
        yield* openFollowerWriteGateAt(view);
        const globals = yield* Globals;
        yield* Ref.set(globals.RESET_IN_PROGRESS, true);
        yield* blockCommitmentAction;
        return yield* activeLivenessReasons(globals);
      }).pipe(
        Effect.provideService(NodeConfig, {
          COMMIT_EVENT_DEPTH: depth,
        } as unknown as NodeConfig["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<unknown[], unknown, never>,
  );

const anchorHold = expect.objectContaining({
  source: COMMIT_ANCHOR_SOURCE,
  reason: COMMIT_ANCHOR_UNAVAILABLE,
});

describe("the commitment tick publishes the commit anchor hold", () => {
  it("names the hold on /readyz while the follower has no block d below the applied view", async () => {
    expect(await tickReasons(1)).toContainEqual(anchorHold);
  });

  it("raises no hold at d = 0", async () => {
    expect(await tickReasons(0)).not.toContainEqual(anchorHold);
  });
});
