import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { blockCommitmentAction } from "../src/fibers/block-commitment.block-commitment-action.js";
import {
  COMMIT_HORIZON_LAG_SOURCE,
  COMMIT_HORIZON_LAG_UNAVAILABLE,
} from "../src/fibers/block-commitment.commit-horizon-lag-readiness.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { provideDatabaseLayers } from "./utils.js";

/** One commitment tick that does nothing past its readiness publication: a
 * reset in progress skips the rest of the tick. */
const tickReasons = (lagBlocks: number) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // No follower cursor: the block lagBlocks below the covered tip is
        // unknown.
        yield* sql`DELETE FROM l1_follower_cursor`;
        const globals = yield* Globals;
        yield* Ref.set(globals.RESET_IN_PROGRESS, true);
        yield* blockCommitmentAction;
        return yield* activeLivenessReasons(globals);
      }).pipe(
        Effect.provideService(NodeConfig, {
          HISTORY_COMMIT_HORIZON_LAG_BLOCKS: lagBlocks,
        } as unknown as NodeConfig["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<unknown[], unknown, never>,
  );

const lagHold = expect.objectContaining({
  source: COMMIT_HORIZON_LAG_SOURCE,
  reason: COMMIT_HORIZON_LAG_UNAVAILABLE,
});

describe("the commitment tick publishes the horizon lag hold", () => {
  it("names the hold on /readyz while the follower lacks the lagged block", async () => {
    expect(await tickReasons(1)).toContainEqual(lagHold);
  });

  it("raises no hold at no lag", async () => {
    expect(await tickReasons(0)).not.toContainEqual(lagHold);
  });
});
