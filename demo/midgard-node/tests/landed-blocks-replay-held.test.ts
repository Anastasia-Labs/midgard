/**
 * The replay port's hold on a block whose event replay failed locally
 * (`replay_held`) counts every failure on the block, whatever its message:
 * a fault whose message varies (an address, a counter, a timestamp) still
 * names the hold `replay_stuck` after `LOCAL_FAULT_RUN_LIMIT` failures in a
 * row, and the block is still replayed on the backoff.
 */
import { LOCAL_FAULT_RUN_LIMIT } from "@al-ft/midgard-fault-proofs";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { ReplayOutcome } from "../src/landed-blocks/replay.js";
import { heldReplays } from "../src/landed-blocks/replay-held.js";

const HEADER_HASH = "ab".repeat(28);

describe("the held replay of a foreign block", () => {
  it("names the block stuck after the bound even when every failure's message differs", async () => {
    const clock = { ms: 0 };
    let replays = 0;
    const replay = heldReplays(
      (): Effect.Effect<ReplayOutcome> =>
        Effect.sync(() => {
          replays += 1;
          return {
            kind: "replay_held",
            detail: `event replay failed locally at offset ${replays.toString()}`,
          };
        }),
      () => clock.ms,
    );
    const kinds: string[] = [];
    for (let attempt = 0; attempt < LOCAL_FAULT_RUN_LIMIT + 1; attempt += 1) {
      clock.ms += 60_000;
      const outcome = await Effect.runPromise(
        replay({ headerHash: HEADER_HASH } as never),
      );
      kinds.push(outcome.kind);
    }
    expect(replays).toBe(LOCAL_FAULT_RUN_LIMIT + 1);
    expect(kinds).toEqual([
      ...Array.from({ length: LOCAL_FAULT_RUN_LIMIT - 1 }, () => "replay_held"),
      "replay_stuck",
      "replay_stuck",
    ]);
  });
});
