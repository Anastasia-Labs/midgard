import { describe, expect, it } from "vitest";

import {
  assertWatcherFullBlockReplayResultV1,
  evaluateWatcherBlockReplayV1,
} from "../../src/verification/block-replay.js";

describe("watcher full block-replay admission V1", () => {
  it("admits only results minted by the full W21-W24-bound entry point", async () => {
    // Deliberately malformed at the authenticated observation boundary. The
    // full entry point still mints the fail-closed result; this keeps the
    // authority test bounded without constructing a complete ledger fixture.
    const result = await evaluateWatcherBlockReplayV1({} as never);

    expect(result).toMatchObject({
      action: "error",
      reasonCodes: ["canonical_reconstruction_failed"],
    });
    expect(() => assertWatcherFullBlockReplayResultV1(result)).not.toThrow();
    expect(() => assertWatcherFullBlockReplayResultV1({ ...result })).toThrow(
      "watcher full block-replay result is not admitted",
    );
  });
});
