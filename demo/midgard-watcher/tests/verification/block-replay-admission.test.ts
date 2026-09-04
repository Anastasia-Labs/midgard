import { describe, expect, it } from "vitest";

import {
  assertWatcherFullBlockReplayResult,
  evaluateWatcherBlockReplay,
} from "../../src/verification/block-replay.js";

describe("watcher full block-replay admission V1", () => {
  it("admits only results minted by the full W21-W24-bound entry point", async () => {
    // Deliberately malformed at the authenticated observation boundary. The
    // full entry point still mints the fail-closed result; this keeps the
    // authority test bounded without constructing a complete ledger fixture.
    const result = await evaluateWatcherBlockReplay({} as never);

    expect(result).toMatchObject({
      action: "error",
      reasonCodes: ["canonical_reconstruction_failed"],
    });
    expect(() => assertWatcherFullBlockReplayResult(result)).not.toThrow();
    expect(() => assertWatcherFullBlockReplayResult({ ...result })).toThrow(
      "watcher full block-replay result is not admitted",
    );
  });
});
