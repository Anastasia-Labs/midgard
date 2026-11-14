import { describe, expect, it } from "vitest";

import { parseWatcherConfig } from "../../src/runtime/config.js";
import {
  rejected,
  validConfig,
} from "./config.explicit-local-devnet-configuration.js";

describe("watcher configuration: l1.origin", () => {
  it("admits an optional operator l1.origin override, absent by default", () => {
    expect("origin" in parseWatcherConfig(validConfig()).l1).toBe(false);
    const blockHash = "ab".repeat(32);
    const input = validConfig();
    Object.assign(input.l1, { origin: { slot: 0, blockHash } });
    const parsed = parseWatcherConfig(input);
    expect(parsed.l1.origin).toEqual({ slot: 0, blockHash });
    expect(Object.isFrozen(parsed.l1.origin)).toBe(true);
  });

  it("rejects a malformed l1.origin at its exact path", () => {
    const cases: ReadonlyArray<readonly [unknown, string, string]> = [
      [null, "invalid_value", "$.l1.origin"],
      [undefined, "invalid_value", "$.l1.origin"],
      ["12.ab", "invalid_value", "$.l1.origin"],
      [{ slot: 1 }, "missing_required_field", "$.l1.origin.blockHash"],
      [
        { slot: 1, blockHash: "ab".repeat(32), height: 2 },
        "unknown_field",
        "$.l1.origin",
      ],
      [
        { slot: -1, blockHash: "ab".repeat(32) },
        "out_of_bounds",
        "$.l1.origin.slot",
      ],
      [
        { slot: 1.5, blockHash: "ab".repeat(32) },
        "invalid_value",
        "$.l1.origin.slot",
      ],
      [
        { slot: "1", blockHash: "ab".repeat(32) },
        "invalid_value",
        "$.l1.origin.slot",
      ],
      [
        { slot: 1, blockHash: "AB".repeat(32) },
        "invalid_value",
        "$.l1.origin.blockHash",
      ],
      [
        { slot: 1, blockHash: "ab".repeat(31) },
        "invalid_value",
        "$.l1.origin.blockHash",
      ],
    ];
    for (const [origin, code, path] of cases) {
      const input = validConfig();
      Object.assign(input.l1, { origin });
      rejected(
        () => parseWatcherConfig(input),
        code as Parameters<typeof rejected>[1],
        path,
      );
    }
  });
});
