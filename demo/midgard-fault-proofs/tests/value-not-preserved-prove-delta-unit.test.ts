import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { midgardMpfTerminalBranchKeepsTwoChildren } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { proveDeltaUnit } from "../src/value-not-preserved/union-plan.prove-delta-unit.js";

// A contribution that brings a unit's delta to zero removes the unit from the
// delta map, and `union_update` then needs the terminal-Branch group opening
// (`terminal_branch_keeps_two_children`). Any other contribution sends none.
describe("value-not-preserved delta-unit proofs", () => {
  it("sends the group opening with every removal that needs one, and only then", async () => {
    const units = Array.from({ length: 256 }, (_, index) =>
      Buffer.concat([Buffer.alloc(27, 0x33), Buffer.from([index])]),
    );
    const trie = await Trie.fromList(
      units.map((key) => ({ key, value: Buffer.from([0x01]) })),
    );
    let opened = 0;
    for (const key of units) {
      const kept = await proveDeltaUnit(trie, key, false);
      expect(kept.opening).toBe("");
      const removed = await proveDeltaUnit(trie, key, true);
      expect(removed.proof).toEqual(kept.proof);
      const terminal = removed.proof.at(-1);
      if (terminal !== undefined && "Branch" in terminal) {
        expect(
          midgardMpfTerminalBranchKeepsTwoChildren(
            Buffer.from(terminal.Branch.neighbors, "hex"),
            Buffer.from(removed.opening, "hex"),
          ),
        ).toBe(true);
      } else {
        expect(removed.opening).toBe("");
      }
      if (removed.opening !== "") opened += 1;
      await trie.delete(key);
    }
    expect(opened).toBeGreaterThan(0);
  });
});
