import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import { transitionOutputIsWhole } from "../src/transition-trace/output-phases.js";
import { TRANSITION_OUTPUTS_GRAMMAR_BATCH } from "../src/transition-trace/phases.js";

const yields = (suffix: string) =>
  readFileSync(
    new URL(
      `../../../onchain/aiken/validators/fraud-proofs/transition-trace/accepted-transaction-yields${suffix}`,
      import.meta.url,
    ),
    "utf8",
  );

const aikenConstant = (name: string): number => {
  const match = new RegExp(`^pub const ${name} = (\\d+)$`, "mu").exec(
    yields(".ak"),
  );
  if (match?.[1] === undefined) throw new Error(`${name} is not declared`);
  return Number(match[1]);
};

describe("accepted-transaction output step", () => {
  it("mirrors the yields' compiled constants", () => {
    expect(TRANSITION_OUTPUTS_GRAMMAR_BATCH).toBe(
      aikenConstant("outputs_grammar_batch"),
    );
  });

  // The Aiken tests of `output_step_is_whole` are the vectors: each names its
  // expected path, so both sides decide the same bytes the same way.
  it("decides every Aiken vector as `output_step_is_whole` does", () => {
    const vectors = [
      ...yields(".test.ak").matchAll(
        /^test whole_step_(replays|stages)_\w+\(\) \{\n {2}let item =\s*#"([0-9a-f]+)"/gmu,
      ),
    ].map(([, path, item]) => ({ item: item!, whole: path === "replays" }));
    expect(vectors.length).toBe(
      yields(".test.ak").match(/^test whole_step_/gmu)?.length,
    );
    expect(vectors.filter(({ whole }) => whole).length).toBeGreaterThan(0);
    expect(vectors.filter(({ whole }) => !whole).length).toBeGreaterThan(0);
    for (const { item, whole } of vectors)
      expect({ item, whole: transitionOutputIsWhole(item) }).toEqual({
        item,
        whole,
      });
  });

  // A whole enterprise output, then the same bytes with an odd digit, a
  // non-hex digit, or nothing: the yield reads bytes, so the mirror refuses
  // any text that is not whole bytes rather than deciding a truncation.
  it("refuses text that is not whole hex bytes", () => {
    const item = `a200581d60${"aa".repeat(28)}018200a0`;
    expect(transitionOutputIsWhole(item)).toBe(true);
    for (const text of [`${item}0`, `${item.slice(0, -2)}g0`, ""])
      expect(() => transitionOutputIsWhole(text)).toThrow(
        "Transition output is not hex",
      );
  });
});
