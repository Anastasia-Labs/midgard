/**
 * The consensus-maximum accepted L2 transaction (kind 0): 819 inputs of 40
 * bytes and 761 minimal outputs of 43 bytes, so neither field takes one more
 * item under the 32,768-byte field bound. Its fault proof runs from Init
 * through conviction and removal in one thread of 1,608 steps, and the honest
 * operator's root at the same shape is refused at the terminal comparison.
 * The emulator deployment profiles size block maturity for this thread
 * (demo/scripts/deployment-profile-budgets.mjs), so its shape is pinned here.
 */
import {
  encodeMidgardSpendInputItem,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import { TRANSITION_OUTPUTS_GRAMMAR_BATCH } from "../src/transition-trace/phases.js";
import { transitionTraceFinalCaseRunner } from "./support/transition-trace-final-cases.js";

const MAXIMUM = { assetCount: 0, spentCount: 819, outputCount: 761 } as const;
const TIMEOUT = 1_800_000;
const run = transitionTraceFinalCaseRunner();

const input = (outputIndex: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, 0xed), outputIndex });
// The runner's output at zero assets: an enterprise address and lovelace.
const output = encodeMidgardTxOutput({
  address: Buffer.from(`60${"aa".repeat(28)}`, "hex"),
  value: { lovelace: 2_000_000n, assets: new Map() },
});
type ProfileBudgets = Readonly<{
  MAXIMUM_TRANSITION_TRACE_SHAPE: Readonly<Record<string, number>>;
  maximumTransitionTraceThreadSteps: () => number;
}>;
const budgets = (await import(
  new URL("../../scripts/deployment-profile-budgets.mjs", import.meta.url).href
)) as ProfileBudgets;
const fieldBytes = (count: number, item: (index: number) => Buffer) =>
  encodeMidgardFieldPreimage(Array.from({ length: count }, (_, i) => item(i)))
    .length;

describe("consensus-maximum accepted transaction", () => {
  it("fills both replayed fields to the field bound", () => {
    expect(input(0)).toHaveLength(38);
    expect(output).toHaveLength(41);
    expect(fieldBytes(MAXIMUM.spentCount, input)).toBe(32_763);
    expect(fieldBytes(MAXIMUM.outputCount, () => output)).toBe(32_726);
    expect(fieldBytes(MAXIMUM.spentCount + 1, input)).toBeGreaterThan(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(fieldBytes(MAXIMUM.outputCount + 1, () => output)).toBeGreaterThan(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
  });

  it("is the thread the deployment profiles size block maturity for", () => {
    expect(budgets.MAXIMUM_TRANSITION_TRACE_SHAPE).toEqual({
      spendInputs: MAXIMUM.spentCount,
      outputs: MAXIMUM.outputCount,
      outputsCertifiedPerStep: TRANSITION_OUTPUTS_GRAMMAR_BATCH,
    });
    // Route, binding and open; a delete per input; a certification per batch
    // of outputs; a step per output; the terminal comparison.
    expect(budgets.maximumTransitionTraceThreadSteps()).toBe(
      3 +
        MAXIMUM.spentCount +
        Math.ceil(MAXIMUM.outputCount / TRANSITION_OUTPUTS_GRAMMAR_BATCH) +
        MAXIMUM.outputCount +
        1,
    );
  });

  it(
    "convicts and removes a wrong transition root at the maximum shape",
    () => run(MAXIMUM),
    TIMEOUT,
  );

  it(
    "refuses the honest operator's root at the terminal comparison at the maximum shape",
    () => run({ ...MAXIMUM, honest: true }),
    TIMEOUT,
  );
});
