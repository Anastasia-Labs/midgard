/**
 * The accepted L2 transaction (kind 0) whose fault proof takes the most steps:
 * 819 spend inputs of 40-byte items and 780 outputs of 42-byte items, so
 * neither field takes one more item under the 32,768-byte field bound. Each
 * output is the smallest one the phase-11 step stages: 40 bytes, an
 * enterprise address, lovelace alone and a one-byte inline datum, replayed in
 * 7 steps. Its fault proof runs from Init through conviction and removal in
 * one thread of 6,308 steps, and the honest operator's root at the same shape
 * is refused at the terminal comparison. The emulator deployment profiles
 * size block maturity for this thread
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
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  nextTransitionDatumPhase,
  nextTransitionOutputPhase,
  transitionOutputIsWhole,
} from "../src/transition-trace/output-phases.js";
import { TRANSITION_OUTPUTS_GRAMMAR_BATCH } from "../src/transition-trace/phases.js";
import { transitionTraceFinalCaseRunner } from "./support/transition-trace-final-cases.js";

const TIMEOUT = 1_800_000;
const run = transitionTraceFinalCaseRunner();

const input = (outputIndex: number) =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, 0xed), outputIndex });
const output = (
  extra: Partial<Parameters<typeof encodeMidgardTxOutput>[0]> = {},
) =>
  encodeMidgardTxOutput({
    address: Buffer.from(`60${"aa".repeat(28)}`, "hex"),
    value: { lovelace: 0n, assets: new Map() },
    ...extra,
  });
const smallestStaged = output({
  datum: { kind: "inline", cbor: Buffer.from("00", "hex") },
});
const MAXIMUM = {
  assetCount: 0,
  spentCount: 819,
  outputCount: 780,
  outputCbor: smallestStaged.toString("hex"),
} as const;
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

const staged: SDK.TransitionTraceFinalState = {
  kind: 0n,
  phase: 2n,
  proof_commitment: { hash: "ff".repeat(32) },
  tx_id: "",
  inputs_walk: "",
  outputs_walk: "",
  output_count: 1n,
  output_hash: "",
  output_index: 0n,
  current_root: "",
  summaries: { summaries: [] },
  scan_cbor: "",
  value_cbor: "",
  value_start: 0n,
  value_end: 0n,
  value_summary: null,
  descriptor_cbor: "",
  deposit_index: 0n,
  deposit_source_cbor: "",
  deposit_asset_count: 0n,
  datum_walk: null,
};
const summary = { root: "01".repeat(32), cbor_length: 1n, memory: 4n };
/**
 * The steps an output the phase-11 step stages takes, counted by the
 * off-chain stepper: phase 11, the scan, value and datum steps up to the
 * assembly, the assembly and the insert.
 */
const stagedSteps = (bytes: Buffer) => {
  const hex = bytes.toString("hex");
  let state = staged;
  let steps = 3;
  while (state.phase !== 3n) {
    state =
      state.phase === 8n
        ? nextTransitionDatumPhase(state, hex, [summary, summary, summary])
            .state
        : nextTransitionOutputPhase(state, hex).state;
    steps += 1;
  }
  return steps;
};

describe("the longest accepted-transaction fault proof", () => {
  it("fills both replayed fields to the field bound", () => {
    expect(input(0)).toHaveLength(38);
    expect(smallestStaged).toHaveLength(40);
    expect(fieldBytes(MAXIMUM.spentCount, input)).toBe(32_763);
    expect(fieldBytes(MAXIMUM.outputCount, () => smallestStaged)).toBe(32_763);
    expect(fieldBytes(MAXIMUM.spentCount + 1, input)).toBeGreaterThan(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(
      fieldBytes(MAXIMUM.outputCount + 1, () => smallestStaged),
    ).toBeGreaterThan(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
  });

  it("stages the smallest output with the most steps per field byte", () => {
    const ada = output();
    const script = output({
      script_ref: { language: "PlutusV3", scriptBytes: Buffer.alloc(0) },
    });
    const asset = output({
      value: {
        lovelace: 0n,
        assets: new Map([["44".repeat(28), new Map([["", 1n]])]]),
      },
      datum: { kind: "inline", cbor: Buffer.from("00", "hex") },
    });
    expect(ada).toHaveLength(37);
    expect(transitionOutputIsWhole(ada.toString("hex"))).toBe(true);
    expect(transitionOutputIsWhole(smallestStaged.toString("hex"))).toBe(false);
    expect(stagedSteps(smallestStaged)).toBe(7);
    // Field items carry a 2-byte head at these lengths.
    for (const other of [script, asset]) {
      expect(transitionOutputIsWhole(other.toString("hex"))).toBe(false);
      expect(stagedSteps(other) / (other.length + 2)).toBeLessThan(7 / 42);
    }
  });

  it("is the thread the deployment profiles size block maturity for", () => {
    expect(budgets.MAXIMUM_TRANSITION_TRACE_SHAPE).toEqual({
      spendInputs: MAXIMUM.spentCount,
      outputs: MAXIMUM.outputCount,
      outputsCertifiedPerStep: TRANSITION_OUTPUTS_GRAMMAR_BATCH,
      stepsPerOutput: stagedSteps(smallestStaged),
    });
    // Route, binding and open; a delete per input; a certification per batch
    // of outputs; each output's steps; the terminal comparison.
    expect(budgets.maximumTransitionTraceThreadSteps()).toBe(
      3 +
        MAXIMUM.spentCount +
        Math.ceil(MAXIMUM.outputCount / TRANSITION_OUTPUTS_GRAMMAR_BATCH) +
        MAXIMUM.outputCount * stagedSteps(smallestStaged) +
        1,
    );
    expect(budgets.maximumTransitionTraceThreadSteps()).toBe(6_308);
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
