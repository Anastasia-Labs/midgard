import { describe, expect, it } from "vitest";

import { type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  ledgerFromWitness,
  type LedgerMutation,
  recordWitness,
} from "../src/ledger-store/index.js";
import {
  ledgerWitnessJson,
  parseLedgerWitnessJson,
} from "../src/ledger-store/witness-json.js";
import { normalizeWorkflowJson } from "../src/workflow/identity.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";
import {
  event,
  testLedgerEntries,
  testOutRef,
} from "./support/ledger-store-conformance.js";

type Step =
  | { readonly apply: LedgerMutation[] }
  | { readonly read: readonly Buffer[] };

/** Runs the steps; returns every root and every read output and proof. */
const run = async (ledger: Ledger, steps: readonly Step[]) => {
  const seen: string[] = [];
  let current = ledger;
  for (const step of steps) {
    if ("apply" in step) {
      current = await current.apply(step.apply);
      seen.push(current.root);
      continue;
    }
    for (const [outRef, read] of await current.read(step.read))
      seen.push(
        outRef,
        read.output?.toString("hex") ?? "absent",
        (await read.proof()).toString("hex"),
      );
  }
  return seen;
};

const recorded = async (
  entries: readonly LedgerEntry[],
  steps: readonly Step[],
) => {
  const store = createLedgerStore(createMemoryLedgerStorage());
  const admitted = await store.admit(entries, await oracleLedgerRoot(entries));
  const ledger = await store.at(admitted.root);
  const recorder = recordWitness(ledger);
  const expected = await run(recorder.ledger, steps);
  return { root: ledger.root, witness: recorder.witness(), expected };
};

/** What an artifact stores: the normalized value, written and read back. */
const stored = (value: unknown) =>
  JSON.parse(JSON.stringify(normalizeWorkflowJson(value))) as unknown;

// Reads and proofs of present and absent keys, then a delete that collapses
// the two-leaf root branch onto the remaining leaf, then a read after it.
const COLLAPSE: readonly Step[] = [
  { read: [testOutRef(0), testOutRef(700)] },
  { apply: event([0], []) },
  { read: [testOutRef(1)] },
];
// A block of events with inserts and deletes over a larger ledger.
const BLOCK: readonly Step[] = [
  { read: [testOutRef(3), testOutRef(701)] },
  { apply: event([0, 1, 2], [100, 101]) },
  { apply: event([100, 3], [102, 103]) },
  { read: [testOutRef(103), testOutRef(5), testOutRef(2)] },
];

describe("a ledger witness's artifact form", () => {
  it.each([
    ["a collapsing delete", testLedgerEntries(0, 2), COLLAPSE],
    ["a block over a larger ledger", testLedgerEntries(0, 60), BLOCK],
  ])(
    "round-trips exactly and replays the same after %s",
    async (_, entries, steps) => {
      const { root, witness, expected } = await recorded(entries, steps);
      const json = stored(ledgerWitnessJson(witness));
      const parsed = parseLedgerWitnessJson(json, "test witness");
      expect(parsed).toEqual(witness);
      // Field order is not part of the shape.
      const reversed = JSON.parse(JSON.stringify(json), (_, value: unknown) =>
        typeof value === "object" && value !== null && !Array.isArray(value)
          ? Object.fromEntries(Object.entries(value).reverse())
          : value,
      ) as unknown;
      expect(parseLedgerWitnessJson(reversed, "test witness")).toEqual(witness);
      expect(await run(ledgerFromWitness(root, parsed), steps)).toEqual(
        expected,
      );
      // The same recorded reads write the same JSON.
      const again = await recorded(entries, steps);
      expect(JSON.stringify(stored(ledgerWitnessJson(again.witness)))).toBe(
        JSON.stringify(json),
      );
    },
  );

  it("refuses a value of another shape with a plain error", async () => {
    const { witness } = await recorded(testLedgerEntries(0, 2), COLLAPSE);
    const json = stored(ledgerWitnessJson(witness)) as {
      nodes: Record<string, unknown>[];
      outputs: Record<string, unknown>[];
    };
    const leaf = json.nodes.find((node) => node.__kind === "Leaf")!;
    const branch = json.nodes.find((node) => node.__kind === "Branch")!;
    const output = json.outputs[0]!;
    const refused: [string, unknown][] = [
      ["an array", []],
      ["a ledger's entry list", [["00", "00"]]],
      ["null", null],
      ["a missing outputs field", { nodes: json.nodes }],
      ["an extra field", { ...json, root: "00" }],
      ["nodes that are not an array", { ...json, nodes: {} }],
      ["outputs that are not an array", { ...json, outputs: "00" }],
      ["a node that is not an object", { ...json, nodes: ["00"] }],
      ["a null node", { ...json, nodes: [null] }],
      [
        "a node of another kind",
        { ...json, nodes: [{ ...leaf, __kind: "Extension" }] },
      ],
      [
        "a leaf with a branch's field",
        { ...json, nodes: [{ ...leaf, children: [] }] },
      ],
      [
        "a branch without children",
        { ...json, nodes: [{ __kind: "Branch", prefix: branch.prefix }] },
      ],
      ["a null output", { ...json, outputs: [null] }],
      [
        "an output with an extra field",
        { ...json, outputs: [{ ...output, datum: "00" }] },
      ],
      [
        "an output with a number out-ref",
        { ...json, outputs: [{ ...output, outRef: 7 }] },
      ],
      [
        "an output with an out-ref in an array",
        { ...json, outputs: [{ ...output, outRef: [output.outRef] }] },
      ],
      [
        "an output with its bytes in an array",
        { ...json, outputs: [{ ...output, output: [output.output] }] },
      ],
      [
        "an output that is not hex bytes",
        { ...json, outputs: [{ ...output, output: "0g" }] },
      ],
      [
        "an output of odd hex length",
        { ...json, outputs: [{ ...output, outRef: "abc" }] },
      ],
    ];
    for (const [what, value] of refused) {
      let thrown: unknown;
      try {
        parseLedgerWitnessJson(value, "test witness");
      } catch (error) {
        thrown = error;
      }
      expect(thrown, what).toBeInstanceOf(Error);
      expect((thrown as Error).constructor, what).toBe(Error);
      expect((thrown as Error).message, what).toMatch(/^test witness: /u);
    }
  });
});
