import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  initialInputSetMergeControl,
  type InputSetFields,
  type InputSetMergeControl,
  type InputSetMergeStep,
  inputSetMergeStep,
  runInputSetMerge,
} from "../src/phase-a.input-set-merge.js";
import {
  emptyMidgardInputResolutionSchedule,
  orderMidgardInputResolutionSchedule,
  prependMidgardInputResolutionSchedule,
} from "../src/validation-machine/input-resolution.js";

// The vectors of `tests-input-set-merge.test.ak`
// (`onchain/aiken/lib/midgard/validation-machine-tests/`): the same fields give
// the same controls, the same schedule and the same duplicate step.
const key = (byte: number): Buffer =>
  encodeMidgardSpendInputItem({ txId: Buffer.alloc(32, byte), outputIndex: 0 });
const [a, b, c] = [key(0xaa), key(0xbb), key(0xcc)];
const EMPTY = Buffer.alloc(0);

const start = (
  fields: InputSetFields,
  patch: Partial<InputSetMergeControl> = {},
): InputSetMergeControl => ({
  ...initialInputSetMergeControl(fields),
  ...patch,
});

const steps = (fields: InputSetFields) => {
  const out: { control: InputSetMergeControl; step: InputSetMergeStep }[] = [];
  runInputSetMerge(fields, (control, step) => out.push({ control, step }));
  return out;
};

const ascendingSchedule = (fields: InputSetFields): Buffer => {
  const ordered = orderMidgardInputResolutionSchedule({
    spend: fields[0],
    reference: fields[1],
    keyOf: (item) => Buffer.from(item),
  });
  let hash = emptyMidgardInputResolutionSchedule();
  for (const { sourceKind, item } of [...ordered].reverse())
    hash = prependMidgardInputResolutionSchedule({
      sourceKind,
      key: item,
      nextHash: hash,
    });
  return hash;
};

describe("InputSets descending merge (TS twin)", () => {
  const run: InputSetFields = [[a, c], [b]];
  const seeded = start(run, { spendCount: 2, cachedOtherHead: c });
  const afterC: InputSetMergeControl = {
    spendCount: 2,
    referenceCount: 1,
    spendSeen: 1,
    referenceSeen: 0,
    lastKey: c,
    lastField: 0,
    lastItem: 1,
    cachedOtherHead: b,
  };
  const afterB: InputSetMergeControl = {
    ...afterC,
    referenceSeen: 1,
    lastKey: b,
    lastField: 1,
    lastItem: 0,
    cachedOtherHead: EMPTY,
  };

  it("seeds, consumes c (0, 1), b (1, 0), a (0, 0) and completes", () => {
    const trace = steps(run);
    expect(trace.map(({ control }) => control)).toStrictEqual([
      start(run),
      seeded,
      afterC,
      afterB,
    ]);
    expect(trace.map(({ step }) => [step.kind, step.opened])).toStrictEqual([
      ["seed", { fieldIndex: 0, itemIndex: 1 }],
      ["consume", { fieldIndex: 1, itemIndex: 0 }],
      ["consume", { fieldIndex: 0, itemIndex: 0 }],
      ["consume", { fieldIndex: 0, itemIndex: 0 }],
    ]);
    const last = trace.at(-1)!.step;
    expect(last.kind === "consume" && last.complete).toBe(true);
  });

  it("prepends the consumed items into the ascending schedule", () => {
    let hash = emptyMidgardInputResolutionSchedule();
    runInputSetMerge(run, (_control, step) => {
      if (step.kind === "consume")
        hash = prependMidgardInputResolutionSchedule({
          sourceKind: step.consumed.fieldIndex === 0 ? "spend" : "reference",
          key: step.key,
          nextHash: hash,
        });
    });
    expect(hash).toStrictEqual(ascendingSchedule(run));
  });

  it("without reference inputs consumes field 0's tail with no seed", () => {
    const fields: InputSetFields = [[a, b], []];
    const step = inputSetMergeStep(start(fields), fields);
    expect(step).toMatchObject({
      kind: "consume",
      opened: { fieldIndex: 0, itemIndex: 1 },
      next: {
        ...start(fields, { spendCount: 2 }),
        spendSeen: 1,
        lastKey: b,
        lastField: 0,
        lastItem: 1,
      },
    });
  });

  it("names a within-field repeat later position first", () => {
    const fields: InputSetFields = [
      [a, b, b],
      [c, c],
    ];
    const repeatPre: InputSetMergeControl = {
      spendCount: 3,
      referenceCount: 2,
      spendSeen: 0,
      referenceSeen: 1,
      lastKey: c,
      lastField: 1,
      lastItem: 1,
      cachedOtherHead: b,
    };
    const trace = steps(fields);
    expect(trace.map(({ control }) => control).at(-1)).toStrictEqual(repeatPre);
    expect(trace.at(-1)!.step).toMatchObject({
      kind: "duplicate",
      first: { fieldIndex: 1, itemIndex: 1 },
      second: { fieldIndex: 1, itemIndex: 0 },
    });
  });

  it("names equal cross-field heads field 0 first", () => {
    const fields: InputSetFields = [
      [a, c],
      [b, c],
    ];
    const trace = steps(fields);
    expect(trace).toHaveLength(2);
    expect(trace[1]!.step).toMatchObject({
      kind: "duplicate",
      first: { fieldIndex: 0, itemIndex: 1 },
      second: { fieldIndex: 1, itemIndex: 1 },
    });
  });

  it.each([
    ["within field 0", [[a, c, c, c], []], [0, 3, 0, 2]],
    ["split across fields", [[a, c, c], [c]], [0, 2, 1, 0]],
  ] as const)(
    "reports only the first detection of a triple copy %s",
    (_name, fields, [ff, fi, sf, si]) => {
      expect(runInputSetMerge(fields)).toMatchObject({
        first: { fieldIndex: ff, itemIndex: fi },
        second: { fieldIndex: sf, itemIndex: si },
      });
    },
  );

  it("reopens field 1 after field 0 runs out", () => {
    // Field 0 = [c] is consumed first; field 1's head b is read again.
    expect(runInputSetMerge([[c], [a, b, b]])).toMatchObject({
      first: { fieldIndex: 1, itemIndex: 2 },
      second: { fieldIndex: 1, itemIndex: 1 },
    });
    expect(runInputSetMerge([[c], [a, b]])).toBeNull();
  });

  it("names equal single heads field 0 first", () => {
    expect(runInputSetMerge([[b], [a, b]])).toMatchObject({
      first: { fieldIndex: 0, itemIndex: 0 },
      second: { fieldIndex: 1, itemIndex: 1 },
    });
  });
});
