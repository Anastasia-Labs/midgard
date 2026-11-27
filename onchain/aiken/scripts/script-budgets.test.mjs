import assert from "node:assert/strict";
import { test } from "node:test";

import {
  budgetName,
  judgeBudgets,
  raiseBudget,
  refitBudget,
  tableProblems,
  tightenTable,
  withHeadroom,
} from "./script-budgets.mjs";

const basis = { mem: 1_000, cpu: 10_000 };
const key = (module, title, env = "default") =>
  JSON.stringify([env, module, title]);

const entry = (fields) => [budgetName(fields), fields];
const table = (...entries) => ({
  budgets: Object.fromEntries(entries),
  growth: {},
});

const readingsOf = (pairs) => new Map(pairs);
const statuses = (verdicts) =>
  Object.fromEntries(verdicts.map((verdict) => [verdict.name, verdict.status]));

test("a reading at or under its ceiling holds, one above it fails", () => {
  const verdicts = judgeBudgets({
    table: table(
      entry({ test: "m/at", ceiling: { mem: 500, cpu: 5_000 } }),
      entry({ test: "m/under", ceiling: { mem: 500, cpu: 5_000 } }),
      entry({ test: "m/over_cpu", ceiling: { mem: 500, cpu: 5_000 } }),
    ),
    basis,
    readings: readingsOf([
      [key("m", "at"), { mem: 500, cpu: 5_000 }],
      [key("m", "under"), { mem: 1, cpu: 1 }],
      [key("m", "over_cpu"), { mem: 1, cpu: 5_001 }],
    ]),
  });
  assert.deepEqual(statuses(verdicts), {
    "m/at": "ok",
    "m/under": "ok",
    "m/over_cpu": "over",
  });
});

test("a budget with no reading, or no reading for its control, fails", () => {
  const verdicts = judgeBudgets({
    table: table(
      entry({ test: "m/gone", ceiling: { mem: 5, cpu: 5 } }),
      entry({
        test: "m/here",
        minusControl: "gone_control",
        ceiling: { mem: 5, cpu: 5 },
      }),
      entry({
        test: "m/other_env",
        env: "testnet",
        ceiling: { mem: 5, cpu: 5 },
      }),
    ),
    basis,
    readings: readingsOf([
      [key("m", "here"), { mem: 1, cpu: 1 }],
      [key("m", "other_env"), { mem: 1, cpu: 1 }],
    ]),
  });
  assert.deepEqual(statuses(verdicts), {
    "m/gone": "missing",
    "m/here - gone_control": "missing",
    "testnet:m/other_env": "missing",
  });
});

test("a difference budget judges the test minus its control, sign included", () => {
  const verdicts = judgeBudgets({
    table: table(
      entry({
        test: "m/step",
        minusControl: "fixture",
        ceiling: { mem: 100, cpu: -40 },
      }),
    ),
    basis,
    readings: readingsOf([
      [key("m", "step"), { mem: 300, cpu: 160 }],
      [key("m", "fixture"), { mem: 200, cpu: 200 }],
    ]),
  });
  assert.deepEqual(verdicts[0].measured, { mem: 100, cpu: -40 });
  assert.equal(verdicts[0].status, "ok");
  const dearer = judgeBudgets({
    table: table(
      entry({
        test: "m/step",
        minusControl: "fixture",
        ceiling: { mem: 100, cpu: -40 },
      }),
    ),
    basis,
    readings: readingsOf([
      [key("m", "step"), { mem: 300, cpu: 161 }],
      [key("m", "fixture"), { mem: 200, cpu: 200 }],
    ]),
  });
  assert.equal(dearer[0].status, "over");
});

test("an above-basis budget fails once its reading fits the basis", () => {
  const recorded = entry({
    test: "m/too_big",
    ceiling: { mem: 5_000, cpu: 50_000 },
    aboveBasis: { why: "the predicate cannot fit one transaction" },
  });
  const judge = (reading) =>
    judgeBudgets({
      table: table(recorded),
      basis,
      readings: readingsOf([[key("m", "too_big"), reading]]),
    })[0].status;
  assert.equal(judge({ mem: 4_000, cpu: 9_000 }), "ok");
  assert.equal(judge({ mem: 900, cpu: 10_001 }), "ok");
  assert.equal(judge({ mem: 1_000, cpu: 10_000 }), "now-fits");
  assert.equal(judge({ mem: 5_001, cpu: 9_000 }), "over");
});

test("a growth band holds both directions of movement", () => {
  const low = entry({
    test: "m/at_one",
    minusControl: "control_at_one",
    ceiling: { mem: 900, cpu: 9_000 },
  });
  const high = entry({
    test: "m/at_max",
    minusControl: "control_at_max",
    ceiling: { mem: 900, cpu: 9_000 },
  });
  const band = {
    ...table(low, high),
    growth: {
      flat: {
        of: high[0],
        from: low[0],
        within: { mem: { min: -50, max: 50 }, cpu: { min: -500, max: 500 } },
        why: "cost does not scale",
      },
    },
  };
  const judge = (highMem) =>
    judgeBudgets({
      table: band,
      basis,
      readings: readingsOf([
        [key("m", "at_one"), { mem: 500, cpu: 5_000 }],
        [key("m", "control_at_one"), { mem: 100, cpu: 1_000 }],
        [key("m", "at_max"), { mem: highMem, cpu: 5_000 }],
        [key("m", "control_at_max"), { mem: 100, cpu: 1_000 }],
      ]),
    }).find((verdict) => verdict.name === "flat").status;
  assert.equal(judge(500), "ok");
  assert.equal(judge(450), "ok");
  assert.equal(judge(551), "out-of-band");
  assert.equal(judge(449), "out-of-band");
});

test("the table refuses a ceiling over the basis without a reason, and misfiled names", () => {
  assert.deepEqual(
    tableProblems(
      table(entry({ test: "m/t", ceiling: { mem: 5, cpu: 5 } })),
      basis,
    ),
    [],
  );
  assert.match(
    tableProblems(
      table(entry({ test: "m/t", ceiling: { mem: 1_001, cpu: 5 } })),
      basis,
    ).join("\n"),
    /above the basis/u,
  );
  assert.match(
    tableProblems(
      { budgets: { "m/other": { test: "m/t", ceiling: { mem: 1, cpu: 1 } } } },
      basis,
    ).join("\n"),
    /must be filed as "m\/t"/u,
  );
  assert.match(tableProblems({ budgets: {} }, basis).join("\n"), /empty/u);
});

test("tighten only lowers, to the reading plus 1%", () => {
  const original = table(
    entry({ test: "m/slack", ceiling: { mem: 900, cpu: 9_000 } }),
    entry({ test: "m/snug", ceiling: { mem: 101, cpu: 1_010 } }),
    entry({ test: "m/over", ceiling: { mem: 100, cpu: 100 } }),
  );
  const verdicts = judgeBudgets({
    table: original,
    basis,
    readings: readingsOf([
      [key("m", "slack"), { mem: 100, cpu: 1_000 }],
      [key("m", "snug"), { mem: 100, cpu: 1_000 }],
      [key("m", "over"), { mem: 200, cpu: 200 }],
    ]),
  });
  const { table: tightened, lowered } = tightenTable(original, verdicts);
  assert.deepEqual(lowered, ["m/slack"]);
  assert.deepEqual(tightened.budgets["m/slack"].ceiling, {
    mem: 101,
    cpu: 1_010,
  });
  assert.deepEqual(tightened.budgets["m/over"].ceiling, { mem: 100, cpu: 100 });
});

test("raise lifts only an over-ceiling budget, with its reason, never past the basis", () => {
  const original = table(
    entry({ test: "m/t", ceiling: { mem: 100, cpu: 9_000 } }),
  );
  const over = { status: "over", measured: { mem: 200, cpu: 1_000 } };
  const raised = raiseBudget({
    table: original,
    basis,
    name: "m/t",
    verdict: over,
    reason: "the step now binds the outpoint",
  });
  assert.deepEqual(raised.budgets["m/t"].ceiling, { mem: 202, cpu: 9_000 });
  assert.equal(
    raised.budgets["m/t"].raisedBecause,
    "the step now binds the outpoint",
  );
  assert.deepEqual(
    raiseBudget({
      table: original,
      basis,
      name: "m/t",
      verdict: { status: "over", measured: { mem: 995, cpu: 1 } },
      reason: "r",
    }).budgets["m/t"].ceiling,
    { mem: 1_000, cpu: 9_000 },
  );
  assert.throws(
    () =>
      raiseBudget({
        table: original,
        basis,
        name: "m/t",
        verdict: { status: "ok", measured: { mem: 50, cpu: 50 } },
        reason: "r",
      }),
    /not over its ceiling/u,
  );
  assert.throws(
    () =>
      raiseBudget({
        table: original,
        basis,
        name: "m/t",
        verdict: { status: "over", measured: { mem: 1_001, cpu: 1 } },
        reason: "r",
      }),
    /above the basis/u,
  );
  assert.throws(
    () =>
      raiseBudget({
        table: original,
        basis,
        name: "m/t",
        verdict: over,
        reason: " ",
      }),
    /--reason/u,
  );
});

test("refit settles only a now-fits budget: drops aboveBasis, lowers to the reading plus 1%, never past the basis", () => {
  const recorded = entry({
    test: "m/was_big",
    ceiling: { mem: 1_200, cpu: 12_000 },
    aboveBasis: { why: "measured above the basis" },
  });
  const original = table(
    recorded,
    entry({ test: "m/other", ceiling: { mem: 900, cpu: 9_000 } }),
  );
  const refit = (measured) =>
    refitBudget({
      table: original,
      basis,
      name: recorded[0],
      verdict: { status: "now-fits", measured },
    });
  const settled = refit({ mem: 500, cpu: 5_000 });
  assert.deepEqual(settled.budgets["m/was_big"], {
    test: "m/was_big",
    ceiling: { mem: 505, cpu: 5_050 },
  });
  assert.deepEqual(settled.budgets["m/other"], original.budgets["m/other"]);
  assert.deepEqual(tableProblems(settled, basis), []);
  const capped = refit({ mem: 995, cpu: 10_000 });
  assert.deepEqual(capped.budgets["m/was_big"].ceiling, basis);
  for (const status of ["ok", "over", "missing"])
    assert.throws(
      () =>
        refitBudget({
          table: original,
          basis,
          name: recorded[0],
          verdict: { status, measured: { mem: 500, cpu: 5_000 } },
        }),
      /not now-fits/u,
    );
  assert.throws(
    () =>
      refitBudget({
        table: original,
        basis,
        name: "m/absent",
        verdict: { status: "now-fits", measured: { mem: 1, cpu: 1 } },
      }),
    /no budget/u,
  );
});

test("headroom rounds the 1% up on both signs", () => {
  assert.equal(withHeadroom(100), 101);
  assert.equal(withHeadroom(101), 103);
  assert.equal(withHeadroom(-29_819_234), -29_521_041);
  assert.equal(withHeadroom(0), 0);
});
