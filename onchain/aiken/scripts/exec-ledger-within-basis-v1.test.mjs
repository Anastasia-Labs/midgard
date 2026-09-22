import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import {
  GOAL_SPEC_EXECUTION_BASIS_V1,
  checkWithinBasisExecLedger,
} from "./exec-ledger-within-basis-v1.mjs";

// Every assertion this module makes is about a *judgement*, not about Aiken, so
// the reading source is stubbed and no compile is paid for. What a stub cannot
// fake is the thing being tested: whether a ledger that measured nothing, or one
// whose basis was edited, or one whose fresh reading contradicts its own
// judgement, leaves this gate exiting zero. An unguarded guard is worth nothing
// — issues #519 and #523 were both a gate that could not fail.
//
// The basis is the exported program constant rather than a fifth typed copy of
// the pair, so this file cannot pass while disagreeing with what the lanes are
// actually judged at.
const declaredBasis = GOAL_SPEC_EXECUTION_BASIS_V1;

const ledgerWith = (overrides) => ({
  basis: { ...declaredBasis },
  modules: [
    {
      module: "midgard/example.test",
      neutralisation: ["rejects_something"],
      rows: {
        accepts_something: { mem: 1000, cpu: 2000, basisFit: "within" },
      },
    },
  ],
  ...overrides,
});

const stubMeasure =
  (readings) =>
  // The real measurer takes both the rows and the selectors in one invocation.
  (_module, _tests) =>
    new Map(Object.entries(readings));

const passingMeasure = stubMeasure({
  accepts_something: { mem: 1000, cpu: 2000 },
  rejects_something: { mem: 10, cpu: 20 },
});

const run = (ledger, { update = false, measure = passingMeasure } = {}) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-exec-ledger-selftest-"));
  try {
    const ledgerPath = join(directory, "ledger.json");
    writeFileSync(ledgerPath, `${JSON.stringify(ledger, null, 2)}\n`);
    let out = "";
    let err = "";
    const code = checkWithinBasisExecLedger({
      ledgerPath,
      lane: "Example",
      declaredBasis,
      update,
      measure,
      stdout: { write: (chunk) => (out += chunk) },
      stderr: { write: (chunk) => (err += chunk) },
    });
    return { code, out, err, written: JSON.parse(readFileSync(ledgerPath)) };
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("passes a ledger whose fresh reading matches to the unit", () => {
  const result = run(ledgerWith({}));
  assert.equal(result.code, 0);
  assert.match(result.out, /"status": "pass"/u);
  assert.match(result.out, /"rows": 1/u);
});

test("fails a ledger that measured nothing at all", () => {
  for (const empty of [
    ledgerWith({ modules: [] }),
    ledgerWith({
      modules: [
        {
          module: "midgard/example.test",
          neutralisation: ["rejects_something"],
          rows: {},
        },
      ],
    }),
  ]) {
    const result = run(empty);
    assert.equal(result.code, 1);
    assert.match(result.err, /no measured rows at all/u);
  }
});

test("fails a ledger with no `modules` array by name, not by TypeError", () => {
  const result = run(ledgerWith({ modules: undefined }));
  assert.equal(result.code, 1);
  assert.match(result.err, /declares no `modules` array/u);
});

// The same hardening one level in. A module entry missing its `rows` key threw a
// raw `TypeError` out of `Object.keys` — nonzero either way, but a stack trace
// naming the checker instead of a complaint naming the ledger, and every message
// from such an entry rendered its nameless module as the literal `undefined:`.
test("fails a module entry with no usable `rows` by name, not by TypeError", () => {
  for (const rows of [undefined, null, "nope", []]) {
    const result = run(
      ledgerWith({
        modules: [
          {
            module: "midgard/example.test",
            neutralisation: ["rejects_something"],
            rows,
          },
        ],
      }),
    );
    assert.equal(result.code, 1);
    assert.match(result.err, /midgard\/example\.test: .*declares no `rows`/u);
  }
});

test("names an entry with no `module` string rather than printing `undefined`", () => {
  const result = run(
    ledgerWith({
      modules: [{ neutralisation: ["rejects_something"], rows: undefined }],
    }),
  );
  assert.equal(result.code, 1);
  assert.match(result.err, /<unnamed ledger entry>: .*declares no `rows`/u);
  assert.doesNotMatch(result.err, /undefined:/u);
});

// A non-array `neutralisation` used to reach `.length` on whatever it was: a
// string would report its character count as a selector count and walk into
// `measure` with the string spread character by character.
test("refuses a `neutralisation` that is not an array", () => {
  const result = run(
    ledgerWith({
      modules: [
        {
          module: "midgard/example.test",
          neutralisation: "rejects_something",
          rows: {
            accepts_something: { mem: 1000, cpu: 2000, basisFit: "within" },
          },
        },
      ],
    }),
  );
  assert.equal(result.code, 1);
  assert.match(result.err, /declares no neutralisation selectors/u);
});

test("fails a basis edited away from the one the lane is judged at", () => {
  const result = run(
    ledgerWith({ basis: { memoryUnits: 99_000_000, cpuUnits: 8_000_000_000 } }),
  );
  assert.equal(result.code, 1);
  assert.match(result.err, /is not the basis this lane is judged at/u);
});

test("refuses a module that declares no neutralisation selectors", () => {
  const result = run(
    ledgerWith({
      modules: [
        {
          module: "midgard/example.test",
          neutralisation: [],
          rows: {
            accepts_something: { mem: 1000, cpu: 2000, basisFit: "within" },
          },
        },
      ],
    }),
  );
  assert.equal(result.code, 1);
  assert.match(result.err, /declares no neutralisation selectors/u);
});

test("refuses a selector that did not run", () => {
  const result = run(ledgerWith({}), {
    measure: stubMeasure({ accepts_something: { mem: 1000, cpu: 2000 } }),
  });
  assert.equal(result.code, 1);
  assert.match(result.err, /selector 'rejects_something' did not run/u);
});

test("reports a changed reading as a drift, and absorbs it only on update", () => {
  const drifted = stubMeasure({
    accepts_something: { mem: 1234, cpu: 2000 },
    rejects_something: { mem: 10, cpu: 20 },
  });
  const reported = run(ledgerWith({}), { measure: drifted });
  assert.equal(reported.code, 1);
  assert.match(reported.err, /drifted/u);
  assert.equal(reported.written.modules[0].rows.accepts_something.mem, 1000);

  const absorbed = run(ledgerWith({}), { measure: drifted, update: true });
  assert.equal(absorbed.code, 0);
  assert.equal(absorbed.written.modules[0].rows.accepts_something.mem, 1234);
  assert.equal(
    absorbed.written.modules[0].rows.accepts_something.basisFit,
    "within",
  );
});

// The structural failure `--update` must not launder: a re-take that pushes a
// `within` row past the basis is a change to the lane's feasibility claim, not a
// number to re-pin, and the ledger must not be rewritten as evidence otherwise.
test("refuses a `within` row the fresh reading exceeds, in update mode too", () => {
  const overBasis = stubMeasure({
    accepts_something: { mem: 13_200_001, cpu: 2000 },
    rejects_something: { mem: 10, cpu: 20 },
  });
  for (const update of [false, true]) {
    const result = run(ledgerWith({}), { measure: overBasis, update });
    assert.equal(result.code, 1);
    assert.match(result.err, /recorded 'within' but measured/u);
    assert.equal(result.written.modules[0].rows.accepts_something.mem, 1000);
  }
});

test("refuses any judgement other than `within`", () => {
  const result = run(
    ledgerWith({
      modules: [
        {
          module: "midgard/example.test",
          neutralisation: ["rejects_something"],
          rows: {
            accepts_something: { mem: 1000, cpu: 2000, basisFit: "exceeds" },
          },
        },
      ],
    }),
  );
  assert.equal(result.code, 1);
  assert.match(result.err, /unexpected basisFit/u);
});
