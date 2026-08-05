// Seeded-defect fixtures for the runner-backed evidence gates.
//
// Each fixture is a real project that a real runner is spawned against through
// the identical code path the published measurement uses, so the negative
// self-tests measure the gate rather than describing it. A gate that merely
// claimed to reject skipped tests would still pass a source review; one that
// has to produce ERR_FOCUSED_CHECK_NOT_EXECUTED from an actual Vitest report
// cannot.

import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

import {
  deriveAikenOutcome,
  deriveVitestOutcome,
  runAikenCheck,
  runVitest,
} from "./runner-reports.mjs";

export const vitestFixtures = {
  passing: {
    file: "tests/self-test.test.ts",
    source: `import { describe, expect, it } from "vitest";

describe("runner-check self-test", () => {
  it("executes and passes", () => {
    expect(1).toBe(1);
  });
});
`,
  },
  failing: {
    file: "tests/self-test.test.ts",
    source: `import { describe, expect, it } from "vitest";

describe("runner-check self-test", () => {
  it("executes and passes", () => {
    expect(1).toBe(1);
  });
  it("executes and fails", () => {
    expect(1).toBe(2);
  });
});
`,
  },
  "zero-collection": {
    file: "tests/self-test.test.ts",
    source: `import { describe } from "vitest";

describe("runner-check self-test declaring no test", () => {});
`,
  },
  skipped: {
    file: "tests/self-test.test.ts",
    source: `import { describe, it } from "vitest";

describe("runner-check self-test", () => {
  it.skip("is declared but never executed", () => {});
});
`,
  },
  "renamed-title": {
    file: "tests/self-test.test.ts",
    requiredTitles: ["executes and passes"],
    source: `import { describe, expect, it } from "vitest";

describe("runner-check self-test", () => {
  it("was renamed and no longer answers to its citation", () => {
    expect(1).toBe(1);
  });
});
`,
  },
  "missing-file": {
    file: "tests/self-test.test.ts",
    source: null,
  },
};

export const evaluateVitestFixture = ({ fixtureName, packageRoot }) => {
  const fixture = vitestFixtures[fixtureName];
  if (fixture === undefined) {
    throw new Error(`unknown vitest fixture ${fixtureName}`);
  }
  const fixtureRoot = mkdtempSync(join(tmpdir(), "midgard-vitest-fixture-"));
  try {
    if (fixture.source !== null) {
      const fixturePath = resolve(fixtureRoot, fixture.file);
      mkdirSync(dirname(fixturePath), { recursive: true });
      writeFileSync(fixturePath, fixture.source);
    }
    return deriveVitestOutcome({
      label: `vitest fixture ${fixtureName}`,
      requiredTitles: fixture.requiredTitles ?? [],
      ...runVitest({
        packageRoot,
        testFile: fixture.file,
        root: fixtureRoot,
      }),
    });
  } finally {
    rmSync(fixtureRoot, { recursive: true, force: true });
  }
};

// A dependency-free Aiken project: it needs no network, compiles in well under
// a second, and exercises the identical spawn/parse/derive path the published
// on-chain measurement uses.
const aikenProject = `name = "midgard/evidence-runner-self-test"
version = "0.0.0"
plutus = "v3"

[repository]
user = "midgard"
project = "evidence-runner-self-test"
platform = "github"

dependencies = []
`;

export const aikenFixtures = {
  passing: {
    modules: { "selftest/probe": "test selftest_probe() {\n  1 + 1 == 2\n}\n" },
    declared: [{ module: "selftest/probe", selector: "selftest_probe" }],
  },
  failing: {
    modules: { "selftest/probe": "test selftest_probe() {\n  1 + 1 == 3\n}\n" },
    declared: [{ module: "selftest/probe", selector: "selftest_probe" }],
  },
  // The exact V-1 shape: a selector that matches nothing. `aiken check` exits 0
  // and reports 0 collected tests, so only the derivation can fail it closed.
  "zero-collection": {
    modules: { "selftest/probe": "test selftest_other() {\n  True\n}\n" },
    declared: [{ module: "selftest/probe", selector: "selftest_probe" }],
  },
  "missing-selector": {
    modules: { "selftest/probe": "test selftest_probe() {\n  True\n}\n" },
    declared: [
      { module: "selftest/probe", selector: "selftest_probe" },
      { module: "selftest/probe", selector: "selftest_probe_absent" },
    ],
  },
  "module-mismatch": {
    modules: {
      "selftest/elsewhere": "test selftest_probe() {\n  True\n}\n",
    },
    declared: [{ module: "selftest/probe", selector: "selftest_probe" }],
  },
};

export const evaluateAikenFixture = (fixtureName) => {
  const fixture = aikenFixtures[fixtureName];
  if (fixture === undefined) {
    throw new Error(`unknown aiken fixture ${fixtureName}`);
  }
  const projectRoot = mkdtempSync(join(tmpdir(), "midgard-aiken-fixture-"));
  try {
    writeFileSync(resolve(projectRoot, "aiken.toml"), aikenProject);
    for (const [moduleName, source] of Object.entries(fixture.modules)) {
      const modulePath = resolve(projectRoot, "lib", `${moduleName}.ak`);
      mkdirSync(dirname(modulePath), { recursive: true });
      writeFileSync(modulePath, source);
    }
    const selectors = [
      ...new Set(fixture.declared.map(({ selector }) => selector)),
    ];
    return deriveAikenOutcome({
      label: `aiken fixture ${fixtureName}`,
      declared: fixture.declared,
      ...runAikenCheck({ projectRoot, selectors }),
    });
  } finally {
    rmSync(projectRoot, { recursive: true, force: true });
  }
};

// Fixture mode: run one seeded fixture and nothing else, so a gate's negative
// self-tests can spawn that very gate and observe its real exit code.
export const runFixtureMode = ({ argv, packageRoot }) => {
  const vitestArgument = argv.find((argument) =>
    argument.startsWith("--vitest-fixture="),
  );
  const aikenArgument = argv.find((argument) =>
    argument.startsWith("--aiken-fixture="),
  );
  if (vitestArgument === undefined && aikenArgument === undefined) {
    return false;
  }
  const [kind, fixtureName] =
    vitestArgument === undefined
      ? ["aiken", aikenArgument.slice("--aiken-fixture=".length)]
      : ["vitest", vitestArgument.slice("--vitest-fixture=".length)];
  try {
    const outcome =
      kind === "vitest"
        ? evaluateVitestFixture({ fixtureName, packageRoot })
        : evaluateAikenFixture(fixtureName);
    process.stdout.write(
      `${kind} fixture ${fixtureName}: ${String(outcome.passed)}/${String(
        outcome.collected,
      )} passed\n`,
    );
    process.exit(0);
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exit(1);
  }
  return true;
};
