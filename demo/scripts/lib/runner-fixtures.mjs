// Seeded-defect fixtures for the runner-backed evidence gates.
//
// Each fixture is a real project that a real runner is spawned against through
// the identical code path the published measurement uses, so the negative
// self-tests measure the gate rather than describing it. A gate that merely
// claimed to reject skipped tests would still pass a source review; one that
// has to produce ERR_FOCUSED_CHECK_NOT_EXECUTED from an actual Vitest report
// cannot.

import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";

import {
  aikenCompilerVersion,
  deriveAikenOutcome,
  deriveVitestOutcome,
  forkAikenBinary,
  runAikenCheck,
  runVitest,
  stockAikenBinary,
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
  // Issue #538. The widened per-task leg batches every manifest-declared
  // selector into one invocation and addresses each as `<module>.{<test>}`, so
  // the fixtures below exercise that shape rather than the single bare-name one.
  // A batch that fails closed on one member is the property that matters: a
  // batched run which reported the other members' passes would republish 114
  // real results as though the 115th had also been measured.
  "batch-passing": {
    modules: {
      "selftest/probe.test":
        "test selftest_probe_one() {\n  True\n}\n\ntest selftest_probe_two() {\n  1 + 1 == 2\n}\n",
      "selftest/other.test": "test selftest_other_one() {\n  True\n}\n",
    },
    patterns: [
      "selftest/probe.{selftest_probe_one}",
      "selftest/probe.{selftest_probe_two}",
      "selftest/other.{selftest_other_one}",
    ],
    declared: [
      // Declared by the stem the manifest cites, accepting the `.test` sibling
      // the stem also selects — exactly the widened leg's declaration shape.
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_one",
      },
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_two",
      },
      {
        module: "selftest/other",
        modules: ["selftest/other", "selftest/other.test"],
        selector: "selftest_other_one",
      },
    ],
  },
  "batch-one-failing": {
    modules: {
      "selftest/probe.test":
        "test selftest_probe_one() {\n  True\n}\n\ntest selftest_probe_two() {\n  1 + 1 == 3\n}\n",
      "selftest/other.test": "test selftest_other_one() {\n  True\n}\n",
    },
    patterns: [
      "selftest/probe.{selftest_probe_one}",
      "selftest/probe.{selftest_probe_two}",
      "selftest/other.{selftest_other_one}",
    ],
    declared: [
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_one",
      },
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_two",
      },
      {
        module: "selftest/other",
        modules: ["selftest/other", "selftest/other.test"],
        selector: "selftest_other_one",
      },
    ],
  },
  // One member of the batch matches nothing. Aiken still exits 0 and still
  // reports the other members as passing, so only the derivation can fail it.
  "batch-zero-collection": {
    modules: {
      "selftest/probe.test": "test selftest_probe_one() {\n  True\n}\n",
      "selftest/other.test": "test selftest_other_one() {\n  True\n}\n",
    },
    patterns: [
      "selftest/probe.{selftest_probe_one}",
      "selftest/probe.{selftest_probe_absent}",
      "selftest/other.{selftest_other_one}",
    ],
    declared: [
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_one",
      },
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_absent",
      },
      {
        module: "selftest/other",
        modules: ["selftest/other", "selftest/other.test"],
        selector: "selftest_other_one",
      },
    ],
  },
  // The accepted-module set is a bounded list, not a prefix free-for-all: a
  // selector collected from a module outside it is still a mismatch.
  "batch-module-mismatch": {
    modules: {
      "selftest/elsewhere.test": "test selftest_probe_one() {\n  True\n}\n",
    },
    patterns: ["selftest/elsewhere.{selftest_probe_one}"],
    declared: [
      {
        module: "selftest/probe",
        modules: ["selftest/probe", "selftest/probe.test"],
        selector: "selftest_probe_one",
      },
    ],
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
    const selectors = fixture.patterns ?? [
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

// Compiler-identity fixtures (issue #538). A gate that publishes a per-compiler
// result has to know which binary produced it. These exercise that resolution
// end to end: an unavailable binary must fail closed rather than fall through to
// whatever `aiken` is first on PATH, the reported identity must come from the
// binary itself rather than from a constant in the gate, and the two compilers
// the repository pins must both be spawnable and agree on a real project.
export const compilerFixtures = {
  missing: "an unavailable compiler must fail closed",
  "stub-version": "the identity must be read from the binary",
  dual: "both pinned compilers must run and agree",
};

export const evaluateCompilerFixture = (fixtureName) => {
  if (compilerFixtures[fixtureName] === undefined) {
    throw new Error(`unknown compiler fixture ${fixtureName}`);
  }
  const fixtureRoot = mkdtempSync(join(tmpdir(), "midgard-compiler-fixture-"));
  try {
    if (fixtureName === "missing") {
      // Throws ERR_AIKEN_BINARY_UNAVAILABLE; reaching the line below is itself
      // the failure this fixture exists to catch.
      aikenCompilerVersion(resolve(fixtureRoot, "no-such-aiken"));
      throw new Error(
        "compiler fixture missing: an unavailable binary reported a version",
      );
    }
    if (fixtureName === "stub-version") {
      const stubVersion = "aiken v0.0.0+0000000";
      const stub = resolve(fixtureRoot, "stub-aiken");
      writeFileSync(stub, `#!/bin/sh\nprintf '%s\\n' '${stubVersion}'\n`);
      chmodSync(stub, 0o755);
      const reported = aikenCompilerVersion(stub);
      if (reported !== stubVersion) {
        throw new Error(
          `compiler fixture stub-version: reported ${reported}, not the ${stubVersion} the spawned binary printed`,
        );
      }
      return { collected: 1, passed: 1 };
    }
    const project = resolve(fixtureRoot, "project");
    mkdirSync(resolve(project, "lib", "selftest"), { recursive: true });
    writeFileSync(resolve(project, "aiken.toml"), aikenProject);
    writeFileSync(
      resolve(project, "lib", "selftest", "probe.ak"),
      "test selftest_probe() {\n  1 + 1 == 2\n}\n",
    );
    const declared = [{ module: "selftest/probe", selector: "selftest_probe" }];
    const under = (binary, role) =>
      deriveAikenOutcome({
        label: `compiler fixture dual (${role})`,
        declared,
        ...runAikenCheck({
          projectRoot: project,
          selectors: ["selftest/probe.{selftest_probe}"],
          binary,
        }),
      });
    const stock = under(stockAikenBinary(), "released authority");
    const fork = under(forkAikenBinary(), "patched fork");
    if (stock.passed !== fork.passed || stock.passed !== declared.length) {
      throw new Error(
        `compiler fixture dual: the two compilers disagreed (${String(
          stock.passed,
        )} vs ${String(fork.passed)} of ${String(declared.length)})`,
      );
    }
    return { collected: stock.collected + fork.collected, passed: 2 };
  } finally {
    rmSync(fixtureRoot, { recursive: true, force: true });
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
  const compilerArgument = argv.find((argument) =>
    argument.startsWith("--compiler-fixture="),
  );
  if (
    vitestArgument === undefined &&
    aikenArgument === undefined &&
    compilerArgument === undefined
  ) {
    return false;
  }
  const [kind, fixtureName] =
    vitestArgument !== undefined
      ? ["vitest", vitestArgument.slice("--vitest-fixture=".length)]
      : aikenArgument !== undefined
        ? ["aiken", aikenArgument.slice("--aiken-fixture=".length)]
        : ["compiler", compilerArgument.slice("--compiler-fixture=".length)];
  try {
    const outcome =
      kind === "vitest"
        ? evaluateVitestFixture({ fixtureName, packageRoot })
        : kind === "aiken"
          ? evaluateAikenFixture(fixtureName)
          : evaluateCompilerFixture(fixtureName);
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
