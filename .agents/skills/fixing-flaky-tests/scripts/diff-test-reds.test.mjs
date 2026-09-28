// Self-tests for diff-test-reds.mjs. Every case writes a small list and small
// vitest-shaped reports into a temp directory; none runs a test suite.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { after, describe, it } from "node:test";
import { fileURLToPath } from "node:url";

import { fileMatches, main } from "./diff-test-reds.mjs";

const SCRIPT = join(
  dirname(fileURLToPath(import.meta.url)),
  "diff-test-reds.mjs",
);
const work = mkdtempSync(join(tmpdir(), "diff-test-reds-test-"));
after(() => rmSync(work, { recursive: true, force: true }));

let counter = 0;
const writeJson = (value) => {
  counter += 1;
  const path = join(work, `file-${String(counter)}.json`);
  writeFileSync(
    path,
    typeof value === "string" ? value : JSON.stringify(value),
  );
  return path;
};

const writeText = (text) => {
  counter += 1;
  const path = join(work, `file-${String(counter)}.log`);
  writeFileSync(path, text);
  return path;
};

const PACKAGE = "/home/someone/midgard-wt/demo/midgard-watcher";

const entry = (overrides) => ({
  suite: "midgard-watcher",
  file: "tests/availability/runtime.test.ts",
  name: "runtime refuses a stale head",
  kind: "deterministic",
  reason: "known red on base",
  ...overrides,
});

const list = (entries) => ({
  version: 1,
  program: "dabond",
  base: "93a16f8d8",
  generatedFrom: ["gate run 1"],
  entries,
});

const file = (relative, assertions, status) => ({
  name: `${PACKAGE}/${relative}`,
  status:
    status ??
    (assertions.some(([, s]) => s === "failed") ? "failed" : "passed"),
  message: "",
  assertionResults: assertions.map(([fullName, s]) => ({
    fullName,
    status: s,
    title: fullName,
  })),
});

const report = (...files) => ({
  numTotalTests: 0,
  success: false,
  testResults: files,
});

const run = (args) => {
  let stdout = "";
  let stderr = "";
  const code = main(args, {
    stdout: (text) => {
      stdout += text;
    },
    stderr: (text) => {
      stderr += text;
    },
  });
  return { code, stdout, stderr };
};

const runJson = (listValue, ...reports) => {
  const result = run([
    "--accepted",
    writeJson(listValue),
    "--suite",
    "midgard-watcher",
    ...reports.map(writeJson),
    "--json",
  ]);
  return {
    ...result,
    parsed: result.code === 2 ? undefined : JSON.parse(result.stdout),
  };
};

describe("verdicts", () => {
  it("a failure with no entry is NEW and exits 1", () => {
    const { code, parsed } = runJson(
      list([entry({})]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "failed"],
          ["runtime brand new red", "failed"],
        ]),
      ),
    );
    assert.equal(code, 1);
    assert.deepEqual(
      parsed.new.map((item) => item.name),
      ["runtime brand new red"],
    );
    assert.equal(parsed.new[0].file, "tests/availability/runtime.test.ts");
    assert.deepEqual(
      parsed.accepted.map((item) => item.name),
      ["runtime refuses a stale head"],
    );
  });

  it("only accepted failures exit 0", () => {
    const { code, parsed } = runJson(
      list([entry({})]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "failed"],
          ["runtime other", "passed"],
        ]),
      ),
    );
    assert.equal(code, 0);
    assert.equal(parsed.new.length, 0);
    assert.equal(parsed.accepted.length, 1);
  });

  it("the text output names the new failure and the verdict", () => {
    const result = run([
      "--accepted",
      writeJson(list([])),
      "--suite",
      "midgard-watcher",
      writeJson(report(file("tests/a.test.ts", [["a fails", "failed"]]))),
    ]);
    assert.equal(result.code, 1);
    assert.match(
      result.stdout,
      /NEW failures \(1\):\n {2}tests\/a\.test\.ts :: a fails/u,
    );
    assert.match(result.stdout, /verdict: 1 new failure\(s\) \(exit 1\)/u);
  });

  it("a deterministic entry whose test passed is STALE, without failing the run", () => {
    const { code, parsed } = runJson(
      list([entry({})]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "passed"],
        ]),
      ),
    );
    assert.equal(code, 0);
    assert.deepEqual(
      parsed.stale.map((item) => item.name),
      ["runtime refuses a stale head"],
    );
  });

  it("an entry whose test did not run is neither accepted nor stale", () => {
    const { parsed } = runJson(
      list([entry({}), entry({ name: "runtime skipped one" })]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime skipped one", "skipped"],
        ]),
      ),
    );
    assert.equal(parsed.stale.length, 0);
    assert.equal(parsed.notRun.length, 2);
  });

  it("a flaky entry that fails is ACCEPTED-FLAKY; one that passes is not stale", () => {
    const failing = runJson(
      list([entry({ kind: "flaky" })]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "failed"],
        ]),
      ),
    );
    assert.equal(failing.code, 0);
    assert.equal(failing.parsed.acceptedFlaky.length, 1);
    assert.equal(failing.parsed.accepted.length, 0);
    const passing = runJson(
      list([entry({ kind: "flaky" })]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "passed"],
        ]),
      ),
    );
    assert.equal(passing.parsed.stale.length, 0);
    assert.equal(passing.parsed.flakyPassed.length, 1);
  });

  it("a failure in any of several reports counts, and is not stale", () => {
    const { code, parsed } = runJson(
      list([entry({})]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "passed"],
        ]),
      ),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "failed"],
        ]),
      ),
    );
    assert.equal(code, 0);
    assert.equal(parsed.accepted.length, 1);
    assert.equal(parsed.stale.length, 0);
  });
});

describe("the * wildcard", () => {
  it("accepts every failing test in the file and a file that failed as a whole", () => {
    const { code, parsed } = runJson(
      list([
        entry({ file: "tests/a.test.ts", name: "*" }),
        entry({ file: "tests/broken.test.ts", name: "*" }),
      ]),
      report(
        file("tests/a.test.ts", [
          ["one", "failed"],
          ["two", "failed"],
          ["three", "passed"],
        ]),
        file("tests/broken.test.ts", [], "failed"),
      ),
    );
    assert.equal(code, 0);
    assert.equal(parsed.accepted.length, 3);
    assert.equal(parsed.new.length, 0);
  });

  it("does not reach another file", () => {
    const { code, parsed } = runJson(
      list([entry({ file: "tests/a.test.ts", name: "*" })]),
      report(file("tests/b.test.ts", [["one", "failed"]])),
    );
    assert.equal(code, 1);
    assert.equal(parsed.new[0].file, "tests/b.test.ts");
  });

  it("a file that failed as a whole is NEW without a * entry", () => {
    const { code, parsed } = runJson(
      list([entry({ file: "tests/broken.test.ts" })]),
      report(file("tests/broken.test.ts", [], "failed")),
    );
    assert.equal(code, 1);
    assert.match(parsed.new[0].name, /failed as a whole/u);
  });

  it("marks the failures it accepts and warns when tests in its file passed", () => {
    const result = run([
      "--accepted",
      writeJson(list([entry({ file: "tests/a.test.ts", name: "*" })])),
      "--suite",
      "midgard-watcher",
      writeJson(
        report(
          file("tests/a.test.ts", [
            ["known red", "failed"],
            ["brand new regression", "failed"],
            ["still green", "passed"],
          ]),
        ),
      ),
    ]);
    assert.equal(result.code, 0);
    assert.match(result.stdout, /brand new regression \(via \*\)/u);
    assert.match(
      result.stdout,
      /warning: the "\*" entry for tests\/a\.test\.ts .* 1 test\(s\) passed/u,
    );
  });

  it("a specific entry wins over a * entry for the same file", () => {
    const { parsed } = runJson(
      list([
        entry({ file: "tests/a.test.ts", name: "*" }),
        entry({ file: "tests/a.test.ts", name: "one", kind: "flaky" }),
      ]),
      report(file("tests/a.test.ts", [["one", "failed"]])),
    );
    assert.equal(parsed.accepted.length, 0);
    assert.equal(parsed.acceptedFlaky.length, 1);
    assert.equal(parsed.acceptedFlaky[0].wildcard, undefined);
  });

  it("a * entry whose file passed is STALE", () => {
    const { parsed } = runJson(
      list([entry({ file: "tests/a.test.ts", name: "*" })]),
      report(file("tests/a.test.ts", [["one", "passed"]])),
    );
    assert.equal(parsed.stale.length, 1);
  });
});

describe("matching", () => {
  it("absolute report paths match package-relative entries only under the suite's directory", () => {
    assert.equal(
      fileMatches(
        `${PACKAGE}/tests/a.test.ts`,
        "tests/a.test.ts",
        "midgard-watcher",
      ),
      true,
    );
    assert.equal(
      fileMatches(
        `${PACKAGE}/tests/a.test.ts`,
        "./tests/a.test.ts",
        "midgard-watcher",
      ),
      true,
    );
    assert.equal(
      fileMatches(
        "/x/demo/midgard-node/tests/a.test.ts",
        "tests/a.test.ts",
        "midgard-watcher",
      ),
      false,
    );
    assert.equal(
      fileMatches(
        `${PACKAGE}/tests/sub/a.test.ts`,
        "a.test.ts",
        "midgard-watcher",
      ),
      false,
    );
    assert.equal(
      fileMatches(
        "midgard-watcher/tests/a.test.ts",
        "tests/a.test.ts",
        "midgard-watcher",
      ),
      true,
    );
    assert.equal(
      fileMatches("tests/a.test.ts", "tests/a.test.ts", "midgard-watcher"),
      true,
    );
  });

  it("a relative report path matches only its own file", () => {
    assert.equal(
      fileMatches("tests/b.test.ts", "tests/a.test.ts", "midgard-watcher"),
      false,
    );
  });

  it("skipped, todo and pending tests are not failures", () => {
    const { code, parsed } = runJson(
      list([]),
      report(
        file("tests/a.test.ts", [
          ["one", "skipped"],
          ["two", "todo"],
          ["three", "pending"],
        ]),
      ),
    );
    assert.equal(code, 0);
    assert.equal(parsed.new.length, 0);
  });

  it("names are compared with vitest's leading space trimmed", () => {
    const { code } = runJson(
      list([entry({ name: "outer inner fails" })]),
      report(
        file("tests/availability/runtime.test.ts", [
          [" outer inner fails", "failed"],
        ]),
      ),
    );
    assert.equal(code, 0);
  });

  it("entries of another suite never accept a failure", () => {
    const { code } = runJson(
      list([entry({ suite: "midgard-node" })]),
      report(
        file("tests/availability/runtime.test.ts", [
          ["runtime refuses a stale head", "failed"],
        ]),
      ),
    );
    assert.equal(code, 1);
  });

  it("warns when the reports are not under the suite's directory", () => {
    const { parsed } = runJson(list([]), {
      testResults: [
        {
          name: "/x/demo/midgard-node/tests/a.test.ts",
          status: "passed",
          assertionResults: [],
        },
      ],
    });
    assert.match(parsed.warnings[0], /is --suite right\?/u);
  });
});

describe("run logs", () => {
  // Abridged from vitest 3.0.7 output with colour on, for a file whose test
  // leaked a rejection: every test passed, the JSON report said success, and
  // the run exited 1.
  const summary = (extra = "") =>
    "\u001b[2m Test Files \u001b[22m \u001b[1m\u001b[32m1 passed\u001b[39m\u001b[22m\u001b[90m (1)\u001b[39m\n" +
    extra +
    "\u001b[2m   Duration \u001b[22m 281ms\n";
  const errorsLine =
    "\u001b[2m     Errors \u001b[22m \u001b[1m\u001b[31m1 error\u001b[39m\u001b[22m\n";
  const passing = () =>
    writeJson(report(file("tests/a.test.ts", [["one", "passed"]])));
  const runWithLog = (text) =>
    run([
      "--accepted",
      writeJson(list([])),
      "--suite",
      "midgard-watcher",
      "--log",
      writeText(text),
      passing(),
      "--json",
    ]);

  it("errors outside any test are a NEW failure", () => {
    const result = runWithLog(summary(errorsLine));
    assert.equal(result.code, 1);
    const parsed = JSON.parse(result.stdout);
    assert.equal(parsed.new[0].file, "(run)");
    assert.match(parsed.new[0].name, /1 error\(s\)/u);
  });

  it("a log with no vitest summary is a NEW failure", () => {
    const result = runWithLog(" RUN  v3.0.7 /x\n ✓ tests/a.test.ts (1 test)\n");
    assert.equal(result.code, 1);
    assert.match(JSON.parse(result.stdout).new[0].name, /no vitest summary/u);
  });

  it("a clean log adds nothing, and no log is warned about", () => {
    const clean = runWithLog(summary());
    assert.equal(clean.code, 0, clean.stdout);
    assert.deepEqual(JSON.parse(clean.stdout).warnings, []);
    const { parsed } = runJson(list([]), report(file("tests/a.test.ts", [])));
    assert.match(parsed.warnings.at(-1), /no --log given/u);
  });

  describe('a "(run)" entry', () => {
    const errorsLines = (n) =>
      `\u001b[2m     Errors \u001b[22m \u001b[1m\u001b[31m${String(n)} errors\u001b[39m\u001b[22m\n`;
    const runEntry = (overrides) =>
      entry({
        file: "(run)",
        name: "errors outside any test: vitest reported 2 error(s); they are not in the JSON report",
        ...overrides,
      });
    const runWithEntries = (entries, text) => {
      const result = run([
        "--accepted",
        writeJson(list(entries)),
        "--suite",
        "midgard-watcher",
        "--log",
        writeText(text),
        passing(),
        "--json",
      ]);
      return { ...result, parsed: JSON.parse(result.stdout) };
    };

    it("accepts errors outside any test up to the recorded count", () => {
      for (const n of [1, 2]) {
        const { code, parsed } = runWithEntries(
          [runEntry({})],
          summary(errorsLines(n)),
        );
        assert.equal(code, 0, JSON.stringify(parsed.new));
        assert.equal(parsed.accepted.length, 1);
        assert.equal(parsed.accepted[0].file, "(run)");
        assert.equal(parsed.accepted[0].acceptedUpTo, 2);
        assert.deepEqual(parsed.notRun, []);
      }
    });

    it("a count above the recorded one is NEW", () => {
      const { code, parsed } = runWithEntries(
        [runEntry({})],
        summary(errorsLines(3)),
      );
      assert.equal(code, 1);
      assert.match(parsed.new[0].name, /3 error\(s\)/u);
      assert.equal(parsed.notRun.length, 1);
    });

    it("never accepts a run that printed no summary", () => {
      const { code, parsed } = runWithEntries(
        [runEntry({})],
        " RUN  v3.0.7 /x\n",
      );
      assert.equal(code, 1);
      assert.match(parsed.new[0].name, /no vitest summary/u);
    });

    it("of another suite accepts nothing", () => {
      const { code } = runWithEntries(
        [runEntry({ suite: "midgard-node" })],
        summary(errorsLines(1)),
      );
      assert.equal(code, 1);
    });

    it("a flaky entry reports ACCEPTED-FLAKY", () => {
      const { code, parsed } = runWithEntries(
        [runEntry({ kind: "flaky" })],
        summary(errorsLines(2)),
      );
      assert.equal(code, 0);
      assert.equal(parsed.acceptedFlaky.length, 1);
    });

    it("whose name carries no count exits 2", () => {
      const result = run([
        "--accepted",
        writeJson(list([runEntry({ name: "*" })])),
        "--suite",
        "midgard-watcher",
        "--log",
        writeText(summary(errorsLines(1))),
        passing(),
      ]);
      assert.equal(result.code, 2);
      assert.match(result.stderr, /must report the base's count/u);
    });
  });

  it("an unreadable log exits 2", () => {
    const result = run([
      "--accepted",
      writeJson(list([])),
      "--suite",
      "midgard-watcher",
      "--log",
      join(work, "absent.log"),
      passing(),
    ]);
    assert.equal(result.code, 2);
  });
});

describe("bad input exits 2", () => {
  const good = () =>
    writeJson(report(file("tests/a.test.ts", [["one", "passed"]])));
  const cases = [
    ["no arguments", () => []],
    [
      "no report",
      () => ["--accepted", writeJson(list([])), "--suite", "midgard-watcher"],
    ],
    ["no suite", () => ["--accepted", writeJson(list([])), good()]],
    [
      "unknown option",
      () => [
        "--accepted",
        writeJson(list([])),
        "--suite",
        "s",
        "--bogus",
        good(),
      ],
    ],
    [
      "missing list file",
      () => ["--accepted", join(work, "absent.json"), "--suite", "s", good()],
    ],
    [
      "list is not JSON",
      () => ["--accepted", writeJson("{not json"), "--suite", "s", good()],
    ],
    [
      "wrong version",
      () => [
        "--accepted",
        writeJson({ ...list([]), version: 2 }),
        "--suite",
        "s",
        good(),
      ],
    ],
    [
      "unknown top-level key",
      () => [
        "--accepted",
        writeJson({ ...list([]), extra: 1 }),
        "--suite",
        "s",
        good(),
      ],
    ],
    [
      "entry missing reason",
      () => [
        "--accepted",
        writeJson(list([{ ...entry({}), reason: undefined }])),
        "--suite",
        "s",
        good(),
      ],
    ],
    [
      "entry with a bad kind",
      () => [
        "--accepted",
        writeJson(list([entry({ kind: "sometimes" })])),
        "--suite",
        "s",
        good(),
      ],
    ],
    [
      "duplicate entries",
      () => [
        "--accepted",
        writeJson(list([entry({}), entry({ reason: "again" })])),
        "--suite",
        "s",
        good(),
      ],
    ],
    [
      "report without testResults",
      () => [
        "--accepted",
        writeJson(list([])),
        "--suite",
        "s",
        writeJson({ results: [] }),
      ],
    ],
    [
      "assertion without status",
      () => [
        "--accepted",
        writeJson(list([])),
        "--suite",
        "s",
        writeJson({
          testResults: [
            {
              name: "/a",
              status: "failed",
              assertionResults: [{ fullName: "x" }],
            },
          ],
        }),
      ],
    ],
  ];
  for (const [label, args] of cases) {
    it(label, () => {
      const result = run(args());
      assert.equal(result.code, 2, result.stdout);
      assert.match(result.stderr, /usage: diff-test-reds\.mjs/u);
    });
  }
});

describe("command line", () => {
  it("exits 1 on a new failure and 0 on an accepted one", () => {
    const accepted = writeJson(
      list([entry({ file: "tests/a.test.ts", name: "a fails" })]),
    );
    const newRed = spawnSync(
      process.execPath,
      [
        SCRIPT,
        "--accepted",
        accepted,
        "--suite",
        "midgard-watcher",
        writeJson(report(file("tests/a.test.ts", [["a other", "failed"]]))),
      ],
      { encoding: "utf8" },
    );
    assert.equal(newRed.status, 1, newRed.stderr);
    const knownRed = spawnSync(
      process.execPath,
      [
        SCRIPT,
        "--accepted",
        accepted,
        "--suite",
        "midgard-watcher",
        writeJson(report(file("tests/a.test.ts", [["a fails", "failed"]]))),
      ],
      { encoding: "utf8" },
    );
    assert.equal(knownRed.status, 0, knownRed.stderr);
    const bad = spawnSync(
      process.execPath,
      [SCRIPT, "--suite", "midgard-watcher"],
      { encoding: "utf8" },
    );
    assert.equal(bad.status, 2);
  });
});
