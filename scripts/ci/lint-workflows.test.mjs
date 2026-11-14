// Every check in lint-workflows.mjs is proven able to fail on a fixture, the
// real repository is proven clean, and "could not check" is proven distinct
// from "clean".

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { checks, loadYaml, lintWorkflows } from "./lint-workflows.mjs";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const script = join(repositoryRoot, "scripts/ci/lint-workflows.mjs");
const yaml = loadYaml(repositoryRoot);
const skip =
  yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

const checkout =
  "actions/checkout@11d5960a326750d5838078e36cf38b85af677262 # v4.4.0";
const gateJob = (needs, extra = "") => `  gate:
    if: \${{ !cancelled() }}
    needs: [${needs.join(", ")}]
    runs-on: ubuntu-latest
    timeout-minutes: 5
    steps:
      - name: gate
        env:
          NEEDS: \${{ toJSON(needs) }}
        run: node scripts/ci/check-needs-results.mjs --allow success${extra}
`;
const job = (name, body = "") => `  ${name}:
    runs-on: ubuntu-latest
    timeout-minutes: 10
${body}    steps:
      - uses: ${checkout}
`;
const workflow = (jobs, head = "permissions:\n  contents: read\n") =>
  `name: fixture\non: [push]\n${head}jobs:\n${jobs.join("")}`;
const clean = workflow([
  job("a"),
  job("b", "    needs: [a]\n"),
  gateJob(["a", "b"]),
]);

const lintFixture = (files) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-lint-workflows-"));
  try {
    for (const [path, text] of Object.entries(files)) {
      mkdirSync(dirname(join(root, path)), { recursive: true });
      writeFileSync(join(root, path), text);
    }
    return lintWorkflows(root, yaml);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
};
const found = (text, path = ".github/workflows/w.yml") =>
  lintFixture({ [path]: text }).findings.map((finding) => finding.check);

test("the repository's workflows and actions lint clean", { skip }, () => {
  const result = lintWorkflows(repositoryRoot, yaml);
  assert.equal(result.couldNotCheck, undefined);
  assert.ok(result.files.length >= 6, `${String(result.files.length)} files`);
  assert.deepEqual(result.findings, []);
});

test("a clean fixture has no findings", { skip }, () => {
  assert.deepEqual(found(clean), []);
});

const reds = {
  "unpinned-action": clean.replace(checkout, "actions/checkout@v4"),
  permissions: workflow([job("a"), job("b"), gateJob(["a", "b"])], ""),
  timeout: clean.replace("    timeout-minutes: 10\n", ""),
  "gate-missing": workflow([job("a"), job("b")]),
  "gate-if": clean.replace("    if: ${{ !cancelled() }}\n", ""),
  "gate-allowlist": clean.replace("--allow success", "--allow success,failure"),
  "gate-needs-closure": workflow([
    job("a"),
    job("b", "    needs: [a]\n"),
    gateJob(["b"]),
  ]),
  "gate-coverage": workflow([job("a"), job("b"), gateJob(["a"])]),
  "skipped-counts-as-success": workflow([
    job("a"),
    job("b", "    if: github.event_name == 'push'\n"),
    gateJob(["a", "b"]).replace("--allow success", "--allow success,skipped"),
  ]),
  "continue-on-error": workflow([
    job("a", "    continue-on-error: true\n"),
    job("b"),
    gateJob(["a", "b"]),
  ]),
  "needs-unknown": workflow([
    job("a"),
    job("b", "    needs: [nope]\n"),
    gateJob(["a", "b"]),
  ]),
  "aiken-check-unguarded": clean.replace(
    `      - uses: ${checkout}\n`,
    `      - uses: ${checkout}\n      - run: cd onchain/aiken && aiken check\n`,
  ),
  "marker-reason": `${clean}# workflow-lint: allow timeout\n`,
};

test("every check has a red fixture", () => {
  assert.deepEqual(Object.keys(reds).sort(), [...checks].sort());
});

for (const [check, text] of Object.entries(reds)) {
  test(`${check}: a violating fixture is reported`, { skip }, () => {
    assert.ok(
      found(text).includes(check),
      `${check} not in ${JSON.stringify(found(text))}`,
    );
  });
}

test(
  "gate-allowlist also refuses a gate without NEEDS, without --allow, or with its own if",
  { skip },
  () => {
    assert.ok(
      found(clean.replace("NEEDS: ${{ toJSON(needs) }}", "OTHER: x")).includes(
        "gate-allowlist",
      ),
    );
    assert.ok(
      found(clean.replace(" --allow success", "")).includes("gate-allowlist"),
    );
    assert.ok(
      found(
        clean.replace(
          "      - name: gate\n",
          "      - name: gate\n        if: false\n",
        ),
      ).includes("gate-allowlist"),
    );
  },
);

test(
  "aiken-check-unguarded flags a direct check anywhere, and only a command",
  { skip },
  () => {
    const withRun = (run) =>
      found(
        clean.replace(
          `      - uses: ${checkout}\n`,
          `      - uses: ${checkout}\n      - run: |\n${run
            .split("\n")
            .map((line) => `          ${line}`)
            .join("\n")}\n`,
        ),
      );
    assert.deepEqual(
      withRun("node scripts/guard-focused-selector.mjs --all"),
      [],
    );
    assert.deepEqual(
      withRun("git ls-files -z '*.ak' | xargs -0 aiken fmt --check"),
      [],
    );
    assert.deepEqual(withRun("# a bare aiken check exits 0 on nothing"), []);
    assert.deepEqual(withRun("set -e\naiken check -m state_queue"), [
      "aiken-check-unguarded",
    ]);
    assert.deepEqual(withRun("test -d build && aiken check"), [
      "aiken-check-unguarded",
    ]);
    assert.deepEqual(
      found(
        "runs:\n  using: composite\n  steps:\n    - shell: bash\n      run: aiken check\n",
        ".github/actions/x/action.yml",
      ),
      ["aiken-check-unguarded"],
    );
  },
);

test(
  "unpinned-action scans composite actions and needs the version comment",
  { skip },
  () => {
    const action = (uses) =>
      `runs:\n  using: composite\n  steps:\n    - uses: ${uses}\n`;
    assert.deepEqual(
      found(action(checkout), ".github/actions/x/action.yml"),
      [],
    );
    assert.deepEqual(
      found(
        action("actions/checkout@11d5960a326750d5838078e36cf38b85af677262"),
        ".github/actions/x/action.yml",
      ),
      ["unpinned-action"],
    );
    assert.deepEqual(
      found(action("./.github/actions/local"), ".github/actions/x/action.yml"),
      [],
    );
  },
);

test(
  "an exemption with a reason is honoured only in its scope",
  { skip },
  () => {
    const exempted = reds.timeout.replace(
      "  a:\n",
      "  a:\n    # workflow-lint: allow timeout — fixture reason\n",
    );
    assert.deepEqual(found(exempted), []);
    const elsewhere = reds.timeout.replace(
      "  gate:\n",
      "  gate:\n    # workflow-lint: allow timeout — wrong job\n",
    );
    assert.ok(found(elsewhere).includes("timeout"));
    const unpinned = reds["unpinned-action"].replaceAll(
      "      - uses: actions/checkout@v4\n",
      "      # workflow-lint: allow unpinned-action — fixture reason\n      - uses: actions/checkout@v4\n",
    );
    assert.deepEqual(found(unpinned), []);
    assert.ok(
      found(`${clean}# workflow-lint: allow timeout —\n`).includes(
        "marker-reason",
      ),
    );
    assert.ok(
      found(`${clean}# workflow-lint: allow no-such-check — reason\n`).includes(
        "marker-reason",
      ),
    );
  },
);

test(
  "could not check is not clean: a missing yaml package or no workflows",
  { skip },
  () => {
    assert.match(
      lintWorkflows(repositoryRoot, null).couldNotCheck ?? "",
      /yaml/u,
    );
    assert.match(
      lintFixture({ "README.md": "" }).couldNotCheck ?? "",
      /no workflow files/u,
    );
  },
);

test(
  "the CLI exits 0 on the repository, 1 on findings, 2 on usage, 3 when it cannot look",
  { skip },
  () => {
    const cli = (...args) =>
      spawnSync(process.execPath, [script, ...args], { encoding: "utf8" });
    assert.equal(cli().status, 0);
    assert.equal(cli("--bogus").status, 2);
    const root = mkdtempSync(join(tmpdir(), "midgard-lint-workflows-cli-"));
    try {
      assert.equal(
        cli("--root", root).status,
        3,
        "no workflows: could not check",
      );
      mkdirSync(join(root, ".github/workflows"), { recursive: true });
      writeFileSync(join(root, ".github/workflows/w.yml"), reds.timeout);
      const red = cli("--root", root);
      assert.equal(red.status, 1);
      assert.match(red.stderr, /\[timeout\]/u);
    } finally {
      rmSync(root, { recursive: true, force: true });
    }
  },
);
