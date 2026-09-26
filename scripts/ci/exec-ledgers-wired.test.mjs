// Every execution ledger is re-measured in CI, and derived from the files
// rather than from a hand-kept list: each `onchain/aiken/scripts/*-exec-ledger-v1.json`
// must be read by some `verify-*-exec-ledger-v1.mjs`, and each such verifier
// must run as its own step of `.github/workflows/aiken-ci.yml`, guarded with
// `!cancelled()` so one red ledger cannot hide the verdict of the next. Three
// ledgers once sat in the tree with a verifier and no step, which is how a
// ledger stops being a measurement.

import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);

const loadYaml = () => {
  try {
    return createRequire(join(repositoryRoot, "demo/package.json"))("yaml");
  } catch {
    return undefined;
  }
};
const yaml = loadYaml();
const skip =
  yaml === undefined && process.env.GITHUB_ACTIONS !== "true"
    ? "could not check: yaml absent (run `pnpm --dir demo install`)"
    : false;

const scriptsDir = "onchain/aiken/scripts";
const workflow = ".github/workflows/aiken-ci.yml";

export const findUnwiredLedgers = (root) => {
  const dir = join(root, scriptsDir);
  const names = readdirSync(dir);
  const ledgers = names.filter((name) => /-exec-ledger-v1\.json$/u.test(name));
  const verifiers = names.filter((name) =>
    /^verify-.+-exec-ledger-v1\.mjs$/u.test(name),
  );
  const problems = [];
  if (ledgers.length === 0)
    problems.push(`no execution ledgers found in ${scriptsDir}`);
  const sources = new Map(
    verifiers.map((name) => [name, readFileSync(join(dir, name), "utf8")]),
  );
  for (const ledger of ledgers) {
    if (![...sources.values()].some((source) => source.includes(ledger))) {
      problems.push(`${ledger} is read by no verify-*-exec-ledger-v1.mjs`);
    }
  }
  const document = yaml.parse(readFileSync(join(root, workflow), "utf8"));
  const steps = Object.values(document?.jobs ?? {}).flatMap(
    (job) => job?.steps ?? [],
  );
  for (const verifier of verifiers) {
    const invocation = new RegExp(
      `(^|\\s)node\\s+scripts/${verifier.replaceAll(".", "\\.")}(\\s|$)`,
      "mu",
    );
    const running = steps.filter(
      (step) => typeof step?.run === "string" && invocation.test(step.run),
    );
    if (running.length === 0) {
      problems.push(`${verifier} is not run by ${workflow}`);
      continue;
    }
    for (const step of running) {
      if (!String(step.if ?? "").includes("!cancelled()")) {
        problems.push(
          `${verifier} runs in a step without \`!cancelled()\`, so an earlier red step hides it`,
        );
      }
    }
  }
  return problems;
};

const fixtureRoot = ({ ledgers, verifiers, steps }) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-ledgers-wired-"));
  mkdirSync(join(root, scriptsDir), { recursive: true });
  mkdirSync(join(root, ".github/workflows"), { recursive: true });
  for (const ledger of ledgers)
    writeFileSync(join(root, scriptsDir, ledger), "{}");
  for (const [name, body] of Object.entries(verifiers)) {
    writeFileSync(join(root, scriptsDir, name), body);
  }
  writeFileSync(
    join(root, workflow),
    yaml.stringify({ jobs: { aiken: { "runs-on": "ubuntu-latest", steps } } }),
  );
  return root;
};

const guarded = "${{ !cancelled() && steps.compiler.outcome == 'success' }}";

test(
  "every execution ledger in the repository is verified by a guarded aiken-ci step",
  { skip },
  () => {
    assert.deepEqual(findUnwiredLedgers(repositoryRoot), []);
  },
);

test(
  "a ledger no verifier reads, an unrun verifier and an unguarded step are each reported",
  { skip },
  () => {
    const root = fixtureRoot({
      ledgers: [
        "a-exec-ledger-v1.json",
        "orphan-exec-ledger-v1.json",
        "b-exec-ledger-v1.json",
      ],
      verifiers: {
        "verify-a-exec-ledger-v1.mjs": "read('a-exec-ledger-v1.json')",
        "verify-b-exec-ledger-v1.mjs": "read('b-exec-ledger-v1.json')",
        "verify-c-exec-ledger-v1.mjs": "read('b-exec-ledger-v1.json')",
      },
      steps: [
        { run: "node scripts/verify-a-exec-ledger-v1.mjs", if: guarded },
        { run: "node scripts/verify-b-exec-ledger-v1.mjs" },
      ],
    });
    try {
      const problems = findUnwiredLedgers(root);
      assert.equal(problems.length, 3, problems.join("\n"));
      assert.match(
        problems.join("\n"),
        /orphan-exec-ledger-v1\.json is read by no/u,
      );
      assert.match(
        problems.join("\n"),
        /verify-c-exec-ledger-v1\.mjs is not run/u,
      );
      assert.match(
        problems.join("\n"),
        /verify-b-exec-ledger-v1\.mjs runs in a step without/u,
      );
    } finally {
      rmSync(root, { recursive: true, force: true });
    }
  },
);

test("a tree with no ledgers at all is a finding, not a pass", { skip }, () => {
  const root = fixtureRoot({ ledgers: [], verifiers: {}, steps: [] });
  try {
    assert.match(
      findUnwiredLedgers(root).join("\n"),
      /no execution ledgers found/u,
    );
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});
