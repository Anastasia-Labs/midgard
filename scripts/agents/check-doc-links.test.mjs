import assert from "node:assert/strict";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { checkDocLinks, documentationFiles } from "./check-doc-links.mjs";
import {
  fixtureRepository,
  isolateGit,
  runScript,
  temporaryDirectory,
} from "./fixture-repository.mjs";

isolateGit();

const here = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(here, "../..");
const script = join(here, "check-doc-links.mjs");

const layout = {
  ".gitignore": "artifacts/\n",
  "src/index.ts": "export {};\n",
  "pkg/package.json": "{}\n",
  "pkg/.gitignore": "docs/\nbuild/\n",
  "pkg/lib/proof.ak": "\n",
  "docs/guide/other.md": "# Other\n",
};

const check = (guide, extra = {}, untracked = {}) =>
  checkDocLinks(
    fixtureRepository(
      { ...layout, "docs/guide/index.md": `# Guide\n\n${guide}\n`, ...extra },
      untracked,
    ),
  ).findings;

test("references that resolve pass", () => {
  assert.deepEqual(
    check(
      [
        "See [other](other.md), [root](/src/index.ts) and [section](#guide).",
        "The entry point is `src/index.ts`; from `pkg`, run `lib/proof.ak`.",
        "The directory `src/` holds sources; `other.md` has no slash.",
        "An anchor or line suffix is stripped: `src/index.ts:12`, `src/index.ts#L3`,",
        "`src/index.ts:65-71,93-98`, `src/index.ts:183–187`.",
      ].join("\n\n"),
    ),
    [],
  );
});

test("a broken link is a finding", () => {
  assert.deepEqual(check("See [gone](gone.md)."), [
    "docs/guide/index.md:3: missing link gone.md",
  ]);
});

test("a link that leaves the repository is a finding", () => {
  assert.deepEqual(check("See [up](../../../outside.md)."), [
    "docs/guide/index.md:3: missing link ../../../outside.md",
  ]);
});

test("a missing backticked path is a finding, rooted or relative", () => {
  assert.deepEqual(check("Run `src/missing.ts`.\n\nRead `notes/plan.md`."), [
    "docs/guide/index.md:3: missing path `src/missing.ts`",
    "docs/guide/index.md:5: missing path `notes/plan.md`",
  ]);
});

test("a path that exists only untracked is a finding", () => {
  assert.deepEqual(check("Run `src/new.ts`.", {}, { "src/new.ts": "" }), [
    "docs/guide/index.md:3: missing path `src/new.ts`",
  ]);
});

test("placeholders, globs, commit-qualified paths and assignments are skipped", () => {
  assert.deepEqual(
    check(
      "`src/<name>.ts`, `src/*.ts`, `src/{a,b}.ts`, `src/.../x.ts`, `$ROOT/x.ts`, `abc1234:src/gone.ts`, `FILE=src/gone.ts`, `.git/hooks/x.sh`",
    ),
    [],
  );
});

test("a relative path that is the tail of a tracked path passes", () => {
  assert.deepEqual(check("The proof lives in `lib/proof.ak`."), []);
});

test("paths git ignores are accepted as local outputs", () => {
  assert.deepEqual(check("Logs land in `artifacts/run/log.json`."), []);
});

test("a rooted path counts as ignored only from the repository root", () => {
  // pkg/.gitignore ignores pkg/docs/, but `docs/...` names the root docs/.
  assert.deepEqual(check("Read `docs/missing.md`."), [
    "docs/guide/index.md:3: missing path `docs/missing.md`",
  ]);
});

test("the generated blueprint is accepted without being tracked", () => {
  assert.deepEqual(
    check("Build `onchain/aiken/plutus.json` first.", {
      "onchain/aiken/aiken.toml": "\n",
    }),
    [],
  );
});

test("each skip marker skips only the block it annotates", () => {
  const findings = check(
    [
      "<!-- doc-links:future -->",
      "Write `src/future.ts` later.",
      "",
      "- `src/historical.ts` was deleted. <!-- doc-links:historical -->",
      "- From another repository: `other/tool.py`. <!-- doc-links:external -->",
      "- Still checked: `src/unmarked.ts`.",
    ].join("\n"),
  );
  assert.deepEqual(findings, [
    "docs/guide/index.md:8: missing path `src/unmarked.ts`",
  ]);
});

test("a run-relative file skips relative tokens but still checks rooted ones", () => {
  const findings = check(
    "<!-- doc-links:run-relative -->\n\nWrote `run/result.json` and `src/gone.ts`.",
  );
  assert.deepEqual(findings, [
    "docs/guide/index.md:5: missing path `src/gone.ts`",
  ]);
});

test("fenced code is not checked", () => {
  assert.deepEqual(
    check("```sh\ncat src/missing.ts `src/missing.ts`\n```"),
    [],
  );
});

test("the files checked are the documentation ones", () => {
  assert.deepEqual(
    documentationFiles(
      new Set([
        "AGENTS.md",
        "demo/AGENTS.md",
        "CLAUDE.md",
        ".agents/skills/build/references/notes.md",
        "docs/a.md",
        "docs/site/b.mdx",
        "README.md",
        "demo/README.md",
      ]),
    ),
    [
      ".agents/skills/build/references/notes.md",
      "AGENTS.md",
      "CLAUDE.md",
      "demo/AGENTS.md",
      "docs/a.md",
      "docs/site/b.mdx",
    ],
  );
});

test("the command line exits 1 on findings and 0 when clean", () => {
  const dirty = fixtureRepository({ "docs/a.md": "See [x](x.md).\n" });
  const failed = runScript(script, ["--root", dirty]);
  assert.equal(failed.status, 1, failed.stderr);
  assert.match(failed.stderr, /missing link x\.md/u);

  const clean = fixtureRepository({ "docs/a.md": "See [a](a.md).\n" });
  const passed = runScript(script, ["--root", clean]);
  assert.equal(passed.status, 0, passed.stderr);
});

test("could not look is exit 2, never a pass", () => {
  const notARepository = runScript(script, [
    "--root",
    temporaryDirectory("midgard-agent-plain-"),
  ]);
  assert.equal(notARepository.status, 2, notARepository.stderr);
  assert.match(notARepository.stderr, /could not look/u);

  const nothingTracked = runScript(script, ["--root", fixtureRepository({})]);
  assert.equal(nothingTracked.status, 2, nothingTracked.stderr);
});

test("this repository's documentation references resolve", () => {
  assert.deepEqual(checkDocLinks(repositoryRoot).findings, []);
});
