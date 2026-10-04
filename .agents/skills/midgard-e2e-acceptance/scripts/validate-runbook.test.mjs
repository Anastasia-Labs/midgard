import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  cpSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const skillDir = resolve(scriptDir, "..");
const validator = join(scriptDir, "validate-runbook.mjs");

const validate = (args = []) =>
  spawnSync(process.execPath, [validator, ...args], { encoding: "utf8" });

// Validates a copy of the skill with one document edited.
const validateEdited = (document, edit) => {
  const copy = mkdtempSync(join(tmpdir(), "e2e-runbook-"));
  try {
    cpSync(skillDir, copy, { recursive: true });
    const path = join(copy, document);
    const before = readFileSync(path, "utf8");
    const after = edit(before);
    assert.notEqual(after, before, "the edit must change the document");
    writeFileSync(path, after);
    return validate(["--skill-dir", copy]);
  } finally {
    rmSync(copy, { recursive: true, force: true });
  }
};

const live = "references/live-acceptance.md";
const recovery = "references/recovery.md";
const readiness = "references/release-readiness.md";

test("the committed runbook matches the sources", () => {
  const result = validate();
  assert.equal(result.status, 0, result.stderr);
  assert.equal(JSON.parse(result.stdout).status, "ok");
});

test("a retired hand-run command fails as stale", () => {
  const result = validateEdited(live, (text) =>
    text.replace(
      "```bash\npnpm --dir",
      '```bash\nnode "$TOOLS_CLI" e2e-run-step --id init-protocol\npnpm --dir',
    ),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /forbidden stale instruction: e2e-run-step/);
});

test("an e2e-stack flag the command does not declare fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace(
      '"$STACK_CONFIG" --setup-only',
      '"$STACK_CONFIG" --resume-from',
    ),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /undeclared e2e-stack flag: --resume-from/);
});

test("contributor plan flags cannot be passed to the underlying e2e-stack command", () => {
  const result = validateEdited(live, (text) =>
    text.replace('"$STACK_CONFIG" --setup-only', '"$STACK_CONFIG" --plan'),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /undeclared e2e-stack flag: --plan/);
});

test("a flag named on its own that no CLI declares fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace("`--check` loads", "`--dry-check` loads"),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /names a flag no CLI declares: --dry-check/);
});

test("an undeclared operator command fails", () => {
  const result = validateEdited(recovery, (text) =>
    text.replace(
      "node dist/index.js prepare-hub-oracle-one-shot-nonce",
      "node dist/index.js prepare-hub-oracle-nonce-again",
    ),
  );
  assert.equal(result.status, 1);
  assert.match(
    result.stderr,
    /Midgard CLI command is not declared: prepare-hub-oracle-nonce-again/,
  );
});

test("a missing package script fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace(
      'run e2e-stack --config "$STACK_CONFIG" --check',
      'run e2e-stack-check --config "$STACK_CONFIG"',
    ),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /package script is missing: e2e-stack-check/);
});

test("a stack step missing from the step table fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace(/^\| `storage-identity` .*\n/m, ""),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /stack step is undocumented: storage-identity/);
});

test("a step the stack does not run fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace("| `operator`   ", "| `operators`  "),
  );
  assert.equal(result.status, 1);
  assert.match(
    result.stderr,
    /documented stack step does not exist: operators/,
  );
});

test("a node command the stack does not run fails", () => {
  const result = validateEdited(live, (text) =>
    text.replace(
      "with `register-active-operator` after",
      "with `activate-operator` after",
    ),
  );
  assert.equal(result.status, 1);
  assert.match(
    result.stderr,
    /names a node command the stack does not run: activate-operator/,
  );
});

test("a stop message the stack does not print fails", () => {
  const result = validateEdited(recovery, (text) =>
    text.replace("`Malformed stack checkpoint`", "`Corrupt stack checkpoint`"),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /stop message the stack does not print/);
});

test("a dropped release-readiness gate fails", () => {
  const result = validateEdited(readiness, (text) =>
    text.replaceAll("watcher_crash_rollback_matrix", "crash_matrix"),
  );
  assert.equal(result.status, 1);
  assert.match(
    result.stderr,
    /state-correction gate watcher_crash_rollback_matrix/,
  );
});

test("an undocumented finalizer stack gate fails", () => {
  const result = validateEdited(readiness, (text) =>
    text.replace("- `stack_deposit_credit`: ", "- deposit credit: "),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /finalizer stack gate stack_deposit_credit/);
});

test("a stack gate the finalizer does not derive fails", () => {
  const result = validateEdited(readiness, (text) =>
    text.replace("`stack_settlement`", "`stack_settlement_rows`"),
  );
  assert.equal(result.status, 1);
  assert.match(
    result.stderr,
    /names a stack gate the finalizer dropped: stack_settlement_rows/,
  );
});

test("a runbook that omits how to point the finalizer at a stack run fails", () => {
  const result = validateEdited(readiness, (text) =>
    text.replaceAll("`--stack-config`", "the stack configuration"),
  );
  assert.equal(result.status, 1);
  assert.match(result.stderr, /finalizer stack input: `--stack-config`/);
});
