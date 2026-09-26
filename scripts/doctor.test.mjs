import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  appendFileSync,
  chmodSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  checkHooks,
  checkNode,
  checkPnpm,
  EXIT,
  formatReport,
  main,
  runDoctor,
} from "./doctor.mjs";

// checkHooks runs git with this process's environment; keep the developer's
// own configuration out of it.
process.env.GIT_CONFIG_GLOBAL = "/dev/null";
process.env.GIT_CONFIG_NOSYSTEM = "1";

const git = (cwd, ...args) => {
  const run = spawnSync("git", args, {
    cwd,
    encoding: "utf8",
    env: {
      ...process.env,
      GIT_AUTHOR_NAME: "t",
      GIT_AUTHOR_EMAIL: "t@example.invalid",
      GIT_COMMITTER_NAME: "t",
      GIT_COMMITTER_EMAIL: "t@example.invalid",
    },
  });
  assert.equal(run.status, 0, run.stderr);
};

const HOOK = "#!/usr/bin/env bash\nexit 0\n";

// A repository whose versioned pre-commit is installed as a symlink, the way
// .githooks/install does it.
const withRepo = (body) => {
  const scratch = realpathSync(
    mkdtempSync(join(tmpdir(), "doctor-hooks-test-")),
  );
  try {
    const repo = join(scratch, "repo");
    mkdirSync(join(repo, ".githooks"), { recursive: true });
    git(repo, "init", "--quiet");
    writeFileSync(join(repo, ".githooks/pre-commit"), HOOK);
    chmodSync(join(repo, ".githooks/pre-commit"), 0o755);
    git(repo, "add", "--", ".githooks/pre-commit");
    git(repo, "commit", "--quiet", "-m", "hooks");
    mkdirSync(join(repo, ".git/hooks"), { recursive: true });
    symlinkSync(
      join(repo, ".githooks/pre-commit"),
      join(repo, ".git/hooks/pre-commit"),
    );
    return body(repo, scratch);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

const byName = (rows) => Object.fromEntries(rows.map((r) => [r.name, r]));

test("installed, executable hooks pass", () =>
  withRepo((repo) => {
    const rows = checkHooks({ root: repo });
    assert.deepEqual(
      rows.map((r) => r.status),
      ["ok"],
    );
  }));

test("a hook that lost its executable bit fails with chmod +x on the resolved file", () =>
  withRepo((repo) => {
    chmodSync(join(repo, ".githooks/pre-commit"), 0o644);
    const row = byName(checkHooks({ root: repo }))["hooks:pre-commit"];
    assert.equal(row.status, "failed");
    assert.match(row.detail, /is not executable, so git skips it/u);
    assert.equal(row.fix, `chmod +x ${join(repo, ".githooks/pre-commit")}`);
  }));

test("a non-executable <hook>.local that the pre-commit chains to fails", () =>
  withRepo((repo) => {
    writeFileSync(join(repo, ".git/hooks/pre-commit.local"), HOOK);
    chmodSync(join(repo, ".git/hooks/pre-commit.local"), 0o644);
    const row = byName(checkHooks({ root: repo }))["hooks:pre-commit.local"];
    assert.equal(row.status, "failed");
    assert.equal(
      row.fix,
      `chmod +x ${join(repo, ".git/hooks/pre-commit.local")}`,
    );
  }));

test("a dangling hook symlink and an uninstalled versioned hook fail", () =>
  withRepo((repo) => {
    rmSync(join(repo, ".git/hooks/pre-commit"));
    symlinkSync(
      join(repo, ".githooks/gone"),
      join(repo, ".git/hooks/pre-commit"),
    );
    writeFileSync(join(repo, ".githooks/pre-push"), HOOK);
    chmodSync(join(repo, ".githooks/pre-push"), 0o755);
    git(repo, "add", "--", ".githooks/pre-push");
    git(repo, "commit", "--quiet", "-m", "pre-push");
    const rows = byName(checkHooks({ root: repo }));
    assert.equal(rows["hooks:pre-commit"].status, "failed");
    assert.match(rows["hooks:pre-commit"].detail, /points at nothing/u);
    assert.equal(rows["hooks:pre-push"].status, "failed");
    assert.match(rows["hooks:pre-push"].detail, /not installed/u);
    assert.equal(rows["hooks:pre-push"].fix, "bash .githooks/install");
  }));

test("a hook committed without its executable bit fails with update-index", () =>
  withRepo((repo) => {
    writeFileSync(join(repo, ".githooks/commit-msg"), HOOK);
    chmodSync(join(repo, ".githooks/commit-msg"), 0o644);
    git(repo, "add", "--", ".githooks/commit-msg");
    git(repo, "commit", "--quiet", "-m", "commit-msg");
    const row = byName(checkHooks({ root: repo }))["hooks:commit-msg"];
    // Both findings may appear under one name; the index one is first.
    assert.equal(row.status, "failed");
    const rows = checkHooks({ root: repo }).filter(
      (r) => r.name === "hooks:commit-msg",
    );
    assert.ok(
      rows.some(
        (r) => r.fix === "git update-index --chmod=+x .githooks/commit-msg",
      ),
    );
  }));

test("core.hooksPath is honoured, and a hook from another checkout warns", () =>
  withRepo((repo, scratch) => {
    const other = join(scratch, "other/.githooks");
    mkdirSync(other, { recursive: true });
    writeFileSync(join(other, "pre-commit"), HOOK);
    chmodSync(join(other, "pre-commit"), 0o755);
    const custom = join(scratch, "custom-hooks");
    mkdirSync(custom);
    symlinkSync(join(other, "pre-commit"), join(custom, "pre-commit"));
    // Written into the temporary repository's own config file.
    appendFileSync(
      join(repo, ".git/config"),
      `[core]\n\thooksPath = ${custom}\n`,
    );
    const row = byName(checkHooks({ root: repo }))["hooks:pre-commit"];
    assert.equal(row.status, "warn");
    assert.match(row.detail, /another checkout's copy/u);
  }));

test("outside a repository the hooks check is unknown, not ok", () => {
  const scratch = mkdtempSync(join(tmpdir(), "doctor-norepo-"));
  try {
    const rows = checkHooks({ root: scratch });
    assert.equal(rows[0].status, "unknown");
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
});

const withDemo = (body) => {
  const scratch = mkdtempSync(join(tmpdir(), "doctor-versions-"));
  try {
    mkdirSync(join(scratch, "demo"));
    writeFileSync(join(scratch, "demo/.nvmrc"), "22.22.2\n");
    writeFileSync(
      join(scratch, "demo/package.json"),
      JSON.stringify({
        engines: { node: ">=22.16.0" },
        packageManager: "pnpm@9.15.4+sha512.abc",
      }),
    );
    return body(scratch);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
};

test("node: pinned is ok, other versions warn, below the floor fails", () =>
  withDemo((root) => {
    assert.equal(checkNode({ root, version: "v22.22.2" }).status, "ok");
    const newer = checkNode({ root, version: "v24.13.1" });
    assert.equal(newer.status, "warn");
    assert.equal(newer.fix, "nvm install 22.22.2 && nvm use 22.22.2");
    assert.equal(checkNode({ root, version: "v20.1.0" }).status, "failed");
    assert.equal(
      checkNode({ root: join(root, "nowhere"), version: "v22.22.2" }).status,
      "unknown",
    );
  }));

test("pnpm: the packageManager version passes, anything else fails or is unknown", () =>
  withDemo((root) => {
    const at = (reported) => checkPnpm({ root, run: () => reported });
    assert.equal(at({ version: "9.15.4" }).status, "ok");
    const wrong = at({ version: "11.9.0" });
    assert.equal(wrong.status, "failed");
    assert.match(wrong.fix, /corepack prepare pnpm@9\.15\.4 --activate/u);
    assert.equal(
      at({ error: "spawn pnpm ENOENT", missing: true }).status,
      "failed",
    );
    assert.equal(at({ error: "timed out" }).status, "unknown");
    assert.equal(at({ version: "garbage" }).status, "unknown");
  }));

const fixed = (status) => () => ({
  name: status,
  status,
  detail: `${status} detail`,
  fix: `fix ${status}`,
});

test("exit codes: failed 1, unknown 3, warnings alone 0", async () => {
  const root = "/nonexistent";
  const code = async (statuses) =>
    (
      await runDoctor({
        root,
        checks: Object.fromEntries(statuses.map((s) => [s, fixed(s)])),
      })
    ).exitCode;
  assert.equal(await code(["ok", "warn"]), EXIT.ok);
  assert.equal(await code(["ok", "unknown", "warn"]), EXIT.unknown);
  assert.equal(await code(["unknown", "failed"]), EXIT.failed);
  const thrown = await runDoctor({
    root,
    checks: {
      boom: () => {
        throw new Error("kaput");
      },
    },
  });
  assert.equal(thrown.rows[0].status, "unknown");
  assert.equal(thrown.exitCode, EXIT.unknown);
});

test("--report-only is compact, prints every fix, and always exits 0", async () => {
  let out = "";
  const exitCode = await main(["--report-only"], {
    root: "/nonexistent",
    stdout: (text) => {
      out += text;
    },
    stderr: () => {},
    checks: { ok: fixed("ok"), failed: fixed("failed") },
  });
  assert.equal(exitCode, 0);
  assert.match(out, /1 item\(s\) need attention \(ok: ok\)/u);
  assert.match(out, /fix: fix failed/u);
  const full = await main([], {
    root: "/nonexistent",
    stdout: () => {},
    stderr: () => {},
    checks: { failed: fixed("failed") },
  });
  assert.equal(full, EXIT.failed);
  assert.match(
    formatReport({ rows: [fixed("warn")()], exitCode: 0 }, { compact: false }),
    /WARN warn: warn detail\n {5}fix: fix warn/u,
  );
});

test("the tracked session-start hook only runs doctor --report-only", () => {
  const settings = JSON.parse(
    readFileSync(
      join(dirname(fileURLToPath(import.meta.url)), "../.claude/settings.json"),
      "utf8",
    ),
  );
  // No permissions or other keys: the file is shared by every session.
  assert.deepEqual(Object.keys(settings), ["hooks"]);
  assert.deepEqual(Object.keys(settings.hooks), ["SessionStart"]);
  const hooks = settings.hooks.SessionStart.flatMap((entry) => entry.hooks);
  assert.deepEqual(
    hooks.map((hook) => hook.command),
    ['node "$CLAUDE_PROJECT_DIR/scripts/doctor.mjs" --report-only'],
  );
});
