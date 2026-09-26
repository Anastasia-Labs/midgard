// Exercises scripts/ci/build-aiken-fork.sh without building anything: `cargo`
// and `rustup` are stubs on PATH that record their arguments and "install" a
// stub compiler, so the test proves the recipe the script hands cargo, the
// cache short-circuit, and the post-build identity assertion.

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import { pinnedAikenFork } from "../../onchain/aiken/scripts/pinned-compiler.mjs";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const script = join(repositoryRoot, "scripts/ci/build-aiken-fork.sh");
const pin = pinnedAikenFork();

const writeExecutable = (path, body) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `#!/usr/bin/env bash\n${body}\n`);
  chmodSync(path, 0o755);
};

const stubCompiler = (path, version) =>
  writeExecutable(path, `echo '${version}'`);

// A fake cargo whose `install` writes a compiler reporting `installs`.
const withSandbox = (installs, callback) => {
  const sandbox = mkdtempSync(join(tmpdir(), "midgard-build-aiken-fork-"));
  try {
    const bin = join(sandbox, "bin");
    const log = join(sandbox, "cargo.log");
    writeExecutable(
      join(bin, "cargo"),
      [
        `echo "$*" >> '${log}'`,
        'if [ "$2" = "--version" ]; then exit 0; fi',
        'root=""; while [ "$#" -gt 0 ]; do if [ "$1" = "--root" ]; then root="$2"; fi; shift; done',
        `mkdir -p "$root/bin"; printf '#!/usr/bin/env bash\\necho "%s"\\n' '${installs}' > "$root/bin/aiken"; chmod +x "$root/bin/aiken"`,
      ].join("\n"),
    );
    writeExecutable(join(bin, "rustup"), `echo "rustup $*" >> '${log}'`);
    const run = (...args) =>
      spawnSync("bash", [script, ...args], {
        encoding: "utf8",
        env: {
          ...process.env,
          PATH: `${bin}:${dirname(process.execPath)}:/usr/bin:/bin`,
        },
      });
    return callback({ sandbox, log, run });
  } finally {
    rmSync(sandbox, { recursive: true, force: true });
  }
};

test("--help succeeds and a missing --prefix is a usage error, never a default install root", () => {
  withSandbox(pin.version, ({ run, log }) => {
    assert.equal(run("--help").status, 0);
    const missing = run();
    assert.equal(missing.status, 2);
    assert.match(missing.stderr, /--prefix is required/u);
    assert.equal(run("--nonsense").status, 2);
    assert.equal(existsSync(log), false, "no cargo invocation on usage errors");
  });
});

test("--print-pin prints the workflows' fork coordinates", () => {
  withSandbox(pin.version, ({ run }) => {
    const printed = run("--print-pin");
    assert.equal(printed.status, 0, printed.stderr);
    assert.match(
      printed.stdout,
      new RegExp(`^AIKEN_FORK_REV=${pin.rev}$`, "mu"),
    );
  });
});

test("a prefix already holding the pinned compiler is left alone", () => {
  withSandbox(pin.version, ({ sandbox, log, run }) => {
    const prefix = join(sandbox, "fork");
    stubCompiler(join(prefix, "bin/aiken"), pin.version);
    const result = run("--prefix", prefix);
    assert.equal(result.status, 0, result.stderr);
    assert.match(result.stdout, /nothing to build/u);
    assert.equal(existsSync(log), false, "cargo must not run on a cache hit");
  });
});

test("a stale prefix is rebuilt from the pinned rev with --locked into that prefix only", () => {
  withSandbox(pin.version, ({ sandbox, log, run }) => {
    const prefix = join(sandbox, "fork");
    stubCompiler(join(prefix, "bin/aiken"), "aiken v1.1.22+39d6b04");
    const result = run("--prefix", prefix);
    assert.equal(result.status, 0, result.stderr);
    const install = readFileSync(log, "utf8")
      .split("\n")
      .find((line) => line.includes(" install "));
    assert.ok(install, "cargo install ran");
    for (const expected of [
      "+1.94.1",
      `--git ${pin.repo}`,
      `--rev ${pin.rev}`,
      "--locked",
      `--root ${prefix}`,
    ]) {
      assert.ok(install.includes(expected), `${expected} in '${install}'`);
    }
  });
});

test("a build that does not report the pin fails", () => {
  withSandbox("aiken v1.1.22+39d6b04", ({ sandbox, run }) => {
    const result = run("--prefix", join(sandbox, "fork"));
    assert.equal(result.status, 1);
    assert.match(result.stderr, /not the pinned/u);
  });
});
