import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  utimesSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

const DAY_SECONDS = 86_400;

test("a new run folder clears run folders older than a week and keeps newer ones", (t) => {
  const scratch = realpathSync(
    mkdtempSync(join(tmpdir(), "midgard-run-folders-")),
  );
  t.after(() => rmSync(scratch, { recursive: true, force: true }));
  const registry = join(scratch, `midgard-contrib-${process.getuid()}`);
  const runs = join(registry, "runs");
  mkdirSync(runs, { recursive: true });
  chmodSync(registry, 0o700);
  const run = (name, days) => {
    const path = join(runs, name);
    mkdirSync(path);
    const when = Date.now() / 1000 - days * DAY_SECONDS;
    utimesSync(path, when, when);
    return path;
  };
  const old = run("old", 8);
  const recent = run("recent", 6);
  const created = execFileSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `import { runDirectory } from ${JSON.stringify(
        new URL("./build.mjs", import.meta.url).href,
      )}; process.stdout.write(runDirectory());`,
    ],
    { env: { ...process.env, TMPDIR: scratch }, encoding: "utf8" },
  );
  assert.equal(existsSync(old), false);
  assert.equal(existsSync(recent), true);
  assert.equal(existsSync(created), true);
});
