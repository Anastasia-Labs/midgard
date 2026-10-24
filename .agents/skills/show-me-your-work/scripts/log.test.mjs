import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { appendDecision, HEADER } from "./log.mjs";

test("preserves earlier decisions and escapes multiline/formula cells", (t) => {
  const dir = mkdtempSync(join(tmpdir(), "pstack-log-"));
  t.after(() => rmSync(dir, { recursive: true }));
  const file = join(dir, "decisions.tsv");
  appendDecision(
    file,
    ["start", "bounded scope", "review", "commit abc", "open"],
    new Date("2026-10-03T12:00:00Z"),
  );
  const before = readFileSync(file, "utf8");
  appendDecision(
    file,
    ["verify", "=1+1", "a\tb\nc", " @formula", "passed"],
    new Date("2026-10-03T12:01:00Z"),
  );
  assert.equal(
    readFileSync(file, "utf8"),
    before +
      "2026-10-03T12:01:00.000Z\tverify\t'=1+1\ta b c\t' @formula\tpassed\n",
  );
  assert.equal(
    before,
    HEADER +
      "2026-10-03T12:00:00.000Z\tstart\tbounded scope\treview\tcommit abc\topen\n",
  );
});

test("refuses to corrupt an existing foreign file or accept an incomplete row", (t) => {
  const dir = mkdtempSync(join(tmpdir(), "pstack-log-"));
  t.after(() => rmSync(dir, { recursive: true }));
  const file = join(dir, "user.tsv");
  writeFileSync(file, "user work\n");
  assert.throws(
    () => appendDecision(file, ["a", "b", "c", "d", "e"]),
    /invalid header/u,
  );
  assert.throws(() => appendDecision(file, ["a"]), /Expected/u);
  assert.equal(readFileSync(file, "utf8"), "user work\n");
});
