import assert from "node:assert/strict";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { runAikenCheck } from "./runner-reports.mjs";

// A stub that answers `--version` as stock Aiken and would print a green report
// for anything else: the report must never be read.
test("runAikenCheck refuses a compiler other than the pinned fork", () => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-runner-reports-"));
  try {
    const stub = join(directory, "aiken");
    writeFileSync(
      stub,
      `#!/bin/sh\nif [ "$1" = "--version" ]; then echo 'aiken v1.1.22+39d6b04'; exit 0; fi\necho '{"summary":{"total":1,"passed":1,"failed":0}}'\n`,
    );
    chmodSync(stub, 0o755);
    assert.throws(
      () =>
        runAikenCheck({
          projectRoot: directory,
          selectors: ["selftest/probe.{selftest_probe}"],
          binary: stub,
        }),
      (error) =>
        error.code === "ERR_AIKEN_NOT_PINNED" &&
        /reports 'aiken v1\.1\.22\+39d6b04'/u.test(error.message),
    );
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
