import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  blueprintHash,
  blueprintSourceHash,
  buildRecordPath,
  checkBlueprintStamp,
} from "./blueprint-stamp.mjs";

const pin = "aiken v1.1.23+5adf783";
const stampPath = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "blueprint-stamp.mjs",
);

// A minimal tree with everything the stamp reads: both pinned workflows, the
// Aiken project inputs, a blueprint, and a build record written the way
// `deployment-profiles.mjs build` writes it.
const withTree = (run) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-blueprint-stamp-"));
  try {
    const write = (path, contents) => {
      mkdirSync(dirname(join(root, path)), { recursive: true });
      writeFileSync(join(root, path), contents);
    };
    for (const workflow of ["aiken-ci.yml", "midgard-node-ci.yml"]) {
      write(
        `.github/workflows/${workflow}`,
        `env:\n  AIKEN_FORK_VERSION: ${pin}\n`,
      );
    }
    write("onchain/aiken/aiken.toml", 'name = "midgard/selftest"\n');
    write("onchain/aiken/aiken.lock", "# lock\n");
    write("onchain/aiken/lib/midgard/probe.ak", "pub const x = 1\n");
    write("onchain/aiken/validators/probe.ak", "validator probe { }\n");
    write("onchain/aiken/env/default.ak", "pub const network = 0\n");
    write("onchain/aiken/plutus.json", '{"validators":[]}\n');
    const blueprintPath = join(root, "onchain/aiken/plutus.json");
    const writeRecord = (overrides = {}) =>
      writeFileSync(
        buildRecordPath(blueprintPath),
        JSON.stringify({
          profile: { name: "preprod-testing" },
          blueprintHash: blueprintHash(blueprintPath),
          sourceHash: blueprintSourceHash(root),
          compiler: pin,
          ...overrides,
        }),
      );
    writeRecord();
    run({ root, blueprintPath, write, writeRecord });
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
};

test("a blueprint built from the current sources by the pin is fresh", () => {
  withTree(({ root }) => {
    assert.equal(checkBlueprintStamp({ root }).status, "fresh");
  });
});

test("any change to a compiler input makes the blueprint stale", () => {
  for (const path of [
    "onchain/aiken/lib/midgard/probe.ak",
    "onchain/aiken/validators/probe.ak",
    "onchain/aiken/env/default.ak",
    "onchain/aiken/aiken.toml",
    "onchain/aiken/aiken.lock",
    "onchain/aiken/validators/added.ak",
  ]) {
    withTree(({ root, write }) => {
      write(path, "changed\n");
      const verdict = checkBlueprintStamp({ root });
      assert.equal(verdict.status, "stale", path);
      assert.match(verdict.detail, /sources .* changed/u);
      assert.equal(
        verdict.fix,
        "pnpm --dir demo deployment:build preprod-testing",
      );
    });
  }
});

test("a change outside the compiler inputs leaves it fresh", () => {
  withTree(({ root, write }) => {
    write("onchain/aiken/scripts/helper.mjs", "export {};\n");
    write("onchain/aiken/build/packages/x", "cache\n");
    assert.equal(checkBlueprintStamp({ root }).status, "fresh");
  });
});

test("a blueprint built by another compiler is stale and names both", () => {
  withTree(({ root, writeRecord }) => {
    writeRecord({ compiler: "aiken v1.1.22+39d6b04" });
    const verdict = checkBlueprintStamp({ root });
    assert.equal(verdict.status, "stale");
    assert.match(
      verdict.detail,
      /built by 'aiken v1\.1\.22\+39d6b04', not the pinned 'aiken v1\.1\.23\+5adf783'/u,
    );
  });
});

test("a blueprint edited after its record, or an unstamped record, is stale", () => {
  withTree(({ root, write }) => {
    write("onchain/aiken/plutus.json", '{"validators":[{}]}\n');
    assert.match(checkBlueprintStamp({ root }).detail, /modified after/u);
  });
  withTree(({ root, writeRecord }) => {
    writeRecord({ sourceHash: undefined, compiler: undefined });
    const verdict = checkBlueprintStamp({ root });
    assert.equal(verdict.status, "stale");
    assert.match(verdict.detail, /predates source and compiler stamping/u);
  });
});

test("no record, or no blueprint, is missing; only the latter is absent", () => {
  withTree(({ root, blueprintPath }) => {
    unlinkSync(buildRecordPath(blueprintPath));
    const verdict = checkBlueprintStamp({ root });
    assert.equal(verdict.status, "missing");
    assert.equal(verdict.blueprintAbsent, undefined);
    unlinkSync(blueprintPath);
    assert.equal(checkBlueprintStamp({ root }).blueprintAbsent, true);
  });
});

test("an unreadable record cannot be judged rather than passing", () => {
  withTree(({ root, blueprintPath }) => {
    writeFileSync(buildRecordPath(blueprintPath), "not json");
    assert.equal(checkBlueprintStamp({ root }).status, "unknown");
  });
});

test("the CLI exits 1 on a stale blueprint and names the rebuild command", () => {
  withTree(({ blueprintPath, write }) => {
    write("onchain/aiken/validators/probe.ak", "changed\n");
    // The CLI judges against its own repository's pin and sources, so point it
    // at a blueprint whose record cannot match them.
    const result = spawnSync(process.execPath, [stampPath, blueprintPath], {
      encoding: "utf8",
    });
    assert.equal(result.status, 1);
    assert.match(result.stderr, /blueprint stale/u);
    assert.match(
      result.stderr,
      /Rebuild it: pnpm --dir demo deployment:build/u,
    );
  });
});
