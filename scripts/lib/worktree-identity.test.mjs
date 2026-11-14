import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  hostPortOffset,
  testDatabasePrefix,
  worktreeIdentity,
} from "./worktree-identity.mjs";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);
const cli = join(repositoryRoot, "scripts/lib/worktree-identity.mjs");
const typescriptTwin = join(
  repositoryRoot,
  "demo/midgard-node/tests/worktree-identity.ts",
);

const sha8 = (text) =>
  createHash("sha256").update(text).digest("hex").slice(0, 8);

// A main checkout (`.git` directory), a linked worktree (`.git` file) and a
// directory outside any checkout, each with a nested subdirectory.
const withCheckouts = (callback) => {
  const base = realpathSync(
    mkdtempSync(join(tmpdir(), "midgard-worktree-id-")),
  );
  try {
    const main = join(base, "Main Checkout");
    const linked = join(base, "linked_wt");
    const loose = join(base, "loose");
    mkdirSync(join(main, ".git"), { recursive: true });
    mkdirSync(join(main, "demo/pkg"), { recursive: true });
    mkdirSync(join(linked, "demo/pkg"), { recursive: true });
    writeFileSync(
      join(linked, ".git"),
      `gitdir: ${join(main, ".git/worktrees/linked_wt")}\n`,
    );
    mkdirSync(loose);
    return callback({ main, linked, loose });
  } finally {
    rmSync(base, { recursive: true, force: true });
  }
};

test("a main checkout keeps the historical defaults", () => {
  withCheckouts(({ main }) => {
    const identity = worktreeIdentity(join(main, "demo/pkg"));
    assert.deepEqual(identity, {
      root: main,
      isMainCheckout: true,
      slug: "main-checkout",
      hash: sha8(main),
    });
    assert.equal(testDatabasePrefix("midgard_test", identity), "midgard_test");
    assert.equal(hostPortOffset(identity), 0);
  });
});

test("a linked worktree derives its own prefix and port offset", () => {
  withCheckouts(({ linked }) => {
    const identity = worktreeIdentity(join(linked, "demo/pkg"));
    assert.equal(identity.root, linked);
    assert.equal(identity.isMainCheckout, false);
    assert.equal(identity.slug, "linked-wt");
    assert.equal(identity.hash, sha8(linked));
    assert.equal(
      testDatabasePrefix("midgard_test", identity),
      `midgard_test_${sha8(linked)}`,
    );
    const offset = hostPortOffset(identity);
    assert.ok(offset >= 10 && offset <= 990 && offset % 10 === 0, `${offset}`);
  });
});

test("a directory outside any checkout counts as a main checkout", () => {
  withCheckouts(({ loose }) => {
    const identity = worktreeIdentity(loose);
    // tmpdir() itself is never inside a checkout on CI or a dev box; if it
    // were, the walk would find that checkout instead, which is also correct.
    if (identity.root !== loose) return;
    assert.equal(identity.isMainCheckout, true);
    assert.equal(testDatabasePrefix("midgard_test", identity), "midgard_test");
  });
});

test("the CLI prints the identity and single fields", () => {
  withCheckouts(({ linked }) => {
    const json = spawnSync(process.execPath, [cli, "--root", linked], {
      encoding: "utf8",
    });
    assert.equal(json.status, 0, json.stderr);
    const parsed = JSON.parse(json.stdout);
    assert.equal(parsed.hash, sha8(linked));
    assert.equal(parsed.isMainCheckout, false);
    const hash = spawnSync(process.execPath, [cli, "hash", "--root", linked], {
      encoding: "utf8",
    });
    assert.equal(hash.stdout.trim(), sha8(linked));
    const usage = spawnSync(process.execPath, [cli, "bogus"], {
      encoding: "utf8",
    });
    assert.equal(usage.status, 2);
  });
});

// The Vitest suites use the TypeScript twin. Loading it needs type stripping
// (unflagged from Node 22.18 and 23.6); an older Node skips with the reason
// rather than passing.
test("the TypeScript twin agrees with this module", async (t) => {
  let twin;
  try {
    twin = await import(typescriptTwin);
  } catch (error) {
    t.skip(
      `could not load the TypeScript twin: ${error.code ?? error.message}`,
    );
    return;
  }
  withCheckouts(({ main, linked, loose }) => {
    for (const start of [main, join(main, "demo/pkg"), linked, loose]) {
      const expected = worktreeIdentity(start);
      assert.deepEqual(twin.worktreeIdentity(start), expected, start);
      for (const family of ["midgard_test", "midgard_tools_test"]) {
        assert.equal(
          twin.testDatabasePrefix(family, expected),
          testDatabasePrefix(family, expected),
        );
      }
    }
  });
  assert.deepEqual(
    twin.worktreeIdentity(repositoryRoot),
    worktreeIdentity(repositoryRoot),
  );
});
