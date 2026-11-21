#!/usr/bin/env node

// Which checkout is this, and what names may it use on shared local services?
//
// Several checkouts of this repository run side by side: the main checkout and
// any number of `git worktree` checkouts. They share one test Postgres on 5433,
// one Docker daemon and one set of host ports. Resources named by a fixed
// default (`midgard_test_w1`, compose project `midgard_phase4_process_run1`,
// host port 5544) collide, and a collision on a test database silently
// interleaves two suites' rows.
//
// The rule every consumer follows: the MAIN checkout keeps the historical
// default names and ports exactly, so nothing changes for it; a LINKED worktree
// derives its own from `hash`. An explicit environment override always wins
// over either default; that choice belongs to the consumer, not to this module.
//
// `isMainCheckout` means the git common directory is `<root>/.git`, which is
// the same as `<root>/.git` being a directory. A linked worktree (and a
// submodule) has a `.git` file instead. A directory with no git metadata at all
// counts as a main checkout: there is no sibling worktree to collide with, and
// the historical defaults are the safe answer.
//
// `demo/midgard-node/tests/worktree-identity.ts` is the TypeScript twin used
// by the Vitest suites, which cannot import this file. Keep the two identical
// in behaviour; `worktree-identity.test.mjs` compares them.
//
// CLI: `node scripts/lib/worktree-identity.mjs [--json | <field>] [--root <dir>]`
// prints the whole identity as JSON, or one field (root, isMainCheckout, slug,
// hash, portOffset) as a bare value. Exit 2 on a usage error.

import { createHash } from "node:crypto";
import { existsSync, realpathSync, statSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

/** The nearest directory at or above `start` that has a `.git` entry. */
const findCheckoutRoot = (start) => {
  let directory = start;
  for (;;) {
    if (existsSync(join(directory, ".git"))) return directory;
    const parent = dirname(directory);
    if (parent === directory) return undefined;
    directory = parent;
  }
};

export const worktreeIdentity = (root = process.cwd()) => {
  const start = realpathSync(root);
  const checkoutRoot = findCheckoutRoot(start) ?? start;
  const gitEntry = join(checkoutRoot, ".git");
  const isMainCheckout =
    !existsSync(gitEntry) || statSync(gitEntry).isDirectory();
  const slug =
    basename(checkoutRoot)
      .toLowerCase()
      .replace(/[^a-z0-9]+/gu, "-")
      .replace(/^-+|-+$/gu, "") || "checkout";
  const hash = createHash("sha256")
    .update(checkoutRoot)
    .digest("hex")
    .slice(0, 8);
  return { root: checkoutRoot, isMainCheckout, slug, hash };
};

/**
 * The default test-database prefix for one suite family. The main checkout
 * keeps `family` itself (`midgard_test` gives `midgard_test_w1` as before); a
 * linked worktree gets `<family>_<hash>`.
 */
export const testDatabasePrefix = (family, identity = worktreeIdentity()) =>
  identity.isMainCheckout ? family : `${family}_${identity.hash}`;

/**
 * How far a linked worktree shifts every default host port: a multiple of 10
 * in 10..990, zero for the main checkout. Multiples of 10 keep two services
 * whose defaults differ by a non-multiple of 10 (2337 and 2442, say) from
 * landing on each other's port in two different worktrees.
 */
export const hostPortOffset = (identity = worktreeIdentity()) =>
  identity.isMainCheckout
    ? 0
    : 10 * (1 + (Number.parseInt(identity.hash.slice(0, 6), 16) % 99));

const fields = ["root", "isMainCheckout", "slug", "hash", "portOffset"];

const main = (argv) => {
  let root = process.cwd();
  let field = "--json";
  for (let index = 0; index < argv.length; index += 1) {
    if (argv[index] === "--root" && index + 1 < argv.length) {
      root = argv[index + 1];
      index += 1;
    } else if (argv[index] === "--json" || fields.includes(argv[index])) {
      field = argv[index];
    } else {
      console.error(
        `usage: worktree-identity.mjs [--json | ${fields.join(" | ")}] [--root <dir>]`,
      );
      return 2;
    }
  }
  const identity = worktreeIdentity(root);
  const full = { ...identity, portOffset: hostPortOffset(identity) };
  console.log(field === "--json" ? JSON.stringify(full) : String(full[field]));
  return 0;
};

if (
  process.argv[1] &&
  realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
