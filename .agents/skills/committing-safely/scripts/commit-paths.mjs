#!/usr/bin/env node
// commit-paths: commit exactly the named paths from the working tree, through a
// temporary index, without touching anything else another session has staged
// or left unstaged.
//
//   node .agents/skills/committing-safely/scripts/commit-paths.mjs \
//     -m "Subject line" [-m "Body paragraph"] [-F message-file] \
//     [--patch file.patch] [--allow-unchecked] [--dry-run] -- <paths...>
//
// What it does, in order:
//   1. Refuses: no paths, a directory, a path outside the repository,
//      onchain/aiken/plutus.json, a path with no change against HEAD, a path
//      whose real-index entry holds a staged version that is neither HEAD's
//      nor the one being committed (someone else's staged work), an
//      in-progress merge/rebase/cherry-pick/revert, and a message that carries
//      a tool attribution trailer.
//   2. Builds a temporary index from HEAD and adds only those paths (or, with
//      --patch, applies only that patch).
//   3. Checks the formatting of exactly the content being committed: prettier
//      for demo/**/*.{ts,tsx,md} (demo's own prettier), and for *.ak the
//      pinned Aiken fork's formatter plus the trailing-whitespace
//      normalization CI applies. It refuses unformatted content; it never
//      rewrites anything. A missing tool is reported as "could not check",
//      separately from "unformatted".
//   4. Writes the commit with `git commit-tree` and moves HEAD with a
//      compare-and-swap `git update-ref`, so a commit another session lands
//      in the meantime is never silently reverted. No git hooks run: neither
//      the repository pre-commit hook nor the Nix pre-commit.local shim
//      (which stashes every unstaged file mid-commit), nor post-commit.
//   5. Resets the real index for the committed paths only, so they do not
//      show as reverse-staged, and prints the commit with its stat.
//
// Exit codes:
//   0  committed (or, with --dry-run, would commit)
//   1  refused; nothing committed
//   2  usage error; nothing committed
//   3  could not check formatting (a tool is missing); nothing committed.
//      Re-run with --allow-unchecked to commit anyway.
//   4  committed, but resetting the real index for the committed paths failed;
//      the message says what to run
//
// Environment:
//   MIDGARD_PRETTIER_BIN  prettier to use (default <repo>/demo/node_modules/.bin/prettier)
//   MIDGARD_AIKEN_BIN     aiken to use (default `aiken` on PATH)

import "node:child_process";
import "node:fs";
import "node:os";
import "node:path";
import "./commit-paths.parse-args.mjs";
import "./commit-paths.main.mjs";
import "./commit-paths.registration.mjs";
