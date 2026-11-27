/**
 * TypeScript twin of `scripts/lib/worktree-identity.mjs` for the Vitest
 * suites, which cannot import a repository-root script. It carries only what
 * the suites need: the identity and the default test-database prefix. Keep it
 * behaviourally identical to the root module; `scripts/lib/
 * worktree-identity.test.mjs` imports both and compares them.
 *
 * Only `node:` imports and erasable type syntax, so plain Node can load this
 * file with type stripping for that comparison.
 */
import { createHash } from "node:crypto";
import { existsSync, realpathSync, statSync } from "node:fs";
import { basename, dirname, join } from "node:path";

export type WorktreeIdentity = {
  readonly root: string;
  readonly isMainCheckout: boolean;
  readonly slug: string;
  readonly hash: string;
};

const findCheckoutRoot = (start: string): string | undefined => {
  let directory = start;
  for (;;) {
    if (existsSync(join(directory, ".git"))) return directory;
    const parent = dirname(directory);
    if (parent === directory) return undefined;
    directory = parent;
  }
};

export const worktreeIdentity = (
  root: string = process.cwd(),
): WorktreeIdentity => {
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
 * The main checkout keeps `family` itself, so its shard names are unchanged; a
 * linked worktree gets `<family>_<hash>`, so two checkouts running the suite
 * against the shared test server never share a database.
 */
export const testDatabasePrefix = (
  family: string,
  identity: WorktreeIdentity = worktreeIdentity(),
): string => (identity.isMainCheckout ? family : `${family}_${identity.hash}`);
