import {
  mkdirSync,
  readdirSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";

/**
 * The workspace bundle cache's lifetime: which bundles a running process
 * uses, and which ones publishing a new bundle prunes. Nothing here decides
 * what a bundle contains (`workspace-bundle.js`).
 */

/** Bundles not used for this long are pruned when a new one is published. */
const PRUNE_AFTER_MS = 24 * 60 * 60 * 1000;
/**
 * At most this many bundles are kept when a new one is published, the most
 * recently used ones, even when the others were used within the day. Two for
 * each of the eight packages whose suites are bundled, so each keeps its
 * current bundle and the one before it under any mix of runs. A bundle is
 * about 30 MB, so the cache stays near half a gigabyte instead of holding
 * every bundle a day of edits produced (31 bundles, 870 MB, measured in one
 * checkout).
 */
export const MAX_BUNDLES = 16;
/**
 * Each run names its pid in this directory of the bundle it loads. Neither the
 * age limit nor the cap removes a bundle a running process named: its forks
 * import from it for as long as the run lasts.
 */
const USERS_DIRECTORY = "users";

const processEnded = (pid) => {
  try {
    process.kill(pid, 0);
    return false;
  } catch (error) {
    // EPERM: a live process of another user.
    return error.code === "ESRCH";
  }
};

/** The pids named in `directory`'s users directory. */
const bundleUsers = (directory) => {
  try {
    return readdirSync(join(directory, USERS_DIRECTORY))
      .filter((name) => /^[1-9]\d*$/u.test(name))
      .map(Number);
  } catch {
    return [];
  }
};

/** Names this process a user of `directory`, dropping users that ended. */
export const useBundle = (directory) => {
  const users = join(directory, USERS_DIRECTORY);
  mkdirSync(users, { recursive: true });
  for (const pid of bundleUsers(directory))
    if (processEnded(pid)) rmSync(join(users, String(pid)), { force: true });
  writeFileSync(join(users, String(process.pid)), "");
};

/**
 * Removes from `root` every entry other than `keep` that no running process
 * uses and that either went unused for `PRUNE_AFTER_MS` or falls outside the
 * `MAX_BUNDLES` most recently used bundles (`keep` counts as one). A bundle's
 * modification time is its last use. A staging directory counts only toward
 * the age limit, so a build in progress is never evicted for room.
 */
export const pruneWorkspaceBundles = (root, keep) => {
  const remove = (path) => {
    if (bundleUsers(path).every(processEnded))
      rmSync(path, { recursive: true, force: true });
  };
  const bundles = [];
  for (const entry of readdirSync(root)) {
    const path = join(root, entry);
    if (path === keep) continue;
    let used;
    try {
      used = statSync(path).mtimeMs;
    } catch {
      // Another run pruned it first.
      continue;
    }
    if (Date.now() - used > PRUNE_AFTER_MS) remove(path);
    else if (!entry.startsWith(".")) bundles.push({ path, used });
  }
  bundles.sort((a, b) => b.used - a.used);
  for (const { path } of bundles.slice(MAX_BUNDLES - 1)) remove(path);
};
