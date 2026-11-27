import { execFile } from "node:child_process";
import { mkdir, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "midgard-watcher";

import {
  createWatcherEvidenceReader,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

/** The watcher package as its image installs it: `dist/index.js` in the working directory. */
export const watcherImage = fileURLToPath(
  new URL("../../../midgard-watcher/", import.meta.url),
);

/** Runs the reader with this machine's Node in the watcher package, as the container would. */
export const localExec =
  (cwd = watcherImage): WatcherEvidenceExec =>
  ({ script, request }) =>
    new Promise((resolve, reject) => {
      execFile(
        process.execPath,
        ["--input-type=module", "-e", script, request],
        { cwd, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
        (error, stdout, stderr) =>
          error === null ? resolve(stdout) : reject(new Error(stderr)),
      );
    });

/** Temporary roots `recordedWatcher` made; each test file removes them. */
export const directories: string[] = [];

export const FINGERPRINT = "ab".repeat(28);

/** A watcher's state as its container sees it: its process configuration and journals. */
export const recordedWatcher = async () => {
  const root = await mkdtemp(join(tmpdir(), "watcher-evidence-"));
  directories.push(root);
  const journalDirectory = join(root, "workflows");
  await mkdir(journalDirectory);
  const storePath = join(root, "watcher.sqlite");
  const processConfigPath = join(root, "watcher-process.json");
  await writeFile(
    processConfigPath,
    JSON.stringify({
      decisionJournalDirectory: journalDirectory,
      watcherConfig: { storage: { path: storePath } },
    }),
  );
  const reader = (exec: WatcherEvidenceExec = localExec()) =>
    createWatcherEvidenceReader({
      exec,
      hostProcessConfigPath: processConfigPath,
      containerProcessConfigPath: processConfigPath,
      hubOraclePolicyId: FINGERPRINT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
  return { root, journalDirectory, storePath, processConfigPath, reader };
};
