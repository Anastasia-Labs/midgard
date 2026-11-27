/**
 * Temporary directories and files for suites that exercise real filesystem
 * behaviour — process supervisors, runners, and the deployment run-state
 * writers — rather than mocking `node:fs`.
 *
 * {@link createTrackedTempDirFactory} registers its own `afterEach`, so a suite
 * that uses it cannot leak a directory by forgetting to clean up. That is why
 * this is a factory rather than a bare `mkdtemp` wrapper.
 */

import { access, mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach } from "vitest";

export const createTrackedTempDirFactory = (
  prefix: string,
): (() => Promise<string>) => {
  let tempDirs: string[] = [];

  afterEach(async () => {
    await Promise.all(
      tempDirs.map((dir) => rm(dir, { recursive: true, force: true })),
    );
    tempDirs = [];
  });

  return async (): Promise<string> => {
    const dir = await mkdtemp(join(tmpdir(), prefix));
    tempDirs.push(dir);
    return dir;
  };
};

export const writeScript = async (
  dir: string,
  name: string,
  source: string,
): Promise<string> => {
  const path = join(dir, name);
  await writeFile(path, source, "utf8");
  return path;
};

export const waitForFile = async (path: string): Promise<void> => {
  const deadline = Date.now() + 5_000;
  while (Date.now() < deadline) {
    try {
      await access(path);
      return;
    } catch {
      await new Promise((resolve) => setTimeout(resolve, 25));
    }
  }
  throw new Error(`timed out waiting for ${path}`);
};
