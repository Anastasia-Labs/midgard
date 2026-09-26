import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { TestProject } from "vitest/node";

declare module "vitest" {
  export interface ProvidedContext {
    readonly tempRoot: string;
  }
}

/**
 * One temporary root per run, removed at teardown, so the fixture
 * directories tests create under it never outlive the run.
 */
export default async function setup(project: TestProject) {
  const root = await mkdtemp(join(tmpdir(), "da-committee-node-test-"));
  project.provide("tempRoot", root);
  return () => rm(root, { recursive: true, force: true });
}
