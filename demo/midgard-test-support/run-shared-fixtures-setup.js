import {
  createRunSharedFixtureDirectory,
  invalidateRunSharedFixtures,
  removeRunSharedFixtureDirectory,
} from "./run-shared-fixtures.js";

/**
 * Vitest global setup for the run-scoped shared directory
 * (`run-shared-fixtures.js`). List it in the `globalSetup` of every project
 * whose files call `loadOrCreateRunSharedFixture` or a `memoizeForRun`
 * producer; without it each file produces its own values.
 *
 * Vitest runs a global setup once for every project that lists it, each
 * through its own module runner but in one process, so the directory is
 * created once per run and shared by every such project. It is created
 * before any worker starts: Vitest copies this process's environment into
 * each worker when it starts running files.
 */
const DIRECTORY = Symbol.for("@al-ft/midgard-test-support/run-shared-fixtures");
const RERUN_INVALIDATION = Symbol.for(
  "@al-ft/midgard-test-support/run-shared-fixtures/rerun-invalidation",
);

export const setup = async (project) => {
  const registry = globalThis;
  registry[DIRECTORY] ??= createRunSharedFixtureDirectory();
  const directory = await registry[DIRECTORY];
  // Watch mode keeps the directory for the whole session; a rerun after a
  // source change must not read values the earlier sources produced. Every
  // project shares one Vitest instance, so one handler covers them all.
  if (registry[RERUN_INVALIDATION] === undefined) {
    registry[RERUN_INVALIDATION] = true;
    project.onTestsRerun(() => invalidateRunSharedFixtures(directory));
  }
};

/** Removes the run's shared values; the later calls for the other projects
 * find nothing left to remove. */
export const teardown = async () => {
  const directory = globalThis[DIRECTORY];
  if (directory !== undefined)
    await removeRunSharedFixtureDirectory(await directory);
};
