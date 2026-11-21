/** Hand-written types for `run-shared-fixtures.js`. */

export declare const RUN_SHARED_FIXTURE_DIRECTORY_ENV: string;

export declare const createRunSharedFixtureDirectory: () => Promise<string>;

export declare const removeRunSharedFixtureDirectory: (
  directory: string,
) => Promise<void>;

export declare const invalidateRunSharedFixtures: (
  directory: string,
) => Promise<void>;

export declare const makeRunScratchDirectory: (
  prefix: string,
) => Promise<string>;

export declare const loadOrCreateRunSharedFixture: <Shared, Created>(
  name: string,
  create: () => Promise<{ readonly shared: Shared; readonly created: Created }>,
) => Promise<{ readonly shared: Shared; readonly created?: Created }>;

export declare const memoizeForRun: <T>(
  name: string,
  produce: () => Promise<T>,
) => () => Promise<T>;
