/**
 * Hand-written types for `vitest.js`. See the note at the top of that file for
 * why the implementation is not TypeScript.
 *
 * These are spelled structurally rather than against Vitest's own config types
 * so that this file stays loadable with nothing installed or built, which is
 * the whole reason the module beside it is plain JavaScript.
 */

export declare const midgardSourceEnvironments: () => {
  ssr: { resolve: { conditions: string[] } };
  __vitest__: { resolve: { conditions: string[] } };
};

export declare const isolatedForksPool: (options: {
  readonly maxForks: number;
  readonly heapMb?: number;
}) => {
  readonly pool: "forks";
  readonly isolate: true;
  readonly maxWorkers: number;
  readonly execArgv: string[];
};

export declare const rawSqlLoaderPlugin: () => {
  readonly name: string;
  readonly load: (id: string) => string | null;
};

export declare const blueprintStampGlobalSetup: string;

export declare const runSharedFixturesGlobalSetup: string;

export declare const interactiveEmulatorBlueprint: string;
export declare const interactiveEmulatorSetup: string;
export declare const interactiveEmulatorPlugin: () => {
  name: string;
  transform(code: string, id: string): { code: string; map: null } | null;
};

export declare const curatedEmulatorBlueprint: string;
export declare const curatedEmulatorSetup: string;
export declare const curatedEmulatorPlugin: () => {
  name: string;
  transform(code: string, id: string): { code: string; map: null } | null;
};

export declare const preprodPublicBlueprint: string;
export declare const preprodPublicSetup: string;
export declare const preprodPublicPlugin: () => {
  name: string;
  transform(code: string, id: string): { code: string; map: null } | null;
};

export declare const preprodRehearsalPlugin: () => {
  name: string;
  transform(code: string, id: string): { code: string; map: null } | null;
};

/** CI seconds at which a file belongs to the slow tier; see vitest.js. */
export declare const SLOW_TIER_FILE_SECONDS: number;

/** The tier `MIDGARD_TEST_TIER` selects. */
export declare const selectedTestTier: () => "fast" | "slow" | "all";

/**
 * The projects restricted to the tier `MIDGARD_TEST_TIER` selects, by the
 * CI seconds in the package's duration table; unchanged without a tier.
 */
export declare const durationTierProjects: <
  Project extends {
    readonly test?: {
      readonly include?: readonly string[];
      readonly exclude?: readonly string[];
    };
  },
>(
  projects: readonly Project[],
  options: {
    readonly tablePath: string;
    readonly packageDirectory: string;
    readonly slowSeconds?: number;
  },
) => Project[];

/** `config` with its `test.projects` restricted by `durationTierProjects`. */
export declare const withDurationTier: <Config extends object>(
  config: Config,
  options: {
    readonly tablePath: string;
    readonly packageDirectory: string;
    readonly slowSeconds?: number;
  },
) => Config;

export declare const workspaceBundleProjects: <
  Project extends {
    readonly extends?: true | string;
    readonly test: { readonly name: string };
  },
>(
  project: Project,
  options: { readonly packageDirectory: string },
) => Project[];
