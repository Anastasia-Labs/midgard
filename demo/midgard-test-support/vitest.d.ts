/**
 * Hand-written types for `vitest.js`. See the note at the top of that file for
 * why the implementation is not TypeScript.
 *
 * These are spelled structurally rather than against Vitest's own config types
 * so that this file stays loadable with nothing installed or built, which is
 * the whole reason the module beside it is plain JavaScript.
 */

export declare const midgardSourceSsr: () => {
  resolve: { conditions: string[] };
};

export declare const cmlMemoryReserveExecArgv: string;

export declare const isolatedForksPool: (options: {
  readonly maxForks: number;
  readonly heapMb?: number;
  readonly execArgv?: readonly string[];
}) => {
  readonly pool: "forks";
  readonly poolOptions: {
    readonly forks: {
      readonly isolate: true;
      readonly singleFork: false;
      readonly minForks: number;
      readonly maxForks: number;
      readonly execArgv: string[];
    };
  };
};

export declare const rawSqlLoaderPlugin: () => {
  readonly name: string;
  readonly load: (id: string) => string | null;
};

export declare const blueprintStampGlobalSetup: string;

export declare const interactiveEmulatorBlueprint: string;
export declare const interactiveEmulatorSetup: string;
export declare const interactiveEmulatorPlugin: () => {
  name: string;
  transform(code: string, id: string): { code: string; map: null } | null;
};

export declare const workspaceBundleProjects: <
  Project extends {
    readonly extends?: true | string;
    readonly test: { readonly name: string };
  },
>(
  project: Project,
  options: { readonly packageDirectory: string },
) => Project[];
