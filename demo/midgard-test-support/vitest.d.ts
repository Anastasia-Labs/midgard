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

export declare const isolatedForksPool: (options: {
  readonly maxForks: number;
  readonly heapMb?: number;
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
