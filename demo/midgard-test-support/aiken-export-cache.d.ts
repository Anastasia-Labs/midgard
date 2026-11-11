/** Hand-written types for `aiken-export-cache.js`. */

/** A function of the on-chain project and the extra `aiken export` flags. */
export type AikenFunction = {
  readonly module: string;
  readonly name: string;
  readonly flags: readonly string[];
};

export declare const exportAikenFunctions: (
  functions: readonly AikenFunction[],
  options?: {
    readonly root?: string;
    readonly compiler?: string;
    readonly lease?: (action: () => Promise<void>) => Promise<void>;
    readonly log?: (line: string) => void;
  },
) => Promise<readonly Uint8Array[]>;
