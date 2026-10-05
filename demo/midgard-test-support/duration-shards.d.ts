/**
 * Hand-written types for `duration-shards.js`; see `vitest.d.ts` for why the
 * config-time modules of this package are plain JavaScript.
 */

export interface ShardDurationTable {
  readonly files: ReadonlyMap<string, number>;
  readonly defaultSeconds: number;
  readonly forksPerShard: number;
  readonly reservedSeconds: ReadonlyMap<string, number>;
}

export declare const readShardDurationTable: (
  path: string,
) => ShardDurationTable;

export declare const planDurationShards: (input: {
  readonly entries: readonly { readonly id: string; readonly file: string }[];
  readonly count: number;
  readonly table: ShardDurationTable;
}) => Map<string, number>;

// Structurally a Vitest sequencer constructor; spelled loosely so this file
// needs nothing from Vitest's types.
export declare const durationShardSequencer: (options: {
  readonly tablePath: string;
}) => new (ctx: never) => object;
