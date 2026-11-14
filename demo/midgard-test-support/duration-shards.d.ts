/**
 * Hand-written types for `duration-shards.js`; see `vitest.d.ts` for why the
 * config-time modules of this package are plain JavaScript.
 */

import type {
  ModuleDiagnostic,
  Reporter,
  TestSequencerConstructor,
} from "vitest/node";

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

/** Projected seconds of each shard, shard 1 first; see duration-plan.js. */
export declare const projectShardSeconds: (input: {
  readonly entries: readonly { readonly id: string; readonly file: string }[];
  readonly count: number;
  readonly table: ShardDurationTable;
}) => number[];

export declare const durationShardSequencer: (options: {
  readonly tablePath: string;
}) => TestSequencerConstructor;

/** Seconds a Vitest test module kept its fork busy. */
export declare const fileTaskSeconds: (
  diagnostic: Pick<
    ModuleDiagnostic,
    | "prepareDuration"
    | "environmentSetupDuration"
    | "setupDuration"
    | "collectDuration"
    | "duration"
  >,
) => number;

/** Writes each test file's seconds to a JSON record at the end of a run. */
export declare class FileDurationsReporter
  implements Pick<Reporter, "onInit" | "onTestRunEnd">
{
  constructor(outputPath: string);
  onInit(vitest: Parameters<NonNullable<Reporter["onInit"]>>[0]): void;
  onTestRunEnd(
    testModules?: Parameters<NonNullable<Reporter["onTestRunEnd"]>>[0],
  ): void;
}

/**
 * Spread into a Vitest config's `test`: the duration sequencer and the
 * package's reporters, plus a `FileDurationsReporter` when
 * `MIDGARD_FILE_DURATIONS_OUT` is set.
 */
export declare const durationShards: <Reporter>(options: {
  readonly tablePath: string;
  readonly reporters: readonly Reporter[];
}) => {
  readonly reporters: (Reporter | FileDurationsReporter)[];
  readonly sequence: { readonly sequencer: TestSequencerConstructor };
};
