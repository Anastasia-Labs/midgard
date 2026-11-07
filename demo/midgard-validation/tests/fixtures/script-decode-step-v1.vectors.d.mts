/**
 * Types for `script-decode-step-v1.vectors.mjs`, authored as plain ESM so the
 * generator can load it on bare `node`. See `cek-core-step-v1.vectors.d.mts`
 * for why a `.d.mts` beside a `.mjs`. A name here that no longer exists in the
 * module degrades silently to `any`, so
 * `tests/script-decode-step-v1.golden.test.ts` checks the export list against
 * the real module.
 */

/** Blueprint title, blueprint script hash, and compiled code, all hex. */
export declare const SCRIPT_DECODE_BLUEPRINT_SCRIPTS: readonly (readonly [
  string,
  string,
  string,
])[];

/** Label, language, script bytes, and the fuel of each Walk. */
export declare const scriptDecodeStepScripts: () => (readonly [
  string,
  number,
  Buffer,
  number,
])[];

export type ScriptDecodeStepRecordViewV1 =
  | {
      readonly k: "decoded";
      readonly programVersion: number;
      readonly phase2Class: number;
      readonly termRoot: string;
    }
  | {
      readonly k: "refused";
      readonly class: number;
      readonly bitOffset: number;
    };

/** One walk: its script, and the hash of every run datum it writes. */
export type ScriptDecodeStepRowV1 = {
  readonly label: string;
  readonly language: number;
  readonly fuel: number;
  readonly script: string;
  readonly scriptHash: string;
  readonly initialRunHash: string;
  readonly steps: readonly {
    readonly visible: number;
    readonly runHash: string;
  }[];
  readonly record: ScriptDecodeStepRecordViewV1;
  readonly recordHash: string;
};

/** The parts of a planned walk the rows read. */
type PlannedWalk<Run, Record> = {
  readonly run: Run & { readonly scriptHash: Uint8Array };
  readonly steps: readonly {
    readonly reveals: readonly unknown[];
    readonly after: Run;
  }[];
  readonly record: Record;
};

export declare const buildScriptDecodeStepRows: <
  Run,
  Record,
  Data,
>(implementations: {
  readonly planWalk: (
    language: number,
    script: Uint8Array,
    fuel: number,
  ) => PlannedWalk<Run, Record>;
  readonly runData: (run: Run) => Data;
  readonly recordData: (record: Record) => Data;
  readonly serialise: (data: Data) => Uint8Array;
  readonly blake2b256: (bytes: Uint8Array) => Uint8Array;
}) => ScriptDecodeStepRowV1[];
