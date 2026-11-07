/**
 * The datums and redeemers of the `script_decode_v1` validator, built with the
 * step machine port so they are byte-for-byte the Data the validator decodes.
 */
import {
  type ScriptDecodeData,
  scriptDecodeFrameData,
  scriptDecodeMachineData,
  scriptDecodeMachineHash,
  type ScriptDecodeRecord,
  scriptDecodeRecordData,
  type ScriptDecodeRun,
  scriptDecodeRunData,
  type ScriptDecodeWalkSegment,
  serialiseScriptDecodeData,
} from "@al-ft/midgard-validation";

const cborHex = (data: ScriptDecodeData): string =>
  serialiseScriptDecodeData(data).toString("hex");

/** The run token's asset name `"R" ‖ H`, hex. */
export const scriptDecodeRunTokenName = (scriptHashHex: string): string =>
  `52${scriptHashHex}`;

/** `Constr 0 [B bytes]`: staged script bytes as one byte string. */
export const scriptDecodeStagingDatum = (bytes: Uint8Array): string =>
  cborHex({ constr: 0, fields: [bytes] });

export const scriptDecodeRunDatum = (run: ScriptDecodeRun): string =>
  cborHex(scriptDecodeRunData(run));

export const scriptDecodeRecordDatum = (record: ScriptDecodeRecord): string =>
  cborHex(scriptDecodeRecordData(record));

/** Mint `Bind { language, staging, run_output }`. */
export const scriptDecodeBindRedeemer = ({
  language,
  staging,
  runOutput,
}: {
  readonly language: number;
  readonly staging: readonly bigint[];
  readonly runOutput: bigint;
}): string =>
  cborHex({
    constr: 0,
    fields: [BigInt(language), [...staging], runOutput],
  });

/** Mint `Finish { record_output }`. */
export const scriptDecodeFinishRedeemer = (recordOutput: bigint): string =>
  cborHex({ constr: 1, fields: [recordOutput] });

/** Spend `Walk { run_output }`. */
export const scriptDecodeWalkRedeemer = (runOutput: bigint): string =>
  cborHex({ constr: 0, fields: [runOutput] });

/** A Walk segment at its chain `position`, as one staker steps it. */
export type ScriptDecodePositionedSegment = Readonly<{
  position: number;
  segment: ScriptDecodeWalkSegment;
}>;

/**
 * A class staker's redeemer `{ run, links, staging, segments }`: the run it
 * steps, a link `{ position, from, to }` per segment it steps, the staging of
 * the run's bytes, and those segments `{ machine, reveals, base, fuel }`.
 */
export const scriptDecodeStakeRedeemer = ({
  run,
  staging,
  segments,
}: {
  readonly run: Readonly<{ txHash: string; outputIndex: number }>;
  readonly staging: readonly bigint[];
  readonly segments: readonly ScriptDecodePositionedSegment[];
}): string =>
  cborHex({
    constr: 0,
    fields: [
      {
        constr: 0,
        fields: [Buffer.from(run.txHash, "hex"), BigInt(run.outputIndex)],
      },
      segments.map(({ position, segment }) => ({
        constr: 0,
        fields: [
          BigInt(position),
          scriptDecodeMachineHash(segment.machine),
          scriptDecodeMachineHash(segment.reached),
        ],
      })),
      [...staging],
      segments.map(({ segment }) => ({
        constr: 0,
        fields: [
          scriptDecodeMachineData(segment.machine),
          segment.reveals.map(scriptDecodeFrameData),
          segment.base,
          BigInt(segment.fuel),
        ],
      })),
    ],
  });

/** Spend `Close`. */
export const SCRIPT_DECODE_CLOSE_REDEEMER = cborHex({ constr: 1, fields: [] });
