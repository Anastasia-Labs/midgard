/**
 * The datums and redeemers of the `script_decode_v1` validator, built with the
 * step machine port so they are byte-for-byte the Data the validator decodes.
 */
import {
  type ScriptDecodeData,
  type ScriptDecodeFrame,
  scriptDecodeFrameData,
  type ScriptDecodeRecord,
  scriptDecodeRecordData,
  type ScriptDecodeRun,
  scriptDecodeRunData,
  serialiseScriptDecodeData,
} from "@al-ft/midgard-validation";

const cborHex = (data: ScriptDecodeData): string =>
  serialiseScriptDecodeData(data).toString("hex");

/** Bytes per staged chunk: the size of one Plutus Data byte string. */
export const SCRIPT_DECODE_STAGING_CHUNK_BYTES = 64;

/** The run token's asset name `"R" ‖ H`, hex. */
export const scriptDecodeRunTokenName = (scriptHashHex: string): string =>
  `52${scriptHashHex}`;

/** `Constr 0 [List [B chunk, ..]]`: staged script bytes, 64 per chunk. */
export const scriptDecodeStagingDatum = (bytes: Uint8Array): string => {
  const chunks: Uint8Array[] = [];
  for (
    let offset = 0;
    offset < bytes.length;
    offset += SCRIPT_DECODE_STAGING_CHUNK_BYTES
  ) {
    chunks.push(
      bytes.subarray(offset, offset + SCRIPT_DECODE_STAGING_CHUNK_BYTES),
    );
  }
  return cborHex({ constr: 0, fields: [chunks] });
};

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

/** Spend `Walk { staging, reveals, base, fuel, run_output }`. */
export const scriptDecodeWalkRedeemer = ({
  staging,
  reveals,
  base,
  fuel,
  runOutput,
}: {
  readonly staging: readonly bigint[];
  readonly reveals: readonly ScriptDecodeFrame[];
  readonly base: Uint8Array;
  readonly fuel: number;
  readonly runOutput: bigint;
}): string =>
  cborHex({
    constr: 0,
    fields: [
      [...staging],
      reveals.map(scriptDecodeFrameData),
      base,
      BigInt(fuel),
      runOutput,
    ],
  });

/** Spend `Close`. */
export const SCRIPT_DECODE_CLOSE_REDEEMER = cborHex({ constr: 1, fields: [] });
