/**
 * Finds what an earlier, interrupted record of a script left on chain: its
 * staged bytes, and its open run with the walk cursor at that run, so a rerun
 * continues the run instead of binding a new one.
 */
import {
  nextScriptDecodeChainedWalk,
  type ScriptDecodeWalkCursor,
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import {
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { scriptDecodeRunDatum, scriptDecodeRunTokenName } from "./data.js";
import {
  type ScriptDecodeDeployment,
  scriptDecodeStagingDatums,
} from "./submit.js";

/** Ticks the replay first tries in one Walk while it moves to a run. */
const REPLAY_INITIAL_FUEL = 1_024;

/** The most ticks the replay tries in one Walk. */
const REPLAY_MAX_FUEL = 65_536;

/** An open run and the walk cursor at it. */
export type ScriptDecodeRunPosition = Readonly<{
  runUtxo: UTxO;
  cursor: ScriptDecodeWalkCursor;
}>;

/**
 * For each staging datum of `scriptBytes` in order, a UTxO at the staging
 * address holding it (the first in out-ref order), or undefined where none
 * does. Staging carries no token, so any UTxO with the right datum serves:
 * a Walk checks the staged bytes against the run's script hash.
 */
export const findStagedScriptDecodeChunks = async ({
  lucid,
  scriptBytes,
  address,
}: {
  readonly lucid: LucidEvolution;
  readonly scriptBytes: Uint8Array;
  /** Where the bytes were staged; the wallet's address by default. */
  readonly address?: string;
}): Promise<(UTxO | undefined)[]> => {
  const held = (
    await lucid.utxosAt(address ?? (await lucid.wallet().address()))
  ).sort((left, right) => {
    const a = `${left.txHash}#${left.outputIndex.toString().padStart(10, "0")}`;
    const b = `${right.txHash}#${right.outputIndex.toString().padStart(10, "0")}`;
    return a < b ? -1 : a > b ? 1 : 0;
  });
  const staged: (UTxO | undefined)[] = [];
  for (const datum of scriptDecodeStagingDatums(scriptBytes)) {
    staged.push(
      held.find((utxo) => utxo.datum === datum && !staged.includes(utxo)),
    );
  }
  return staged;
};

/**
 * The staged UTxOs at the wallet's address that hold `scriptBytes`, one per
 * staging datum in order, or undefined while any is missing.
 */
export const findStagedScriptDecodeBytes = async (input: {
  readonly lucid: LucidEvolution;
  readonly scriptBytes: Uint8Array;
  readonly address?: string;
}): Promise<UTxO[] | undefined> => {
  const staged = await findStagedScriptDecodeChunks(input);
  return staged.every((utxo) => utxo !== undefined)
    ? (staged as UTxO[])
    : undefined;
};

/** The machine's bit cursor a run datum carries, if it parses as a run. */
const runBitCursor = (datum: string): number | undefined => {
  try {
    const run = Data.from(datum);
    if (!(run instanceof Constr) || run.index !== 0) return undefined;
    const machine = run.fields[5];
    if (!(machine instanceof Constr) || machine.index !== 0) return undefined;
    const cursor = machine.fields[0];
    return typeof cursor === "bigint" ? Number(cursor) : undefined;
  } catch {
    return undefined;
  }
};

/**
 * The walk cursor whose run datum is `datum`, replayed from the start. The
 * decoder only reads forward, so the replay takes the largest Walks that stay
 * below the run's bit cursor, then single ticks while at it. Undefined when
 * the walk passes that cursor, or finishes, without writing `datum`.
 */
const replayToRun = (
  start: ScriptDecodeWalkCursor,
  datum: string,
  bitCursor: number,
): ScriptDecodeWalkCursor | undefined => {
  let cursor = start;
  let fuel = REPLAY_INITIAL_FUEL;
  for (;;) {
    if (scriptDecodeRunDatum(cursor.run) === datum) return cursor;
    if (scriptDecodeWalkRecord(cursor) !== undefined) return undefined;
    const at = cursor.run.machine.cursor;
    if (at > bitCursor) return undefined;
    const ticks = at === bitCursor ? 1 : fuel;
    const { next } = nextScriptDecodeChainedWalk(cursor, ticks);
    if (ticks > 1 && next.run.machine.cursor >= bitCursor) {
      fuel = Math.max(1, Math.floor(fuel / 2));
      continue;
    }
    cursor = next;
    if (ticks > 1) fuel = Math.min(REPLAY_MAX_FUEL, fuel * 2);
  }
};

/**
 * The open run of `scriptBytes` furthest along whose datum the honest walk
 * writes, with the cursor at it; undefined when there is none. Bind is
 * permissionless, so several runs of one script may be open; any honest one
 * finishes into the same record.
 */
export const resumeScriptDecodeRun = async ({
  lucid,
  deployment,
  language,
  scriptBytes,
  scriptHashHex,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly language: number;
  readonly scriptBytes: Uint8Array;
  readonly scriptHashHex: string;
}): Promise<ScriptDecodeRunPosition | undefined> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runs = await lucid.utxosAtWithUnit(
    spendingScriptAddress,
    `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`,
  );
  const candidates = runs
    .flatMap((runUtxo) => {
      if (typeof runUtxo.datum !== "string") return [];
      const bitCursor = runBitCursor(runUtxo.datum);
      return bitCursor === undefined
        ? []
        : [{ runUtxo, datum: runUtxo.datum, bitCursor }];
    })
    .sort((left, right) => right.bitCursor - left.bitCursor);
  const start = startScriptDecodeWalk(language, scriptBytes);
  for (const { runUtxo, datum, bitCursor } of candidates) {
    const cursor = replayToRun(start, datum, bitCursor);
    if (cursor !== undefined) return { runUtxo, cursor };
  }
  return undefined;
};

/**
 * The walk cursor at `runUtxo`, a run of `scriptBytes`, when the honest walk
 * writes its datum; undefined otherwise. The replay starts at `from` (an
 * earlier cursor of the same walk) when given, else at the start.
 */
export const scriptDecodeRunCursorAt = ({
  language,
  scriptBytes,
  runUtxo,
  from,
}: {
  readonly language: number;
  readonly scriptBytes: Uint8Array;
  readonly runUtxo: UTxO;
  readonly from?: ScriptDecodeWalkCursor;
}): ScriptDecodeWalkCursor | undefined => {
  if (typeof runUtxo.datum !== "string") return undefined;
  const bitCursor = runBitCursor(runUtxo.datum);
  if (bitCursor === undefined) return undefined;
  const start = from ?? startScriptDecodeWalk(language, scriptBytes);
  if (start.run.machine.cursor > bitCursor) return undefined;
  return replayToRun(start, runUtxo.datum, bitCursor);
};
