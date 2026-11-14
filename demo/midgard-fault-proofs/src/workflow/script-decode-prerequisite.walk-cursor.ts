/**
 * The walk cursor at a run of a script being recorded. A run's datum fixes
 * the cursor, so the cursor a captured Walk predicts for the run it writes is
 * kept by that datum's digest; the next action finds it there instead of
 * replaying the walk from its start. A restarted process replays once.
 */
import {
  type ScriptDecodeWalkCursor,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import type { UTxO } from "@lucid-evolution/lucid";

import { scriptDecodeRunDatum } from "../script-decode/data.js";
import { SCRIPT_DECODE_INITIAL_FUEL } from "../script-decode/record.js";
import { scriptDecodeRunCursorAt } from "../script-decode/resume.js";
import { scriptDecodeDatumDigest } from "./script-decode-prerequisite.actions.js";

/** Cursors kept at once; each record needs the current and the next. */
const MAX_CURSORS = 16;

const cursors = new Map<string, ScriptDecodeWalkCursor>();
const fuels = new Map<string, number>();

const key = (scriptHashHex: string, datumCbor: string): string =>
  `${scriptHashHex}\u0000${scriptDecodeDatumDigest(datumCbor)}`;

const keep = (entry: string, cursor: ScriptDecodeWalkCursor): void => {
  cursors.delete(entry);
  cursors.set(entry, cursor);
  while (cursors.size > MAX_CURSORS) {
    cursors.delete(cursors.keys().next().value!);
  }
};

/** Remembers `cursor`, which a Walk of `scriptHashHex` writes as its run. */
export const rememberScriptDecodeCursor = (
  scriptHashHex: string,
  cursor: ScriptDecodeWalkCursor,
  fuel: number,
): void => {
  keep(key(scriptHashHex, scriptDecodeRunDatum(cursor.run)), cursor);
  fuels.set(scriptHashHex, fuel);
};

/** The fuel the last Walk of `scriptHashHex` took, as the next one's guess. */
export const scriptDecodeFuelGuess = (scriptHashHex: string): number =>
  fuels.get(scriptHashHex) ?? SCRIPT_DECODE_INITIAL_FUEL;

/**
 * The walk cursor at `runUtxo`, a run of the script `scriptBytes` in
 * `language`; undefined when the honest walk never writes its datum.
 */
export const scriptDecodeCursorAtRun = ({
  language,
  scriptBytes,
  scriptHashHex,
  runUtxo,
}: {
  readonly language: number;
  readonly scriptBytes: Uint8Array;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
}): ScriptDecodeWalkCursor | undefined => {
  if (typeof runUtxo.datum !== "string") return undefined;
  const entry = key(scriptHashHex, runUtxo.datum);
  const known = cursors.get(entry);
  if (known !== undefined) return known;
  const start = startScriptDecodeWalk(language, scriptBytes);
  const cursor = scriptDecodeRunCursorAt({
    language,
    scriptBytes,
    runUtxo,
    from: start,
  });
  if (cursor !== undefined) keep(entry, cursor);
  return cursor;
};
