/**
 * The in-place CBOR reader under the Data constant ticks of the script-decode
 * step machine (`onchain/aiken/lib/midgard/script-decode-v1.ak`): Flat chunk
 * walking, CBOR heads, and the stops a malformed item ends in.
 */
import {
  at,
  byteAt,
  type DecodeEnv,
  type Exec,
  refuse,
  ScriptDecodeClass,
  type Tick,
  to,
} from "./script-decode.step-exec.js";
import type { DataMode, Source, Summary } from "./script-decode.step-types.js";

export const SKIM_CHUNKS_PER_TICK = 32;
export const CBOR_BREAK = 255;
export const MAX_DATA_BYTES_CHUNK = 64n;

export type Got<T> =
  | { readonly k: "got"; readonly value: T; readonly source: Source }
  | { readonly k: "end" | "truncated"; readonly at: number }
  | { readonly k: "invalid" };

export type Stop = Exclude<Got<unknown>, { k: "got" }>;

export type Head = readonly [
  major: number,
  additional: number,
  argument: bigint,
];

/** The bytes left in the current Flat chunk, opening the next one if spent. */
export const openChunk = (env: DecodeEnv, source: Source): Got<number> => {
  if (source.left > 0) return { k: "got", value: source.left, source };
  const p = source.position;
  if (p >= env.flatLength) return { k: "truncated", at: p };
  const n = byteAt(env, p);
  if (n === 0) return { k: "end", at: p };
  if (p + 1 + n > env.flatLength) return { k: "truncated", at: p };
  return { k: "got", value: n, source: { position: p + 1, left: n } };
};

export const takeByte = (env: DecodeEnv, source: Source): Got<number> => {
  const opened = openChunk(env, source);
  if (opened.k !== "got") return opened;
  const s = opened.source;
  return {
    k: "got",
    value: byteAt(env, s.position),
    source: { position: s.position + 1, left: s.left - 1 },
  };
};

export const takeUint = (
  env: DecodeEnv,
  source: Source,
  width: number,
): Got<bigint> => {
  let value = 0n;
  let current = source;
  for (let index = 0; index < width; index += 1) {
    const taken = takeByte(env, current);
    if (taken.k !== "got") return taken;
    value = value * 256n + BigInt(taken.value);
    current = taken.source;
  }
  return { k: "got", value, source: current };
};

/** A CBOR head after its initial byte; argument -1 for additional 31. */
export const headFrom = (
  env: DecodeEnv,
  initial: number,
  source: Source,
): Got<Head> => {
  const major = Math.floor(initial / 32);
  const additional = initial % 32;
  if (additional < 24) {
    return { k: "got", value: [major, additional, BigInt(additional)], source };
  }
  if (additional === 31)
    return { k: "got", value: [major, additional, -1n], source };
  if (additional > 27) return { k: "invalid" };
  const width =
    additional === 24 ? 1 : additional === 25 ? 2 : additional === 26 ? 4 : 8;
  const taken = takeUint(env, source, width);
  if (taken.k !== "got") return taken;
  return {
    k: "got",
    value: [major, additional, taken.value],
    source: taken.source,
  };
};

export const readHead = (env: DecodeEnv, source: Source): Got<Head> => {
  const taken = takeByte(env, source);
  if (taken.k !== "got") return taken;
  return headFrom(env, taken.value, taken.source);
};

export const skim = (ex: Exec, origin: Source): Tick =>
  to(ex, { k: "data", source: origin, mode: { k: "skim" } });

export const dataStop = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  got: Stop,
): Tick => {
  if (got.k === "end") {
    return refuse(env, ex, ScriptDecodeClass.invalidData, 8 * (got.at + 1));
  }
  if (got.k === "truncated") {
    return refuse(env, ex, ScriptDecodeClass.flatTruncated, 8 * got.at);
  }
  return skim(ex, origin);
};

/**
 * Steps over Flat chunks from the chunk head at `p`; at the terminating empty
 * chunk, `summary` (when given) becomes the constant's value, otherwise the
 * Data is invalid.
 */
export const stepChunks = (
  env: DecodeEnv,
  ex: Exec,
  start: number,
  summary: Summary | undefined,
): Tick => {
  let p = start;
  for (let budget = SKIM_CHUNKS_PER_TICK; budget > 0; budget -= 1) {
    if (p >= env.flatLength) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, 8 * p);
    }
    const n = byteAt(env, p);
    if (n === 0) {
      return summary === undefined
        ? refuse(env, ex, ScriptDecodeClass.invalidData, 8 * (p + 1))
        : at(ex, 8 * (p + 1), {
            k: "valueReturn",
            summary,
            memory: summary.memory,
          });
    }
    if (p + 1 + n > env.flatLength) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, 8 * p);
    }
    p += 1 + n;
  }
  const mode: DataMode =
    summary === undefined ? { k: "skim" } : { k: "trail", summary };
  return to(ex, { k: "data", source: { position: p, left: 0 }, mode });
};

/** The next chunk head or break of an indefinite byte string. */
export const nextChunk = (
  env: DecodeEnv,
  ex: Exec,
  origin: Source,
  finish: (source: Source) => Tick,
  resume: (source: Source, length: number) => Tick,
): Tick => {
  const taken = takeByte(env, origin);
  if (taken.k !== "got") return dataStop(env, ex, origin, taken);
  if (taken.value === CBOR_BREAK) return finish(taken.source);
  const head = headFrom(env, taken.value, taken.source);
  if (head.k !== "got") return dataStop(env, ex, origin, head);
  const [major, , length] = head.value;
  if (major !== 2 || length < 0n || length > MAX_DATA_BYTES_CHUNK) {
    return skim(ex, origin);
  }
  return resume(head.source, Number(length));
};

/** Up to `remaining` bytes from the current Flat chunk. */
export const takeSlice = (
  env: DecodeEnv,
  source: Source,
  remaining: number,
): Got<Buffer> => {
  const opened = openChunk(env, source);
  if (opened.k !== "got") return opened;
  const s = opened.source;
  const k = Math.min(remaining, opened.value);
  return {
    k: "got",
    value: env.window.subarray(s.position, s.position + k),
    source: { position: s.position + k, left: s.left - k },
  };
};
