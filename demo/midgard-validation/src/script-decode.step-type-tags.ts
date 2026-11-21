/**
 * The type tag list of a constant in the script-decode step machine, as in
 * `onchain/aiken/lib/midgard/script-decode-v1.ak`.
 */
import { hashMidgardCekBlobChunk } from "@al-ft/midgard-core";

import {
  intData,
  serialiseScriptDecodeData,
} from "./script-decode.step-data.js";
import {
  at,
  type DecodeEnv,
  type Exec,
  have,
  push,
  read,
  refuse,
  ScriptDecodeClass,
  type Tick,
} from "./script-decode.step-exec.js";
import type { ConstantType, Control } from "./script-decode.step-types.js";

const MAX_TYPE_TAGS = 32;
const TYPE_TAGS_PER_TICK = 8;

/**
 * One type tag through the kind grammar: `pending` types remain, the next
 * symbol sits under `top` (0 a type, 1 one application, 2 two).
 */
const kindStep = (
  pending: number,
  top: number,
  bad: boolean,
  tag: number,
): readonly [number, number, boolean] => {
  if (bad || pending === 0) return [pending, top, true];
  if (tag === 7)
    return top === 2 ? [pending, top, true] : [pending, top + 1, false];
  if (tag === 5 || tag === 12) {
    return top === 1 ? [pending, 0, false] : [pending, top, true];
  }
  if (tag === 6) {
    return top === 2 ? [pending + 1, 0, false] : [pending, top, true];
  }
  if (tag === 14 || tag === 15) return [pending, top, true];
  return top === 0 ? [pending - 1, 0, false] : [pending, top, true];
};

const SIMPLE_TYPES: Readonly<Record<number, ConstantType>> = {
  0: { k: "integer" },
  1: { k: "bytestring" },
  2: { k: "string" },
  3: { k: "unit" },
  4: { k: "boolean" },
  8: { k: "data" },
  9: { k: "g1" },
  10: { k: "g2" },
  11: { k: "ml" },
  13: { k: "value" },
};

const parseType = (
  tags: readonly number[],
  from: number,
): readonly [ConstantType, number] => {
  const tag = tags[from];
  if (tag === undefined) throw new Error("script decode: empty type tags");
  if (tag === 5 || tag === 12) {
    const [element, after] = parseType(tags, from + 1);
    return [{ k: tag === 5 ? "list" : "array", element }, after];
  }
  if (tag === 6) {
    const [first, middle] = parseType(tags, from + 1);
    const [second, after] = parseType(tags, middle);
    return [{ k: "pair", first, second }, after];
  }
  const simple = SIMPLE_TYPES[tag];
  if (simple === undefined) throw new Error("script decode: bad type tag");
  return [simple, from + 1];
};

export const tickTypes = (
  env: DecodeEnv,
  ex: Exec,
  control: Extract<Control, { k: "types" }>,
): Tick => {
  let { count, pending, top, bad, tags } = control;
  let c = ex.cursor;
  for (let budget = TYPE_TAGS_PER_TICK; budget > 0; budget -= 1) {
    if (!have(env, c, 1)) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, c);
    }
    if (read(env, c, 1) === 0) {
      const after = c + 1;
      if (bad || pending !== 0) {
        return refuse(env, ex, ScriptDecodeClass.unknownConstantType, after);
      }
      if (count > MAX_TYPE_TAGS) {
        return refuse(env, ex, ScriptDecodeClass.constantTypeTooLarge, after);
      }
      const ordered = [...tags].reverse();
      const [ty] = parseType(ordered, 0);
      return at(
        push(ex, {
          k: "constant",
          typeHash: Buffer.from(
            hashMidgardCekBlobChunk(
              serialiseScriptDecodeData(ordered.map(intData)),
            ),
          ),
        }),
        after,
        { k: "value", ty },
      );
    }
    if (!have(env, c + 1, 4)) {
      return refuse(env, ex, ScriptDecodeClass.flatTruncated, c + 1);
    }
    const tag = read(env, c + 1, 4);
    [pending, top, bad] = kindStep(pending, top, bad, tag);
    if (tag !== 7 && count + 1 <= MAX_TYPE_TAGS) tags = [tag, ...tags];
    count += 1;
    c += 5;
  }
  return at(ex, c, { k: "types", count, pending, top, bad, tags });
};
