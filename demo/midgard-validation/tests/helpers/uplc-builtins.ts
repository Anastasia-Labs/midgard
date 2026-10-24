import { UPLCBuiltinTag } from "@harmoniclabs/uplc";

import { harmonicBuiltinTag } from "../../src/cek-builtin.cardano-tags.js";
import { Builtin, MAX_CARDANO_BUILTIN_TAG } from "../../src/uplc-term.js";

/**
 * Midgard's builtin terms by name, for tests that build terms by hand. The
 * names are HarmonicLabs' (`UPLCBuiltinTag`); the term holds Cardano's tag
 * for that builtin, as every Midgard term does.
 */
export type BuiltinName = keyof typeof UPLCBuiltinTag;

const CARDANO_TAG_OF_HARMONIC: ReadonlyMap<number, number> = new Map(
  Array.from({ length: MAX_CARDANO_BUILTIN_TAG + 1 }, (_, cardanoTag) => [
    harmonicBuiltinTag(cardanoTag) as number,
    cardanoTag,
  ]),
);

/** A fresh builtin term for the builtin `name`. */
export const builtin = (name: BuiltinName): Builtin =>
  new Builtin(CARDANO_TAG_OF_HARMONIC.get(UPLCBuiltinTag[name])!);
