/**
 * Builtin tags as Cardano's flat encoding numbers them (Plutus
 * `Builtins.hs`), and the one translation to `@harmoniclabs/uplc`'s numbers.
 *
 * Midgard's machine, its material and every tag-keyed table use Cardano's
 * numbers. The reference library numbers six BLS builtins differently
 * (hashToGroup, compress, uncompress where Cardano has compress, uncompress,
 * hashToGroup), so a call into its evaluator or cost model goes through
 * `harmonicBuiltinTag`. Every other tag is the same in both.
 */
import type { UPLCBuiltinTag } from "@harmoniclabs/uplc";

/** Cardano's flat tags for the BLS builtins the two numberings disagree on. */
export const MIDGARD_CEK_CARDANO_BLS_TAGS = Object.freeze({
  g1Compress: 58,
  g1Uncompress: 59,
  g1HashToGroup: 60,
  g2Compress: 65,
  g2Uncompress: 66,
  g2HashToGroup: 67,
});

const CARDANO_TO_HARMONIC: ReadonlyMap<number, number> = new Map([
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g1Compress, 59],
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g1Uncompress, 60],
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g1HashToGroup, 58],
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g2Compress, 66],
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g2Uncompress, 67],
  [MIDGARD_CEK_CARDANO_BLS_TAGS.g2HashToGroup, 65],
]);

/** The reference library's tag for Cardano's builtin `cardanoTag`. */
export const harmonicBuiltinTag = (cardanoTag: number): UPLCBuiltinTag =>
  (CARDANO_TO_HARMONIC.get(cardanoTag) ?? cardanoTag) as UPLCBuiltinTag;
