import { encodeCbor } from "./cbor.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import { EMPTY_NULL_ROOT } from "./native-constants.js";

export type ScriptLanguageName = "PlutusV3" | "MidgardV1";
export type CanonicalCostModelView = readonly number[];
export type ScriptLanguageViewMap = ReadonlyMap<number, CanonicalCostModelView>;

export const ScriptLanguageTags = {
  PlutusV3: 2,
  MidgardV1: 0x80,
} as const satisfies Record<ScriptLanguageName, number>;

export type ScriptLanguageTag =
  (typeof ScriptLanguageTags)[keyof typeof ScriptLanguageTags];

export const SCRIPT_LANGUAGE_NAMES_BY_TAG = new Map<
  ScriptLanguageTag,
  ScriptLanguageName
>([
  [ScriptLanguageTags.PlutusV3, "PlutusV3"],
  [ScriptLanguageTags.MidgardV1, "MidgardV1"],
]);

const freezeCostModelView = (
  values: readonly number[],
): CanonicalCostModelView => Object.freeze([...values]);

export const PLUTUS_V3_CANONICAL_COST_MODEL_VIEW = freezeCostModelView([
  100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305,
  8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000,
  100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1,
  22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848,
  0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558,
  1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169,
  4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852,
  32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305,
  -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203,
  7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716,
  549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566,
  4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142,
  32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333,
  10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6,
  442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18,
  1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36,
  158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72,
  2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623,
  251, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719,
  0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1,
  158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1,
  1964219, 24520, 3,
]);

export const MIDGARD_CANONICAL_COST_MODEL_VIEW = freezeCostModelView(
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
);

export const SCRIPT_LANGUAGE_COST_MODEL_VIEWS = new Map<
  ScriptLanguageTag,
  CanonicalCostModelView
>([
  [ScriptLanguageTags.PlutusV3, PLUTUS_V3_CANONICAL_COST_MODEL_VIEW],
  [ScriptLanguageTags.MidgardV1, MIDGARD_CANONICAL_COST_MODEL_VIEW],
]);

export type SupportedScriptLanguageTag = {
  readonly name: ScriptLanguageName;
  readonly tag: ScriptLanguageTag;
};

export const MIDGARD_SUPPORTED_SCRIPT_LANGUAGES = Object.freeze([
  Object.freeze({
    name: "PlutusV3",
    tag: ScriptLanguageTags.PlutusV3,
  }),
  Object.freeze({
    name: "MidgardV1",
    tag: ScriptLanguageTags.MidgardV1,
  }),
] as const satisfies readonly SupportedScriptLanguageTag[]);

export const isScriptLanguageTag = (tag: number): tag is ScriptLanguageTag =>
  SCRIPT_LANGUAGE_NAMES_BY_TAG.has(tag as ScriptLanguageTag);

export const scriptLanguageNameToTag = (
  language: ScriptLanguageName,
): ScriptLanguageTag => ScriptLanguageTags[language];

export const scriptLanguageTagToName = (
  tag: ScriptLanguageTag,
): ScriptLanguageName => {
  const name = SCRIPT_LANGUAGE_NAMES_BY_TAG.get(tag);
  if (name === undefined) {
    throw new Error(`unsupported script language tag ${tag.toString(10)}`);
  }
  return name;
};

const normalizeScriptLanguageTag = (
  tag: ScriptLanguageTag | ScriptLanguageName,
): ScriptLanguageTag =>
  typeof tag === "string" ? scriptLanguageNameToTag(tag) : tag;

export const buildScriptLanguageViews = (
  languages: readonly (ScriptLanguageTag | ScriptLanguageName)[],
): Map<ScriptLanguageTag, CanonicalCostModelView> => {
  const unique = new Set(languages.map(normalizeScriptLanguageTag));
  const result = new Map<ScriptLanguageTag, CanonicalCostModelView>();
  for (const tag of [...unique].sort((left, right) => left - right)) {
    const view = SCRIPT_LANGUAGE_COST_MODEL_VIEWS.get(tag);
    if (view === undefined) {
      throw new Error(`unsupported script language tag ${tag.toString(10)}`);
    }
    result.set(tag, view);
  }
  return result;
};

export const computeScriptIntegrityHash = (
  redeemerTxWitsHash: Uint8Array,
  scriptLanguageViews: ScriptLanguageViewMap,
): Hash32 => {
  if (scriptLanguageViews.size === 0) {
    return Buffer.from(EMPTY_NULL_ROOT);
  }
  return computeHash32(
    encodeCbor([
      ensureHash32(redeemerTxWitsHash, "redeemer_tx_wits_hash"),
      new Map(
        [...scriptLanguageViews.entries()].sort(
          ([left], [right]) => left - right,
        ),
      ),
    ]),
  );
};

export const computeScriptIntegrityHashForLanguages = (
  redeemerTxWitsHash: Uint8Array,
  languages: readonly (ScriptLanguageTag | ScriptLanguageName)[],
): Hash32 =>
  computeScriptIntegrityHash(
    redeemerTxWitsHash,
    buildScriptLanguageViews(languages),
  );
