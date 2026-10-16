import type { CataloguedClass } from "./cek-step-fuzz.catalogue.js";

/** Steps per catalogued class. */
export type ClassCounts = Readonly<Partial<Record<CataloguedClass, number>>>;

/**
 * A CI row's catalogued steps: per vector label, the steps of each class,
 * listing only vectors with any; and the steps of each class over the row. A
 * change inside a class shows as a changed vector here, never as a hidden
 * one. When the fix that removes a class composes, its pins go with it.
 */
export type StepRowPins = {
  readonly vectors: Readonly<Record<string, ClassCounts>>;
  readonly totals: ClassCounts;
};

/** The fixed seed set's catalogued steps. */
export const STEP_FIXED_SET_PINS: StepRowPins = {
  vectors: {
    "seed 4028760067 builtin 0": { interimDataDecoding: 1 },
    "seed 4028760099 builtin 2": { interimDataDecoding: 2 },
    "seed 4028760130 builtin 4": { interimDataDecoding: 1 },
    "seed 4028760149 builtin 5": { interimDataDecoding: 2 },
    "seed 4028760164 builtin 6": { interimDataDecoding: 2 },
    "seed 4028760165 builtin 6": { interimDataDecoding: 1 },
    "seed 4028760193 builtin 8": { interimDataDecoding: 2 },
    "seed 4028760244 builtin 11": { interimDataDecoding: 1 },
    "seed 4028760288 builtin 14": { interimDataDecoding: 1 },
    "seed 4028760355 builtin 18": { blsInvalidPointPending: 1 },
    "seed 4028760450 builtin 24": { interimDataDecoding: 2 },
    "seed 4028760561 builtin 31": { interimDataDecoding: 1 },
    "seed 4028760596 builtin 33": { interimDataDecoding: 1 },
    "seed 4028760624 builtin 35": { interimDataDecoding: 2 },
    "seed 4028760626 builtin 35": { interimDataDecoding: 2 },
    "seed 4028760641 builtin 36": { interimDataDecoding: 1 },
    "seed 4028760643 builtin 36": { interimDataDecoding: 1 },
    "seed 4028760644 builtin 36": { interimDataDecoding: 3 },
    "seed 4028760656 builtin 37": { interimDataDecoding: 3 },
    "seed 4028760657 builtin 37": { interimDataDecoding: 1 },
    "seed 4028760658 builtin 37": { interimDataDecoding: 1 },
    "seed 4028760659 builtin 37": { interimDataDecoding: 1 },
    "seed 4028760673 builtin 38": { interimDataDecoding: 1 },
    "seed 4028760677 builtin 38": { interimDataDecoding: 1 },
    "seed 4028760689 builtin 39": { interimDataDecoding: 1 },
    "seed 4028760691 builtin 39": { interimDataDecoding: 1 },
    "seed 4028760725 builtin 41": { interimDataDecoding: 1 },
    "seed 4028760737 builtin 42": { interimDataDecoding: 2 },
    "seed 4028760755 builtin 43": { interimDataDecoding: 1 },
    "seed 4028760756 builtin 43": { interimDataDecoding: 2 },
    "seed 4028760773 builtin 44": { interimDataDecoding: 2 },
    "seed 4028760801 builtin 46": { interimDataDecoding: 1 },
    "seed 4028760804 builtin 46": { interimDataDecoding: 1 },
    "seed 4028760816 builtin 47": { interimDataDecoding: 1 },
    "seed 4028760817 builtin 47": { interimDataDecoding: 1 },
    "seed 4028760818 builtin 47": { interimDataDecoding: 1 },
    "seed 4028760881 builtin 51": { interimDataDecoding: 1 },
    "seed 4028760882 builtin 51": { interimDataDecoding: 1 },
    "seed 4028760885 builtin 51": { interimDataDecoding: 2 },
    "seed 4028760896 builtin 52": { interimDataDecoding: 1 },
    "seed 4028761008 builtin 59": { interimDataDecoding: 1 },
    "seed 4028761075 builtin 63": { interimDataDecoding: 2 },
    "seed 4028761093 builtin 64": { interimDataDecoding: 1 },
    "seed 4028761136 builtin 67": { interimDataDecoding: 1 },
    "seed 4028761235 builtin 73": { interimDataDecoding: 1 },
    "seed 4028761249 builtin 74": { interimDataDecoding: 1 },
    "seed 4028761250 builtin 74": { interimDataDecoding: 3 },
    "seed 4028761380 builtin 82": { interimDataDecoding: 2 },
    "seed 4028761395 builtin 83": { interimDataDecoding: 1 },
    "seed 4028761443 builtin 86": { interimDataDecoding: 1 },
    "seed 4028761444 builtin 86": { interimDataDecoding: 1 },
  },
  totals: { interimDataDecoding: 69, blsInvalidPointPending: 1 },
};

/** The checked-in corpus's catalogued steps. */
export const STEP_CORPUS_PINS: StepRowPins = {
  vectors: {
    "seed 4028760067 builtin 0": { interimDataDecoding: 1 },
    "seed 4028760355 builtin 18": { blsInvalidPointPending: 1 },
    "seed 4028760881 builtin 51": { interimDataDecoding: 1 },
    "seed 2090822232 builtin 84": { trailingEmptyBytesDecoding: 1 },
    "seed 1996701279 builtin 50": { trailingEmptyBytesDecoding: 1 },
    "seed 948653697 builtin 48": { trailingEmptyBytesDecoding: 1 },
    "seed 1806888635 builtin 43": { trailingEmptyBytesDecoding: 1 },
    "seed 363418071 builtin 38": { trailingEmptyBytesDecoding: 1 },
    "seed 505612127 builtin 30": { nestedConstantCost: 1 },
    "seed 2002769749 builtin 52": {
      nestedConstantCost: 1,
      interimDataDecoding: 1,
    },
    "seed 607448503 builtin 77": { nestedConstantCost: 1 },
    "seed 1676007778 builtin 28": { nestedConstantCost: 1 },
    "seed 656232640 builtin 56": { nestedConstantCost: 2 },
  },
  totals: {
    interimDataDecoding: 3,
    blsInvalidPointPending: 1,
    trailingEmptyBytesDecoding: 5,
    nestedConstantCost: 6,
  },
};
