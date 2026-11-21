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
    "seed 4028760629 builtin 35": { serialiseLoop: 1 },
    "seed 4028760658 builtin 37": { nestedConstantCost: 1 },
    "seed 4028760880 builtin 51": { serialiseLoop: 1 },
    "seed 4028760881 builtin 51": { serialiseLoop: 1 },
    "seed 4028760883 builtin 51": { serialiseLoop: 1 },
    "seed 4028760884 builtin 51": { serialiseLoop: 1 },
  },
  totals: {
    nestedConstantCost: 1,
    serialiseLoop: 5,
  },
};

/** The checked-in corpus's catalogued steps. */
export const STEP_CORPUS_PINS: StepRowPins = {
  vectors: { "seed 4028760881 builtin 51": { serialiseLoop: 1 } },
  totals: { serialiseLoop: 1 },
};
