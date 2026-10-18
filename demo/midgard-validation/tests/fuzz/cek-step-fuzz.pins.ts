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
    "seed 4028760658 builtin 37": { nestedConstantCost: 1 },
    "seed 4028760994 builtin 58": { nestedConstantCost: 1 },
    "seed 4028761122 builtin 66": { blsInvalidPointPending: 1 },
    "seed 4028761125 builtin 66": { blsInvalidPointPending: 1 },
  },
  totals: { nestedConstantCost: 2, blsInvalidPointPending: 2 },
};

/** The checked-in corpus's catalogued steps. */
export const STEP_CORPUS_PINS: StepRowPins = {
  vectors: {
    "seed 505612127 builtin 30": { nestedConstantCost: 1 },
    "seed 2002769749 builtin 52": { nestedConstantCost: 1 },
    "seed 607448503 builtin 77": { nestedConstantCost: 1 },
    "seed 1676007778 builtin 28": { nestedConstantCost: 1 },
  },
  totals: { nestedConstantCost: 4 },
};
