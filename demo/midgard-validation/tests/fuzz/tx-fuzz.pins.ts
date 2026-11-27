import type { CataloguedClass } from "./cek-step-fuzz.catalogue.js";

type Counts = Readonly<Partial<Record<CataloguedClass, number>>>;

/**
 * A CI row's pins: per seed, listing only seeds with any, the count of each
 * catalogued class; and each class's count over the row. A change inside a
 * class shows as a changed seed here, never as a hidden one.
 */
export type TxRowPins = {
  readonly seeds: Readonly<Record<string, Counts>>;
  readonly catalogued: Counts;
};

/** The fixed seed set's pins, by seed in decimal (`0xe3_0000 + i`). */
export const TX_FIXED_SET_PINS: TxRowPins = {
  seeds: { "14876683": { serialiseLoop: 1 } },
  catalogued: { serialiseLoop: 1 },
};

/** The corpus seeds' pins. */
export const TX_CORPUS_PINS: TxRowPins = {
  seeds: { "1088032817": { serialiseLoop: 1 } },
  catalogued: { serialiseLoop: 1 },
};
