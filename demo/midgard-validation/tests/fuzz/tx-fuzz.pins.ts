import type { CataloguedClass } from "./cek-step-fuzz.catalogue.js";
import { REFERENCE_LEG_CLASSES, type TxKnownClass } from "./tx-fuzz.legs.js";

type Counts<Class extends string> = Readonly<Partial<Record<Class, number>>>;

/** One seed's known disagreements and catalogued steps, by class. */
export type TxSeedPins = {
  readonly known?: Counts<TxKnownClass>;
  readonly catalogued?: Counts<CataloguedClass>;
};

/**
 * A CI row's pins with the reference leg on: per seed, listing only seeds
 * with any, the count of each known class and of each catalogued class; and
 * each class's count over the row. A change inside a class shows as a
 * changed seed here, never as a hidden one. When the fix that removes a
 * class composes, its pins go with it.
 */
export type TxRowPins = {
  readonly seeds: Readonly<Record<string, TxSeedPins>>;
  readonly known: Counts<TxKnownClass>;
  readonly catalogued: Counts<CataloguedClass>;
};

const withoutReferenceClasses = (
  counts: Counts<TxKnownClass> | undefined,
): Counts<TxKnownClass> =>
  Object.fromEntries(
    Object.entries(counts ?? {}).filter(
      ([known]) => !REFERENCE_LEG_CLASSES.has(known as TxKnownClass),
    ),
  );

/** A row's pins as the run sees them with the reference leg off. */
export const pinsWithoutReferenceLeg = (pins: TxRowPins): TxRowPins => ({
  seeds: Object.fromEntries(
    Object.entries(pins.seeds).flatMap(([seed, { known, catalogued }]) => {
      const kept = withoutReferenceClasses(known);
      const entry: TxSeedPins = {
        ...(Object.keys(kept).length > 0 ? { known: kept } : {}),
        ...(catalogued === undefined ? {} : { catalogued }),
      };
      return Object.keys(entry).length > 0 ? [[seed, entry]] : [];
    }),
  ),
  known: withoutReferenceClasses(pins.known),
  catalogued: pins.catalogued,
});

/** The fixed seed set's pins, by seed in decimal (`0xe3_0000 + i`). */
export const TX_FIXED_SET_PINS: TxRowPins = {
  seeds: {
    "14876674": { known: { blsTagNumbering: 1 } },
    "14876681": { known: { startupCharge: 1 } },
    "14876682": { known: { blsTagNumbering: 1 } },
    "14876683": { known: { blsTagNumbering: 1 } },
    "14876686": { known: { blsTagNumbering: 1 } },
    "14876687": { known: { costBeforeC1: 1 } },
    "14876691": { known: { startupCharge: 1 } },
    "14876693": { known: { blsTagNumbering: 1 } },
    "14876695": { known: { costBeforeC1: 1 } },
    "14876701": { known: { blsTagNumbering: 1 } },
    "14876704": { known: { startupCharge: 1 } },
    "14876709": { known: { blsTagNumbering: 1 } },
    "14876711": { known: { costBeforeC1: 1 } },
    "14876712": { known: { nonUnitReturn: 1 } },
    "14876718": { known: { blsTagNumbering: 1 } },
    "14876721": { known: { nonUnitReturn: 1 } },
    "14876725": { known: { blsTagNumbering: 1 } },
    "14876729": { known: { costBeforeC1: 1 } },
    "14876730": { known: { blsTagNumbering: 1 } },
    "14876731": { known: { blsTagNumbering: 1 } },
    "14876734": { known: { costBeforeC1: 1 } },
    "14876735": { known: { costBeforeC1: 1 } },
  },
  known: {
    blsTagNumbering: 11,
    costBeforeC1: 6,
    nonUnitReturn: 2,
    startupCharge: 3,
  },
  catalogued: {},
};

/** The corpus seeds' pins. */
export const TX_CORPUS_PINS: TxRowPins = {
  seeds: {
    "322290577": { known: { costBeforeC1: 1 } },
    "330470803": { known: { caseOnConstant: 1 } },
    "590911258": { known: { caseOnConstant: 1 } },
    "1088032817": { known: { costBeforeC1: 1 } },
    "1405241274": { known: { blsTagNumbering: 1 } },
    "1479182247": { known: { costBeforeC1: 1 } },
  },
  known: {
    blsTagNumbering: 1,
    caseOnConstant: 2,
    costBeforeC1: 3,
  },
  catalogued: {},
};
