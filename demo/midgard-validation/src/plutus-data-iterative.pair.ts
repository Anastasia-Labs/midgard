import { type Data, DataPair } from "@harmoniclabs/plutus-data";

/**
 * The only place a harmonic `DataPair` is constructed.
 *
 * Harmonic plutus-data 1.2.6 builds the constructor's assertion message
 * eagerly by stringifying both halves, which walks each half recursively. How
 * a pair is built without that walk is an open decision; keeping every
 * construction behind this one function (enforced by the
 * `midgard/no-recursive-plutus-data` lint rule) makes that a one-file change.
 */
export const midgardDataPair = <K extends Data, V extends Data>(
  fst: K,
  snd: V,
): DataPair<K, V> => new DataPair(fst, snd);
