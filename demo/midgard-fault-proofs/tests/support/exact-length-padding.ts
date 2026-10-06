/**
 * Searches for the padding that brings a fixture to an exact encoded length,
 * the way boundary tests pin a field at its consensus maximum.
 *
 * The search tries every padding in `[from, to)` in order and returns the
 * first candidate whose measured length is exactly `target`, so the result
 * does not depend on the measure being monotone. Each try encodes a fixture
 * near the boundary (tens of kilobytes), so a scan over a few thousand
 * paddings costs about a second. The result is therefore retained per worker
 * process, keyed on every input: the `candidate` and `measure` functions
 * themselves and the three numbers. Every call returns its own copy of the
 * bytes. Both functions must be pure (the same padding always builds and
 * measures the same bytes); declare them once at module scope so repeated
 * calls share the key, as a closure built per call never does.
 */

type ExactLengthSearch = {
  /** The length the measured candidate must reach exactly. */
  readonly target: number;
  /** First padding tried. */
  readonly from: number;
  /** Paddings at or above this are not tried. */
  readonly to: number;
  /** The fixture bytes for one padding. */
  readonly candidate: (padding: number) => Uint8Array;
  /** The encoded length that must equal `target`. */
  readonly measure: (candidate: Uint8Array) => number;
};

type Retained = Map<string, Uint8Array>;

const retained = new WeakMap<
  ExactLengthSearch["candidate"],
  WeakMap<ExactLengthSearch["measure"], Retained>
>();

const retainedFor = ({ candidate, measure }: ExactLengthSearch): Retained => {
  let byMeasure = retained.get(candidate);
  if (byMeasure === undefined) {
    byMeasure = new WeakMap();
    retained.set(candidate, byMeasure);
  }
  let results = byMeasure.get(measure);
  if (results === undefined) {
    results = new Map();
    byMeasure.set(measure, results);
  }
  return results;
};

/**
 * The first candidate in `[from, to)` whose measure is exactly `target`, as a
 * fresh Buffer; throws when no padding in the range reaches it.
 */
export const searchExactLengthPadding = (search: ExactLengthSearch): Buffer => {
  const { target, from, to, candidate, measure } = search;
  const results = retainedFor(search);
  const key = `${target}:${from}:${to}`;
  let found = results.get(key);
  if (found === undefined) {
    for (let padding = from; padding < to && found === undefined; padding++) {
      const bytes = candidate(padding);
      if (measure(bytes) === target) found = Uint8Array.from(bytes);
    }
    if (found === undefined)
      throw new Error(
        `no padding in [${from}, ${to}) measures exactly ${target} bytes`,
      );
    results.set(key, found);
  }
  return Buffer.from(found);
};
