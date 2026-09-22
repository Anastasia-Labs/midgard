/**
 * The two forced-direction output-field shapes the networkId suites pin.
 *
 * Both are properties of the §8 carriage planner, not of any one measurement:
 * the §10 scan folds the field in batches, so the bound on a provable forced
 * transaction is the largest field the carriage can authenticate, not the
 * largest one a single transaction can decode. They live here rather than in a
 * suite because the fit ledger and the lifecycle suite must pin the same two
 * numbers, and a divergence between them would be a silent coverage hole.
 */

/**
 * Largest outputs-field item count the planner still places in raw-UTxO
 * carriage (15,139 preimage bytes at 352 minimal outputs). The scan opens this
 * shape with `Open` and folds it in `ceil(352 / 64) = 6` batches.
 */
export const MAXIMUM_SUPPORTED_OUTPUT_COUNT = 352;

/**
 * The adjacent shape one output above it, which the planner places in tier-3
 * certified carriage. Its §5.1 item count is provisional, so the scan has to
 * certify the envelope grammar first — `StartGrammar` plus
 * `ceil(353 / 64) - 1 = 5` further grammar batches at the payable driver batch
 * and `FinishGrammar` — and only then folds, 48 outputs per batch over chunked
 * carriage. It is convictable, not a bound.
 */
export const MAXIMUM_CERTIFIED_OUTPUT_COUNT =
  MAXIMUM_SUPPORTED_OUTPUT_COUNT + 1;
