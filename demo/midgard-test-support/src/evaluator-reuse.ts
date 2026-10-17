/**
 * Exact-request reuse for the custom Plutus evaluators test harnesses hand to
 * Lucid (Scalus, or an isolated-process UPLC evaluator).
 *
 * Lucid's fixed-point evaluation re-submits an unchanged transaction to
 * confirm its budgets are stable, and suites build the same transaction more
 * than once. Midgard's Lucid patch already reuses the last result of the
 * default Aiken evaluator; an explicit custom evaluator gets no reuse there.
 * Plutus evaluation is a pure function of the request, so a harness that
 * wraps its evaluator with {@link withExactRequestReuse} returns a copy of an
 * earlier result for a byte-identical request instead of running the same
 * scripts again.
 *
 * The key is everything a Lucid custom evaluator receives: the transaction
 * CBOR, the resolved UTxOs, the network, the slot configuration, the whole
 * protocol parameters (cost models, protocol limits and the per-transaction
 * execution budget) and the CBOR of the cost models object. The wrapped
 * evaluator's own options (a protocol major version, say) are fixed per
 * instance, so they are part of the key by construction. UTxOs and parameters
 * are compared as their whole JSON, which is stricter than any evaluator's
 * view of them.
 *
 * Every distinct request still reaches the wrapped evaluator; a failed
 * evaluation is never retained, so a refusal is re-evaluated (and re-reports
 * its own error) every time. Callers never share the retained objects: a hit
 * returns a structured clone. The cache holds at most `capacity` results and
 * evicts the least recently used.
 */

type EvaluationRequest = {
  readonly tx: string;
  readonly additionalUTxOs: unknown;
  readonly context: {
    readonly network?: unknown;
    readonly slotConfig: {
      readonly zeroTime: number;
      readonly zeroSlot: number;
      readonly slotLength: number;
    };
    readonly protocolParameters: unknown;
    readonly costModels?: { readonly to_cbor_hex: () => string } | null;
  };
};

// Method syntax on purpose: any Lucid custom evaluator, whose request type is
// narrower than this structural one, is accepted.
type ReusableEvaluator = {
  evaluate(request: EvaluationRequest): Promise<unknown>;
};

/** Default bound on retained results per wrapped evaluator. */
export const DEFAULT_EVALUATOR_REUSE_CAPACITY = 16;

const withBigInts = (_key: string, value: unknown): unknown =>
  typeof value === "bigint" ? `${value.toString()}n` : value;

const exactRequestKey = ({
  tx,
  additionalUTxOs,
  context,
}: EvaluationRequest): string =>
  JSON.stringify(
    [
      tx,
      additionalUTxOs,
      context.network ?? null,
      context.slotConfig.zeroTime,
      context.slotConfig.zeroSlot,
      context.slotConfig.slotLength,
      context.protocolParameters,
      context.costModels?.to_cbor_hex() ?? null,
    ],
    withBigInts,
  );

/**
 * `evaluator`, returning a structured clone of a retained result for a
 * request identical to one it already evaluated successfully.
 */
export const withExactRequestReuse = <Wrapped extends ReusableEvaluator>(
  evaluator: Wrapped,
  { capacity = DEFAULT_EVALUATOR_REUSE_CAPACITY } = {},
): Wrapped => {
  if (!Number.isInteger(capacity) || capacity < 1)
    throw new Error(`evaluator reuse capacity must be >= 1, got ${capacity}`);
  const retained = new Map<string, unknown>();
  const evaluate = async (request: EvaluationRequest): Promise<unknown> => {
    const key = exactRequestKey(request);
    if (retained.has(key)) {
      const result = retained.get(key);
      // Most recently used last, so eviction takes the oldest.
      retained.delete(key);
      retained.set(key, result);
      return structuredClone(result);
    }
    const fresh = await evaluator.evaluate(request);
    retained.set(key, structuredClone(fresh));
    if (retained.size > capacity)
      retained.delete(retained.keys().next().value as string);
    return fresh;
  };
  return { ...evaluator, evaluate } as Wrapped;
};
