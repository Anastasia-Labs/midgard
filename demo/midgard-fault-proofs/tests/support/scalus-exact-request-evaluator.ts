import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

type ScalusEvaluator = ReturnType<typeof createScalusEvaluator>;
type EvaluationInput = Parameters<ScalusEvaluator["evaluate"]>[0];
type EvaluationResult = Awaited<ReturnType<ScalusEvaluator["evaluate"]>>;

const withBigInts = (_key: string, value: unknown): unknown =>
  typeof value === "bigint" ? `${value.toString()}n` : value;

/**
 * Everything the Scalus evaluator reads: the transaction bytes, the resolved
 * UTxOs it turns into the UTxO map, the slot configuration, and the protocol
 * parameters it takes the cost models and protocol major version from. The
 * UTxOs and parameters are compared as their whole JSON, which is stricter
 * than the evaluator's own view of them, so two requests share a key only
 * when the evaluator would receive byte-identical arguments.
 */
const exactRequestKey = ({
  tx,
  additionalUTxOs,
  context,
}: EvaluationInput): string =>
  JSON.stringify(
    [
      tx,
      additionalUTxOs,
      context.slotConfig.zeroTime,
      context.slotConfig.zeroSlot,
      context.slotConfig.slotLength,
      context.protocolParameters,
    ],
    withBigInts,
  );

/**
 * The real Scalus evaluator, with the exact-request reuse Midgard's Lucid
 * patch already gives the default Aiken evaluator: Lucid's fixed-point
 * evaluation re-submits an unchanged transaction once more to confirm the
 * budgets are stable, and Plutus evaluation is a pure function of its inputs,
 * so a byte-identical repeat of the immediately preceding request returns a
 * copy of that request's result instead of evaluating the same scripts again.
 * Every distinct request is still evaluated by Scalus, a failed evaluation is
 * never retained, and callers never share the retained result's objects.
 */
export const createExactRequestReusingScalusEvaluator = (): ScalusEvaluator => {
  const scalus = createScalusEvaluator();
  let previous:
    | { readonly key: string; readonly result: EvaluationResult }
    | undefined;
  return {
    name: scalus.name,
    evaluate: async (input) => {
      const key = exactRequestKey(input);
      if (previous !== undefined && previous.key === key) {
        return structuredClone(previous.result);
      }
      previous = undefined;
      const result = await scalus.evaluate(input);
      previous = { key, result: structuredClone(result) };
      return result;
    },
  };
};
