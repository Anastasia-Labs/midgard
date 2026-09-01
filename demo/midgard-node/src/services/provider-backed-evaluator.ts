import type { LucidOptions } from "@lucid-evolution/lucid";

type Evaluator = NonNullable<LucidOptions["evaluator"]>;
type EvaluationInput = Parameters<Evaluator["evaluate"]>[0];

export const createProviderBackedEvaluator = (
  evaluateTx: (
    tx: EvaluationInput["tx"],
    additionalUTxOs: EvaluationInput["additionalUTxOs"],
  ) => ReturnType<Evaluator["evaluate"]>,
): Evaluator => ({
  name: "local-ogmios-cardano-node",
  evaluate: ({ tx, additionalUTxOs }) => evaluateTx(tx, additionalUTxOs),
});
