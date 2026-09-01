import type { LucidOptions } from "@lucid-evolution/lucid";

type Evaluator = NonNullable<LucidOptions["evaluator"]>;
type EvalRedeemer = Awaited<ReturnType<Evaluator["evaluate"]>>[number];
type Fetch = typeof globalThis.fetch;

const REDEEMER_PURPOSES = new Set<EvalRedeemer["redeemer_tag"]>([
  "spend",
  "mint",
  "publish",
  "withdraw",
  "vote",
  "propose",
]);

export const createProviderBackedEvaluator = (
  ogmiosUrl: string,
  fetchImpl: Fetch = globalThis.fetch,
): Evaluator => ({
  name: "local-ogmios-cardano-node",
  evaluate: async ({ tx }) => {
    const response = await fetchImpl(ogmiosUrl, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "evaluateTransaction",
        params: { transaction: { cbor: tx } },
        id: "midgard-da-local-evaluation",
      }),
    });
    const body: unknown = await response.json();
    if (!response.ok) {
      throw new Error(formatOgmiosError(response.status, body));
    }
    return parseEvaluationResult(body);
  },
});

const parseEvaluationResult = (body: unknown): EvalRedeemer[] => {
  if (!isRecord(body) || !Array.isArray(body.result)) {
    throw new Error(`Invalid Ogmios evaluation response: ${JSON.stringify(body)}`);
  }
  return body.result.map((entry, index) => {
    if (
      !isRecord(entry) ||
      !isRecord(entry.validator) ||
      !isRecord(entry.budget) ||
      typeof entry.validator.purpose !== "string" ||
      !REDEEMER_PURPOSES.has(
        entry.validator.purpose as EvalRedeemer["redeemer_tag"],
      ) ||
      !isNonNegativeSafeInteger(entry.validator.index) ||
      !isNonNegativeSafeInteger(entry.budget.memory) ||
      !isNonNegativeSafeInteger(entry.budget.cpu)
    ) {
      throw new Error(
        `Invalid Ogmios evaluation result at index ${index.toString()}`,
      );
    }
    return {
      redeemer_tag: entry.validator.purpose as EvalRedeemer["redeemer_tag"],
      redeemer_index: entry.validator.index,
      ex_units: {
        mem: entry.budget.memory,
        steps: entry.budget.cpu,
      },
    };
  });
};

const formatOgmiosError = (status: number, body: unknown): string => {
  if (isRecord(body) && isRecord(body.error)) {
    const code = body.error.code;
    const message = body.error.message;
    if (
      (typeof code === "number" || typeof code === "string") &&
      typeof message === "string"
    ) {
      return `Ogmios JSON-RPC error ${String(code)}: ${message}`;
    }
  }
  return `Ogmios evaluation request failed with HTTP ${status.toString()}: ${JSON.stringify(body)}`;
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const isNonNegativeSafeInteger = (value: unknown): value is number =>
  typeof value === "number" && Number.isSafeInteger(value) && value >= 0;
