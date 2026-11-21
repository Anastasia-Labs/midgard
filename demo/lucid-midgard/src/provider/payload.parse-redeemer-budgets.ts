import { ProviderPayloadError } from "../core/errors.js";
import { isObject } from "./payload.validate-supported-script-languages.js";
import type { MidgardRedeemerBudget } from "./types.js";

const DECIMAL = /^(?:0|[1-9][0-9]*)$/u;

/** Parses a `POST /tx/evaluate` 200 body. */
export const parseRedeemerBudgets = (
  payload: unknown,
  endpoint: string,
): readonly MidgardRedeemerBudget[] => {
  const invalid = (detail: string) =>
    new ProviderPayloadError(endpoint, "Invalid evaluation payload", detail);
  if (!isObject(payload) || !Array.isArray(payload.redeemers)) {
    throw invalid("expected { redeemers: [...] }");
  }
  return payload.redeemers.map((entry: unknown, position) => {
    if (!isObject(entry)) throw invalid(`redeemers[${position}]`);
    const { tag, index, memory, steps } = entry;
    if (
      typeof tag !== "number" ||
      !Number.isSafeInteger(tag) ||
      tag < 0 ||
      [index, memory, steps].some(
        (value) => typeof value !== "string" || !DECIMAL.test(value),
      )
    ) {
      throw invalid(`redeemers[${position}]`);
    }
    return {
      tag,
      index: BigInt(index as string),
      memory: BigInt(memory as string),
      steps: BigInt(steps as string),
    };
  });
};
