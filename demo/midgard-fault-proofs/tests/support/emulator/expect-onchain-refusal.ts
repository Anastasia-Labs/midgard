/**
 * Set when the suite runs against a blueprint whose refusing validators are
 * traced (scripts/build-traced-emulator-blueprint.mjs). A pinned refusal must
 * then carry its trace, so a negative cannot pass on an untraced script.
 */
const TRACED_REFUSALS_REQUIRED =
  process.env.MIDGARD_EMULATOR_TRACED_REFUSALS === "1";

/**
 * The trace a traced validator's failure carries, or null when untraced. The
 * evaluator's message can arrive JSON-quoted.
 */
const refusalTrace = (text: string): string | null => {
  let message = text.trim();
  if (message.startsWith('"')) {
    try {
      message = String(JSON.parse(message));
    } catch {
      // Not a JSON string; match the raw text.
    }
  }
  const at = message.indexOf(" Trace ");
  return at === -1 ? null : message.slice(at + " Trace ".length).trim();
};

/**
 * Assert a negative reaches local UPLC evaluation and fails in a validator,
 * rather than passing because the off-chain builder happened to throw.
 *
 * `check` pins the refusing check by its trace. Plain builds carry no trace,
 * so the pin applies only when the refusing validator is traced, and the run
 * fails if MIDGARD_EMULATOR_TRACED_REFUSALS=1 and it is not.
 */
export const expectOnchainRefusal = async (
  build: () => Promise<unknown>,
  check?: RegExp,
): Promise<string> => {
  let failure: unknown;
  try {
    await build();
  } catch (error) {
    failure = error;
  }
  if (failure === undefined) {
    throw new Error(
      "expected the validator to refuse this transaction, but it succeeded",
    );
  }
  const text = failure instanceof Error ? failure.message : String(failure);
  if (!/failed script execution/u.test(text)) {
    throw new Error(
      `expected an on-chain validator refusal, got a non-validator failure: ${text}`,
    );
  }
  if (check === undefined) return text;
  const trace = refusalTrace(text);
  if (trace === null) {
    if (TRACED_REFUSALS_REQUIRED) {
      throw new Error(
        `expected a traced refusal matching ${String(check)}, but the refusing validator is untraced: ${text}`,
      );
    }
    return text;
  }
  if (!check.test(trace)) {
    throw new Error(
      `expected the refusal of the check matching ${String(check)}, but another check refused: ${text}`,
    );
  }
  return text;
};
