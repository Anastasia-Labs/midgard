import { recordCheckedPin, TRACED_REFUSALS } from "./traced-refusals.js";

/**
 * The check a negative expects to refuse it. `refusedBy` names the validator
 * module that holds the check (for example
 * "fraud_proofs/missing_signature/forced_witness");
 * scripts/run-traced-refusals.mjs reads these names from the suites to decide
 * which validators to trace, so it must be a string literal. `check` matches
 * that validator's trace.
 */
export type RefusalPin = {
  readonly refusedBy: string;
  readonly check: RegExp;
};

/**
 * The trace a traced validator's failure carries, or null when untraced. The
 * evaluator's message can arrive JSON-quoted, alone or after a caller's
 * prefix such as a lifecycle stage label.
 */
const refusalTrace = (text: string): string | null => {
  let message = text.trim();
  const quoted =
    /"((?:[^"\\]|\\.)*failed script execution(?:[^"\\]|\\.)*)"/u.exec(message);
  if (quoted !== null) {
    try {
      message = String(JSON.parse(`"${quoted[1]!}"`));
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
 * `pin` names the refusing check. Plain builds carry no trace, so the pin is
 * checked only in the traced run (scripts/run-traced-refusals.mjs), which
 * fails a pinned refusal that arrives untraced.
 */
export const expectOnchainRefusal = async (
  build: () => Promise<unknown>,
  pin?: RefusalPin,
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
  if (pin === undefined) return text;
  const { refusedBy, check } = pin;
  const trace = refusalTrace(text);
  if (trace === null) {
    if (TRACED_REFUSALS) {
      throw new Error(
        `expected ${refusedBy} to refuse with a trace matching ${String(check)}, but the refusing validator is untraced: ${text}`,
      );
    }
    return text;
  }
  if (!check.test(trace)) {
    throw new Error(
      `expected the check in ${refusedBy} matching ${String(check)} to refuse, but another check refused: ${text}`,
    );
  }
  recordCheckedPin(refusedBy);
  return text;
};
