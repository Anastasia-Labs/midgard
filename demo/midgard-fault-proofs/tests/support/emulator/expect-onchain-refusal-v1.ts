/**
 * Assert a negative reaches local UPLC evaluation and fails in a validator,
 * rather than passing because the off-chain builder happened to throw.
 */
export const expectOnchainRefusalV1 = async (
  build: () => Promise<unknown>,
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
  return text;
};
