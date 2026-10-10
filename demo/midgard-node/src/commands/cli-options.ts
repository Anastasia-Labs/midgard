/**
 * CLI option parsers and error text with no service dependencies, shared by
 * the command layer (`cli-runtime.ts`) and the node's runtime modules, which
 * must not load the command layer's tool L1 access.
 */
export const parsePositiveIntegerOption = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer`);
  }
  return parsed;
};

export const parseNonNegativeIntegerOption = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer`);
  }
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer`);
  }
  return parsed;
};

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
