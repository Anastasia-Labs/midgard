/**
 * Minimal shared fallback formatting for opaque errors.
 * This keeps logs and operator-facing failure messages consistent without
 * spreading ad hoc stringification logic across the codebase.
 */
/**
 * Converts unknown thrown values into a stable human-readable string.
 */
export const formatUnknownError = (error: unknown): string => {
  if (error instanceof Error) {
    return `${error.name}: ${error.message}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};
