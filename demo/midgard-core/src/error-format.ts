/**
 * Minimal shared fallback formatting for opaque errors.
 * This keeps logs and operator-facing failure messages consistent without
 * spreading ad hoc stringification logic across the codebase.
 */
export type FormatUnknownErrorOptions = {
  readonly includeCause?: boolean;
  readonly includeStack?: boolean;
};

/**
 * Converts unknown thrown values into a stable human-readable string.
 */
export const formatUnknownError = (
  error: unknown,
  options: FormatUnknownErrorOptions = {},
): string => {
  if (error instanceof Error) {
    const message =
      options.includeStack === true && error.stack !== undefined
        ? error.stack
        : `${error.name}: ${error.message}`;
    const cause = (error as { readonly cause?: unknown }).cause;
    return options.includeCause === true && cause !== undefined
      ? `${message}; cause=${formatUnknownError(cause, options)}`
      : message;
  }
  if (typeof error === "string") {
    return error;
  }
  if (typeof error === "object" && error !== null && "message" in error) {
    const message = String((error as { readonly message: unknown }).message);
    const cause = (error as { readonly cause?: unknown }).cause;
    return options.includeCause === true && cause !== undefined
      ? `${message}; cause=${formatUnknownError(cause, options)}`
      : message;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};
