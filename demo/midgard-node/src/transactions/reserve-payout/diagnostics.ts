export const formatLayout = (layout: unknown): string =>
  JSON.stringify(layout, (_key, value) =>
    typeof value === "bigint" ? value.toString() : value,
  );

export const formatCauseSummary = (cause: unknown): string => {
  if (cause instanceof Error && cause.message.length > 0) {
    return cause.message;
  }
  if (typeof cause === "string") {
    return cause;
  }
  return String(cause);
};
