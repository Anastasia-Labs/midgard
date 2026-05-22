export const formatLayout = (layout: unknown): string =>
  JSON.stringify(layout, (_key, value) =>
    typeof value === "bigint" ? value.toString() : value,
  );
