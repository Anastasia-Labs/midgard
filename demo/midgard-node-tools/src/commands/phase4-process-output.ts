export const parseTrailingJsonObject = (
  label: string,
  output: string,
): Readonly<Record<string, unknown>> => {
  let objectStart = output.lastIndexOf("{");
  while (objectStart >= 0) {
    try {
      const parsed: unknown = JSON.parse(output.slice(objectStart).trim());
      if (
        typeof parsed === "object" &&
        parsed !== null &&
        !Array.isArray(parsed)
      ) {
        return parsed as Readonly<Record<string, unknown>>;
      }
    } catch {
      // Structured logs can contain JSON fragments. Continue backwards until
      // one object consumes the complete trailing output.
    }
    if (objectStart === 0) break;
    objectStart = output.lastIndexOf("{", objectStart - 1);
  }
  throw new Error(`${label} did not emit a trailing JSON object`);
};

export const assertSatisfiedReconciliation = (
  label: string,
  output: string,
): void => {
  const parsed = parseTrailingJsonObject(label, output);
  if (!("status" in parsed) || parsed.status !== "satisfied") {
    throw new Error(`${label} is not satisfied:\n${output}`);
  }
};
