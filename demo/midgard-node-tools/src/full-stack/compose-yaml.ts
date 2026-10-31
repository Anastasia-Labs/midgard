/** A value Compose replaces instead of merging with the base file's (`!override`). */
const OVERRIDE = Symbol("override");
type Overridden = { [OVERRIDE]: unknown };
export const override = (value: unknown): Overridden => ({
  [OVERRIDE]: value,
});

/** YAML flow style: JSON, plus the Compose merge tag JSON cannot carry. */
export function composeYaml(value: unknown): string {
  if (value !== null && typeof value === "object" && OVERRIDE in value)
    return `!override ${composeYaml((value as Overridden)[OVERRIDE])}`;
  if (Array.isArray(value)) return `[${value.map(composeYaml).join(", ")}]`;
  if (value !== null && typeof value === "object")
    return `{${Object.entries(value)
      .map(([key, item]) => `${JSON.stringify(key)}: ${composeYaml(item)}`)
      .join(", ")}}`;
  return JSON.stringify(value);
}
