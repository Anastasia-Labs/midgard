import { parse } from "yaml";

/** One series selector in an alert rule's PromQL expression. */
export type AlertRuleSelector = Readonly<{
  alert: string;
  metric: string;
  matchers: readonly Readonly<{
    label: string;
    operator: "=" | "!=" | "=~" | "!~";
    /** The matched values; a regex matcher's alternatives, split on `|`. */
    values: readonly string[];
  }>[];
}>;

const KEYWORDS = new Set([
  "and",
  "or",
  "unless",
  "bool",
  "offset",
  "on",
  "ignoring",
  "by",
  "without",
  "group_left",
  "group_right",
]);
const GROUPING =
  /\b(?:on|ignoring|by|without|group_left|group_right)\s*\([^)]*\)/gu;
const SELECTOR = /([A-Za-z_:][A-Za-z0-9_:]*)\s*(?:\{([^}]*)\})?/gu;
const MATCHER = /([A-Za-z_][A-Za-z0-9_]*)\s*(=~|!~|!=|=)\s*"([^"]*)"/gu;

/**
 * The series selectors of every alert rule in a Prometheus rules file:
 * each metric name its expression selects (a name not followed by `(`, so
 * not a function), with its label matchers. Range durations and grouping
 * clauses are dropped first.
 */
export const alertRuleSelectors = (
  rulesYaml: string,
): readonly AlertRuleSelector[] => {
  const document = parse(rulesYaml) as {
    groups: { rules: { alert: string; expr: string }[] }[];
  };
  return document.groups.flatMap(({ rules }) =>
    rules.flatMap(({ alert, expr }) => {
      const text = expr.replace(/\[[^\]]*\]/gu, "").replace(GROUPING, "");
      const selectors: AlertRuleSelector[] = [];
      for (const match of text.matchAll(SELECTOR)) {
        const [whole, metric, labels] = match;
        const after = text.slice(match.index + whole.length).trimStart();
        if (
          KEYWORDS.has(metric!) ||
          (labels === undefined && after.startsWith("("))
        )
          continue;
        selectors.push({
          alert,
          metric: metric!,
          matchers: [...(labels ?? "").matchAll(MATCHER)].map(
            ([, label, operator, value]) => ({
              label: label!,
              operator: operator as "=" | "!=" | "=~" | "!~",
              values: operator!.endsWith("~") ? value!.split("|") : [value!],
            }),
          ),
        });
      }
      if (selectors.length === 0)
        throw new Error(`alert ${alert} selects no series`);
      return selectors;
    }),
  );
};
