// GitHub Actions `branches` / `paths` filter matching, for the tests that
// check which workflows a change triggers.

// GitHub filter glob: `**` crosses `/` (and `**/` may match no directory at
// all), `*` and `?` do not.
export const globToRegExp = (glob) =>
  new RegExp(
    `^${glob
      .split(/(\*\*\/|\*\*|\*|\?)/u)
      .map((part) =>
        part === "**/"
          ? "(?:.*/)?"
          : part === "**"
            ? ".*"
            : part === "*"
              ? "[^/]*"
              : part === "?"
                ? "[^/]"
                : part.replace(/[.+^${}()|[\]\\]/gu, "\\$&"),
      )
      .join("")}$`,
    "u",
  );

/** Whether `value` passes an ordered filter list, with `!` negation. */
export const matchesFilter = (patterns, value) => {
  let matched = false;
  for (const pattern of patterns) {
    if (pattern.startsWith("!")) {
      if (globToRegExp(pattern.slice(1)).test(value)) matched = false;
    } else if (globToRegExp(pattern).test(value)) matched = true;
  }
  return matched;
};
