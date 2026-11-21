import { readFileSync } from "node:fs";
import { isAbsolute, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

// module-size-exceptions.json is the one inventory of modules retained above
// the 500-line limit. Each entry's `max` is a ceiling: the file may shrink
// without touching the entry, growth past the ceiling fails, and an entry
// whose file is back at or under 500 lines is obsolete and fails until it is
// deleted. `reason` says why the file stays large; size history lives in git,
// so a reason carrying a `(+N)`/`(-N)` change note is refused.
//
// `file` is relative to demo/. Demo TS/JS entries also get an ESLint
// max-lines cap (eslint.config.mjs); the rest (native sources, SQL migrations
// and agent tooling outside demo, spelled `../<path>`) are held to their
// count here only.

const root = fileURLToPath(new URL("../", import.meta.url));

// A size-change note such as "C3 (+15): ..." or "(-2)".
const SIZE_CHANGE_NOTE = /\([+\u2212-]\d+\)/u;

/** The one spelling of a repository file relative to demo/, or null. */
export const canonicalExceptionPath = (demoRoot, file) => {
  if (typeof file !== "string" || file === "" || isAbsolute(file)) return null;
  const repository = resolve(demoRoot, "..");
  const fromRepository = relative(repository, resolve(demoRoot, file));
  if (
    fromRepository === ".." ||
    fromRepository.startsWith(`..${sep}`) ||
    isAbsolute(fromRepository)
  )
    return null;
  const posix = fromRepository.split(sep).join("/");
  return posix.startsWith("demo/")
    ? posix.slice("demo/".length)
    : `../${posix}`;
};

/** Every reason the listed caps are malformed, exceeded or obsolete. */
export const moduleSizeExceptionFailures = (
  exceptions,
  { demoRoot = root, read = (path) => readFileSync(path, "utf8") } = {},
) => {
  const seen = new Set();
  const failures = [];
  for (const { file, max, reason } of exceptions) {
    if (
      canonicalExceptionPath(demoRoot, file) !== file ||
      seen.has(file) ||
      !Number.isInteger(max) ||
      max <= 500 ||
      typeof reason !== "string" ||
      reason.trim().length === 0 ||
      SIZE_CHANGE_NOTE.test(reason)
    ) {
      failures.push(`Invalid or duplicate exception: ${String(file)}`);
      continue;
    }
    seen.add(file);
    try {
      const lines = read(resolve(demoRoot, file)).split(/\r\n|\r|\n/u);
      if (lines.at(-1) === "") lines.pop();
      if (lines.length > max) {
        failures.push(
          `${file}: ${lines.length} lines exceeds its cap ${max}; split the module, or raise the cap and say why in its reason.`,
        );
      } else if (lines.length <= 500) {
        failures.push(
          `${file}: ${lines.length} lines is within the 500-line limit; delete the obsolete exception.`,
        );
      }
    } catch (error) {
      failures.push(`${file}: ${error.message}`);
    }
  }
  return failures;
};

const exceptions = JSON.parse(
  readFileSync(resolve(root, "module-size-exceptions.json"), "utf8"),
);
const failures = moduleSizeExceptionFailures(exceptions);
if (failures.length) {
  process.stderr.write(`${failures.join("\n")}\n`);
  process.exitCode = 1;
} else {
  process.stdout.write(`Verified ${exceptions.length} module-size ceilings.\n`);
}
