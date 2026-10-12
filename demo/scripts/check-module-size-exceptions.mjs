import { readFileSync } from "node:fs";
import { isAbsolute, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

// module-size-exceptions.json is the one inventory of modules retained above
// the 500-line limit. Each entry caps one file at its exact current size, so
// growth and an obsolete waiver both fail until the entry is updated.
//
// `file` is relative to demo/. Demo TS/JS entries also get an ESLint
// max-lines cap (eslint.config.mjs); the rest (native sources, SQL migrations
// and agent tooling outside demo, spelled `../<path>`) are held to their
// count here only.

const root = fileURLToPath(new URL("../", import.meta.url));

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

/** Every reason the listed caps are malformed or no longer exact. */
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
      reason.trim().length === 0
    ) {
      failures.push(`Invalid or duplicate exception: ${String(file)}`);
      continue;
    }
    seen.add(file);
    try {
      const lines = read(resolve(demoRoot, file)).split(/\r\n|\r|\n/u);
      if (lines.at(-1) === "") lines.pop();
      if (lines.length !== max) {
        failures.push(
          `${file}: recorded ${max}, actual ${lines.length}; remove an obsolete exception or update its cap and justification.`,
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
  process.stdout.write(
    `Verified ${exceptions.length} explicit module-size caps.\n`,
  );
}
