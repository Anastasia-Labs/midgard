import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("../", import.meta.url));
const exceptions = JSON.parse(
  readFileSync(resolve(root, "module-size-exceptions.json"), "utf8"),
);
const seen = new Set();
const failures = [];
for (const { file, max, reason } of exceptions) {
  if (
    typeof file !== "string" ||
    file.startsWith("/") ||
    file.split("/").includes("..") ||
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
    const lines = readFileSync(resolve(root, file), "utf8").split(
      /\r\n|\r|\n/u,
    );
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
if (failures.length) {
  process.stderr.write(`${failures.join("\n")}\n`);
  process.exitCode = 1;
} else {
  process.stdout.write(
    `Verified ${exceptions.length} explicit module-size caps.\n`,
  );
}
