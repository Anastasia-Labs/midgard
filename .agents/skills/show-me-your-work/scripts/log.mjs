import {
  closeSync,
  existsSync,
  openSync,
  readFileSync,
  writeSync,
} from "node:fs";
import { fileURLToPath } from "node:url";

export const HEADER = "ts\tphase\tdecision\twhy\tevidence\tresult\n";
export const cell = (value) => {
  const cleaned = String(value).replace(/[\t\r\n]+/gu, " ");
  return /^[=+@-]/u.test(cleaned.trimStart()) ? `'${cleaned}` : cleaned;
};

export function appendDecision(path, values, now = new Date()) {
  if (values.length !== 5)
    throw new Error("Expected phase, decision, why, evidence, result");
  if (existsSync(path) && !readFileSync(path, "utf8").startsWith(HEADER)) {
    throw new Error(
      "Existing log has an invalid header; preserve it and choose a new path",
    );
  }
  const fd = openSync(path, "a", 0o600);
  try {
    if (readFileSync(path).length === 0) writeSync(fd, HEADER);
    writeSync(fd, [now.toISOString(), ...values.map(cell)].join("\t") + "\n");
  } finally {
    closeSync(fd);
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  try {
    const [path, ...values] = process.argv.slice(2);
    if (!path)
      throw new Error(
        "Usage: node log.mjs <file> <phase> <decision> <why> <evidence> <result>",
      );
    appendDecision(path, values);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
