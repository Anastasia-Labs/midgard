/**
 * Waits for the requested number of milliseconds.
 */
export const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Parses a duration flag into milliseconds.
 */
export const parseDurationMs = (value) => {
  if (typeof value !== "string" || value.trim().length === 0) {
    throw new Error("duration must be a non-empty string");
  }
  const trimmed = value.trim().toLowerCase();
  const match = /^(\d+)(ms|s|m|h)?$/.exec(trimmed);
  if (match === null) {
    throw new Error(`Invalid duration value: ${value}`);
  }
  const amount = Number.parseInt(match[1], 10);
  const unit = match[2] ?? "s";
  if (!Number.isFinite(amount) || amount <= 0) {
    throw new Error(`Duration must be a positive integer: ${value}`);
  }
  switch (unit) {
    case "ms":
      return amount;
    case "s":
      return amount * 1000;
    case "m":
      return amount * 60_000;
    case "h":
      return amount * 3_600_000;
    default:
      throw new Error(`Unsupported duration unit: ${unit}`);
  }
};

/**
 * Parses CLI arguments for the nominal-activity workload.
 */
export const parseArgs = (argv) => {
  const out = {};
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      continue;
    }
    if (token === "--help" || token === "-h") {
      out.help = "true";
      continue;
    }
    const eq = token.indexOf("=");
    if (eq >= 0) {
      const key = token.slice(2, eq);
      const value = token.slice(eq + 1);
      out[key] = value;
      continue;
    }
    const key = token.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) {
      out[key] = next;
      i += 1;
    } else {
      out[key] = "true";
    }
  }
  return out;
};

/**
 * Parses a boolean environment or CLI value.
 */
export const boolFrom = (value, defaultValue) => {
  if (value === undefined) return defaultValue;
  const normalized = String(value).trim().toLowerCase();
  return normalized !== "false" && normalized !== "0" && normalized !== "no";
};

/**
 * Parses a numeric environment or CLI value.
 */
export const numberFrom = (value, fallback, name) => {
  if (value === undefined) return fallback;
  const parsed = Number.parseInt(String(value), 10);
  if (!Number.isFinite(parsed)) {
    throw new Error(`${name} must be a valid integer`);
  }
  return parsed;
};

/**
 * Prints the script usage message.
 */
export const usage = () => {
  console.log(`Nominal Midgard activity generator

Usage:
  node scripts/throughput-nominal-activity.mjs [options]

Options:
  --duration <value>         Total run duration (e.g. 300s, 5m, 1h)
  --target-txs <n>           Max successful submits before stopping
  --min-interval-ms <n>      Minimum wait between submissions
  --max-interval-ms <n>      Maximum wait between submissions
  --submit-endpoint <url>    Midgard node HTTP endpoint
  --metrics-endpoint <url>   Prometheus metrics endpoint
  --env-file <path>          Env file with genesis seed phrases
  --wallet-mode <mode>       random (default) or round_robin
  --metrics-poll-ms <n>      Metrics poll interval
  --help                     Show this message
`);
};

/** @typedef {{ outref: string; value: string }} NodeUtxo */

/**
 * Escapes a string for literal use in a regular expression.
 */
const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

/**
 * Extracts a Prometheus counter value from metrics text.
 */
export const extractCounter = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, "m"));
    if (m !== null) {
      return Number(m[1]);
    }
  }
  return 0;
};
