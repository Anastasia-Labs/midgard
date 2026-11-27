// Renders the value table of docs/public-testnet-parameters.md from the
// preprod-public deployment profile: every scalar the profile sets, with its
// unit, a readable form and the profile line that sets it. `--check` fails
// when the table is stale, or when the document's prose never names a
// parameter the profile sets (a new parameter needs its review entry).
import { readFile, writeFile } from "node:fs/promises";

import { isMap, isScalar, isSeq, LineCounter, parseDocument } from "yaml";

const START = "<!-- BEGIN PUBLIC_TESTNET_PARAMETERS_GENERATED: do not edit -->";
const END = "<!-- END PUBLIC_TESTNET_PARAMETERS_GENERATED -->";
const PROFILE_URL = new URL(
  "../../../config/deployments/preprod-public.yaml",
  import.meta.url,
);
const DOCUMENT_URL = new URL(
  "../../../docs/public-testnet-parameters.md",
  import.meta.url,
);
// The profile, relative to the document.
const PROFILE_LINK = "../config/deployments/preprod-public.yaml";

// Units of the parameters whose name does not carry one (`_ms`, `_lovelace`,
// `Lovelace`). A parameter missing from here fails the render.
const UNITS = {
  name: "text",
  network: "text",
  profile: "text",
  type: "text",
  required: "signatures",
  keyHash: "payment key hash",
  operators: "payment key hash",
  excluded_fraud_proof_categories: "family",
  confirmation_depth: "blocks",
  commit_event_depth: "blocks",
  security_parameter: "blocks",
  active_slot_coeff: "ratio",
  slot_length_ms: "ms",
  retention_days: "days",
  max_bisection_rounds: "rounds",
  max_inactivity_strikes: "strikes",
  coins_per_utxo_byte: "lovelace per byte",
};

const unitOf = (name) => {
  if (name.endsWith("_ms")) return "ms";
  if (/(?:_lovelace|Lovelace)$/u.test(name)) return "lovelace";
  const unit = UNITS[name];
  if (unit === undefined)
    throw new Error(`No unit for profile parameter '${name}'`);
  return unit;
};

const grouped = (digits) => digits.replace(/\B(?=(\d{3})+(?!\d))/gu, ",");

const DURATION_PARTS = [
  ["d", 86_400_000],
  ["h", 3_600_000],
  ["min", 60_000],
  ["s", 1_000],
  ["ms", 1],
];

const duration = (ms) => {
  if (ms === 0) return "0";
  const parts = [];
  let rest = ms;
  for (const [label, size] of DURATION_PARTS) {
    const count = Math.floor(rest / size);
    rest -= count * size;
    if (count > 0) parts.push(`${grouped(String(count))} ${label}`);
  }
  return parts.join(" ");
};

const ada = (lovelace) => {
  const fraction = String(lovelace % 1_000_000)
    .padStart(6, "0")
    .replace(/0+$/u, "");
  const whole = grouped(String(Math.floor(lovelace / 1_000_000)));
  return `${whole}${fraction === "" ? "" : `.${fraction}`} ADA`;
};

const readable = (unit, value) => {
  if (typeof value !== "number") return "";
  if (unit === "ms") return duration(value);
  if (unit === "lovelace") return ada(value);
  return value >= 10_000 ? grouped(String(value)) : "";
};

export const profileRows = (source) => {
  const lineCounter = new LineCounter();
  const document = parseDocument(source, { lineCounter, uniqueKeys: true });
  if (document.errors.length > 0)
    throw new Error(`Invalid profile YAML: ${document.errors.join("; ")}`);
  const rows = [];
  const walk = (node, path, name) => {
    if (isMap(node)) {
      for (const pair of node.items) {
        const key = String(pair.key.value);
        walk(pair.value, path === "" ? key : `${path}.${key}`, key);
      }
    } else if (isSeq(node)) {
      node.items.forEach((item, index) =>
        walk(item, `${path}[${index}]`, name),
      );
    } else if (isScalar(node)) {
      const unit = unitOf(name);
      rows.push({
        path,
        name,
        value: node.value,
        unit,
        readable: readable(unit, node.value),
        line: lineCounter.linePos(node.range[0]).line,
      });
    } else throw new Error(`${path}: unsupported profile node`);
  };
  walk(document.contents, "", "");
  return rows;
};

// A Markdown table padded the way Prettier pads one, so the document stays
// formatted.
const markdownTable = (header, body) => {
  const widths = header.map((_, column) =>
    Math.max(3, ...[header, ...body].map((cells) => cells[column].length)),
  );
  const line = (cells) =>
    `| ${cells.map((cell, column) => cell.padEnd(widths[column])).join(" | ")} |`;
  return [
    line(header),
    line(widths.map((width) => "-".repeat(width))),
    ...body.map(line),
  ];
};

export const renderTable = (rows) =>
  [
    START,
    "",
    ...markdownTable(
      ["Parameter", "Value", "Unit", "Readable", "Set at"],
      rows.map(({ path, value, unit, readable, line }) => [
        `\`${path}\``,
        `\`${value}\``,
        unit,
        readable,
        `[L${line}](${PROFILE_LINK}#L${line})`,
      ]),
    ),
    "",
    END,
  ].join("\n");

// The parameter names the reviewed prose must cite: every map key outside
// the treasury script's members and the sequences' entries.
const citedNames = (rows) => [
  ...new Set(
    rows.filter(({ path }) => !/\]\./u.test(path)).map(({ name }) => name),
  ),
];

if (import.meta.url === new URL(process.argv[1], "file:").href) {
  const rows = profileRows(await readFile(PROFILE_URL, "utf8"));
  const generated = renderTable(rows);
  const source = await readFile(DOCUMENT_URL, "utf8");
  const start = source.indexOf(START);
  const end = source.indexOf(END);
  if (start < 0 || end < start)
    throw new Error(
      "docs/public-testnet-parameters.md is missing the generated table markers",
    );
  const afterEnd = end + END.length;
  const prose = `${source.slice(0, start)}${source.slice(afterEnd)}`;
  const uncited = citedNames(rows).filter(
    (name) => !prose.includes(`\`${name}\``),
  );
  if (uncited.length > 0)
    throw new Error(
      `docs/public-testnet-parameters.md does not review: ${uncited.join(", ")}`,
    );
  if (process.argv.includes("--check")) {
    if (source.slice(start, afterEnd) !== generated)
      throw new Error(
        "docs/public-testnet-parameters.md is stale; run the parameter documentation sync command",
      );
  } else
    await writeFile(
      DOCUMENT_URL,
      `${source.slice(0, start)}${generated}${source.slice(afterEnd)}`,
      "utf8",
    );
}
