// Convert a legacy (tx-validation branch) CBOR Midgard native-tx fixture to
// the current binary wire format. Reads hex from argv, --in <file>, or stdin;
// writes hex to stdout (or --out <file>).
//
// Usage:
//   tsx scripts/convert-legacy-cbor.ts --kind full     <hex>
//   tsx scripts/convert-legacy-cbor.ts --kind compact  --in fixture.hex
//   echo 84018c... | tsx scripts/convert-legacy-cbor.ts --kind compact

import fs from "node:fs";
import {
  convertLegacyCborToBinaryMidgardNativeTxBodyCompact,
  convertLegacyCborToBinaryMidgardNativeTxCompact,
  convertLegacyCborToBinaryMidgardNativeTxFull,
  convertLegacyCborToBinaryMidgardNativeTxWitnessSetCompact,
} from "../src/codec/native-cbor-legacy.js";

type Kind = "full" | "compact" | "body-compact" | "ws-compact";

const KIND_CONVERTERS: Record<Kind, (bytes: Uint8Array) => Buffer> = {
  "full": convertLegacyCborToBinaryMidgardNativeTxFull,
  "compact": convertLegacyCborToBinaryMidgardNativeTxCompact,
  "body-compact": convertLegacyCborToBinaryMidgardNativeTxBodyCompact,
  "ws-compact": convertLegacyCborToBinaryMidgardNativeTxWitnessSetCompact,
};

const KIND_LABELS = Object.keys(KIND_CONVERTERS) as Kind[];

const usage = (): never => {
  process.stderr.write(
    `usage: convert-legacy-cbor --kind <${KIND_LABELS.join("|")}> [--in <file>] [--out <file>] [hex]\n`,
  );
  process.exit(2);
};

const readStdinHex = async (): Promise<string> => {
  const chunks: Buffer[] = [];
  for await (const chunk of process.stdin) {
    chunks.push(chunk as Buffer);
  }
  return Buffer.concat(chunks).toString("utf8");
};

const parseHex = (raw: string, source: string): Buffer => {
  const trimmed = raw.trim().replace(/^0x/i, "").replace(/\s+/g, "");
  if (trimmed.length === 0) {
    process.stderr.write(`error: no hex bytes provided (${source})\n`);
    process.exit(1);
  }
  if (!/^[0-9a-f]+$/i.test(trimmed) || trimmed.length % 2 !== 0) {
    process.stderr.write(`error: invalid hex in ${source}\n`);
    process.exit(1);
  }
  return Buffer.from(trimmed, "hex");
};

const main = async () => {
  const argv = process.argv.slice(2);
  let kind: Kind | undefined;
  let inPath: string | undefined;
  let outPath: string | undefined;
  let inlineHex: string | undefined;

  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--kind") {
      const next = argv[++i];
      if (!KIND_LABELS.includes(next as Kind)) usage();
      kind = next as Kind;
    } else if (a === "--in") {
      inPath = argv[++i];
    } else if (a === "--out") {
      outPath = argv[++i];
    } else if (a === "--help" || a === "-h") {
      usage();
    } else if (!a.startsWith("--")) {
      inlineHex = a;
    } else {
      usage();
    }
  }
  if (kind === undefined) usage();

  let hex: string;
  if (inlineHex !== undefined) {
    hex = inlineHex;
  } else if (inPath !== undefined) {
    hex = fs.readFileSync(inPath, "utf8");
  } else if (!process.stdin.isTTY) {
    hex = await readStdinHex();
  } else {
    usage();
  }

  const bytes = parseHex(hex!, inPath ?? "stdin/argv");
  const out = KIND_CONVERTERS[kind!](bytes).toString("hex");

  if (outPath !== undefined) {
    fs.writeFileSync(outPath, out);
  } else {
    process.stdout.write(out + "\n");
  }
};

main().catch((e) => {
  process.stderr.write(`error: ${e instanceof Error ? e.message : String(e)}\n`);
  process.exit(1);
});
