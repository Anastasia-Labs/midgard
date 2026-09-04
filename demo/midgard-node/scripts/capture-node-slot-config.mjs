#!/usr/bin/env node

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";

import {
  buildNodeSlotConfigEvidenceV1,
  fetchOgmiosGenesisPayloadV1,
} from "./node-slot-config-evidence.mjs";

const option = (name, fallback = "") =>
  process.argv
    .find((value) => value.startsWith(`--${name}=`))
    ?.slice(name.length + 3) ?? fallback;
const network = option("network", process.env.NETWORK).trim();
const output = option("out").trim();
if (network.length === 0 || output.length === 0) {
  throw new Error(
    "usage: capture-node-slot-config.mjs --network=<network> --out=<path> [--ogmios-url=<url>]",
  );
}
const outputPath = resolve(output);
const ogmiosUrl = option("ogmios-url", process.env.L1_OGMIOS_KEY).trim();
let ogmiosGenesisPayload;
if (network === "Custom") {
  if (ogmiosUrl.length === 0) {
    throw new Error("Custom network requires --ogmios-url");
  }
  ogmiosGenesisPayload = await fetchOgmiosGenesisPayloadV1({ ogmiosUrl });
}
const document = buildNodeSlotConfigEvidenceV1({
  network,
  ogmiosUrl,
  ogmiosGenesisPayload,
});
mkdirSync(dirname(outputPath), { recursive: true });
writeFileSync(outputPath, `${JSON.stringify(document, null, 2)}\n`);
console.log(JSON.stringify({ outputPath, document }, null, 2));
