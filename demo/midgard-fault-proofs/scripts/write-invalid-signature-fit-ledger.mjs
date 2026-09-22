import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.ts";

const [logPath, blueprintPath] = process.argv.slice(2);
if (!logPath || !blueprintPath) {
  throw new Error(
    "Usage: node scripts/write-invalid-signature-fit-ledger.mjs LOG BLUEPRINT",
  );
}
const log = await readFile(logPath, "utf8");
const blueprint = await readFile(blueprintPath);
const digest = createHash("sha256").update(blueprint).digest("hex");
if (
  !log.includes(`[invalid-signature-blueprint] ${digest}`) ||
  !/Test Files\s+\d+ passed/u.test(log) ||
  /Test Files[^\n]*\d+ failed/u.test(log)
) {
  throw new Error(
    "Capture must be a passing run bound to the supplied blueprint",
  );
}
const measurements = [];
const row = (name, kind, maximumShape, value) => ({
  name,
  kind,
  maximumShape,
  signedBytes: value.bytes,
  memoryUnits: BigInt(value.memory),
  cpuUnits: BigInt(value.cpu),
});
const forcedShapes = new Set();
for (const line of log.split("\n")) {
  const forced = /^\[invalid-signature-forced-lifecycle-([^\]]+)\] (.*)$/u.exec(
    line,
  );
  if (forced) {
    const shape = forced[1];
    if (forcedShapes.has(shape)) throw new Error(`Duplicate shape ${shape}`);
    forcedShapes.add(shape);
    const entries = JSON.parse(forced[2]);
    const labels = [
      "init-cancel01",
      "cancel01",
      "init-cancel02",
      "bind-cancel02",
      "cancel02",
      "init",
      "bind",
    ];
    entries.forEach((entry, index) => {
      const publication = index >= 7 && index < entries.length - 2;
      const label =
        labels[index] ??
        (index === entries.length - 2
          ? "final"
          : index === entries.length - 1
            ? "removal"
            : `field-publication-${index - 7}`);
      measurements.push(
        row(
          `forced-${shape}-${label}`,
          publication ? "publication" : "lifecycle",
          shape,
          entry,
        ),
      );
    });
  }
  const accepted = /^\[invalid-signature-accepted-fit\] (.*)$/u.exec(line);
  if (accepted) {
    const entry = JSON.parse(accepted[1]);
    measurements.push(
      row(
        `accepted-${entry.stage}`,
        "lifecycle",
        "accepted invalid signature",
        entry,
      ),
    );
  }
  const publication = /^\[invalid-signature-publication\] (.*)$/u.exec(line);
  if (publication) {
    const entry = JSON.parse(publication[1]);
    measurements.push(
      row(
        `reference-${entry.step}`,
        "publication",
        "fully applied registered validator",
        entry,
      ),
    );
  }
}
const expectedShapes = [
  "0-selected-single",
  "139-selected-single",
  "317-selected-single",
  "0-1-single",
  "0--1-single",
  "317-selected-deep64",
];
if (
  forcedShapes.size !== expectedShapes.length ||
  expectedShapes.some((shape) => !forcedShapes.has(shape)) ||
  measurements.length !== 68
) {
  throw new Error(
    "Capture omitted an accepted, publication, or maximum forced lifecycle row",
  );
}
const ledger = buildVanRossemFitLedger({
  category: "invalidSignature",
  blueprintSha256: digest,
  compilerVersion: JSON.parse(blueprint.toString()).preamble.compiler.version,
  measurements,
});
await writeVanRossemFitLedger(
  fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/invalid-signature-wrongful-rejection-v1-fit-ledger.json",
      import.meta.url,
    ),
  ),
  ledger,
);
console.log(`${ledger.entries.length} measurements; ${ledger.ledgerSha256}`);
