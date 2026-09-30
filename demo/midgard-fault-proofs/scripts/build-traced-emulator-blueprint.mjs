#!/usr/bin/env node
/**
 * Overlay traced validators onto the plain blueprint, so an emulator negative
 * can assert which on-chain check refused it.
 *
 * A fully traced blueprint does not fit: traced scripts exceed the reference
 * script publication and proof-fit limits the emulator suites enforce. Only
 * the validators whose refusal a suite pins are swapped for their traced
 * build; everything else keeps the exact plain code.
 *
 * Build the traced blueprint from the same sources as the plain one:
 *
 *   aiken build --env preprod_testing --trace-level verbose \
 *     --trace-filter all -o <traced.json>      (in onchain/aiken)
 *
 * then:
 *
 *   node scripts/build-traced-emulator-blueprint.mjs \
 *     --plain ../../onchain/aiken/plutus.json --traced <traced.json> \
 *     --out <overlay.json> --title fraud_proofs/missing_signature/forced_witness
 *
 * and run the suite with MIDGARD_REAL_BLUEPRINT_PATH=<overlay.json> and
 * MIDGARD_EMULATOR_TRACED_REFUSALS=1. `--title` repeats; each names a
 * validator module (every handler under it is swapped) and must match one.
 */
import { readFileSync, writeFileSync } from "node:fs";
import { parseArgs } from "node:util";

const { values } = parseArgs({
  options: {
    plain: { type: "string" },
    traced: { type: "string" },
    out: { type: "string" },
    title: { type: "string", multiple: true },
  },
  strict: true,
});
if (!values.plain || !values.traced || !values.out || !values.title?.length) {
  console.error(
    "usage: build-traced-emulator-blueprint.mjs --plain <plutus.json> " +
      "--traced <traced.json> --out <overlay.json> --title <module>...",
  );
  process.exit(2);
}

const plain = JSON.parse(readFileSync(values.plain, "utf8"));
const traced = new Map(
  JSON.parse(readFileSync(values.traced, "utf8")).validators.map(
    (validator) => [validator.title, validator],
  ),
);
const moduleOf = (title) => title.slice(0, title.indexOf("."));

for (const module of values.title) {
  const swapped = plain.validators.filter(
    (validator) => moduleOf(validator.title) === module,
  );
  if (swapped.length === 0) {
    console.error(`no validator in module ${module}`);
    process.exit(1);
  }
  for (const validator of swapped) {
    const replacement = traced.get(validator.title);
    if (replacement === undefined) {
      console.error(`traced blueprint lacks ${validator.title}`);
      process.exit(1);
    }
    validator.compiledCode = replacement.compiledCode;
    validator.hash = replacement.hash;
    console.log(
      `${validator.title}: ${(replacement.compiledCode.length / 2).toString()} traced bytes`,
    );
  }
}
writeFileSync(values.out, JSON.stringify(plain, null, 2) + "\n");
