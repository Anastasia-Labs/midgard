#!/usr/bin/env node

import { readFile, readdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const plutarchDir = path.resolve(scriptDir, "..");

const DIRECT_TITLES = new Map([
  [
    "active-operators-mint.unapplied.plutus.json",
    "operator_directory/active_operators.mint.mint",
  ],
  [
    "active-operators-spend.unapplied.plutus.json",
    "operator_directory/active_operators.spend.spend",
  ],
  [
    "cek-program-material-spend.plutus.json",
    "user_events/cek_program_material_v1.spend.spend",
  ],
  [
    "computation-thread-mint.unapplied.plutus.json",
    "computation_thread.mint.mint",
  ],
  [
    "da-attestation-mint.unapplied.plutus.json",
    "da_attestation.da_attestation.mint",
  ],
  [
    "da-attestation-spend.unapplied.plutus.json",
    "da_attestation.da_attestation.spend",
  ],
  [
    "da-params-governor-mint.unapplied.plutus.json",
    "da_params_governor.da_params_governor.mint",
  ],
  [
    "da-params-governor-spend.unapplied.plutus.json",
    "da_params_governor.da_params_governor.spend",
  ],
  ["deposit-mint.unapplied.plutus.json", "user_events/deposit.mint.mint"],
  ["deposit-spend.unapplied.plutus.json", "user_events/deposit.spend.spend"],
  [
    "field-preimage-certificate-mint.plutus.json",
    "field_preimage_certificate.field_preimage_certificate.mint",
  ],
  [
    "field-preimage-certificate-spend.plutus.json",
    "field_preimage_certificate.field_preimage_certificate.spend",
  ],
  [
    "fraud-proof-catalogue-mint.unapplied.plutus.json",
    "fraud_proof_catalogue.mint.mint",
  ],
  [
    "fraud-proof-catalogue-spend.plutus.json",
    "fraud_proof_catalogue.spend.else",
  ],
  ["fraud-proof-mint.unapplied.plutus.json", "fraud_proof.mint.mint"],
  ["fraud-proof-spend.plutus.json", "fraud_proof.spend.else"],
  ["hub-oracle-mint.unapplied.plutus.json", "hub_oracle.mint.mint"],
  ["membership-stake.plutus.json", "phas.membership.withdraw"],
  [
    "mpf-chunked-verify-withdraw.plutus.json",
    "mpf_chunked_verify.verify.withdraw",
  ],
  ["non-membership-stake.plutus.json", "pexcludes.exclusion.withdraw"],
  ["payout-mint.unapplied.plutus.json", "payout.mint.mint"],
  ["payout-spend.unapplied.plutus.json", "payout.spend.spend"],
  [
    "registered-operators-mint.unapplied.plutus.json",
    "operator_directory/registered_operators.mint.mint",
  ],
  [
    "registered-operators-spend.unapplied.plutus.json",
    "operator_directory/registered_operators.spend.spend",
  ],
  ["reserve-spend.unapplied.plutus.json", "reserve.spend.spend"],
  ["reserve-withdraw.plutus.json", "reserve.withdraw.else"],
  [
    "retired-operators-mint.unapplied.plutus.json",
    "operator_directory/retired_operators.mint.mint",
  ],
  [
    "retired-operators-spend.unapplied.plutus.json",
    "operator_directory/retired_operators.spend.spend",
  ],
  ["scheduler-mint.unapplied.plutus.json", "scheduler.mint.mint"],
  ["scheduler-spend.unapplied.plutus.json", "scheduler.spend.spend"],
  ["settlement-mint.unapplied.plutus.json", "settlement.mint.mint"],
  ["settlement-spend.unapplied.plutus.json", "settlement.spend.spend"],
  ["state-queue-mint.unapplied.plutus.json", "state_queue.mint.mint"],
  ["state-queue-spend.unapplied.plutus.json", "state_queue.spend.spend"],
  [
    "tx-field-preimage-spend.plutus.json",
    "user_events/tx_field_preimage_v1.spend.spend",
  ],
  [
    "tx-field-receipt-mint.unapplied.plutus.json",
    "user_events/tx_field_receipt_v1.mint.mint",
  ],
  [
    "tx-field-receipt-spend.plutus.json",
    "user_events/tx_field_receipt_spend_v1.spend.spend",
  ],
  ["tx-order-mint.unapplied.plutus.json", "user_events/tx_order_v1.mint.mint"],
  [
    "tx-order-spend.unapplied.plutus.json",
    "user_events/tx_order_v1.spend.spend",
  ],
  [
    "user-event-witness-publish.unapplied.plutus.json",
    "user_events/witness.main.publish",
  ],
  ["withdrawal-mint.unapplied.plutus.json", "user_events/withdrawal.mint.mint"],
  [
    "withdrawal-spend.unapplied.plutus.json",
    "user_events/withdrawal.spend.spend",
  ],
]);

const underscores = (value) => value.replaceAll("-", "_");

export const offchainTitleForGeneratedFile = (fileName) => {
  const direct = DIRECT_TITLES.get(fileName);
  if (direct !== undefined) return direct;

  const step = fileName.match(
    /^fraud-proof-(.+)-step-(\d{2})\.unapplied\.plutus\.json$/u,
  );
  if (step !== null) {
    return `fraud_proofs/${underscores(step[1])}/step_${step[2]}.main.spend`;
  }

  const transitionTrace = fileName.match(
    /^fraud-proof-transition-trace-(.+)-v1\.unapplied\.plutus\.json$/u,
  );
  if (transitionTrace !== null) {
    return `fraud_proofs/transition_trace/${underscores(transitionTrace[1])}_v1.main.spend`;
  }

  const validationTrace = fileName.match(
    /^fraud-proof-validation-trace-(.+)\.unapplied\.plutus\.json$/u,
  );
  if (validationTrace !== null) {
    return `fraud_proofs/validation_trace/${underscores(validationTrace[1])}.main.spend`;
  }

  if (fileName === "fraud-proof-validation-trace-proof-item-v1.plutus.json") {
    return "fraud_proofs/validation_trace/proof_item_v1.main.else";
  }

  throw new Error(`No off-chain blueprint title mapping for ${fileName}`);
};

const parseGeneratedScript = (raw, sourcePath) => {
  const parsed = JSON.parse(raw);
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    parsed.type !== "PlutusScriptV3" ||
    typeof parsed.description !== "string" ||
    !/^midgard\./u.test(parsed.description) ||
    typeof parsed.cborHex !== "string" ||
    !/^[0-9a-f]+$/u.test(parsed.cborHex) ||
    parsed.cborHex.length % 2 !== 0
  ) {
    throw new Error(`Invalid generated Plutarch V3 script: ${sourcePath}`);
  }
  return parsed;
};

export const unwrapTextEnvelopeCborHex = (cborHex, source = "cborHex") => {
  const bytes = Buffer.from(cborHex, "hex");
  if (bytes.length === 0 || (bytes[0] & 0xe0) !== 0x40) {
    throw new Error(`${source} must be a definite CBOR byte string`);
  }

  const additionalInfo = bytes[0] & 0x1f;
  let headerLength;
  let payloadLength;
  if (additionalInfo < 24) {
    headerLength = 1;
    payloadLength = BigInt(additionalInfo);
  } else {
    const lengthBytes =
      additionalInfo === 24
        ? 1
        : additionalInfo === 25
          ? 2
          : additionalInfo === 26
            ? 4
            : additionalInfo === 27
              ? 8
              : 0;
    if (lengthBytes === 0 || bytes.length < 1 + lengthBytes) {
      throw new Error(`${source} has an invalid CBOR byte-string header`);
    }
    headerLength = 1 + lengthBytes;
    payloadLength = 0n;
    for (let index = 1; index < headerLength; index += 1) {
      payloadLength = (payloadLength << 8n) | BigInt(bytes[index]);
    }
  }

  if (payloadLength !== BigInt(bytes.length - headerLength)) {
    throw new Error(`${source} CBOR byte-string length does not match its payload`);
  }
  const payload = bytes.subarray(headerLength);
  if (payload.length === 0 || (payload[0] & 0xe0) !== 0x40) {
    throw new Error(
      `${source} must wrap the single-CBOR script encoding expected by an Aiken blueprint`,
    );
  }
  return payload.toString("hex");
};

export const buildOffchainBlueprint = async ({
  generatedDir = path.join(plutarchDir, "generated"),
  outputPath = path.join(plutarchDir, "plutus.json"),
} = {}) => {
  const fileNames = (await readdir(generatedDir))
    .filter((fileName) => fileName.endsWith(".plutus.json"))
    .sort();
  if (fileNames.length === 0) {
    throw new Error(`No generated Plutarch scripts found in ${generatedDir}`);
  }

  const validators = await Promise.all(
    fileNames.map(async (fileName) => {
      const sourcePath = path.join(generatedDir, fileName);
      const generated = parseGeneratedScript(
        await readFile(sourcePath, "utf8"),
        sourcePath,
      );
      return {
        title: offchainTitleForGeneratedFile(fileName),
        compiledCode: unwrapTextEnvelopeCborHex(
          generated.cborHex,
          `${sourcePath}.cborHex`,
        ),
      };
    }),
  );

  const titles = validators.map(({ title }) => title);
  if (new Set(titles).size !== titles.length) {
    throw new Error(
      "Generated Plutarch scripts map to duplicate blueprint titles",
    );
  }

  const blueprint = {
    preamble: {
      title: "midgard/plutarch-offchain",
      description: "Plutarch contracts adapted for Midgard off-chain consumers",
      version: "1.0.0",
      plutusVersion: "v3",
      compiler: {
        name: "Plutarch",
      },
    },
    validators,
  };
  await writeFile(outputPath, `${JSON.stringify(blueprint, null, 2)}\n`);
  return { outputPath, validatorCount: validators.length, titles };
};

const invokedPath = process.argv[1]
  ? pathToFileURL(path.resolve(process.argv[1])).href
  : undefined;
if (invokedPath === import.meta.url) {
  const result = await buildOffchainBlueprint();
  process.stdout.write(
    `Wrote ${result.validatorCount.toString()} Plutarch validators to ${result.outputPath}\n`,
  );
}
