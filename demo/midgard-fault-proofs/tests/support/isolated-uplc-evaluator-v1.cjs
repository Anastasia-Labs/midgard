"use strict";

const fs = require("node:fs");
const { createRequire } = require("node:module");

const requireFromHere = createRequire(__filename);
const lucidEntry = requireFromHere.resolve("@lucid-evolution/lucid");
const uplcEntry = requireFromHere.resolve("@lucid-evolution/uplc", {
  paths: [lucidEntry],
});
const utilsEntry = requireFromHere.resolve("@lucid-evolution/utils", {
  paths: [lucidEntry],
});
const uplc = requireFromHere(uplcEntry);
const { utxoToTransactionInput, utxoToTransactionOutput } =
  requireFromHere(utilsEntry);

const request = JSON.parse(fs.readFileSync(0, "utf8"), (_key, value) =>
  value !== null && typeof value === "object" && "$bigint" in value
    ? BigInt(value.$bigint)
    : value,
);
const fromHex = (hex) => Buffer.from(hex, "hex");
const inputs = request.additionalUTxOs.map((utxo) =>
  utxoToTransactionInput(utxo).to_cbor_bytes(),
);
const outputs = request.additionalUTxOs.map((utxo) =>
  utxoToTransactionOutput(utxo).to_cbor_bytes(),
);
const result = uplc.eval_phase_two_raw(
  fromHex(request.tx),
  inputs,
  outputs,
  fromHex(request.costModels),
  BigInt(request.maxTxExSteps),
  BigInt(request.maxTxExMem),
  BigInt(request.zeroTime),
  BigInt(request.zeroSlot),
  request.slotLength,
);

fs.writeFileSync(
  1,
  JSON.stringify(result.map((bytes) => Buffer.from(bytes).toString("hex"))),
);
