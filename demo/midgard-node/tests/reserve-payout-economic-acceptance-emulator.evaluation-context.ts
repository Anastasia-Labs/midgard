import { Cbor, CborArray, CborMap, CborUInt } from "@harmoniclabs/cbor";
import {
  type Assets,
  coreToTxOutput,
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";
import { eval_phase_two_raw } from "@lucid-evolution/uplc";
import { expect } from "vitest";

import { CML } from "./deposit-flow-emulator-shared.js";

export const outputValue = (txCbor: string): Assets => {
  const outputs = CML.Transaction.from_cbor_hex(txCbor).body().outputs();
  const total: Assets = {};
  for (let i = 0; i < outputs.len(); i++)
    for (const [unit, quantity] of Object.entries(
      coreToTxOutput(outputs.get(i)).assets,
    ))
      total[unit] = (total[unit] ?? 0n) + quantity;
  return total;
};

// CBOR objects preserve map pairs, including the destination's duplicate keys.
// This changes outputs only; every other transaction field is retained.
export const withOutputs = (
  signedCbor: string,
  replace: ReadonlyMap<number, CML.TransactionOutput>,
): string => {
  const tx = Cbor.parse(signedCbor);
  if (!(tx instanceof CborArray) || !(tx.array[0] instanceof CborMap))
    throw new Error("Expected a complete Cardano transaction");
  const entry = tx.array[0].map.find(
    ({ k }) => k instanceof CborUInt && k.num === 1n,
  );
  if (!(entry?.v instanceof CborArray))
    throw new Error("Missing transaction outputs");
  for (const [index, output] of replace) {
    if (entry.v.array[index] === undefined)
      throw new Error("Invalid output mutation index");
    entry.v.array[index] = Cbor.parse(output.to_cbor_hex());
  }
  return Buffer.from(Cbor.encode(tx)).toString("hex");
};

export const withRedeemer = (
  signedCbor: string,
  inputIndex: bigint,
  data: string,
): string => {
  const tx = Cbor.parse(signedCbor);
  if (!(tx instanceof CborArray) || !(tx.array[1] instanceof CborMap))
    throw new Error("Expected complete transaction witnesses");
  const redeemers = tx.array[1].map.find(
    ({ k }) => k instanceof CborUInt && k.num === 5n,
  )?.v;
  const targeted = (tag: unknown, index: unknown) =>
    tag instanceof CborUInt &&
    tag.num === BigInt(CML.RedeemerTag.Spend) &&
    index instanceof CborUInt &&
    index.num === inputIndex;
  let replacements = 0;
  // Both ledger encodings are valid. Replace only the data item, retaining
  // the original map/list representation, order, indices and execution units.
  if (redeemers instanceof CborMap) {
    for (const { k, v } of redeemers.map) {
      if (
        !(k instanceof CborArray) ||
        k.array.length !== 2 ||
        !(v instanceof CborArray) ||
        v.array.length !== 2
      )
        throw new Error("Invalid completed redeemer map entry");
      if (targeted(k.array[0], k.array[1])) {
        v.array[0] = Cbor.parse(data);
        replacements++;
      }
    }
  } else if (redeemers instanceof CborArray) {
    for (const entry of redeemers.array) {
      if (!(entry instanceof CborArray) || entry.array.length !== 4)
        throw new Error("Invalid completed legacy redeemer entry");
      if (targeted(entry.array[0], entry.array[1])) {
        entry.array[2] = Cbor.parse(data);
        replacements++;
      }
    }
  } else {
    throw new Error("Expected completed redeemer map or list");
  }
  if (replacements !== 1)
    throw new Error("Missing targeted payout spend redeemer");
  const mutated = Buffer.from(Cbor.encode(tx)).toString("hex");
  expect(CML.Transaction.from_cbor_hex(mutated).body().to_cbor_hex()).toBe(
    CML.Transaction.from_cbor_hex(signedCbor).body().to_cbor_hex(),
  );
  return mutated;
};

export const evaluationContext = async (
  lucid: LucidEvolution,
  signedCbor: string,
) => {
  const body = CML.Transaction.from_cbor_hex(signedCbor).body();
  const refs = [body.inputs(), body.reference_inputs()].flatMap((inputs) =>
    Array.from({ length: inputs?.len() ?? 0 }, (_, i) => {
      const input = inputs!.get(i);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    }),
  );
  const resolved = await lucid.utxosByOutRef(refs);
  expect(resolved).toHaveLength(refs.length);
  const utxos = refs.map((ref) => {
    const found = resolved.find(
      (utxo) =>
        utxo.txHash === ref.txHash && utxo.outputIndex === ref.outputIndex,
    );
    if (found === undefined) throw new Error("Missing actual evaluation input");
    return found;
  });
  const config = lucid.config();
  const slots = config.slotConfig ?? SLOT_CONFIG_NETWORK[config.network!];
  if (
    slots === undefined ||
    config.costModels === undefined ||
    config.protocolParameters === undefined
  )
    throw new Error("Missing actual evaluator parameters");
  const inputCbor = utxos.map((utxo) =>
    utxoToTransactionInput(utxo).to_cbor_hex(),
  );
  const outputCbor = utxos.map((utxo) =>
    utxoToTransactionOutput(utxo).to_cbor_hex(),
  );
  const costModelsCbor = config.costModels.to_cbor_hex();
  const parameters = config.protocolParameters;
  const evaluate = (cbor: string) =>
    eval_phase_two_raw(
      Buffer.from(cbor, "hex"),
      inputCbor.map((value) => Buffer.from(value, "hex")),
      outputCbor.map((value) => Buffer.from(value, "hex")),
      Buffer.from(costModelsCbor, "hex"),
      parameters.maxTxExSteps,
      parameters.maxTxExMem,
      BigInt(slots.zeroTime),
      BigInt(slots.zeroSlot),
      slots.slotLength,
    ).map((bytes) => Buffer.from(bytes).toString("hex"));
  const positive = evaluate(signedCbor);
  const redeemers = CML.Transaction.from_cbor_hex(signedCbor)
    .witness_set()
    .redeemers()!
    .to_flat_format();
  expect(positive).toHaveLength(redeemers.len());
  expect(signedCbor.length / 2).toBeLessThanOrEqual(parameters.maxTxSize);
  let memory = 0n;
  let steps = 0n;
  for (let i = 0; i < redeemers.len(); i++) {
    memory += redeemers.get(i).ex_units().mem();
    steps += redeemers.get(i).ex_units().steps();
  }
  expect(memory).toBeLessThanOrEqual(parameters.maxTxExMem);
  expect(steps).toBeLessThanOrEqual(parameters.maxTxExSteps);
  // Establish that the mutation serializer itself preserves the usable script
  // context, including ordered duplicate datum pairs, before testing changes.
  expect(evaluate(withOutputs(signedCbor, new Map()))).toEqual(positive);
  return {
    evaluate,
    evidence: {
      signedCbor,
      inputCbor,
      outputCbor,
      costModelsCbor,
      parameters,
      slots,
      positive,
    },
  };
};
