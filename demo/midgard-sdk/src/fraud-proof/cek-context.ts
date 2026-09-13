import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Constr, Data } from "@lucid-evolution/lucid";

import { hashCekCoreWitness } from "./cek-core.js";
import { ValidationMachineSourceKindSchema } from "./validation-dispute.js";

export const CekContextBoundSchema = Data.Object({
  prepared: Data.Any(),
  control: Data.Any(),
  auxiliary_hash: Data.Bytes(),
  transaction_id: Data.Bytes(),
  source_kind: ValidationMachineSourceKindSchema,
});
export type CekContextBound = Data.Static<typeof CekContextBoundSchema>;
export const CekContextBound = asDataType<CekContextBound>(
  CekContextBoundSchema,
);
export const CekContextDatumSchema = Data.Object({
  fraud_prover: Data.Bytes(),
  data: Data.Nullable(Data.Any()),
});
export type CekContextDatum = Data.Static<typeof CekContextDatumSchema>;
export const CekContextDatum = asDataType<CekContextDatum>(
  CekContextDatumSchema,
);

const array = (value: Data, length: number): Data[] => {
  if (!Array.isArray(value) || value.length !== length)
    throw new Error(`Expected exact CEK context array of ${length} fields`);
  return value;
};
const cborData = (value: unknown): Data => {
  if (typeof value === "bigint") return value;
  if (typeof value === "number" && Number.isSafeInteger(value))
    return BigInt(value);
  if (value instanceof Uint8Array) return Buffer.from(value).toString("hex");
  if (Array.isArray(value)) return value.map(cborData);
  throw new Error("CEK context control contains a non-ledger CBOR value");
};
export const decodeCekContextCborArray = (
  value: string,
  length: number,
): Data[] => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(value))
    throw new Error("Expected canonical CEK context hex bytes");
  return array(cborData(decodeSingleCbor(Buffer.from(value, "hex"))), length);
};
const cborArray = (value: Data, length: number) => {
  if (typeof value !== "string")
    throw new Error("Expected CEK context CBOR bytes");
  return decodeCekContextCborArray(value, length);
};

const record = (fields: Data[]) => new Constr(0, fields);

/** Decode the authenticated canonical wire into the corresponding Aiken records. */
export const deriveCekContextBinding = (input: {
  readonly prepared: Data;
  readonly transactionId: string;
  readonly sourceKind: "Normal" | "Forced";
  readonly workWitnessCbor: string;
  readonly auxiliary: Data;
}) => {
  const cek = cborArray(input.workWitnessCbor, 9);
  const native = cborArray(cek[0]!, 26);
  for (const index of [7, 11, 13, 15, 17, 18, 20, 22]) {
    const peaks = native[index];
    if (!Array.isArray(peaks))
      throw new Error("Expected native frontier peaks");
    native[index] = peaks.map((peak) => record(array(peak, 2)));
  }
  const control = record([record(native), ...cek.slice(1)]);
  const bound = record([
    input.prepared,
    control,
    hashCekCoreWitness(input.auxiliary),
    input.transactionId,
    Data.from(
      Data.to(
        input.sourceKind,
        asDataType<"Normal" | "Forced">(ValidationMachineSourceKindSchema),
      ),
    ),
  ]);
  const contextFields = cborArray(cek[1]!, 25);
  for (const index of [12, 13, 14, 15, 17, 22, 23])
    contextFields[index] = record(array(contextFields[index]!, 4));
  for (const index of [19, 24])
    contextFields[index] = record(array(contextFields[index]!, 3));
  const context = record(contextFields);
  const facts = record(
    [0, 1, 2, 4, 6, 7, 8, 9, 12, 13, 14, 15, 16, 18, 19, 20].map(
      (index) => native[index]!,
    ),
  );
  const staged = record([bound, context, facts, cek[2]!]);
  return { bound, staged, context, native: facts, executionCursor: cek[2]! };
};

export const encodeCekContextRedeemer = (
  inputIndex: bigint,
  outputIndex: bigint,
  transition: Data,
  auxiliary: Data,
  claimedNext?: Data,
): string =>
  Data.to(
    new Constr(1, [
      record([
        inputIndex,
        outputIndex,
        transition,
        auxiliary,
        ...(claimedNext === undefined ? [] : [claimedNext]),
      ]),
    ]),
  );
