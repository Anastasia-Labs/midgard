import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

export const CekCoreMachineStateSchema = Data.Object({
  mode: Data.Integer(),
  execution_index: Data.Integer(),
  focus_root: Data.Bytes(),
  environment_root: Data.Bytes(),
  continuation_root: Data.Bytes(),
  auxiliary: Data.Integer(),
  cpu: Data.Integer(),
  memory: Data.Integer(),
});
export type CekCoreMachineState = Data.Static<typeof CekCoreMachineStateSchema>;
export const CekCoreMachineState = asDataType<CekCoreMachineState>(
  CekCoreMachineStateSchema,
);
export const CekCoreBoundSchema = Data.Object({
  prepared: Data.Any(),
  pre: CekCoreMachineStateSchema,
  post: CekCoreMachineStateSchema,
  witness_hash: Data.Bytes(),
  arm: Data.Integer(),
  group: Data.Integer(),
  progress: Data.Integer(),
  facts: Data.Any(),
});
export type CekCoreBound = Data.Static<typeof CekCoreBoundSchema>;
export const CekCoreBound = asDataType<CekCoreBound>(CekCoreBoundSchema);
export const CekCoreDatumSchema = Data.Object({
  fraud_prover: Data.Bytes(),
  data: Data.Nullable(CekCoreBoundSchema),
});
export type CekCoreDatum = Data.Static<typeof CekCoreDatumSchema>;
export const CekCoreDatum = asDataType<CekCoreDatum>(CekCoreDatumSchema);
export const CekCoreBuiltinRootsSchema = Data.Object({
  arguments_root: Data.Bytes(),
  arguments_count: Data.Integer(),
  result_root: Data.Bytes(),
  builtin_root: Data.Bytes(),
});
export type CekCoreBuiltinRoots = Data.Static<typeof CekCoreBuiltinRootsSchema>;
export const CekCoreBuiltinRoots = asDataType<CekCoreBuiltinRoots>(
  CekCoreBuiltinRootsSchema,
);
export const CekCoreBudgetSchema = Data.Object({
  cpu: Data.Integer(),
  memory: Data.Integer(),
});
export type CekCoreBudget = Data.Static<typeof CekCoreBudgetSchema>;
export const CekCoreBudget = asDataType<CekCoreBudget>(CekCoreBudgetSchema);

export const hashCekCoreWitness = (witness: Data): string =>
  Buffer.from(
    blake2b(
      Buffer.from(aikenSerialisedPlutusDataCbor(Data.to(witness)), "hex"),
      { dkLen: 32 },
    ),
  ).toString("hex");

export const encodeCekCoreRedeemer = (
  inputIndex: bigint,
  outputIndex: bigint,
  evidence:
    | {
        readonly kind: "binder";
        readonly transition: Data;
        readonly step: Data;
      }
    | { readonly kind: "arm"; readonly witness: Data }
    | {
        readonly kind: "settle";
        readonly transition: Data;
        readonly witness: Data;
      },
): string => {
  const fields =
    evidence.kind === "binder"
      ? [evidence.transition, evidence.step]
      : evidence.kind === "settle"
        ? [evidence.transition, evidence.witness]
        : [evidence.witness];
  return Data.to(
    new Constr(1, [new Constr(0, [inputIndex, outputIndex, ...fields])]),
  );
};
