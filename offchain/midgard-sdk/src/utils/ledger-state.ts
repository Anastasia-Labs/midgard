import { CML, Data, OutRef, Script, ScriptType, UTxO, valueToAssets } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { cmlInputToOutRef } from "./cml.js";
import { Header } from "@/types/contracts/ledger-state.js";
import { hashHexWithBlake2b224 } from "./common.js";

export const findSpentAndProducedUTxOs = (
  txCBOR: string
): { spent: OutRef[]; produced: UTxO[] } => {
  try {
    const tx = CML.Transaction.from_cbor_hex(txCBOR);
    const txBody = tx.body();
    const inputs = txBody.inputs();
    const inputsCount = inputs.len();
    const spent: OutRef[] = [];
    for (let i = 0; i < inputsCount; i++) {
      try {
        const input = inputs.get(i);
        spent.push(cmlInputToOutRef(input));
      } catch(e) {
        console.log(e);
      }
    }
    const txHash = CML.hash_transaction(txBody).to_hex();
    const outputs = txBody.outputs();
    const outputsCount = outputs.len();
    const produced: UTxO[] = [];
    for (let i = 0; i < outputsCount; i++) {
      try {
        const output = outputs.get(i);
        const scriptRefObj = output.script_ref();
        let scriptRef: Script | null | undefined = undefined;
        if (scriptRefObj) {
          const scriptRefKind = scriptRefObj.kind();
          const [scriptKind, scriptHex]: [ScriptType, string | undefined] =
            scriptRefKind === CML.ScriptKind.Native
            ? ["Native", scriptRefObj.as_native()?.to_cbor_hex()]
            : scriptRefKind === CML.ScriptKind.PlutusV1
            ? ["PlutusV1", scriptRefObj.as_plutus_v1()?.to_cbor_hex()]
            : scriptRefKind === CML.ScriptKind.PlutusV2
            ? ["PlutusV2", scriptRefObj.as_plutus_v2()?.to_cbor_hex()]
            : ["PlutusV3", scriptRefObj.as_plutus_v3()?.to_cbor_hex()];
          if (scriptHex) {
            scriptRef = { type: scriptKind, script: scriptHex };
          }
        }
        const theUTxO: UTxO = {
          address: output.address().to_bech32(),
          assets: valueToAssets(output.amount()),
          datumHash: output.datum_hash()?.to_hex(),
          datum: output.datum()?.to_cbor_hex(),
          scriptRef,
          txHash,
          outputIndex: i,
        }
        produced.push(theUTxO);
      } catch(e) {
        console.log(e);
      }
    }
    return ({ spent, produced });
  } catch (_e) {
      throw Error("Something went wrong decoding the transaction")
  }
};

export const hashHeader = (header: Header): Effect.Effect<string, Error> =>
  hashHexWithBlake2b224(Data.to(header, Header));
