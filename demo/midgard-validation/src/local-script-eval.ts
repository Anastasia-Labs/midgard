import { Constr, Data, fromHex } from "@lucid-evolution/lucid";
import { Application, parseUPLC, UPLCConst } from "@harmoniclabs/uplc";
import { CEKConst, Machine } from "@harmoniclabs/plutus-machine";
import { dataFromCbor } from "@harmoniclabs/plutus-data";

export type LocalScriptEvalResult =
  | {
      readonly kind: "accepted";
      readonly budget: {
        readonly cpu: bigint;
        readonly memory: bigint;
      };
    }
  | { readonly kind: "script_invalid"; readonly detail: string };

export const evaluateScriptWithHarmonic = (
  scriptBytes: Uint8Array,
  scriptContext: Constr<unknown>,
): LocalScriptEvalResult => {
  try {
    const uplc = parseUPLC(scriptBytes, "cbor").body;
    const context = UPLCConst.data(
      dataFromCbor(fromHex(Data.to(scriptContext as any)) as Uint8Array),
    );
    const result = Machine.eval(new Application(uplc, context));

    if (result.result instanceof CEKConst) {
      return {
        kind: "accepted",
        budget: {
          cpu: result.budgetSpent.cpu,
          memory: result.budgetSpent.mem,
        },
      };
    }

    return {
      kind: "script_invalid",
      detail: "UPLC evaluation did not return a CEK constant",
    };
  } catch (e) {
    return {
      kind: "script_invalid",
      detail: String(e),
    };
  }
};
