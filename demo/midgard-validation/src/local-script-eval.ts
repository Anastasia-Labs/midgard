import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { CEKConst, Machine } from "@harmoniclabs/plutus-machine";
import { Application, parseUPLC, UPLCConst } from "@harmoniclabs/uplc";
import { Constr, Data, fromHex } from "@lucid-evolution/lucid";

export type LocalScriptEvalResult =
  | {
      readonly kind: "accepted";
      readonly budget: {
        readonly cpu: bigint;
        readonly memory: bigint;
      };
    }
  | { readonly kind: "script_invalid"; readonly detail: string };

export const encodeScriptContextCbor = (
  scriptContext: Constr<unknown>,
): Uint8Array => fromHex(Data.to(scriptContext as any)) as Uint8Array;

export const evaluateUplcWithContextCbor = (
  scriptBytes: Uint8Array,
  contextCbor: Uint8Array,
): LocalScriptEvalResult => {
  try {
    const uplc = parseUPLC(scriptBytes, "cbor").body;
    const context = UPLCConst.data(dataFromCbor(contextCbor));
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

export const evaluateScriptWithHarmonic = (
  scriptBytes: Uint8Array,
  scriptContext: Constr<unknown>,
): LocalScriptEvalResult =>
  evaluateUplcWithContextCbor(
    scriptBytes,
    encodeScriptContextCbor(scriptContext),
  );
