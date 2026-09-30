import { type Data, dataFromCbor } from "@harmoniclabs/plutus-data";
import { CEKConst, Machine } from "@harmoniclabs/plutus-machine";
import { Application, parseUPLC, UPLCConst } from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "./cek-constant.js";

export type LocalScriptEvalResult =
  | {
      readonly kind: "accepted";
      readonly budget: {
        readonly cpu: bigint;
        readonly memory: bigint;
      };
    }
  | { readonly kind: "script_invalid"; readonly detail: string };

/**
 * Keeps every map in the order the context builder gave it. Harmonic's own
 * `dataToCbor` is not used: it truncates long byte strings.
 */
export const encodeScriptContextCbor = (scriptContext: Data): Uint8Array =>
  encodeMidgardCekPlutusData(scriptContext);

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
  scriptContext: Data,
): LocalScriptEvalResult =>
  evaluateUplcWithContextCbor(
    scriptBytes,
    encodeScriptContextCbor(scriptContext),
  );
