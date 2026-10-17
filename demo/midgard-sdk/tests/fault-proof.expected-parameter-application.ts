import { encodeCborArrayRaw, readCborBytes } from "@al-ft/midgard-core/codec";
import {
  applyDoubleCborEncoding,
  Data,
  fromHex,
  toHex,
} from "@lucid-evolution/lucid";
import { apply_params_to_script } from "@lucid-evolution/uplc";

/** Apply independently specified test bindings using canonical Plutus Data. */
export const applyExpectedScriptParams = (
  compiledCode: string,
  params: readonly Data[],
): string => {
  const script = readCborBytes(
    fromHex(applyDoubleCborEncoding(compiledCode)),
    0,
    "expected script",
  ).value;
  return applyDoubleCborEncoding(
    toHex(
      apply_params_to_script(
        encodeCborArrayRaw(params.map((param) => fromHex(Data.to(param)))),
        script,
      ),
    ),
  );
};
