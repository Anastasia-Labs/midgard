import { encodeCborArrayRaw, readCborBytes } from "@al-ft/midgard-core/codec";
import {
  applyDoubleCborEncoding,
  Data,
  fromHex,
  toHex,
} from "@lucid-evolution/lucid";
import { apply_params_to_script } from "@lucid-evolution/uplc";

/**
 * The applied script a blueprint entry should yield, computed outside the
 * blueprint module's shape guard so a test can compare the guarded result
 * against it. It checks no arity: it is an oracle, never a deployment path.
 */
export const applyExpectedScriptParams = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  applyDoubleCborEncoding(
    toHex(
      apply_params_to_script(
        encodeCborArrayRaw(params.map((param) => fromHex(Data.to(param)))),
        readCborBytes(
          fromHex(applyDoubleCborEncoding(compiledCode)),
          0,
          "independent application",
        ).value,
      ),
    ),
  );
