import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";

export const makeForcedTxFixture = (
  options: Parameters<typeof makeNativeTx>[0] = {},
) => {
  const native = makeNativeTx(options);
  const tx = materializeMidgardForcedTxFromCanonical(native.tx);
  return { tx, txId: native.txId, txCbor: encodeMidgardForcedTxCanonical(tx) };
};
