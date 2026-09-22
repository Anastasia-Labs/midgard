import {
  encodeMidgardFieldPreimage,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";

import { makeNativeTx } from "./emulator/native-tx.js";

export const makeReferenceNativeTx = ({
  referenceInputCbors,
  ...options
}: Parameters<typeof makeNativeTx>[0] & {
  referenceInputCbors?: readonly Buffer[];
}) => {
  const base = makeNativeTx(options);
  return referenceInputCbors === undefined
    ? base
    : materializeMidgardNativeTxFromCanonical({
        ...base,
        body: {
          ...base.body,
          referenceInputsPreimageCbor:
            encodeMidgardFieldPreimage(referenceInputCbors),
        },
      });
};
