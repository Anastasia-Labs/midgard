import {
  encodeMidgardFieldPreimageForField,
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";

/**
 * The `assetCount` assets a native transaction trace carries under
 * `policyId` and, when the transaction mints them, the witness-script and
 * mint field preimages, with `mintScript` as the witness script.
 */
export const nativeTraceAssets = ({
  assetCount,
  policyId,
  mintScript,
}: {
  readonly assetCount: number;
  readonly policyId: string;
  readonly mintScript?: MidgardVersionedScript;
}) => {
  const assetEntries = Array.from({ length: assetCount }, (_, i) => {
    const name =
      assetCount === 1304
        ? i === 0
          ? ""
          : i <= 256
            ? (i - 1).toString(16).padStart(2, "0")
            : (i - 257).toString(16).padStart(4, "0")
        : i.toString(16).padStart(4, "0");
    return [name, assetCount === 1304 ? (i === 1303 ? 256n : 1n) : 7n] as const;
  });
  const txAssets = new Map([[policyId, new Map(assetEntries)]]);
  const mintFields =
    mintScript !== undefined
      ? {
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            mintScript,
          ]),
          mintPreimageCbor: encodeMidgardFieldPreimageForField({
            fieldIndex: 5,
            items: [
              {
                policyId: Buffer.from(policyId, "hex"),
                assets: assetEntries.map(([name, quantity]) => ({
                  assetName: Buffer.from(name, "hex"),
                  quantity,
                })),
              },
            ],
          }),
        }
      : {};
  return { txAssets, mintFields };
};
