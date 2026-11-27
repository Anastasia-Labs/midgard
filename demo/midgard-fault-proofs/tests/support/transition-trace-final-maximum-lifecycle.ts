/**
 * The shape of the accepted L2 transaction whose fault proof takes the most
 * steps (submit-init-emulator-transition-trace-final-maximum-lifecycle.test.ts
 * pins it and convicts a wrong root at it;
 * submit-init-emulator-transition-trace-final-maximum-lifecycle-honest.test.ts
 * refuses the honest root at it). The two plays are in separate files so
 * that they run in parallel.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";

export const output = (
  extra: Partial<Parameters<typeof encodeMidgardTxOutput>[0]> = {},
) =>
  encodeMidgardTxOutput({
    address: Buffer.from(`60${"aa".repeat(28)}`, "hex"),
    value: { lovelace: 0n, assets: new Map() },
    ...extra,
  });

/** The staged output with the most steps per field byte. */
export const densestStaged = output({
  datum: { kind: "inline", cbor: Buffer.from("00", "hex") },
});

export const MAXIMUM = {
  assetCount: 0,
  spentCount: 819,
  outputCount: 780,
  outputCbor: densestStaged.toString("hex"),
} as const;

export const MAXIMUM_LIFECYCLE_TIMEOUT = 1_800_000;
