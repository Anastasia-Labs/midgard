import { type Assets, type UTxO } from "@lucid-evolution/lucid";

export const REFERENCE_SCRIPT_ADDRESS = "addr_test1reference";

export const txHashFixture = (value: string): string => value.padStart(64, "0");

export const mkUtxo = ({
  txHash,
  outputIndex = 0,
  assets,
  scriptRef = false,
  datum,
  datumHash,
}: {
  readonly txHash: string;
  readonly outputIndex?: number;
  readonly assets: Assets;
  readonly scriptRef?: boolean;
  readonly datum?: string;
  readonly datumHash?: string;
}): UTxO => ({
  txHash: txHashFixture(txHash),
  outputIndex,
  address: REFERENCE_SCRIPT_ADDRESS,
  assets,
  ...(datum === undefined ? {} : { datum }),
  ...(datumHash === undefined ? {} : { datumHash }),
  ...(scriptRef
    ? {
        scriptRef: {
          type: "Native" as const,
          script: "8200",
        },
      }
    : {}),
});
