import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import type { DaAttestationValidatorSet } from "../src/l1/deployment.js";

export const availabilityChallengeValidator =
  (): DaAttestationValidatorSet["availabilityChallenge"] => ({
    ...validator("ee".repeat(28), "addr_test1availability"),
    yields: Object.fromEntries(
      ["open", "settle", "close", "timeout"].map((arm) => [
        arm,
        {
          withdrawalScriptCBOR: "49480100002221200101",
          withdrawalScript: {
            type: "PlutusV3",
            script: "49480100002221200101",
          },
          withdrawalScriptHash: "f1".repeat(28),
        },
      ]),
    ) as SDK.AvailabilityChallengeYieldValidators,
  });

export function stateQueueValidator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["stateQueue"] {
  const yieldValidator = (
    role: string,
  ): DaAttestationValidatorSet["stateQueue"]["yields"]["commit"] => ({
    withdrawalScriptCBOR: "",
    withdrawalScript: { type: "PlutusV3", script: "00" } as never,
    withdrawalScriptHash: role,
  });
  return {
    ...validator(policyId, spendingScriptAddress),
    yields: {
      commit: yieldValidator("c1".repeat(28)),
      unattestedTimeout: yieldValidator("c2".repeat(28)),
      unavailableTimeout: yieldValidator("c3".repeat(28)),
      fraudRemoval: yieldValidator("c4".repeat(28)),
      merge: yieldValidator("c5".repeat(28)),
    },
  };
}

export function validator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["daAttestation"] {
  return {
    mintingScriptCBOR: "",
    mintingScript: { type: "PlutusV3", script: "00" } as never,
    policyId,
    spendingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "00" } as never,
    spendingScriptHash: policyId,
    spendingScriptAddress,
  };
}

export function utxo(
  byte: string,
  outputIndex: number,
  assets: UTxO["assets"] = { lovelace: 5_000_000n },
  extra: Partial<UTxO> = {},
): UTxO {
  return {
    txHash: byte.repeat(32),
    outputIndex,
    address: "addr_test1fixture",
    assets,
    ...extra,
  } as UTxO;
}

export const outRef = (entry: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${entry.txHash}#${entry.outputIndex.toString()}`;
