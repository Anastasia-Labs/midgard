import { createHash } from "node:crypto";

import {
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  mintingPolicyToId,
  type ProtocolParameters,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./helpers/mainnet-protocol-parameters.js";

const evaluatedMintCpu = async (
  parameters: ProtocolParameters,
): Promise<bigint> => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account], parameters);
  const lucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  // UPLC 1.1.0: (program 1.1.0 (lam context (con unit ()))).
  const policy = { type: "PlutusV3" as const, script: "46450101002499" };
  const tx = await lucid
    .newTx()
    .mintAssets({ [mintingPolicyToId(policy) + "01"]: 1n }, Data.void())
    .attach.MintingPolicy(policy)
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  const transaction = CML.Transaction.from_cbor_hex(signed.toCBOR());
  const redeemers = transaction.witness_set().redeemers()!.to_flat_format();
  expect(redeemers.len()).toBe(1);
  await signed.submit();
  return redeemers.get(0).ex_units().steps();
};

describe("pinned mainnet protocol parameters", () => {
  it("pins the current Van Rossem limits and complete Plutus V3 model", () => {
    expect(MAINNET_PROTOCOL_PARAMETERS_SOURCE).toMatchObject({
      epoch: 654,
      protocolMajor: 11,
      protocolMinor: 0,
    });
    expect(MAINNET_PROTOCOL_PARAMETERS).toMatchObject({
      maxTxSize: 16_384,
      maxTxExMem: 16_500_000n,
      maxTxExSteps: 10_000_000_000n,
    });
    expect(MAINNET_PROTOCOL_PARAMETERS.costModels.PlutusV3).toHaveLength(350);
    expect(
      createHash("sha256")
        .update(JSON.stringify(MAINNET_PROTOCOL_PARAMETERS.costModels.PlutusV3))
        .digest("hex"),
    ).toBe("6b74a19b00bcca178a4c723c0ce871d41fc5c5a417d738798faea60930961b4e");
  });

  it("evaluates with the supplied complete model rather than a built-in fallback", async () => {
    const originalCpu = await evaluatedMintCpu(MAINNET_PROTOCOL_PARAMETERS);
    const changed = [...MAINNET_PROTOCOL_PARAMETERS.costModels.PlutusV3];
    // Plutus V3 parameter 29 is cekStartupCost-exBudgetCPU.
    changed[29] = changed[29]! + 1_000_000;
    const changedCpu = await evaluatedMintCpu({
      ...MAINNET_PROTOCOL_PARAMETERS,
      costModels: {
        ...MAINNET_PROTOCOL_PARAMETERS.costModels,
        PlutusV3: changed,
      },
    });
    expect(changedCpu - originalCpu).toBe(1_000_000n);
  });
});
