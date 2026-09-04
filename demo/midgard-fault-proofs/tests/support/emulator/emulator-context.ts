import {
  buildPhasMembershipRewardRegistrationTxProgram,
  MPF_CHUNKED_VERIFY_WITHDRAW_TITLE,
  parsePhasMembershipBlueprint,
  phasMembershipWithdrawalScriptFromBlueprint,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { resolveProverSigner } from "../../../src/index.js";
import { type Blueprint, getCompiledScript, network } from "./blueprints.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";

export const alignUnixTimeToEmulatorSlotBoundary = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  unixTime: number,
): number => lucid.slotToUnixTime(lucid.unixTimeToSlot(unixTime));

export const firstWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.wallet().getUtxos();
  if (utxo === undefined) {
    throw new Error(`Expected wallet UTxO for ${label}`);
  }
  return utxo;
};

/**
 * The wallet's largest-lovelace UTxO. The operator-registration and
 * -activation funding inputs are named explicitly rather than coin-selected,
 * so a single UTxO has to cover the whole bond; wallets seeded with a main
 * balance plus small fee UTxOs expose one of the fee UTxOs first.
 */
export const largestWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const utxos = await lucid.wallet().getUtxos();
  const [utxo] = [...utxos].sort((left, right) =>
    right.assets["lovelace"] === left.assets["lovelace"]
      ? 0
      : (right.assets["lovelace"] ?? 0n) > (left.assets["lovelace"] ?? 0n)
        ? 1
        : -1,
  );
  if (utxo === undefined) {
    throw new Error(`Expected wallet UTxO for ${label}`);
  }
  return utxo;
};

export const expectSingleUtxoWithUnit = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  address: string,
  unit: string,
): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};

export const requireUtxoWithUnit = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  address: string,
  unit: string,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.utxosAtWithUnit(address, unit);
  if (utxo === undefined) {
    throw new Error(`Expected live UTxO for ${label}`);
  }
  return utxo;
};

/**
 * Registers the reward account of the merkelized published-chunk verifier
 * (issue #545). A step on the chunked route withdraws zero from it, which is
 * how the verifier is invoked, so the account must exist first — exactly as the
 *  membership account must for the redeemer-carried route.
 */
export const registerChunkedVerifyRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const script: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, MPF_CHUNKED_VERIFY_WITHDRAW_TITLE),
  };
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, { script }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

export const registerPhasMembershipRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const phasMembershipScript = phasMembershipWithdrawalScriptFromBlueprint(
    parsePhasMembershipBlueprint(realBlueprint),
  );
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, {
      script: phasMembershipScript,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

export const runEmulatorLifecycleStage = async <T>(
  stage: string,
  operation: () => Promise<T>,
): Promise<T> => {
  try {
    return await operation();
  } catch (cause) {
    const serializedCause =
      typeof cause === "object" && cause !== null
        ? JSON.stringify(
            cause,
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          )
        : undefined;
    const detail = [
      cause instanceof Error ? (cause.stack ?? cause.message) : String(cause),
      serializedCause,
    ]
      .filter((value) => value !== undefined && value.length > 0)
      .join("\n");
    throw new Error(`emulator lifecycle stage ${stage} failed: ${detail}`);
  }
};

/**
 * A prover account funded at the address the PROVER SIGNER resolves.
 *
 * `resolveProverSigner` derives the seed's enterprise address — the
 * fraud-prover reward output is matched by payment credential with no stake
 * part — while `generateEmulatorAccount` seeds the ledger at the same seed's
 * base address. Left unaligned, every `signer.selectWallet(lucid)` call site
 * builds against an empty wallet. The address is asked of the production
 * resolver rather than restated here, so the harness cannot drift from it.
 */
export const fundedProverEmulatorAccount = (
  lovelace: bigint,
): EmulatorAccount => {
  const account = generateEmulatorAccount({ lovelace });
  return {
    ...account,
    address: resolveProverSigner({
      network,
      walletSeedPhrase: account.seedPhrase,
    }).address,
  };
};

/**
 * One party's emulator ledger entries: a main balance plus `feeUtxoCount`
 * small UTxOs, seeded twice — once at the base address `selectWallet.fromSeed`
 * derives, once at the enterprise address `resolveProverSigner` derives. A
 * journey mixes both selections, so funding only one of them strands whichever
 * half the next transaction happens to build from.
 */
export const seedDualAddressPartyAccounts = ({
  account,
  feeUtxoCount,
  feeUtxoLovelace,
}: {
  readonly account: EmulatorAccount;
  readonly feeUtxoCount: number;
  readonly feeUtxoLovelace: bigint;
}): readonly EmulatorAccount[] => {
  const main = {
    lovelace:
      account.assets["lovelace"]! - BigInt(feeUtxoCount) * feeUtxoLovelace,
  };
  const fees = { lovelace: feeUtxoLovelace };
  const proverAccount: EmulatorAccount = {
    ...account,
    address: resolveProverSigner({
      network,
      walletSeedPhrase: account.seedPhrase,
    }).address,
  };
  return [
    { ...account, assets: main },
    ...Array.from({ length: feeUtxoCount }, () => ({
      ...account,
      assets: fees,
    })),
    { ...proverAccount, assets: main },
    ...Array.from({ length: feeUtxoCount }, () => ({
      ...proverAccount,
      assets: fees,
    })),
  ];
};

/**
 * Two funded emulator wallets and their Lucid instances: the funder that
 * publishes the fraudulent block and the prover that drives the correction
 * path.
 */
export const newEmulatorParty = async () => {
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = fundedProverEmulatorAccount(20_000_000_000n);
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  // Selected through the signer so the prover Lucid instance and every
  // `signer.selectWallet(lucid)` call site address the same funded wallet.
  proverSigner.selectWallet(proverLucid);
  return { emulator, funderLucid, proverLucid, proverSigner };
};

export const funderPaymentKeyHash = async (
  funderLucid: Awaited<ReturnType<typeof Lucid>>,
): Promise<string> => {
  const credential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential === undefined || credential.type !== "Key") {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  return credential.hash;
};
