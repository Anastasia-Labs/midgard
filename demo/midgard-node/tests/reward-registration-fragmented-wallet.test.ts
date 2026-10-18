/**
 * Stake registrations funded from a wallet of small coins. Each coin is
 * smaller than one registration deposit, so a builder must select enough of
 * them for the deposits, the fee and the change; a wallet whose coins cannot
 * cover that is refused by name, spending nothing, and a wallet view with no
 * output is refused as empty before any build.
 */
import "./helpers/follower-emulator-installed.js";

import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { expect, it } from "vitest";

import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import { ensureRuntimeRewardAccountsRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { RewardRegistrationUnfunded } from "../src/transactions/utils.reward-registration-funding.js";
import { causeTrail } from "./helpers/builder-wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const COIN = 1_500_000n;
const MAX_TX_SIZE = 16_384;

/** A wallet of `coins` outputs of 1.5 ADA each, with the real contracts. */
const smallCoinWallet = async (coins: number) => {
  const wallet = generateEmulatorAccount({ lovelace: COIN });
  const emulator = new Emulator(
    Array.from({ length: coins }, () => ({ ...wallet })),
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize: MAX_TX_SIZE },
  );
  const lucid = await Lucid(emulator, "Preprod");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const funding = await lucid.wallet().getUtxos();
  expect(funding).toHaveLength(coins);
  expect(funding.every((utxo) => utxo.assets.lovelace === COIN)).toBe(true);
  const contracts = await loadRealMidgardContractsForTest({
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const submitted: string[] = [];
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    submitted.push(cbor);
    return submit(cbor);
  };
  const balance = async () =>
    (await lucid.wallet().getUtxos()).reduce(
      (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
      0n,
    );
  return { lucid, contracts, submitted, balance };
};

const certificatesOf = (cbor: string) =>
  CML.Transaction.from_cbor_hex(cbor).body().certs()?.len() ?? 0;
const feeOf = (cbor: string) =>
  CML.Transaction.from_cbor_hex(cbor).body().fee();
/** Its only output is the change: no leftover was given up as fee. */
const returnsItsChange = (cbor: string) =>
  CML.Transaction.from_cbor_hex(cbor).body().outputs().len() === 1;

/** A failure with its whole cause chain in the message. */
const inspected = (error: unknown) => new Error(inspect(error, { depth: 8 }));

const expectRefusedByName = (result: Either.Either<unknown, unknown>) => {
  expect(Either.isLeft(result)).toBe(true);
  const error = Either.isLeft(result) ? result.left : undefined;
  expect(error).toBeInstanceOf(SDK.LucidError);
  expect((error as InstanceType<typeof SDK.LucidError>).cause).toBeInstanceOf(
    RewardRegistrationUnfunded,
  );
};

it("registers every availability credential from a wallet of coins smaller than one deposit", async () => {
  const { lucid, contracts, submitted, balance } = await smallCoinWallet(40);
  const registrations = await runWithoutFollower(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ).pipe(Effect.mapError(inspected)),
  );
  expect(registrations).toHaveLength(4);
  for (const { rewardAddress } of registrations)
    expect((await lucid.rewardAccountAt(rewardAddress)).registered).toBe(true);
  expect(submitted).toHaveLength(4);
  for (const cbor of submitted) {
    expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
    expect(certificatesOf(cbor)).toBe(1);
    expect(returnsItsChange(cbor)).toBe(true);
  }
  const fees = submitted.reduce((sum, cbor) => sum + feeOf(cbor), 0n);
  expect(40n * COIN - (await balance())).toBe(
    4n * PROTOCOL_PARAMETERS_DEFAULT.keyDeposit + fees,
  );
});

it("refuses by name an availability registration its coins cannot cover, spending none", async () => {
  const { lucid, contracts, submitted, balance } = await smallCoinWallet(2);
  const result = await runWithoutFollower(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ).pipe(Effect.either),
  );
  expectRefusedByName(result);
  expect(submitted).toHaveLength(0);
  expect(await balance()).toBe(2n * COIN);
});

it("registers a full batch of 32 runtime credentials from a wallet of coins smaller than one deposit", async () => {
  const { lucid, contracts, submitted, balance } = await smallCoinWallet(200);
  const registrations = await runWithoutFollower(
    ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts).pipe(
      Effect.mapError(inspected),
    ),
  );
  expect(registrations.length).toBeGreaterThanOrEqual(32);
  for (const { rewardAddress } of registrations)
    expect((await lucid.rewardAccountAt(rewardAddress)).registered).toBe(true);
  // The first batch is the largest: 32 certificates, 64 ADA of deposits.
  expect(certificatesOf(submitted[0]!)).toBe(32);
  expect(submitted.reduce((sum, cbor) => sum + certificatesOf(cbor), 0)).toBe(
    registrations.length,
  );
  for (const cbor of submitted) {
    expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
    expect(returnsItsChange(cbor)).toBe(true);
  }
  const fees = submitted.reduce((sum, cbor) => sum + feeOf(cbor), 0n);
  expect(200n * COIN - (await balance())).toBe(
    BigInt(registrations.length) * PROTOCOL_PARAMETERS_DEFAULT.keyDeposit +
      fees,
  );
});

it("refuses by name a runtime batch its coins cannot cover, spending none", async () => {
  // 30 ADA in all, short of the first batch's 64 ADA of deposits.
  const { lucid, contracts, submitted, balance } = await smallCoinWallet(20);
  const result = await runWithoutFollower(
    ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts).pipe(
      Effect.either,
    ),
  );
  expectRefusedByName(result);
  expect(submitted).toHaveLength(0);
  expect(await balance()).toBe(20n * COIN);
});

const expectRefusedAsEmptyView = (result: Either.Either<unknown, unknown>) => {
  expect(Either.isLeft(result)).toBe(true);
  expect(causeTrail(result)).toContain("wallet_view_empty");
};

it("refuses an availability registration from an empty wallet view as empty", async () => {
  const { lucid, contracts, submitted } = await smallCoinWallet(0);
  const result = await runWithoutFollower(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ).pipe(Effect.either),
  );
  expectRefusedAsEmptyView(result);
  expect(submitted).toHaveLength(0);
});

it("refuses a runtime batch from an empty wallet view as empty", async () => {
  const { lucid, contracts, submitted } = await smallCoinWallet(0);
  const result = await runWithoutFollower(
    ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts).pipe(
      Effect.either,
    ),
  );
  expectRefusedAsEmptyView(result);
  expect(submitted).toHaveLength(0);
});
