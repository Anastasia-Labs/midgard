import "./helpers/follower-emulator-installed.js";

import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { afterEach, expect, it, vi } from "vitest";

import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import {
  ensureEventHistoryRewardAccountsRegisteredProgram,
  EventHistoryRegistrationUnfunded,
} from "../src/transactions/script-reward-registration.js";
import { refOf } from "./helpers/builder-wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  amountOf,
  isPlain,
  makeTokenBearingWallet,
} from "./helpers/token-bearing-wallet.js";

/** Out-refs the deployment's nonce helper reserves on top of the deployment's
 * own nonces, so a row can tell a registration that reads the helper from
 * one that derives the nonces itself. */
const nonceHelper = vi.hoisted(() => ({ extra: [] as string[] }));
vi.mock(
  "../src/transactions/reference-scripts.deployment-initialization-nonces.js",
  async (original) => {
    const actual =
      await original<
        typeof import("../src/transactions/reference-scripts.deployment-initialization-nonces.js")
      >();
    return {
      ...actual,
      deploymentInitializationNonceOutRefKeys: (
        contracts: Parameters<
          typeof actual.deploymentInitializationNonceOutRefKeys
        >[0],
      ) =>
        new Set([
          ...actual.deploymentInitializationNonceOutRefKeys(contracts),
          ...nonceHelper.extra,
        ]),
    };
  },
);
afterEach(() => {
  nonceHelper.extra = [];
});

it("registers all four real availability credentials (open, settle, close and timeout) with exact deposits and resumes idempotently", async () => {
  const wallet = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([wallet], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxTxSize: 16_384,
    maxTxExMem: 16_500_000n,
    maxTxExSteps: 10_000_000_000n,
  });
  const lucid = await Lucid(emulator, "Preprod");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
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
  const first = await runWithoutFollower(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ),
  );
  expect(first).toHaveLength(4);
  // The per-header bond yield is retired: the pooled bond needs no credential.
  expect(first.map(({ action }) => action).sort()).toEqual([
    "close",
    "open",
    "settle",
    "timeout",
  ]);
  expect(new Set(first.map(({ scriptHash }) => scriptHash)).size).toBe(4);
  expect(first.every(({ txHash }) => txHash !== null)).toBe(true);
  expect(
    await Promise.all(
      first.map(
        async ({ rewardAddress }) =>
          (await lucid.rewardAccountAt(rewardAddress)).registered,
      ),
    ),
  ).toEqual(Array(4).fill(true));
  expect(submitted).toHaveLength(4);
  for (const [index, cbor] of submitted.entries()) {
    expect(cbor.length / 2).toBeLessThanOrEqual(16_384);
    const tx = CML.Transaction.from_cbor_hex(cbor);
    const certs = tx.body().certs()!;
    expect(certs.len()).toBe(1);
    expect(
      certs
        .get(0)
        .as_stake_registration()!
        .stake_credential()
        .as_script()!
        .to_hex(),
    ).toBe(first[index]!.scriptHash);
  }
  const remaining = (await lucid.wallet().getUtxos()).reduce(
    (n, u) => n + (u.assets.lovelace ?? 0n),
    0n,
  );
  const fees = submitted.reduce(
    (n, cbor) => n + CML.Transaction.from_cbor_hex(cbor).body().fee(),
    0n,
  );
  expect(100_000_000n - remaining).toBe(
    4n * PROTOCOL_PARAMETERS_DEFAULT.keyDeposit + fees,
  );
  const second = await runWithoutFollower(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ),
  );
  expect(second.every(({ txHash }) => txHash === null)).toBe(true);
  expect(
    await Promise.all(
      second.map(
        async ({ rewardAddress }) =>
          (await lucid.rewardAccountAt(rewardAddress)).registered,
      ),
    ),
  ).toEqual(Array(4).fill(true));
  expect(submitted).toHaveLength(4);
});

it("registers history observers within the size limit without spending reserved nonces in a fragmented wallet", async () => {
  const wallet = generateEmulatorAccount({ lovelace: 5_000_000n });
  const emulator = new Emulator(
    Array.from({ length: 450 }, () => ({ ...wallet })),
    {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      maxTxSize: 16_384,
    },
  );
  const lucid = await Lucid(emulator, "Preprod");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const funding = await lucid.wallet().getUtxos();
  expect(funding).toHaveLength(450);
  const contracts = await loadRealMidgardContractsForTest(funding[0]!);
  const submitted: string[] = [];
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    submitted.push(cbor);
    return submit(cbor);
  };
  const run = () =>
    runWithoutFollower(
      ensureEventHistoryRewardAccountsRegisteredProgram(lucid, contracts).pipe(
        Effect.mapError((error) => new Error(inspect(error, { depth: 8 }))),
      ),
    );
  const registrations = await run();
  expect(registrations).toHaveLength(4);
  expect(registrations.every(({ registered }) => registered)).toBe(true);
  expect(submitted).toHaveLength(1);
  const signed = submitted[0]!;
  expect(signed.length / 2).toBeLessThanOrEqual(16_384);
  const body = CML.Transaction.from_cbor_hex(signed).body();
  expect(body.inputs().len()).toBeLessThan(10);
  expect(body.certs()!.len()).toBe(4);
  for (const { recipe } of Object.values(contracts.eventHistory!)) {
    expect(
      await lucid.utxosByOutRef([
        {
          txHash: recipe.initializationNonce.transactionId,
          outputIndex: Number(recipe.initializationNonce.outputIndex),
        },
      ]),
    ).toHaveLength(1);
  }
  const remaining = (await lucid.wallet().getUtxos()).reduce(
    (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
    0n,
  );
  expect(450n * 5_000_000n - remaining).toBe(
    4n * PROTOCOL_PARAMETERS_DEFAULT.keyDeposit + body.fee(),
  );
  expect((await run()).every(({ registered }) => registered)).toBe(true);
  expect(submitted).toHaveLength(1);
});

const historyFixture = async (
  outputs: readonly bigint[],
  nonce?: (lucid: LucidEvolution) => Promise<UTxO>,
) => {
  const wallet = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator(
    outputs.map((lovelace) => ({ ...wallet, assets: { lovelace } })),
    PROTOCOL_PARAMETERS_DEFAULT,
  );
  const lucid = await Lucid(emulator, "Preprod");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const nonceUtxo = nonce === undefined ? undefined : await nonce(lucid);
  const contracts = await loadRealMidgardContractsForTest(
    nonceUtxo ?? { txHash: "11".repeat(32), outputIndex: 0 },
  );
  const run = () =>
    runWithoutFollower(
      ensureEventHistoryRewardAccountsRegisteredProgram(lucid, contracts).pipe(
        Effect.either,
      ),
    );
  return { lucid, address: wallet.address, contracts, nonceUtxo, run };
};

const unspent = async (lucid: LucidEvolution, utxo: UTxO) =>
  (await lucid.utxosByOutRef([utxo])).length === 1;

it("registers history observers from a token-only wallet, keeping its nonce and returning the tokens in change", async () => {
  let unit = "";
  const { lucid, address, nonceUtxo, run } = await historyFixture(
    [300_000_000n],
    async (lucid) => {
      unit = await makeTokenBearingWallet(lucid, { outputs: 3 });
      return (await lucid.utxosAt(await lucid.wallet().address()))[0]!;
    },
  );
  const result = await run();
  expect(Either.isRight(result)).toBe(true);
  expect(
    Either.isRight(result) && result.right.every((r) => r.registered),
  ).toBe(true);
  expect(await unspent(lucid, nonceUtxo!)).toBe(true);
  const after = await lucid.utxosAt(address);
  expect(after.some(isPlain)).toBe(false);
  expect(amountOf(after, unit)).toBe(3n);
});

it("refuses by name a wallet whose only funds sit in reference-script outputs, spending none of them", async () => {
  const { lucid, address, run } = await historyFixture([300_000_000n]);
  const funding = await lucid.utxosAt(address);
  const script = scriptFromNative({
    type: "sig",
    keyHash: paymentCredentialOf(address)!.hash,
  });
  let tx = lucid.newTx().collectFrom(funding);
  for (let index = 0; index < 3; index += 1)
    tx = tx.pay.ToAddressWithData(
      address,
      undefined,
      { lovelace: 80_000_000n },
      script,
    );
  const unsigned = await tx.complete({
    coinSelection: false,
    changeAddress: generateEmulatorAccount({ lovelace: 0n }).address,
    presetWalletInputs: funding,
  });
  await lucid.awaitTx(
    await (await unsigned.sign.withWallet().complete()).submit(),
  );
  const published = await lucid.utxosAt(address);
  expect(published).toHaveLength(3);
  expect(published.every((utxo) => utxo.scriptRef != null)).toBe(true);

  const result = await run();
  expect(Either.isLeft(result)).toBe(true);
  const error = Either.isLeft(result) ? result.left : undefined;
  expect(error).toBeInstanceOf(SDK.LucidError);
  expect((error as InstanceType<typeof SDK.LucidError>).cause).toBeInstanceOf(
    EventHistoryRegistrationUnfunded,
  );
  for (const utxo of published) expect(await unspent(lucid, utxo)).toBe(true);
});

it("leaves out every out-ref the deployment's nonce helper reserves", async () => {
  const { lucid, address, run } = await historyFixture([
    500_000_000n,
    50_000_000n,
  ]);
  const [large, small] = (await lucid.utxosAt(address)).sort((a, b) =>
    Number((b.assets.lovelace ?? 0n) - (a.assets.lovelace ?? 0n)),
  );
  nonceHelper.extra = [refOf(large!)];
  const result = await run();
  expect(Either.isRight(result)).toBe(true);
  expect(await unspent(lucid, large!)).toBe(true);
  expect(await unspent(lucid, small!)).toBe(false);
});
