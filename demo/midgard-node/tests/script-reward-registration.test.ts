import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

it("registers all five real availability credentials with exact deposits and resumes idempotently", async () => {
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
  const first = await Effect.runPromise(
    ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    ),
  );
  expect(first).toHaveLength(5);
  expect(new Set(first.map(({ scriptHash }) => scriptHash)).size).toBe(5);
  expect(first.every(({ txHash }) => txHash !== null)).toBe(true);
  expect(
    await Promise.all(
      first.map(
        async ({ rewardAddress }) =>
          (await lucid.rewardAccountAt(rewardAddress)).registered,
      ),
    ),
  ).toEqual(Array(5).fill(true));
  expect(submitted).toHaveLength(5);
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
    5n * PROTOCOL_PARAMETERS_DEFAULT.keyDeposit + fees,
  );
  const second = await Effect.runPromise(
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
  ).toEqual(Array(5).fill(true));
  expect(submitted).toHaveLength(5);
});
