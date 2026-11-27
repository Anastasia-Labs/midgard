import {
  Emulator,
  generateEmulatorAccount,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { assertAvailabilityChallengeRewardAccountsRegisteredProgram } from "../src/transactions/availability-challenge-registration.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./helpers/mainnet-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

describe("availability challenge reward-account readiness", () => {
  it("refuses absent and partial registration and admits the fully registered deployment", async () => {
    const publisher = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([publisher], {
      ...MAINNET_PROTOCOL_PARAMETERS,
      maxTxSize: 16_384,
      maxTxExMem: 16_500_000n,
      maxTxExSteps: 10_000_000_000n,
    });
    const lucid = await createMainnetEmulatorLucid(emulator, "Preprod");
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const contracts = await loadRealMidgardContractsForTest(
      (await lucid.wallet().getUtxos())[0]!,
    );
    // The pooled DA bond replaced the per-block bond yield: the default
    // readiness set is exactly the four challenge yields the deployment has.
    expect(Object.keys(contracts.availabilityChallenge.yields).sort()).toEqual([
      "close",
      "open",
      "settle",
      "timeout",
    ]);
    const readiness = () =>
      Effect.runPromise(
        assertAvailabilityChallengeRewardAccountsRegisteredProgram(
          lucid,
          contracts,
        ),
      );
    await expect(readiness()).rejects.toThrow(
      /availability challenge open reward account is not registered/iu,
    );

    const openAddress = validatorToRewardAddress(
      "Preprod",
      contracts.availabilityChallenge.yields.open.withdrawalScript,
    );
    const openRegistration = await lucid
      .newTx()
      .register.Stake(openAddress)
      .complete({ localUPLCEval: true });
    const openSigned = await openRegistration.sign.withWallet().complete();
    await lucid.awaitTx(await openSigned.submit());
    await expect(
      Effect.runPromise(
        assertAvailabilityChallengeRewardAccountsRegisteredProgram(
          lucid,
          contracts,
          ["open"],
        ),
      ),
    ).resolves.toBeUndefined();
    await expect(readiness()).rejects.toThrow(
      /availability challenge settle reward account is not registered/iu,
    );

    let remainingRegistrations = lucid.newTx();
    for (const action of ["settle", "close", "timeout"] as const) {
      remainingRegistrations = remainingRegistrations.register.Stake(
        validatorToRewardAddress(
          "Preprod",
          contracts.availabilityChallenge.yields[action].withdrawalScript,
        ),
      );
    }
    const complete = await remainingRegistrations.complete({
      localUPLCEval: true,
    });
    const signed = await complete.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
    await expect(readiness()).resolves.toBeUndefined();
  });
});
