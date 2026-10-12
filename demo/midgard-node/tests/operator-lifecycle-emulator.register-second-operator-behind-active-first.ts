import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  EMULATOR_REQUIRED_BOND_LOVELACE,
  fragmentOperatorWalletUtxos,
  initOperatorLifecycleFixture,
  mkDeterministicRng,
  reconcileLiveWalletUtxos,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";

export const churnOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    seed,
    rounds,
  }: {
    seed: number;
    rounds: number;
  },
) => {
  const rng = mkDeterministicRng(seed);
  for (let round = 0; round < rounds; round += 1) {
    const outputs = 12 + Math.floor(rng() * 30);
    const lovelacePerOutput = BigInt(1_900_000 + Math.floor(rng() * 2_200_000));
    await fragmentOperatorWalletUtxos(lucid, { outputs, lovelacePerOutput });
    const secondOutputs = 8 + Math.floor(rng() * 18);
    const secondLovelacePerOutput = BigInt(
      1_900_000 + Math.floor(rng() * 1_600_000),
    );
    await fragmentOperatorWalletUtxos(lucid, {
      outputs: secondOutputs,
      lovelacePerOutput: secondLovelacePerOutput,
    });
    await reconcileLiveWalletUtxos(lucid, await lucid.wallet().getUtxos());
  }
};

export const assertOperatorActivatedState = async ({
  lucid,
  contracts,
  activeNodeUnit,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  activeNodeUnit: string;
  operatorKeyHash: string;
}) => {
  const activeNodeUtxosAfterActivate = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeNodeUnit,
  );
  expect(activeNodeUtxosAfterActivate.length).toBeGreaterThan(0);
  const activeNodeDatum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(activeNodeUtxosAfterActivate[0]),
  );
  expect(activeNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });

  const registeredNodeUtxosAfterActivate = await fetchRegisteredOperatorNodes(
    lucid,
    contracts,
  );
  expect(registeredNodeUtxosAfterActivate.length).toEqual(0);
};

export const fetchRegisteredOperatorNodes = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
): Promise<readonly UTxO[]> => {
  const utxos = await lucid.utxosAt(
    contracts.registeredOperators.spendingScriptAddress,
  );
  return utxos.filter((utxo) =>
    Object.keys(utxo.assets).some((unit) => {
      if (!unit.startsWith(contracts.registeredOperators.policyId)) {
        return false;
      }
      const assetName = unit.slice(
        contracts.registeredOperators.policyId.length,
      );
      return assetName.startsWith(
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
      );
    }),
  );
};

export const advanceEmulatorPastRegistrationDelay = (
  emulator: Emulator,
): void => {
  emulator.awaitSlot(180);
};

/**
 * Activates the fixture operator, then registers a second operator behind it.
 * With the active set occupied, the second registration has no immediate
 * activation exception, so its activation is gated by its activation time.
 */
export const registerSecondOperatorBehindActiveFirst = async (
  fixture: Awaited<ReturnType<typeof initOperatorLifecycleFixture>>,
) => {
  const { emulator, lucid, referenceScriptsLucid, contracts } = fixture;
  await runWithoutFollower(
    registerOperatorProgram(
      lucid,
      contracts,
      EMULATOR_REQUIRED_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );
  advanceEmulatorPastRegistrationDelay(emulator);
  await runWithoutFollower(
    activateOperatorProgram(
      lucid,
      contracts,
      EMULATOR_REQUIRED_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );

  const second = generateEmulatorAccount({ lovelace: 0n });
  const funding = await lucid
    .newTx()
    .pay.ToAddress(second.address, { lovelace: 4_000_000_000n })
    .complete({ localUPLCEval: true });
  await lucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const secondLucid = await Lucid(emulator, "Custom");
  secondLucid.selectWallet.fromSeed(second.seedPhrase);
  const secondKeyHash = paymentCredentialOf(second.address).hash;
  await runWithoutFollower(
    registerOperatorProgram(
      secondLucid,
      contracts,
      EMULATOR_REQUIRED_BOND_LOVELACE,
      referenceScriptsLucid,
    ),
  );

  const registeredNodes = await Promise.all(
    (await fetchRegisteredOperatorNodes(lucid, contracts)).map((utxo) =>
      Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
    ),
  );
  const registration = registeredNodes.find(
    (node) =>
      node.key !== "Empty" &&
      Data.castFrom(node.data, SDK.RegisteredOperatorDatum).operator ===
        secondKeyHash,
  );
  const activationTime =
    registration === undefined
      ? undefined
      : SDK.registeredNodeKeyToPosixTime(registration.key);
  if (activationTime === undefined) {
    throw new Error("Missing the second operator's registration");
  }
  expect(BigInt(emulator.now())).toBeLessThan(activationTime);

  return {
    secondLucid,
    secondKeyHash,
    activationTime,
    secondActiveNodeUnit: toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + secondKeyHash,
    ),
  };
};

export const describePosixTime = (posixMs: bigint): string =>
  `${new Date(Number(posixMs)).toISOString()} (${posixMs.toString()})`;
