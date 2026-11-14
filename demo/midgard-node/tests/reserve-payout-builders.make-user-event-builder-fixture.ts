import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  makeSeededScriptAccount,
  registerZeroRewardScript,
} from "./reserve-payout-builders.make-reserve-payout-builder-fixture.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  findReferenceScriptUtxoBefore,
  findUtxoWithUnit,
  loadRealContracts,
} from "./reserve-payout-builders.submit-with-wallet.js";

export const makeUserEventBuilderFixture = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const referenceHosts = Array.from({ length: 24 }, () =>
    generateEmulatorAccount({
      lovelace: 2_000_000n,
    }),
  );
  const beneficiary = generateEmulatorAccount({
    lovelace: 2_000_000n,
  });
  const contracts = await loadRealContracts({
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  const hubUnit = toUnit(
    contracts.hubOracle.policyId,
    SDK.HUB_ORACLE_ASSET_NAME,
  );
  const hubDatum = await Effect.runPromise(SDK.makeHubOracleDatum(contracts));
  const hubOracleAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(contracts.hubOracle.policyId),
  );
  const history = SDK.requireEventHistoryContracts(contracts);
  const emptyRoot = Data.to(
    {
      position: "Root",
      next: null,
      protected_until: 0n,
      payload: "RootContent",
    },
    SDK.EventHistoryNode,
  );
  const emulator = new Emulator(
    [
      makeSeededScriptAccount({
        address: contracts.deposit.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.deposit.policyId]: 1n },
        inlineDatum: emptyRoot,
      }),
      makeSeededScriptAccount({
        address: contracts.withdrawal.spendingScriptAddress,
        assets: { lovelace: 3_000_000n, [contracts.withdrawal.policyId]: 1n },
        inlineDatum: emptyRoot,
      }),
      operator,
      ...referenceHosts,
      beneficiary,
      ...referenceHosts.flatMap((host) => [
        makeSeededScriptAccount({
          address: host.address,
          assets: { lovelace: 3_000_000n },
          scriptRef: contracts.deposit.mintingScript,
        }),
        makeSeededScriptAccount({
          address: host.address,
          assets: { lovelace: 3_000_000n },
          scriptRef: contracts.withdrawal.mintingScript,
        }),
      ]),
      makeSeededScriptAccount({
        address: hubOracleAddress,
        assets: { lovelace: 3_000_000n, [hubUnit]: 1n },
        inlineDatum: Data.to(hubDatum, SDK.HubOracleDatum),
      }),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  registerZeroRewardScript(emulator, history.deposit.list.withdrawalScript);
  registerZeroRewardScript(emulator, history.withdrawal.list.withdrawalScript);
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

  const hubOracleRefInput = findUtxoWithUnit(
    await lucid.utxosAt(hubOracleAddress),
    hubUnit,
  );
  const referenceUtxos = (
    await Promise.all(referenceHosts.map((host) => lucid.utxosAt(host.address)))
  ).flat();
  return {
    beneficiary,
    contracts,
    depositMintingReference: findReferenceScriptUtxoBefore(
      referenceUtxos,
      contracts.deposit.mintingScript,
      hubOracleRefInput,
    ),
    hubOracleRefInput,
    lucid,
    withdrawalMintingReference: findReferenceScriptUtxoBefore(
      referenceUtxos,
      contracts.withdrawal.mintingScript,
      hubOracleRefInput,
    ),
  };
};
