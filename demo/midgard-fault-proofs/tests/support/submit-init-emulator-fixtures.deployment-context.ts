import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";

import { resolveProverSigner } from "../../src/index.js";
import { createReferenceScriptPublisher } from "./emulator/reference-script-publisher.js";
import {
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  EMULATOR_PROTOCOL_PARAMETERS,
  fundedProverEmulatorAccount,
  network,
  publishOperatorLifecycleReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
} from "./submit-init-emulator-shared.js";

// This test-fixture refactor preserves the original setup statement order.
export const buildProvedFixtureDeploymentContext = async (
  headerMinimumFee: bigint,
) => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
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

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(funderLucid, emulator.now());
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        realMinFee: headerMinimumFee > 0n,
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
      },
    )),
    referenceScriptAuth,
    referenceScriptPublisher,
  };
  // Operator registration and activation source their four directory
  // validators from published reference scripts. Published from the prover
  // wallet before the header clock is sampled so the funder's nonce UTxO
  // survives and the whole fixture timeline shifts uniformly.
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: proverLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  return {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  };
};
