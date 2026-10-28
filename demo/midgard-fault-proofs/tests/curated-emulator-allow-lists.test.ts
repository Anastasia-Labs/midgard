import {
  type AllowList,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import {
  generateSeedPhrase,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { runWithoutFollower } from "midgard-node/tests/helpers/intent-journal";
import {
  OperatorRegistrationRefusal,
  registerAndActivateOperatorProgram,
  registerOperatorProgram,
} from "midgard-node/transactions/register-active-operator";
import { describe, expect, it } from "vitest";

import { submitResolvedInit } from "../src/submit-init.js";
import {
  CURATED_EMULATOR_SEEDS,
  curatedKeyHash,
} from "./support/curated-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildProvedDoubleSpendFixture } from "./support/submit-init-emulator-fixtures.build-proved-double-spend-fixture.js";
import { buildProvedFixtureDeploymentContext } from "./support/submit-init-emulator-fixtures.deployment-context.js";
import { EMULATOR_HEADER_CLOCK_HEADROOM_MS } from "./support/submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  expectRemovedFraudProofState,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.setup-fraudulent-block.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  emulatorAccountFromSeed,
  makeHeader,
  network,
  onboardEmulatorOperator,
  publishFaultProofWitnessReferenceScripts,
  submitSetupTx,
  withPlaceholderDeposit,
} from "./support/submit-init-emulator-shared.js";

type DeploymentContext = Awaited<
  ReturnType<typeof buildProvedFixtureDeploymentContext>
>;

/** A wallet funded from the context's funder, at its base address. */
const fundedWallet = async (context: DeploymentContext, seedPhrase: string) => {
  const address = emulatorAccountFromSeed(0n, seedPhrase).address;
  const funding = await context.funderLucid
    .newTx()
    .pay.ToAddress(address, { lovelace: 3_000_000_000n })
    .complete();
  await context.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(context.emulator, "Custom");
  lucid.selectWallet.fromSeed(seedPhrase);
  return lucid;
};

describe("preprod-emulator-curated operator allow-list and catalogue", () => {
  it("selects the curated profile, whose only list admits exactly the seeded operator", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.name).toBe("preprod-emulator-curated");
    expect(SELECTED_DEPLOYMENT_PROFILE.allow_lists).toEqual({
      operators: [curatedKeyHash(CURATED_EMULATOR_SEEDS.operator)],
    });
  });

  it("registers only a listed operator, and refuses a thread claimed with another category's catalogue membership proof", async () => {
    const context = await buildProvedFixtureDeploymentContext(0n, false, {
      funder: CURATED_EMULATOR_SEEDS.operator,
      prover: generateSeedPhrase(),
    });
    const { realBlueprint, emulator, funderLucid, contracts, catalogue } =
      context;
    const witnessReferenceScripts =
      await publishFaultProofWitnessReferenceScripts({
        lucid: context.proverLucid,
        realBlueprint,
        computationThreadMintingScript:
          contracts.computationThread.mintingScript,
        fraudProofMintingScript: contracts.fraudProof.mintingScript,
      });
    const headerStartTime =
      alignUnixTimeToEmulatorSlotBoundary(
        funderLucid,
        emulator.now() + 120_000,
      ) - 1;
    const operatorKeyHash = curatedKeyHash(CURATED_EMULATOR_SEEDS.operator);
    expect(paymentCredentialOf(await funderLucid.wallet().address()).hash).toBe(
      operatorKeyHash,
    );
    const header = {
      ...withPlaceholderDeposit(makeHeader(operatorKeyHash, headerStartTime)),
      endTime:
        BigInt(headerStartTime) + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
    };
    if (context.nonceUtxo === undefined)
      throw new Error("the deployment context minted no hub-oracle nonce");
    // Registration, positive: the setup registers and activates the header's
    // operator, the listed key, through the real registered-operators Register.
    const setup = await submitSetupTx({
      lucid: funderLucid,
      contracts,
      nonceUtxo: context.nonceUtxo,
      catalogue,
      header,
    });

    // Registration, negative: the same journey for a key the list omits,
    // signed by that key, is refused by the builder and, past it, by the
    // validator.
    const outsiderSeed = generateSeedPhrase();
    const outsiderLucid = await fundedWallet(context, outsiderSeed);
    const onboardOutsider = (operatorAllowList?: AllowList) =>
      onboardEmulatorOperator({
        lucid: outsiderLucid,
        contracts,
        operatorKeyHash: curatedKeyHash(outsiderSeed),
        operatorAllowList,
        // Reached only once the registration has landed.
        awaitActivation: () => {
          throw new Error("the unlisted operator's registration landed");
        },
      });
    // The helper reports the builder's refusal by its message.
    await expect(onboardOutsider()).rejects.toThrow(
      /not on the deployment's operators allow-list/u,
    );
    // The node's register and register-and-activate commands refuse the key
    // before any L1 read; the node keeps following the chain.
    for (const program of [
      registerOperatorProgram,
      registerAndActivateOperatorProgram,
    ]) {
      const outcome = await runWithoutFollower(
        Effect.either(program(outsiderLucid, contracts, 0n, outsiderLucid)),
      );
      expect(Either.isLeft(outcome) && outcome.left).toBeInstanceOf(
        OperatorRegistrationRefusal,
      );
    }
    await expectOnchainRefusal(() => onboardOutsider(null));

    const doubleSpend = catalogue.categories.doubleSpend;
    const initContracts = (firstStep: {
      readonly spendingScriptAddress: string;
      readonly spendingScriptHash: string;
    }) => ({
      steps: [firstStep] as const,
      computationThread: contracts.computationThread,
      hubOraclePolicyId: contracts.hubOracle.policyId,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    });
    const catalogueReference = {
      policyId: contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        contracts.fraudProofCatalogue.spendingScriptAddress,
      root: catalogue.root,
    };
    // Thread init, negative: the curated catalogue, like the public one,
    // holds every category. A family's thread opens only with that family's
    // own membership proof; the prover claims a catalogued family, with its
    // own category ID and first-step hash, under another category's proof.
    // An excluded category, having no proof of its own, is refused the same
    // way.
    expect(SELECTED_DEPLOYMENT_PROFILE.excluded_fraud_proof_categories).toEqual(
      [],
    );
    const claimed = contracts.fraudProofs.unusedRedeemer;
    expect(catalogue.categories.unusedRedeemer).toMatchObject({
      categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedRedeemer,
      scriptHash: claimed.spendingScriptHash,
    });
    await expectOnchainRefusal(() =>
      submitResolvedInit({
        lucid: context.proverLucid,
        blueprint: realBlueprint,
        network,
        label: "curated init",
        contracts: initContracts(claimed),
        category: {
          categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedRedeemer,
          scriptHash: claimed.spendingScriptHash,
          membershipProofCbor: doubleSpend.membershipProofCbor,
        },
        catalogue: catalogueReference,
        signer: context.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    );
  }, 600_000);

  it("initialises a thread for a key no profile names, and its double-spend proof convicts", async () => {
    const fixture = await buildProvedDoubleSpendFixture({
      seeds: {
        funder: CURATED_EMULATOR_SEEDS.operator,
        prover: generateSeedPhrase(),
      },
    });
    expect(
      Object.values(SELECTED_DEPLOYMENT_PROFILE.allow_lists).flat(),
    ).not.toContain(fixture.proverPaymentKeyHash);
    const removal = await submitRemovalForFixture(fixture);
    expect(removal.transactions.map((tx) => tx.removedHeaderHash)).toEqual([
      fixture.headerHash,
    ]);
    await expectRemovedFraudProofState(fixture);
  }, 600_000);
});
