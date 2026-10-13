import {
  type AllowList,
  AllowListRefusal,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
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

import { resolveProverSigner } from "../src/index.js";
import { submitResolvedInit } from "../src/submit-init.js";
import {
  CURATED_EMULATOR_SEEDS,
  curatedKeyHash,
} from "./support/curated-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildProvedFixtureDeploymentContext } from "./support/submit-init-emulator-fixtures.deployment-context.js";
import { EMULATOR_HEADER_CLOCK_HEADROOM_MS } from "./support/submit-init-emulator-fixtures.instrument-lucid-for-removal.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  emulatorAccountFromSeed,
  expectSingleUtxoWithUnit,
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

/** A prover signer whose enterprise address the funder pays. */
const fundedProver = async (context: DeploymentContext, seedPhrase: string) => {
  const signer = resolveProverSigner({ network, walletSeedPhrase: seedPhrase });
  const funding = await context.funderLucid
    .newTx()
    .pay.ToAddress(signer.address, { lovelace: 3_000_000_000n })
    .complete();
  await context.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const lucid = await Lucid(context.emulator, "Custom");
  signer.selectWallet(lucid);
  return { signer, lucid };
};

describe("preprod-emulator-curated allow-lists and catalogue", () => {
  it("selects the curated profile, whose lists admit exactly the seeded keys", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.name).toBe("preprod-emulator-curated");
    expect(SELECTED_DEPLOYMENT_PROFILE.allow_lists).toEqual({
      operators: [curatedKeyHash(CURATED_EMULATOR_SEEDS.operator)],
      fraud_provers: [curatedKeyHash(CURATED_EMULATOR_SEEDS.fraudProver)],
      da_challengers: [curatedKeyHash(CURATED_EMULATOR_SEEDS.daChallenger)],
    });
  });

  it("registers only a listed operator, and initialises a thread only for a listed prover and a catalogued family", async () => {
    const context = await buildProvedFixtureDeploymentContext(0n, false, {
      funder: CURATED_EMULATOR_SEEDS.operator,
      prover: CURATED_EMULATOR_SEEDS.fraudProver,
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
    const init = (
      prover: Awaited<ReturnType<typeof fundedProver>>,
      category: typeof doubleSpend,
      firstStep: Parameters<typeof initContracts>[0],
      proverAllowList?: AllowList,
    ) =>
      submitResolvedInit({
        proverAllowList,
        lucid: prover.lucid,
        blueprint: realBlueprint,
        network,
        label: "curated init",
        contracts: initContracts(firstStep),
        category,
        catalogue: catalogueReference,
        signer: prover.signer,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts,
        awaitConfirmation: true,
      });
    const doubleSpendFirstStep =
      contracts.fraudProofContracts.doubleSpend.steps[0]!;

    // Thread init, negative: a prover the list omits, refused before any
    // read and, past that check, by the validator.
    const outsiderProver = await fundedProver(context, generateSeedPhrase());
    await expect(
      init(outsiderProver, doubleSpend, doubleSpendFirstStep),
    ).rejects.toBeInstanceOf(AllowListRefusal);
    await expectOnchainRefusal(() =>
      init(outsiderProver, doubleSpend, doubleSpendFirstStep, null),
    );

    // Thread init, negative: a family the curated catalogue leaves out has no
    // membership proof; the listed prover claims it with another category's.
    expect(
      SELECTED_DEPLOYMENT_PROFILE.excluded_fraud_proof_categories,
    ).toContain("unusedRedeemer");
    expect(catalogue.categories.unusedRedeemer).toBeUndefined();
    const listedProver = {
      signer: context.proverSigner,
      lucid: context.proverLucid,
    };
    expect(listedProver.signer.paymentKeyHash).toBe(
      curatedKeyHash(CURATED_EMULATOR_SEEDS.fraudProver),
    );
    const excluded = contracts.fraudProofs.unusedRedeemer;
    await expectOnchainRefusal(() =>
      init(
        listedProver,
        {
          categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.unusedRedeemer,
          scriptHash: excluded.spendingScriptHash,
          membershipProofCbor: doubleSpend.membershipProofCbor,
        },
        excluded,
      ),
    );

    // Thread init, positive: the listed prover and a catalogued family.
    const initialised = await init(
      listedProver,
      doubleSpend,
      doubleSpendFirstStep,
    );
    const firstStep = await expectSingleUtxoWithUnit(
      context.proverLucid,
      initialised.firstStepAddress,
      initialised.computationThreadUnit,
    );
    expect(
      Data.from(firstStep.datum!, FraudProofComputationThreadStepDatum),
    ).toEqual({
      fraud_prover: curatedKeyHash(CURATED_EMULATOR_SEEDS.fraudProver),
      data: null,
    });
  }, 600_000);
});
