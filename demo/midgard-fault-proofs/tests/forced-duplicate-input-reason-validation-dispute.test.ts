/**
 * The forced leaf's rejection reason is bound on-chain to the replayed
 * rejection through its code (`forced_verdict_matches` in
 * `validation-claim-v1.ak`). A leaf whose reason names another code makes the
 * committed claim endpoints invalid, and source verification sends the
 * dispute straight to the award (`validation-trace/source-v1.ak`).
 *
 * Both polarities run against the compiled validators for one forced order
 * that spends the same out-ref twice:
 *
 * - positive: a block committing the reason the writer used to fall back to
 *   (`ValueNotPreserved`) is disproved at source verification, the award
 *   mints the fraud-proof token, and the block is removed;
 * - negative: a block committing the reason the writer now derives
 *   (`DuplicateInput` at the two real positions) cannot be disproved; an
 *   award attempt is refused on-chain, no fraud-proof token exists, and the
 *   block stays.
 *
 * The two blocks are built by the same pipeline and differ only in the leaf's
 * reason, and the source validator admits the award output exactly when the
 * committed endpoints are invalid; so the positive's accepted award and the
 * negative's refused one are the reason-binding conjunct deciding. The
 * off-chain twin of that predicate is checked on both claims as well.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import type { OperatorVerdict } from "@al-ft/midgard-sdk";
import { toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({ forceAwardRoute: false }));
vi.mock("../src/validation-dispute/claim-endpoints.js", async (load) => {
  const actual =
    await load<typeof import("../src/validation-dispute/claim-endpoints.js")>();
  return {
    ...actual,
    // The negative drives the award route the off-chain mirror would never
    // pick for a consistent claim, so the validator itself has to refuse it.
    committedValidationClaimEndpointsAndSourceAreValid: (
      ...args: Parameters<
        typeof actual.committedValidationClaimEndpointsAndSourceAreValid
      >
    ) =>
      hooks.forceAwardRoute
        ? false
        : actual.committedValidationClaimEndpointsAndSourceAreValid(...args),
  };
});

import {
  submitRemoveFraudulentBlock,
  submitValidationDisputeAward,
  submitValidationDisputeOpen,
  submitValidationDisputeVerifySource,
} from "../src/index.js";
import { committedValidationClaimEndpointsAndSourceAreValid } from "../src/validation-dispute/claim-endpoints.js";
import { createReferenceScriptPublisher } from "./support/emulator/reference-script-publisher.js";
import {
  buildDuplicateInputForcedReasonFixture,
  nodeVerdictForDuplicateInputForcedOrder,
} from "./support/emulator/validation-dispute-fixtures.build-duplicate-input-forced-reason-fixture.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  createRealL1TargetLucids,
  createValidationDisputeParties,
  expectOnchainRefusal,
  expectSingleUtxoWithUnit,
  network,
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  stageAuthenticatedValidationDisputePublication,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/** The reason the node committed for every duplicated-input order before. */
const OLD_VERDICT: OperatorVerdict = {
  ForcedTxInvalid: { reason: "ValueNotPreserved" },
};

/** Spend item 0 and spend item 1 are the same out-ref. */
const EXACT_VERDICT: OperatorVerdict = {
  ForcedTxInvalid: {
    reason: {
      DuplicateInput: {
        first_field_index: 0n,
        first_item_index: 0n,
        second_field_index: 0n,
        second_item_index: 1n,
      },
    },
  },
};

/** Stages one block committing `verdict` and opens a dispute against it. */
const openDisputeOverForcedVerdict = async (verdict: OperatorVerdict) => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const {
    emulator,
    operator,
    challenger,
    operatorLucid,
    challengerLucid,
    operatorSigner,
    challengerSigner,
    validityRange,
  } = await createValidationDisputeParties();
  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(operatorLucid, emulator.now());
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
        realValidationTraceDispute: true,
        alwaysFraudProofCatalogue: true,
      },
    )),
    referenceScriptAuth,
    referenceScriptPublisher,
  };
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: challengerLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScripts({
      lucid: challengerLucid,
      realBlueprint,
      computationThreadMintingScript: contracts.computationThread.mintingScript,
      fraudProofMintingScript: contracts.fraudProof.mintingScript,
    });
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await buildDuplicateInputForcedReasonFixture({
    operatorVkey: operatorSigner.paymentKeyHash,
    now: headerStartTime,
    verdict,
  });
  emulator.awaitSlot(
    Math.max(0, Math.ceil((headerStartTime - 120_000 - emulator.now()) / 1000)),
  );
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const { validationDisputePublication, validationDisputeControlPublications } =
    await stageAuthenticatedValidationDisputePublication({
      emulator,
      operatorLucid,
      operatorSeedPhrase: challenger.seedPhrase,
      contracts,
      authPolicy: referenceScriptAuth,
      publisher: referenceScriptPublisher,
      runStage: runEmulatorLifecycleStage,
    });
  const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    validationDisputePublication,
  });
  const initResult = await runEmulatorLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts,
      awaitConfirmation: true,
    }),
  );
  const { targetOperatorLucid, targetChallengerLucid } =
    await createRealL1TargetLucids({
      emulator,
      sourceLucid: challengerLucid,
      operatorSeedPhrase: operator.seedPhrase,
      challengerSeedPhrase: challenger.seedPhrase,
    });
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    targetChallengerLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const openResult = await runEmulatorLifecycleStage("open", () =>
    submitValidationDisputeOpen({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      claim: fixture.claim,
      challengerDescriptor: fixture.challengerDescriptor,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  const [blockTxHash, blockOutputIndex] =
    setup.fraudulentBlockOutRef.split("#");
  const blockUtxos = () =>
    targetChallengerLucid.utxosByOutRef([
      { txHash: blockTxHash!, outputIndex: Number(blockOutputIndex) },
    ]);
  const fraudProofUtxos = () =>
    targetChallengerLucid.utxosAtWithUnit(
      contracts.fraudProof.spendingScriptAddress,
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
  const verifySource = () =>
    runEmulatorLifecycleStage("source", () =>
      submitValidationDisputeVerifySource({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: openResult.nextThreadOutRef,
        sourceReferenceScriptUtxo:
          validationDisputeControlPublications.source.utxo,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
    );
  const awardAndRemove = async (awardThreadOutRef: string) => {
    const awardResult = await runEmulatorLifecycleStage("award", () =>
      submitValidationDisputeAward({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: awardThreadOutRef,
        awardReferenceScriptUtxo:
          validationDisputeControlPublications.award.utxo,
        witnessReferenceScripts,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
    );
    const removal = await publishRemovalReferenceScripts({
      lucid: targetOperatorLucid,
      contracts,
    });
    const removeNow = BigInt(emulator.now());
    await runEmulatorLifecycleStage("remove-fraudulent-block", () =>
      submitRemoveFraudulentBlock({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(contracts, catalogue, {
          validationDisputePublication,
          removalReferenceScripts: removal.published,
        }),
        network,
        signer: challengerSigner,
        fraudCategory: "validationTraceDispute",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    return awardResult;
  };
  const endpointsAreValid = () =>
    committedValidationClaimEndpointsAndSourceAreValid(
      fixture.header,
      fixture.claim,
    );
  return {
    blockUtxos,
    fraudProofUtxos,
    verifySource,
    awardAndRemove,
    endpointsAreValid,
  };
};

describe("forced duplicated-input order: the committed rejection reason", () => {
  it("disproves the fallback reason: award, fraud-proof token, block removed", async () => {
    // The writer no longer commits the fallback for this order.
    expect(await nodeVerdictForDuplicateInputForcedOrder()).not.toStrictEqual(
      OLD_VERDICT,
    );
    const dispute = await openDisputeOverForcedVerdict(OLD_VERDICT);
    expect(dispute.endpointsAreValid()).toBe(false);
    expect(await dispute.blockUtxos()).toHaveLength(1);
    const source = await dispute.verifySource();
    expect(source.outcome).toBe("award");
    const award = await dispute.awardAndRemove(source.nextThreadOutRef);
    expect(await dispute.fraudProofUtxos()).toHaveLength(1);
    expect(
      (await dispute.fraudProofUtxos())[0]!.assets[award.fraudProofUnit],
    ).toBe(1n);
    expect(await dispute.blockUtxos()).toHaveLength(0);
  }, 600_000);

  it("keeps the exact reason: the award is refused, no token, the block stays", async () => {
    const verdict = await nodeVerdictForDuplicateInputForcedOrder();
    expect(verdict).toStrictEqual(EXACT_VERDICT);
    const dispute = await openDisputeOverForcedVerdict(verdict);
    expect(dispute.endpointsAreValid()).toBe(true);
    hooks.forceAwardRoute = true;
    try {
      await expectOnchainRefusal(() => dispute.verifySource());
    } finally {
      hooks.forceAwardRoute = false;
    }
    expect(await dispute.fraudProofUtxos()).toHaveLength(0);
    expect(await dispute.blockUtxos()).toHaveLength(1);
  }, 600_000);
});
