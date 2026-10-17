/**
 * `mint-authorization` direction-A emulator lifecycle: real faults succeed.
 *
 * Direction A convicts an operator-ACCEPTED committed transaction that mints
 * under a policy whose native script is ABSENT from the machine-consulted
 * source surface. Two journeys:
 *
 * - empty fields 1 and 0, end to end — init → step-01 bind → step-02 claim
 *   → step-03 WitnessAbsence (empty field 6) → step-04 AdvanceComplete
 *   (empty field 1, then empty field 0) → step-05 permanent fraud-proof
 *   token → fraudulent-block removal;
 * - one reference input or one spent input, through the step-04 ResolveNext
 *   self-loop against a hand-built pre-state ledger trie, then
 *   AdvanceComplete → step-05. A resolved output carrying the claimed
 *   policy's script refuses the claim whichever input field it sits in.
 *
 * Each journey commits its own block on a fresh harness: the thread token's
 * asset name is `categoryId ‖ header_hash`, so one committed header carries
 * exactly one thread of this family. Lives in its own file because
 * `@lucid-evolution/uplc` never reclaims wasm linear memory and vitest
 * isolates per FILE.
 */
import {
  decodeMidgardLedgerOutputCommitment,
  outRefLabel,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import { MINT_AUTHORIZATION_CATEGORY_LABEL } from "../src/mint-authorization/contracts.js";
import {
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03WitnessAbsence,
  submitMintAuthorizationStep04AdvanceComplete,
  submitMintAuthorizationStep04ResolveNext,
  submitMintAuthorizationStep05,
} from "../src/mint-authorization/index.js";
import { submitResolvedInit } from "../src/submit-init.js";
import {
  buildMintAuthorizationLedgerFixture,
  buildMintAuthorizationSubject,
  makeMintAuthorizationEmulatorHarness,
  mintItemCborV1,
  publishMintAuthorizationReferenceScripts,
  referenceInputItemCbor,
  setupMintAuthorizationScenario,
  smallMintItemCbors,
} from "./support/mint-authorization-emulator.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const ACCUSED_POLICY_INDEX = 0n;
const DIRECTION_SCRIPT_ABSENT = SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT;

describe("mint-authorization direction-A emulator lifecycle", () => {
  it("proves an absent mint policy end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment", async () => {
    const harness = await makeMintAuthorizationEmulatorHarness();
    const {
      realBlueprint,
      funderLucid,
      proverLucid,
      proverSigner,
      catalogue,
      family,
      category,
    } = harness;

    const subject = buildMintAuthorizationSubject({
      mintItemCbors: smallMintItemCbors(),
    });
    const scenario = await setupMintAuthorizationScenario({
      harness,
      subject,
    });
    const { block, setup } = scenario;
    if (block.txInclusion === null) {
      throw new Error("normal mint-authorization fixture has no inclusion");
    }

    // Published only after setup: the harness's one-shot nonce is the funder's
    // first UTxO, so nothing may spend from the funder wallet before setup.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts: harness.contracts,
      });
    const [step01Ref, step02Ref, step03Ref, step04Ref, step05Ref] =
      await publishMintAuthorizationReferenceScripts({
        lucid: funderLucid,
        contracts: family,
      });

    const initResult = await submitResolvedInit({
      label: MINT_AUTHORIZATION_CATEGORY_LABEL,
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: family,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(initResult.computationThreadAssetName).toBe(
      `${category.categoryId}${setup.headerHash}`,
    );

    const step01 = await submitMintAuthorizationStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: family,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: initResult.nextThreadOutRef,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: block.txInclusion,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });
    expect(step01.step02State.bad_tx_id).toBe(block.nativeTxId);

    const step02 = await submitMintAuthorizationStep02({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step01.nextThreadOutRef,
      reconstruction: block.reconstruction,
      policyIndex: ACCUSED_POLICY_INDEX,
      direction: DIRECTION_SCRIPT_ABSENT,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      mintItemCbors: subject.mintItemCbors,
      referenceScriptUtxo: step02Ref,
    });
    expect(step02.step03State.direction).toBe(DIRECTION_SCRIPT_ABSENT);

    const step03 = await submitMintAuthorizationStep03WitnessAbsence({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      witnessSet: subject.witnessSetCompact,
      scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
      referenceScriptUtxo: step03Ref,
    });

    const advance = (threadOutRef: string) =>
      submitMintAuthorizationStep04AdvanceComplete({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        referenceInputsItemCbors: subject.referenceInputItemCbors,
        spendInputsItemCbors: subject.spendInputItemCbors,
        referenceScriptUtxo: step04Ref,
      });
    const step04References = await advance(step03.nextThreadOutRef);
    expect(step04References.nextScan).toStrictEqual({
      fieldIndex: 0n,
      cursor: 0n,
    });
    expect(step04References.nextStepAddress).toBe(
      family.steps[3].spendingScriptAddress,
    );
    const step04 = await advance(step04References.nextThreadOutRef);
    expect(step04.nextScan).toBeNull();

    const step05 = await submitMintAuthorizationStep05({
      lucid: proverLucid,
      contracts: family,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: step04.nextThreadOutRef,
      referenceScriptUtxo: step05Ref,
      witnessReferenceScripts: setup.witnessReferenceScripts,
    });

    // The permanent token is minted and the thread NFT is burned.
    for (const step of family.steps) {
      await expect(
        proverLucid.utxosAtWithUnit(
          step.spendingScriptAddress,
          initResult.computationThreadUnit,
        ),
      ).resolves.toHaveLength(0);
    }
    const [fraudProofUtxo] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (fraudProofUtxo === undefined) {
      throw new Error("step-05 did not mint the fraud-proof token");
    }
    expect(outRefLabel(fraudProofUtxo)).toBe(step05.fraudProofOutRef);
    expect(fraudProofUtxo.assets[step05.fraudProofUnit]).toBe(1n);
    expect(
      Data.from(fraudProofUtxo.datum!, SDK.FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: proverSigner.paymentKeyHash });

    // ——— Removal leg: the minted token takes the fraudulent commitment off
    // the queue; the state-queue node NFT burns and the operator is slashed
    // while the fraud-proof token is retained as permanent evidence.
    const removalDeploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      catalogue,
      { removalReferenceScripts: removalReferenceScriptPublications.published },
    );
    const removeNow = BigInt(harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo: removalDeploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "mintAuthorization",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
      validTo: removeNow + 300_000n,
    });
    expect(removal.fraudCategory).toBe("mintAuthorization");
    expect(removal.fraudCategoryId).toBe(category.categoryId);
    expect(removal.transactions).toHaveLength(1);
    expect(removal.transactions[0]!.slashingApproach).toBe(
      "SlashActiveOperator",
    );

    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalRootUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueRootUnit,
    );
    if (finalRootUtxo === undefined) {
      throw new Error("Removal did not preserve the state-queue root");
    }
    const finalRoot = await Effect.runPromise(
      SDK.utxoToStateQueueUTxO(
        finalRootUtxo,
        harness.contracts.stateQueue.policyId,
      ),
    );
    expect(finalRoot.datum.next).toBe("Empty");

    await expect(
      proverLucid.utxosAtWithUnit(
        harness.contracts.activeOperators.spendingScriptAddress,
        setup.activeOperatorNodeUnit,
      ),
    ).resolves.toHaveLength(0);
    const [finalSchedulerUtxo] = await proverLucid.utxosAtWithUnit(
      harness.contracts.scheduler.spendingScriptAddress,
      toUnit(harness.contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    if (finalSchedulerUtxo === undefined) {
      throw new Error("Removal did not preserve the scheduler");
    }
    expect(Data.from(finalSchedulerUtxo.datum!, SDK.SchedulerDatum)).toBe(
      "NoActiveOperators",
    );

    // The fraud-proof token survives removal untouched at the same out-ref.
    const [retainedFraudProof] = await proverLucid.utxosAtWithUnit(
      step05.fraudProofAddress,
      step05.fraudProofUnit,
    );
    if (retainedFraudProof === undefined) {
      throw new Error("Removal burned the permanent fraud-proof token");
    }
    expect(outRefLabel(retainedFraudProof)).toBe(step05.fraudProofOutRef);
    expect(retainedFraudProof.assets[step05.fraudProofUnit]).toBe(1n);

    // A second removal claim finds nothing left to remove.
    await expect(
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "mintAuthorization",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
      }),
    ).rejects.toThrow(/State queue does not contain block/);
  }, 600_000);

  it.each([
    { field: "reference", matching: false },
    { field: "reference", matching: true },
    { field: "spent", matching: false },
    { field: "spent", matching: true },
  ] as const)(
    "checks the final $field input source with matching policy $matching",
    async ({ field, matching }) => {
      const harness = await makeMintAuthorizationEmulatorHarness();
      const {
        realBlueprint,
        funderLucid,
        proverLucid,
        proverSigner,
        catalogue,
        family,
        category,
      } = harness;

      // One committed reference or spent input; the pre-state ledger trie
      // holds its descriptor, whose reference script hashes to the accused
      // policy only in the matching case.
      const refTxId = "cd".repeat(32);
      const refOutputIndex = 2;
      const ledger = await buildMintAuthorizationLedgerFixture({
        txIdHex: refTxId,
        outputIndex: refOutputIndex,
      });
      const subject = buildMintAuthorizationSubject({
        mintItemCbors: matching
          ? [
              mintItemCborV1({
                policyId: decodeMidgardLedgerOutputCommitment(
                  Buffer.from(ledger.descriptorCbor, "hex"),
                ).referenceScriptHash,
                assetName: Buffer.from("beef", "hex"),
              }),
            ]
          : smallMintItemCbors(),
        [field === "reference"
          ? "referenceInputItemCbors"
          : "spendInputItemCbors"]: [
          referenceInputItemCbor({
            txIdHex: refTxId,
            outputIndex: refOutputIndex,
          }),
        ],
      });
      const scenario = await setupMintAuthorizationScenario({
        harness,
        subject,
        priorLedgerRoot: ledger.rootHex,
      });
      const { block, setup } = scenario;
      if (block.txInclusion === null) {
        throw new Error("normal mint-authorization fixture has no inclusion");
      }
      const [step01Ref, step02Ref, step03Ref, step04Ref, step05Ref] =
        await publishMintAuthorizationReferenceScripts({
          lucid: funderLucid,
          contracts: family,
        });

      const initResult = await submitResolvedInit({
        label: MINT_AUTHORIZATION_CATEGORY_LABEL,
        lucid: proverLucid,
        blueprint: realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: catalogue.root,
        },
        signer: proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      const step01 = await submitMintAuthorizationStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        contracts: family,
        categoryId: category.categoryId,
        network,
        signer: proverSigner,
        threadOutRef: initResult.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      const step02 = await submitMintAuthorizationStep02({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        reconstruction: block.reconstruction,
        policyIndex: ACCUSED_POLICY_INDEX,
        direction: DIRECTION_SCRIPT_ABSENT,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        mintItemCbors: subject.mintItemCbors,
        referenceScriptUtxo: step02Ref,
      });
      const step03 = await submitMintAuthorizationStep03WitnessAbsence({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        witnessSet: subject.witnessSetCompact,
        scriptTxWitsItemCbors: subject.scriptWitnessItemCbors,
        referenceScriptUtxo: step03Ref,
      });
      const step04 = {
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        referenceInputsItemCbors: subject.referenceInputItemCbors,
        spendInputsItemCbors: subject.spendInputItemCbors,
        referenceScriptUtxo: step04Ref,
      };
      const advanceFrom = (threadOutRef: string) =>
        submitMintAuthorizationStep04AdvanceComplete({
          ...step04,
          threadOutRef,
        });
      // A spent input is scanned once the reference inputs are closed.
      const scanOutRef =
        field === "reference"
          ? step03.nextThreadOutRef
          : (await advanceFrom(step03.nextThreadOutRef)).nextThreadOutRef;
      const scannedField = field === "reference" ? 1n : 0n;
      const resolve = () =>
        submitMintAuthorizationStep04ResolveNext({
          ...step04,
          threadOutRef: scanOutRef,
          trie: ledger.trie,
          descriptorCborHex: ledger.descriptorCbor,
        });
      if (matching) {
        await expect(resolve()).rejects.toThrow(
          `the resolved field-${scannedField.toString()} input at cursor 0 carries a reference script hashing to the claimed policy`,
        );
        return;
      }
      const resolveNext = await resolve();
      expect(resolveNext.nextScan).toStrictEqual({
        fieldIndex: scannedField,
        cursor: 1n,
      });
      expect(resolveNext.nextStepAddress).toBe(
        family.steps[3].spendingScriptAddress,
      );
      let advance = await advanceFrom(resolveNext.nextThreadOutRef);
      if (field === "reference") {
        expect(advance.nextScan).toStrictEqual({ fieldIndex: 0n, cursor: 0n });
        advance = await advanceFrom(advance.nextThreadOutRef);
      }
      expect(advance.nextScan).toBeNull();
      const step05 = await submitMintAuthorizationStep05({
        lucid: proverLucid,
        contracts: family,
        categoryId: category.categoryId,
        signer: proverSigner,
        threadOutRef: advance.nextThreadOutRef,
        referenceScriptUtxo: step05Ref,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      const [fraudProofUtxo] = await proverLucid.utxosAtWithUnit(
        step05.fraudProofAddress,
        step05.fraudProofUnit,
      );
      if (fraudProofUtxo === undefined) {
        throw new Error("step-05 did not mint the fraud-proof token");
      }
      expect(fraudProofUtxo.assets[step05.fraudProofUnit]).toBe(1n);
    },
    600_000,
  );
});
