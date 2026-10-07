import { outRefLabel } from "@al-ft/midgard-core";
import {
  FraudProofTokenDatum,
  MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitMissingNativeScriptUtxoCancel,
  submitMissingNativeScriptUtxoInit,
  submitMissingNativeScriptUtxoStep01,
  submitMissingNativeScriptUtxoStep05,
  submitMissingNativeScriptUtxoStep05StartGrammar,
  submitMissingNativeScriptUtxoStep06,
  submitMissingNativeScriptUtxoStep07,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  advanceMissingNativeScriptUtxoToFinal,
  setupMissingNativeScriptUtxoLifecycle,
  submitMissingNativeScriptUtxoFinalRaw,
} from "./support/missing-native-script-utxo-honest-state.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

describe("missing-native-script-utxo standalone emulator lifecycle", () => {
  it.each([
    { terminalPath: "direct", decoyWitnessCount: 0 },
    {
      terminalPath: "staged step-05→06→07",
      decoyWitnessCount: MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT + 1,
    },
  ] as const)(
    "authenticates predecessor material through the $terminalPath path, cancels and resumes, removes the header, and retains permanent evidence",
    async ({ terminalPath, decoyWitnessCount }) => {
      const scenario = await setupMissingNativeScriptUtxoLifecycle({
        decoyWitnessCount,
      });
      const {
        harness,
        refs,
        fixture,
        target,
        prepared,
        txInclusion,
        initParams,
      } = scenario;

      const cancelInit = await submitMissingNativeScriptUtxoInit(initParams);
      const cancelStep01 = await submitMissingNativeScriptUtxoStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${cancelInit.txHash}#${cancelInit.firstStepOutputIndex.toString()}`,
        stateQueueBlockOutRef: target.successorOutRef,
        txInclusion,
        prevUtxosRoot: prepared.prevUtxosRoot,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const cancelled = await submitMissingNativeScriptUtxoCancel({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: cancelStep01.nextThreadOutRef,
        referenceScriptUtxo: refs[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      expect(cancelled.cancelledStepIndex).toBe(1);

      const { step04 } = await advanceMissingNativeScriptUtxoToFinal(scenario);
      let fraudProofUnit: string;
      if (terminalPath === "direct") {
        const proof = await submitMissingNativeScriptUtxoStep05({
          lucid: harness.proverLucid,
          contracts: harness.family,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef: step04.nextThreadOutRef,
          nativeTxCompactCbor: prepared.nativeTxCompactCbor,
          witnessSet: fixture.witnessSet,
          scriptWitnessItems: fixture.scriptWitnessItems,
          referenceScriptUtxo: refs[4],
          witnessReferenceScripts: harness.witnessReferenceScripts,
        });
        fraudProofUnit = proof.fraudProofUnit;
      } else {
        expect(fixture.scriptWitnessItems).toHaveLength(
          MISSING_NATIVE_SCRIPT_TX_DIRECT_WITNESS_LIMIT + 1,
        );
        const grammarStart =
          await submitMissingNativeScriptUtxoStep05StartGrammar({
            lucid: harness.proverLucid,
            contracts: harness.family,
            categoryId: harness.category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: step04.nextThreadOutRef,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            witnessSet: fixture.witnessSet,
            scriptTxWitsItems: fixture.scriptWitnessItems,
            referenceScriptUtxo: refs[4],
          });
        let stagedThreadOutRef = grammarStart.nextThreadOutRef;
        let grammarCheckpointBytes = Buffer.from(
          grammarStart.checkpointBytes,
          "hex",
        );
        let semanticCheckpointBytes: Uint8Array | undefined;
        for (let resume = 0; resume < 4; resume += 1) {
          const grammar = await submitMissingNativeScriptUtxoStep06({
            lucid: harness.proverLucid,
            contracts: harness.family,
            categoryId: harness.category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: stagedThreadOutRef,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            witnessSet: fixture.witnessSet,
            scriptTxWitsItems: fixture.scriptWitnessItems,
            grammarCheckpointBytes,
            referenceScriptUtxo: refs[5],
          });
          stagedThreadOutRef = grammar.nextThreadOutRef;
          if (grammar.action === "StartSemanticScan") {
            semanticCheckpointBytes = Buffer.from(
              grammar.checkpointBytes,
              "hex",
            );
            break;
          }
          grammarCheckpointBytes = Buffer.from(grammar.checkpointBytes, "hex");
        }
        if (semanticCheckpointBytes === undefined) {
          throw new Error("staged lifecycle did not enter semantic scanning");
        }
        let stagedFraudProofUnit: string | undefined;
        for (let resume = 0; resume < 4; resume += 1) {
          const semantic = await submitMissingNativeScriptUtxoStep07({
            lucid: harness.proverLucid,
            contracts: harness.family,
            categoryId: harness.category.categoryId,
            signer: harness.proverSigner,
            threadOutRef: stagedThreadOutRef,
            nativeTxCompactCbor: prepared.nativeTxCompactCbor,
            witnessSet: fixture.witnessSet,
            scriptTxWitsItems: fixture.scriptWitnessItems,
            semanticCheckpointBytes,
            referenceScriptUtxo: refs[6],
            witnessReferenceScripts: harness.witnessReferenceScripts,
          });
          if (semantic.fraudProofUnit !== undefined) {
            stagedFraudProofUnit = semantic.fraudProofUnit;
            break;
          }
          if (semantic.nextThreadOutRef === undefined) {
            throw new Error("staged semantic scan lost its computation thread");
          }
          stagedThreadOutRef = semantic.nextThreadOutRef;
          semanticCheckpointBytes = Buffer.from(
            semantic.checkpointBytes,
            "hex",
          );
        }
        if (stagedFraudProofUnit === undefined) {
          throw new Error("staged lifecycle did not mint a fraud proof");
        }
        fraudProofUnit = stagedFraudProofUnit;
      }
      const proofUtxo = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.family.fraudProof.spendingScriptAddress,
        fraudProofUnit,
      );
      expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
        fraud_prover: harness.proverSigner.paymentKeyHash,
      });

      const removalRefs = await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      });
      const now = BigInt(harness.emulator.now());
      await submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalRefs.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "missingNativeScriptUtxo",
        fraudulentHeaderHash: target.successorHeaderHash,
        requireReferenceScripts: true,
        validFrom: now > 120_000n ? now - 120_000n : 0n,
        validTo: now + 300_000n,
      });
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.contracts.stateQueue.spendingScriptAddress,
          target.successorBlockUnit,
        ),
      ).resolves.toHaveLength(0);
      expect(
        outRefLabel(
          await expectSingleUtxoWithUnit(
            harness.proverLucid,
            harness.family.fraudProof.spendingScriptAddress,
            fraudProofUnit,
          ),
        ),
      ).toBe(outRefLabel(proofUtxo));
    },
    300_000,
  );

  it("refuses an authenticated present script at step-05 against the missing-script control", async () => {
    // Both commitments spend the same native-script input. all [] is satisfied
    // without signatures; only its presence in field 6 differs between them.
    for (const includeRequiredScript of [false, true]) {
      const scenario = await setupMissingNativeScriptUtxoLifecycle({
        nativeScript: { type: "all", scripts: [] },
        includeRequiredScript,
      });
      const { harness, fixture, target } = scenario;
      expect(fixture.scriptWitnessItems).toHaveLength(
        includeRequiredScript ? 1 : 0,
      );
      const { init, step04 } =
        await advanceMissingNativeScriptUtxoToFinal(scenario);
      const submit = () =>
        submitMissingNativeScriptUtxoFinalRaw(
          scenario,
          step04.nextThreadOutRef,
        );
      if (!includeRequiredScript) {
        const proof = await submit();
        const proofUtxo = await expectSingleUtxoWithUnit(
          harness.proverLucid,
          harness.family.fraudProof.spendingScriptAddress,
          proof.fraudProofUnit,
        );
        expect(Data.from(proofUtxo.datum!, FraudProofTokenDatum)).toEqual({
          fraud_prover: harness.proverSigner.paymentKeyHash,
        });
        continue;
      }
      await expectOnchainRefusal(submit, {
        refusedBy: "fraud_proofs/missing_native_script_utxo/step_05",
        check: /required_script_is_present == False/u,
      });
      // Refusal must preserve the honest header and thread and mint no evidence.
      expect(
        outRefLabel(
          await expectSingleUtxoWithUnit(
            harness.proverLucid,
            harness.family.steps[4].spendingScriptAddress,
            init.computationThreadUnit,
          ),
        ),
      ).toBe(step04.nextThreadOutRef);
      expect(
        outRefLabel(
          await expectSingleUtxoWithUnit(
            harness.proverLucid,
            harness.contracts.stateQueue.spendingScriptAddress,
            target.successorBlockUnit,
          ),
        ),
      ).toBe(target.successorOutRef);
      await expect(
        harness.proverLucid.utxosAtWithUnit(
          harness.family.fraudProof.spendingScriptAddress,
          harness.family.fraudProof.policyId + init.computationThreadAssetName,
        ),
      ).resolves.toHaveLength(0);
    }
  }, 300_000);
});
