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
  submitMissingNativeScriptUtxoStep02,
  submitMissingNativeScriptUtxoStep03,
  submitMissingNativeScriptUtxoStep04,
  submitMissingNativeScriptUtxoStep05,
  submitMissingNativeScriptUtxoStep05StartGrammar,
  submitMissingNativeScriptUtxoStep06,
  submitMissingNativeScriptUtxoStep07,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { parseSubmitStep01TxInclusion } from "../src/submit-step-01.js";
import {
  buildMissingNativeScriptUtxoEmulatorFixture,
  makeMissingNativeScriptUtxoEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./support/final-catalogue-emulator-v1.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  makeHeader,
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
      const harness = await makeMissingNativeScriptUtxoEmulatorHarness();
      const refs = await publishFinalFamilyReferenceScripts({
        lucid: harness.proverLucid,
        family: harness.family,
        label: "missing-native-script-utxo",
      });
      const fixture = await buildMissingNativeScriptUtxoEmulatorFixture({
        decoyWitnessCount,
      });
      const predecessor = await setupFraudulentBlock({
        funderLucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        catalogue: harness.catalogue,
        fixture: {
          transactionsRoot: fixture.transactionsRoot,
          l2TransactionCount: fixture.l2TransactionCount,
          utxosRoot: fixture.prevUtxosRoot,
          // The four-transaction setup journey advances by about twenty seconds;
          // keep its predecessor window live after setup so strict successor
          // contiguity remains satisfiable.
          headerDurationMs: EMULATOR_HEADER_CLOCK_HEADROOM_MS,
        },
      });
      const targetStart = emulatorSuccessorHeaderStart({
        predecessorEndTime: predecessor.header.endTime,
        emulator: harness.emulator,
      });
      const targetHeader = {
        ...makeHeader(
          predecessor.header.operatorVkey,
          targetStart,
          await countedTransactionsRoot(
            fixture.transactionsRoot,
            fixture.l2TransactionCount,
          ),
          fixture.l2TransactionCount,
        ),
        prevHeaderHash: predecessor.headerHash,
        prevUtxosRoot: fixture.prevUtxosRoot,
        utxosRoot: fixture.utxosRoot,
      };
      expect(
        targetHeader.endTime + 1n,
        "successor commit validTo must be later than the emulator clock before submission",
      ).toBeGreaterThan(BigInt(harness.emulator.now()));
      const target = await submitSuccessorBlockTx({
        lucid: harness.funderLucid,
        emulator: harness.emulator,
        contracts: harness.contracts,
        anchorBlockUnit: predecessor.stateQueueBlockUnit,
        header: targetHeader,
        hubOracle: predecessor.hubOracle,
        scheduler: predecessor.scheduler,
        activeOperatorNode: predecessor.activeOperatorNode,
        activeOperatorNodeUnit: predecessor.activeOperatorNodeUnit,
      });
      const prepared = {
        ...fixture.prepared,
        headerHash: target.successorHeaderHash,
      };
      const txInclusion = parseSubmitStep01TxInclusion(prepared.txInclusion);
      const deploymentInfo = buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
      );
      const initParams = {
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo,
        network,
        signer: harness.proverSigner,
        fraudulentBlockOutRef: target.successorOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      } as const;

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

      const init = await submitMissingNativeScriptUtxoInit(initParams);
      const step01 = await submitMissingNativeScriptUtxoStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: `${init.txHash}#${init.firstStepOutputIndex.toString()}`,
        stateQueueBlockOutRef: target.successorOutRef,
        txInclusion,
        prevUtxosRoot: prepared.prevUtxosRoot,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const step02 = await submitMissingNativeScriptUtxoStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        nativeTxCompactCbor: prepared.nativeTxCompactCbor,
        spendInputs: fixture.spendInputs,
        badInputIndex: prepared.badInputIndex,
        referenceScriptUtxo: refs[1],
      });
      const step03 = await submitMissingNativeScriptUtxoStep03({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        prepared,
        referenceScriptUtxo: refs[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      });
      const step04 = await submitMissingNativeScriptUtxoStep04({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step03.nextThreadOutRef,
        missingNativeScriptBytes: prepared.missingNativeScriptBytes,
        referenceScriptUtxo: refs[3],
      });
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
});
