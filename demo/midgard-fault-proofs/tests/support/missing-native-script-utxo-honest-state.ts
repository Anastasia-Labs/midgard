import {
  MIDGARD_FIELD_INDEX,
  MissingNativeScriptUtxoStep05DatumSchema,
  MissingNativeScriptUtxoStep05SpendRedeemerSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import { requireLinearFaultThreadUtxo } from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import {
  submitMissingNativeScriptUtxoInit,
  submitMissingNativeScriptUtxoStep01,
  submitMissingNativeScriptUtxoStep02,
  submitMissingNativeScriptUtxoStep03,
  submitMissingNativeScriptUtxoStep04,
} from "../../src/missing-native-script-utxo/index.js";
import { parseSubmitStep01TxInclusion } from "../../src/step-support.js";
import {
  buildMissingNativeScriptUtxoEmulatorFixture,
  makeMissingNativeScriptUtxoEmulatorHarness,
  publishFinalFamilyReferenceScripts,
} from "./final-catalogue-emulator.js";
import {
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  emulatorSuccessorHeaderStart,
  setupFraudulentBlock,
  submitSuccessorBlockTx,
} from "./submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  makeHeader,
  network,
} from "./submit-init-emulator-shared.js";

export const setupMissingNativeScriptUtxoLifecycle = async (
  options: Parameters<typeof buildMissingNativeScriptUtxoEmulatorFixture>[0],
) => {
  const harness = await makeMissingNativeScriptUtxoEmulatorHarness();
  const refs = await publishFinalFamilyReferenceScripts({
    lucid: harness.proverLucid,
    family: harness.family,
    label: "missing-native-script-utxo",
  });
  const fixture = await buildMissingNativeScriptUtxoEmulatorFixture(options);
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

  return { harness, refs, fixture, target, prepared, txInclusion, initParams };
};

export const advanceMissingNativeScriptUtxoToFinal = async ({
  harness,
  refs,
  fixture,
  target,
  prepared,
  txInclusion,
  initParams,
}: Awaited<ReturnType<typeof setupMissingNativeScriptUtxoLifecycle>>) => {
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
  return { init, step04 };
};

/** Bypass only the SDK's script-presence precondition; authenticate the real field opening. */
export const submitMissingNativeScriptUtxoFinalRaw = async (
  scenario: Awaited<ReturnType<typeof setupMissingNativeScriptUtxoLifecycle>>,
  threadOutRef: string,
) => {
  const { harness, refs, prepared, fixture } = scenario;
  const family = "missing-native-script-utxo";
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    family,
    stepIndex: 4,
    threadOutRef,
  });
  const datum = Data.from(
    threadUtxo.datum!,
    MissingNativeScriptUtxoStep05DatumSchema,
  );
  if (datum.data === null)
    throw new Error("expected authenticated step-05 state");
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: datum.data.bad_tx_id,
    nativeTxCompactCbor: prepared.nativeTxCompactCbor,
    itemCbors: fixture.scriptWitnessItems,
    owner: harness.proverSigner.paymentKeyHash,
    witnessSet: fixture.witnessSet,
    anchorWitnessSetHash: datum.data.bad_tx_witness_set_hash,
    label: "missing-native-script-utxo honest-state field 6",
  });
  const opening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      refs[4],
      harness.witnessReferenceScripts.computationThreadMint!,
      harness.witnessReferenceScripts.fraudProofMint!,
    ],
    certificatePolicyId: harness.family.fieldPreimageCertificatePolicyId,
    label: family,
  });
  return await submitLinearFaultFinalize({
    lucid: harness.proverLucid,
    family,
    stepIndex: 4,
    step: harness.family.steps[4],
    computationThread: harness.family.computationThread,
    fraudProof: harness.family.fraudProof,
    signer: harness.proverSigner,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MissingNativeScriptUtxoStep05SpendRedeemerSchema,
    buildFamilyArgs: (layout) => ({
      DirectFinalize: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
        script_tx_wits_opening: opening,
      },
    }),
    referenceScriptUtxo: refs[4],
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
