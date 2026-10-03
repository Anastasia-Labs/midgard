import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  AddressData,
  addressDataFromBech32,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { vi } from "vitest";

import { applyExecutionNativeScriptInvalidScripts } from "../src/execution-native-script-invalid/contracts.js";
import type { ExecutionNativeScriptInvalidEvidence } from "../src/execution-native-script-invalid/family.js";
import {
  ExecutionNativeScriptInvalidStep04DatumSchema,
  ExecutionNativeScriptInvalidStep04RedeemerSchema,
} from "../src/execution-native-script-invalid/schemas.js";
import { submitExecutionNativeScriptInvalidInit } from "../src/execution-native-script-invalid/submit-init.js";
import { submitExecutionNativeScriptInvalidStep01Forced } from "../src/execution-native-script-invalid/submit-step-01.js";
import { submitExecutionNativeScriptInvalidStep02 } from "../src/execution-native-script-invalid/submit-step-02.js";
import { submitExecutionNativeScriptInvalidStep03 } from "../src/execution-native-script-invalid/submit-step-03.js";
import { submitExecutionNativeScriptInvalidStep04 } from "../src/execution-native-script-invalid/submit-step-04-route.js";
import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import {
  type ExecutionNativeFixture,
  forcedReason,
  type retainedExecution,
} from "./forced-reason-coordinate-execution-native.fixture.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSecondHeaderTx,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

type Retained = Awaited<ReturnType<typeof retainedExecution>>;
type Step04Datum = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04DatumSchema
>;
type Step04Redeemer = Data.Static<
  typeof ExecutionNativeScriptInvalidStep04RedeemerSchema
>;

const acceptedPreludeNames = [
  "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
  "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
  "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
] as const;

/**
 * An emulator stage for an executionNativeScriptInvalid proof against a block
 * that commits the fixture's forced transaction under
 * `ExecutionNativeScriptFalse { executionIndex }`.
 */
export const makeStage = async (
  fixture: ExecutionNativeFixture,
  executionIndex: number,
  retained: Retained,
) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyExecutionNativeScriptInvalidScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
  });
  const contracts = {
    steps: applied,
    acceptedPrelude: applied.acceptedPrelude,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    executionNativeScriptInvalid: {
      ...harness.contracts.fraudProofs.executionNativeScriptInvalid,
      spendingScriptHash: applied[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.executionNativeScriptInvalid;
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: fixture.priorLedgerRoot,
    subject: {
      kind: "forced",
      nativeTx: fixture.nativeTx,
      orderKey: fixture.orderKey,
      verdict: { ForcedTxInvalid: { reason: forcedReason(executionIndex) } },
    },
  });
  // The block before the committing one carries the fixture's prior ledger,
  // which the machine states bind.
  const first = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header: { ...block.header, utxosRoot: fixture.priorLedgerRoot },
  });
  const header = {
    ...block.header,
    prevUtxosRoot: fixture.priorLedgerRoot,
    utxosRoot: fixture.priorLedgerRoot,
    validationTracesRoot: retained.machine.validationTracesRoot,
    validationTraceCount: retained.machine.validationTraceCount,
    prevHeaderHash: first.headerHash,
    startTime: block.header.endTime,
    endTime: block.header.endTime + 120_000n,
  };
  const second = await submitSecondHeaderTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    header,
  });
  const references: UTxO[] = [];
  for (const [index, step] of applied.entries())
    references.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `execution-native-coordinate-${index.toString()}`,
        })
      ).utxo,
    );
  const common = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };

  /** Init, the forced bind, source authentication and the item opening. */
  const open = async (evidence: ExecutionNativeScriptInvalidEvidence) => {
    const init = await submitExecutionNativeScriptInvalidInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: second.blockOutRef,
      fraudulentHeaderHash: second.headerHash,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const bound = await submitExecutionNativeScriptInvalidStep01Forced({
      ...common,
      threadOutRef: init.nextThreadOutRef,
      header,
      membership: await buildForcedTransactionLeafMembershipProof({
        reconstruction: block.reconstruction,
        eventKey: fixture.eventKey,
      }),
      executionIndex: BigInt(executionIndex),
      referenceScriptUtxo: references[0]!,
    });
    const authenticated = await submitExecutionNativeScriptInvalidStep02({
      ...common,
      threadOutRef: bound.nextThreadOutRef,
      evidence,
      authentication: retained.machine.authentication,
      referenceScriptUtxo: references[1]!,
    });
    return (
      await submitExecutionNativeScriptInvalidStep03({
        ...common,
        threadOutRef: authenticated.nextThreadOutRef,
        scriptItemCbor: retained.scriptItem,
        referenceScriptUtxo: references[2]!,
      })
    ).nextThreadOutRef;
  };

  const step04Input = (threadOutRef: string) => ({
    ...common,
    threadOutRef,
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    witnessSet: fixture.witnessSet,
    scriptItemCbor: retained.scriptItem,
    addressWitnessItems: fixture.addressWitnessItems,
    referenceScriptUtxo: references[3]!,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });

  /** The prover's step 04: signer evaluation and proof mint. */
  const finalize = async (threadOutRef: string) =>
    await submitExecutionNativeScriptInvalidStep04(step04Input(threadOutRef));

  /**
   * Step 04's direct finalize as a prover would submit it without the
   * builder's local check that the script contradicts the bound verdict, so
   * the validator's own check decides.
   */
  const finalizeUnchecked = async (threadOutRef: string) => {
    const input = step04Input(threadOutRef);
    const stepIndex = 3;
    const family = "executionNativeScriptInvalid";
    const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
      ...common,
      family,
      stepIndex,
      threadOutRef,
    });
    const state = requireLinearFaultStepState<NonNullable<Step04Datum["data"]>>(
      {
        threadUtxo,
        signer: harness.proverSigner,
        schema: asDataType<Step04Datum>(
          ExecutionNativeScriptInvalidStep04DatumSchema,
        ),
        family,
        stepIndex,
      },
    );
    const planned = planFaultProofFieldOpening({
      anchorSourceKind: state.source_kind === 1n ? 1n : 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
      anchorTxId: state.bad_tx_id,
      nativeTxCompactCbor: input.nativeTxCompactCbor,
      itemCbors: input.addressWitnessItems,
      owner: harness.proverSigner.paymentKeyHash,
      publish: false,
      witnessSet: input.witnessSet,
      anchorWitnessSetHash: state.bad_tx_witness_set_hash,
      label: `${family} final field 7`,
    });
    harness.proverSigner.selectWallet(harness.proverLucid);
    const carriageUtxos = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: `${family} final field 7`,
    });
    const witnesses = harness.witnessReferenceScripts;
    const opening = faultProofFieldOpening({
      planned,
      referenceInputs: [
        ...carriageUtxos,
        input.referenceScriptUtxo,
        ...[witnesses.computationThreadMint, witnesses.fraudProofMint].filter(
          (utxo): utxo is UTxO => utxo !== undefined,
        ),
      ],
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${family} final field 7`,
    });
    return await submitLinearFaultFinalize({
      lucid: harness.proverLucid,
      family,
      stepIndex,
      step: contracts.steps[stepIndex]!,
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer: harness.proverSigner,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: asDataType<Step04Redeemer>(
        ExecutionNativeScriptInvalidStep04RedeemerSchema,
      ),
      buildFamilyArgs: (layout) => ({
        DirectFinalize: {
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          script_item_cbor: Buffer.from(input.scriptItemCbor).toString("hex"),
          addr_tx_wits_opening: opening,
        },
      }),
      referenceScriptUtxo: input.referenceScriptUtxo,
      carriageUtxos,
      extraReferenceInputs: [],
      witnessReferenceScripts: witnesses,
      awaitConfirmation: true,
    });
  };

  /** Removal of the committing block under the minted proof. */
  const remove = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const entry = (step: (typeof applied)[number]) => ({
      scriptHash: step.spendingScriptHash,
      contract: {
        type: step.spendingScript.type,
        cborHex: step.spendingScript.script,
      },
    });
    vi.setSystemTime(harness.emulator.now());
    const now = BigInt(harness.emulator.now());
    return await submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: {
        ...base,
        contracts: {
          ...base.contracts,
          fraudProofExecutionNativeScriptInvalid: entry(applied[0]),
          ...Object.fromEntries(
            applied
              .slice(1)
              .map((step, index) => [
                `fraudProofExecutionNativeScriptInvalidStep0${(index + 2).toString()}`,
                entry(step),
              ]),
          ),
          ...Object.fromEntries(
            applied.acceptedPrelude.map((step, index) => [
              acceptedPreludeNames[index]!,
              entry(step),
            ]),
          ),
        },
      },
      network,
      signer: harness.proverSigner,
      fraudCategory: "executionNativeScriptInvalid",
      fraudulentHeaderHash: second.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "execution-native-coordinate",
          source: "emulator",
          renew: async () => undefined,
          release: async () => undefined,
          fail: async () => undefined,
        }),
      },
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
  };

  return {
    nativeTxId: block.nativeTxId,
    open,
    finalize,
    finalizeUnchecked,
    remove,
  };
};
