import { encodeMidgardFieldPreimageV1 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  prepareWithdrawnReferenceInputV1,
  submitWithdrawnReferenceInputInit,
  submitWithdrawnReferenceInputStep01,
  submitWithdrawnReferenceInputStep02,
  submitWithdrawnReferenceInputStep03,
} from "../src/withdrawn-reference-input/index.js";
import { expectOnchainRefusalV1 } from "./support/native-script-decoding-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawnReferenceInputEmulatorHarnessV1,
  publishWithdrawnReferenceInputReferenceScriptsV1,
  setupWithdrawnReferenceInputScenarioV1,
  setupWithdrawnReferenceInputUncheckedScenarioV1,
  submitRawWithdrawnReferenceInputStep02V1,
  submitRawWithdrawnReferenceInputStep03V1,
  WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1,
  withdrawnReferenceInputInfoV1,
} from "./support/withdrawn-reference-input-emulator-v1.js";

type Harness = Awaited<
  ReturnType<typeof makeWithdrawnReferenceInputEmulatorHarnessV1>
>;
type Refs = Awaited<
  ReturnType<typeof publishWithdrawnReferenceInputReferenceScriptsV1>
>;
type UncheckedScenario = Awaited<
  ReturnType<typeof setupWithdrawnReferenceInputUncheckedScenarioV1>
>;
type BindScenario = {
  readonly setup: UncheckedScenario["setup"];
  readonly header: SDK.HeaderV1;
  readonly txInclusion: UncheckedScenario["txInclusion"];
  readonly withdrawalMembership: SDK.WithdrawalSourceMembershipProof;
  readonly referenceInputs: readonly SDK.MidgardTxInput[];
};

const initAndBind = async ({
  harness,
  scenario,
  refs,
}: {
  readonly harness: Harness;
  readonly scenario: BindScenario;
  readonly refs: Refs;
}) => {
  const init = await submitWithdrawnReferenceInputInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.family,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  });
  const step01 = await submitWithdrawnReferenceInputStep01({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    network,
    signer: harness.proverSigner,
    threadOutRef: init.nextThreadOutRef,
    stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    txInclusion: scenario.txInclusion,
    referenceScriptUtxo: refs[0],
  });
  return { init, step01 };
};

const driveToStep03 = async ({
  harness,
  scenario,
  refs,
}: {
  readonly harness: Harness;
  readonly scenario: BindScenario;
  readonly refs: Refs;
}) => {
  const { init, step01 } = await initAndBind({ harness, scenario, refs });
  const step02 = await submitWithdrawnReferenceInputStep02({
    lucid: harness.proverLucid,
    contracts: harness.family,
    categoryId: harness.category.categoryId,
    signer: harness.proverSigner,
    threadOutRef: step01.nextThreadOutRef,
    referenceInputs: scenario.referenceInputs,
    nativeTxCompactCbor: scenario.txInclusion.nativeTxCompactCbor,
    badReferenceInputIndex: 0n,
    referenceScriptUtxo: refs[1],
  });
  return { init, step01, step02 };
};

describe("withdrawn-reference-input adversarial emulator suite", () => {
  it("refuses both different-outref roads at the exact step-03 checks", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const different = { tx_id: "99".repeat(32), output_index: 3n };
    const scenario = await setupWithdrawnReferenceInputUncheckedScenarioV1({
      harness,
      withdrawalInfo: withdrawnReferenceInputInfoV1({ outRef: different }),
    });
    const refs = await publishWithdrawnReferenceInputReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });

    await expect(
      prepareWithdrawnReferenceInputV1({
        header: scenario.header,
        blockTxs: scenario.blockTxs,
        withdrawals: scenario.withdrawals,
      }),
    ).rejects.toThrow(/no-matching-reference-input/);
    const { step02 } = await driveToStep03({ harness, scenario, refs });
    await expect(
      submitWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/l2_outref does not equal/);
    await expectOnchainRefusalV1(() =>
      submitRawWithdrawnReferenceInputStep03V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
      }),
    );

    const forgedClaim: SDK.WithdrawalSourceMembershipProof = {
      ...scenario.withdrawalMembership,
      value: {
        ...scenario.withdrawalMembership.value,
        body: {
          ...scenario.withdrawalMembership.value.body,
          l2_outref: {
            transactionId: WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1.tx_id,
            outputIndex:
              WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF_V1.output_index,
          },
        },
      },
    };
    await expect(
      submitWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: forgedClaim,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/membership-proof-mismatch/);
    await expectOnchainRefusalV1(() =>
      submitRawWithdrawnReferenceInputStep03V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: forgedClaim,
        referenceScriptUtxo: refs[2],
      }),
    );
  }, 600_000);

  it("refuses an invalid withdrawal in the classifier, submitter, and validator", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const scenario = await setupWithdrawnReferenceInputUncheckedScenarioV1({
      harness,
      withdrawalInfo: withdrawnReferenceInputInfoV1({
        validity: "IncorrectWithdrawalSignature",
      }),
    });
    const refs = await publishWithdrawnReferenceInputReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    await expect(
      prepareWithdrawnReferenceInputV1({
        header: scenario.header,
        blockTxs: scenario.blockTxs,
        withdrawals: scenario.withdrawals,
      }),
    ).rejects.toThrow(/withdrawal-not-valid/);
    const { step02 } = await driveToStep03({ harness, scenario, refs });
    await expect(
      submitWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
      }),
    ).rejects.toThrow(/withdrawal-not-valid/);
    await expectOnchainRefusalV1(() =>
      submitRawWithdrawnReferenceInputStep03V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
      }),
    );
  }, 600_000);

  it("refuses a substituted reference-input preimage off-chain and at the door", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const positive = await setupWithdrawnReferenceInputScenarioV1({ harness });
    const scenario = {
      ...positive,
      referenceInputs: positive.prepared.referenceInputs,
      txInclusion: positive.prepared.txInclusion,
      withdrawalMembership: positive.prepared.withdrawalMembership,
    };
    const refs = await publishWithdrawnReferenceInputReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const { init: _init, step01 } = await initAndBind({
      harness,
      scenario,
      refs,
    });
    const injected: SDK.MidgardTxInput = {
      tx_id: "34".repeat(32),
      output_index: 2n,
    };
    await expect(
      submitWithdrawnReferenceInputStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        referenceInputs: [injected],
        nativeTxCompactCbor: scenario.txInclusion.nativeTxCompactCbor,
        badReferenceInputIndex: 0n,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(/§5\.1 preimage commits/);
    const opening: SDK.FieldOpeningV1 = {
      BodyFieldOpening: {
        native_tx_compact_cbor: scenario.txInclusion.nativeTxCompactCbor,
        carriage: {
          Inline: {
            preimage: encodeMidgardFieldPreimageV1([
              SDK.encodeMidgardTxInputCanonicalV1(injected),
            ]).toString("hex"),
          },
        },
      },
    };
    const nextDatum = Data.to(
      {
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: {
          missing_reference_input: injected,
          blocks_withdrawals_root: scenario.header.withdrawalsRoot,
          blocks_withdrawal_count: scenario.header.withdrawalCount,
        },
      },
      SDK.WithdrawnReferenceInputStep03Datum,
    );
    await expectOnchainRefusalV1(() =>
      submitRawWithdrawnReferenceInputStep02V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        nextDatumCbor: nextDatum,
        buildRedeemer: ({ inputIndex, outputIndex }) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  reference_inputs_opening: opening,
                  bad_reference_input_index: 0n,
                },
              ],
            },
            SDK.WithdrawnReferenceInputStep02SpendRedeemer,
          ),
        referenceScriptUtxo: refs[1],
      }),
    );
  }, 600_000);

  it("refuses an out-of-range challenged ordinal off-chain and on-chain", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarnessV1();
    const positive = await setupWithdrawnReferenceInputScenarioV1({ harness });
    const scenario = {
      ...positive,
      referenceInputs: positive.prepared.referenceInputs,
      txInclusion: positive.prepared.txInclusion,
      withdrawalMembership: positive.prepared.withdrawalMembership,
    };
    const refs = await publishWithdrawnReferenceInputReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const { step01 } = await initAndBind({ harness, scenario, refs });
    await expect(
      submitWithdrawnReferenceInputStep02({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        referenceInputs: scenario.referenceInputs,
        nativeTxCompactCbor: scenario.txInclusion.nativeTxCompactCbor,
        badReferenceInputIndex: 7n,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(/out of bounds/);
    const opening: SDK.FieldOpeningV1 = {
      BodyFieldOpening: {
        native_tx_compact_cbor: scenario.txInclusion.nativeTxCompactCbor,
        carriage: {
          Inline: {
            preimage: encodeMidgardFieldPreimageV1(
              scenario.referenceInputs.map(SDK.encodeMidgardTxInputCanonicalV1),
            ).toString("hex"),
          },
        },
      },
    };
    const nextDatum = Data.to(
      {
        fraud_prover: harness.proverSigner.paymentKeyHash,
        data: {
          missing_reference_input: scenario.referenceInputs[0],
          blocks_withdrawals_root: scenario.header.withdrawalsRoot,
          blocks_withdrawal_count: scenario.header.withdrawalCount,
        },
      },
      SDK.WithdrawnReferenceInputStep03Datum,
    );
    await expectOnchainRefusalV1(() =>
      submitRawWithdrawnReferenceInputStep02V1({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step01.nextThreadOutRef,
        nextDatumCbor: nextDatum,
        buildRedeemer: ({ inputIndex, outputIndex }) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  reference_inputs_opening: opening,
                  bad_reference_input_index: 7n,
                },
              ],
            },
            SDK.WithdrawnReferenceInputStep02SpendRedeemer,
          ),
        referenceScriptUtxo: refs[1],
      }),
    );
  }, 600_000);
});
