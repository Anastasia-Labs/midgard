import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  prepareWithdrawnReferenceInput,
  submitWithdrawnReferenceInputInit,
  submitWithdrawnReferenceInputStep01,
  submitWithdrawnReferenceInputStep02,
  submitWithdrawnReferenceInputStep03,
} from "../src/withdrawn-reference-input/index.js";
import { expectOnchainRefusal } from "./support/native-script-decoding-emulator.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  makeWithdrawnReferenceInputEmulatorHarness,
  publishWithdrawnReferenceInputReferenceScripts,
  setupWithdrawnReferenceInputScenario,
  setupWithdrawnReferenceInputUncheckedScenario,
  submitRawWithdrawnReferenceInputStep02,
  submitRawWithdrawnReferenceInputStep03,
  WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF,
  withdrawnReferenceInputInfo,
} from "./support/withdrawn-reference-input-emulator.js";

type Harness = Awaited<
  ReturnType<typeof makeWithdrawnReferenceInputEmulatorHarness>
>;
type Refs = Awaited<
  ReturnType<typeof publishWithdrawnReferenceInputReferenceScripts>
>;
type UncheckedScenario = Awaited<
  ReturnType<typeof setupWithdrawnReferenceInputUncheckedScenario>
>;
type BindScenario = {
  readonly setup: UncheckedScenario["setup"];
  readonly header: SDK.Header;
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
    witnessReferenceScripts: harness.witnessReferenceScripts,
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
    witnessReferenceScripts: harness.witnessReferenceScripts,
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
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const different = { tx_id: "99".repeat(32), output_index: 3n };
    const scenario = await setupWithdrawnReferenceInputUncheckedScenario({
      harness,
      withdrawalInfo: withdrawnReferenceInputInfo({ outRef: different }),
    });
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });

    await expect(
      prepareWithdrawnReferenceInput({
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
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/l2_outref does not equal/);
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );

    const forgedClaim: SDK.WithdrawalSourceMembershipProof = {
      ...scenario.withdrawalMembership,
      value: {
        ...scenario.withdrawalMembership.value,
        body: {
          ...scenario.withdrawalMembership.value.body,
          l2_outref: {
            transactionId: WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF.tx_id,
            outputIndex: WITHDRAWN_REFERENCE_INPUT_ACCUSED_OUTREF.output_index,
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
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/membership-proof-mismatch/);
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: forgedClaim,
        referenceScriptUtxo: refs[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  }, 600_000);

  it("refuses an invalid withdrawal in the classifier, submitter, and validator", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const scenario = await setupWithdrawnReferenceInputUncheckedScenario({
      harness,
      withdrawalInfo: withdrawnReferenceInputInfo({
        validity: "IncorrectWithdrawalSignature",
      }),
    });
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    await expect(
      prepareWithdrawnReferenceInput({
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
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/withdrawal-not-valid/);
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep03({
        lucid: harness.proverLucid,
        contracts: harness.family,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02.nextThreadOutRef,
        withdrawalMembership: scenario.withdrawalMembership,
        referenceScriptUtxo: refs[2],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  }, 600_000);

  it("refuses a substituted reference-input preimage off-chain and at the door", async () => {
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const positive = await setupWithdrawnReferenceInputScenario({ harness });
    const scenario = {
      ...positive,
      referenceInputs: positive.prepared.referenceInputs,
      txInclusion: positive.prepared.txInclusion,
      withdrawalMembership: positive.prepared.withdrawalMembership,
    };
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.family,
    });
    const { step01 } = await initAndBind({
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
    const opening: SDK.FieldOpening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: scenario.txInclusion.nativeTxCompactCbor,
        carriage: {
          Inline: {
            preimage: encodeMidgardFieldPreimage([
              SDK.encodeMidgardTxInputCanonical(injected),
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
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep02({
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
    const harness = await makeWithdrawnReferenceInputEmulatorHarness();
    const positive = await setupWithdrawnReferenceInputScenario({ harness });
    const scenario = {
      ...positive,
      referenceInputs: positive.prepared.referenceInputs,
      txInclusion: positive.prepared.txInclusion,
      withdrawalMembership: positive.prepared.withdrawalMembership,
    };
    const refs = await publishWithdrawnReferenceInputReferenceScripts({
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
    const opening: SDK.FieldOpening = {
      BodyFieldOpening: {
        native_tx_compact_cbor: scenario.txInclusion.nativeTxCompactCbor,
        carriage: {
          Inline: {
            preimage: encodeMidgardFieldPreimage(
              scenario.referenceInputs.map(SDK.encodeMidgardTxInputCanonical),
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
    await expectOnchainRefusal(() =>
      submitRawWithdrawnReferenceInputStep02({
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
