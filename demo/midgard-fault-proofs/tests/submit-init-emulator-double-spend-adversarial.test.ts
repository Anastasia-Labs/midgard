/**
 * Double-spend steps against a party that is not the thread's prover.
 *
 * Continuing a fault-proof thread requires its prover: each row lets an
 * honest prover open a double-spend thread, then a funded stranger builds the
 * next step directly against the chain, keeping the honest prover as the
 * thread's `fraud_prover` and signing with its own key. The step validator
 * refuses the stranger, and the honest prover's own step still lands on the
 * untouched thread.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  HUB_ORACLE_ASSET_NAME,
  MIDGARD_FIELD_INDEX,
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  generateEmulatorAccount,
  Lucid,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
  resolveProverSigner,
} from "../src/runtime.js";
import { PHAS_MEMBERSHIP_WITHDRAW_TITLE } from "../src/step-support.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  parseSpendInputCbors,
  parseSubmitStep01TxInclusion,
  submitInit,
  submitStep01,
  submitStep02,
  submitStep03,
} from "./support/legacy-submit-emulator.js";
import {
  buildTransactionInclusionFixture,
  countedTransactionsRoot,
  EMULATOR_HEADER_CLOCK_HEADROOM_MS,
  midgardTxInput,
} from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectOnchainRefusal,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const STRANGER_FUNDING_LOVELACE = 1_000_000_000n;

/** An honest prover's double-spend thread at step 01, and a funded stranger. */
const scene = async () => {
  const h = await makeFaultProofEmulatorHarness();
  const inclusion = await buildTransactionInclusionFixture();
  const header = makeHeader(
    await funderPaymentKeyHash(h.funderLucid),
    alignUnixTimeToEmulatorSlotBoundary(
      h.funderLucid,
      h.emulator.now() + 120_000,
    ) - 1,
    await countedTransactionsRoot(
      inclusion.transactionsRoot,
      inclusion.l2TransactionCount,
    ),
    inclusion.l2TransactionCount,
  );
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: {
      ...header,
      endTime: header.startTime + BigInt(EMULATOR_HEADER_CLOCK_HEADROOM_MS),
    },
  });
  const deploymentInfo = buildRemovalDeploymentInfo(
    h.contracts,
    h.catalogue,
    {},
  );
  const init = await submitInit({
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    deploymentInfo,
    network,
    signer: h.proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: h.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const steps = h.contracts.fraudProofContracts.doubleSpend.steps;
  const refs = h.faultProofReferenceScripts;

  const stranger = generateEmulatorAccount({ lovelace: 0n });
  const strangerLucid = await Lucid(h.emulator, "Custom");
  const strangerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: stranger.seedPhrase,
  });
  strangerSigner.selectWallet(strangerLucid);
  const funding = await h.funderLucid
    .newTx()
    .pay.ToAddress(strangerSigner.address, {
      lovelace: STRANGER_FUNDING_LOVELACE,
    })
    .pay.ToAddress(strangerSigner.address, {
      lovelace: STRANGER_FUNDING_LOVELACE,
    })
    .complete();
  await h.funderLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  expect(strangerSigner.paymentKeyHash).not.toBe(h.proverSigner.paymentKeyHash);

  const threadAt = (address: string) =>
    expectSingleUtxoWithUnit(
      h.proverLucid,
      address,
      init.computationThreadUnit,
    );
  const step01Inputs = {
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    deploymentInfo,
    network,
    signer: h.proverSigner,
    stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
    witnessReferenceScripts: h.witnessReferenceScripts,
    awaitConfirmation: true,
  };
  return {
    h,
    inclusion,
    setup,
    deploymentInfo,
    init,
    steps,
    refs,
    strangerLucid,
    strangerSigner,
    threadAt,
    step01Inputs,
  };
};

type Scene = Awaited<ReturnType<typeof scene>>;

/** The thread's lovelace and its computation-thread token, carried forward. */
const threadAssets = (scene: Scene, thread: UTxO) => ({
  lovelace: thread.assets.lovelace ?? 0n,
  [scene.init.computationThreadUnit]: 1n,
});

/**
 * Step 01, built by the stranger: it opens `subject` from the block's
 * transaction root and writes the step-02 datum with the honest prover still
 * named as the thread's `fraud_prover`, signing only with its own key.
 */
const strangerStep01 = async (
  scene: Scene,
  thread: UTxO,
  subject: Scene["inclusion"]["tx2"],
) => {
  const { h, strangerLucid, strangerSigner, steps, refs } = scene;
  const txInclusion = parseSubmitStep01TxInclusion(subject.inclusion);
  const hubOracle = await strangerLucid.utxoByUnit(
    toUnit(h.contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
  );
  const [block] = await strangerLucid.utxosByOutRef([
    {
      txHash: scene.setup.fraudulentBlockOutRef.split("#")[0]!,
      outputIndex: Number(scene.setup.fraudulentBlockOutRef.split("#")[1]),
    },
  ]);
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(h.realBlueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasReward = phasMembershipRewardAddress(network, phasScript);
  const step02Datum = Data.to(
    {
      fraud_prover: h.proverSigner.paymentKeyHash,
      data: { verified_tx1_id: txInclusion.nativeTxId },
    },
    DoubleSpendStep02Datum,
  );
  const step02Output = computationThreadOutputPredicate({
    address: steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: scene.init.computationThreadUnit,
  });
  const redeemer = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            RedeemerCarriedInclusion: [
              {
                input_index: requireInputIndex(ctx, thread, "step 01"),
                output_index: requireUniqueOutputIndex(
                  ctx.outputs,
                  step02Output,
                  "step 01 output",
                ),
                hub_ref_input_index: requireReferenceInputIndex(
                  ctx,
                  hubOracle,
                  "hub oracle",
                ),
                state_queue_node_ref_input_index: requireReferenceInputIndex(
                  ctx,
                  block!,
                  "state-queue block",
                ),
                native_tx_id: txInclusion.nativeTxId,
                l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
                transactions_phas_root: txInclusion.transactionsPhasRoot,
                tx_membership_proof: txInclusion.txMembershipProof,
                inclusion_proof_script_withdraw_redeemer_index:
                  requireWithdrawalRedeemerIndex(ctx, phasReward, "phas"),
              },
            ],
          },
        ],
      },
      DoubleSpendStep01SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const tx = await strangerLucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([
      hubOracle,
      block!,
      refs.fraudProofDoubleSpend!.utxo,
      h.witnessReferenceScripts.phasMembershipWithdraw!,
    ])
    .withdraw(
      phasReward,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets(scene, thread),
    )
    .addSignerKey(strangerSigner.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  await strangerLucid.awaitTx(await signed.submit());
};

/**
 * Step 03, built by the stranger: it opens tx1's spend inputs inline and
 * selects `index` as the double-spent input, writing the step-04 datum with
 * the honest prover still named as the thread's `fraud_prover`.
 */
const strangerStep03 = async (scene: Scene, thread: UTxO, index: bigint) => {
  const { h, inclusion, strangerLucid, strangerSigner, steps, refs } = scene;
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: inclusion.tx1.nativeTxId,
    nativeTxCompactCbor: parseSubmitStep01TxInclusion(inclusion.tx1.inclusion)
      .nativeTxCompactCbor,
    itemCbors: inclusion.tx1SpendInputCbors.map((cbor) =>
      Buffer.from(cbor, "hex"),
    ),
    owner: strangerSigner.paymentKeyHash,
    publish: false,
    label: "stranger step 03",
  });
  expect(planned.plan.tier).toBe("Inline");
  const step04Datum = Data.to(
    {
      fraud_prover: h.proverSigner.paymentKeyHash,
      data: {
        verified_tx2_id: inclusion.tx2.nativeTxId,
        double_spent_input: midgardTxInput(
          inclusion.tx1InputsPreimage[Number(index)]!,
        ),
      },
    },
    DoubleSpendStep04Datum,
  );
  const step04Output = computationThreadOutputPredicate({
    address: steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: scene.init.computationThreadUnit,
  });
  const redeemer = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, thread, "step 03"),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              step04Output,
              "step 03 output",
            ),
            tx1_spend_inputs_opening: faultProofFieldOpening({
              planned,
              referenceInputs: ctx.referenceInputs,
              label: "stranger step 03",
            }),
            double_spent_input_index: index,
          },
        ],
      },
      DoubleSpendStep03SpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const tx = await strangerLucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([refs.fraudProofDoubleSpendStep03!.utxo])
    .pay.ToContract(
      steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets(scene, thread),
    )
    .addSignerKey(strangerSigner.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  await strangerLucid.awaitTx(await signed.submit());
};

describe("double-spend steps refuse a party other than the thread's prover", () => {
  it("refuses a stranger's step 01 on another subject, then takes the prover's", async () => {
    const s = await scene();
    const thread = await s.threadAt(s.init.firstStepAddress);

    await expectOnchainRefusal(
      () => strangerStep01(s, thread, s.inclusion.tx2),
      {
        refusedBy: "fraud_proofs/double_spend/step_01",
        check: /fraud_prover \|> has_signed\(extra_signatories\)/u,
      },
    );

    // The thread is where the prover left it, and the prover's own step lands.
    expect(outRefLabel(await s.threadAt(s.init.firstStepAddress))).toBe(
      outRefLabel(thread),
    );
    const step01 = await submitStep01({
      ...s.step01Inputs,
      threadOutRef: outRefLabel(thread),
      txInclusion: parseSubmitStep01TxInclusion(s.inclusion.tx1.inclusion),
      referenceScriptUtxo: s.refs.fraudProofDoubleSpend!.utxo,
    });
    expect(step01.nativeTxId).toBe(s.inclusion.tx1.nativeTxId);
  }, 600_000);

  it("refuses a stranger's step 03 selecting another input, then takes the prover's", async () => {
    const s = await scene();
    const step01 = await submitStep01({
      ...s.step01Inputs,
      threadOutRef: outRefLabel(await s.threadAt(s.init.firstStepAddress)),
      txInclusion: parseSubmitStep01TxInclusion(s.inclusion.tx1.inclusion),
      referenceScriptUtxo: s.refs.fraudProofDoubleSpend!.utxo,
    });
    const step02 = await submitStep02({
      ...s.step01Inputs,
      threadOutRef: outRefLabel(await s.threadAt(step01.secondStepAddress)),
      txInclusion: parseSubmitStep01TxInclusion(s.inclusion.tx2.inclusion),
      referenceScriptUtxo: s.refs.fraudProofDoubleSpendStep02!.utxo,
    });
    const thread = await s.threadAt(step02.thirdStepAddress);

    await expectOnchainRefusal(() => strangerStep03(s, thread, 0n), {
      refusedBy: "fraud_proofs/double_spend/step_03",
      check: /fraud_prover \|> has_signed\(extra_signatories\)/u,
    });

    expect(outRefLabel(await s.threadAt(step02.thirdStepAddress))).toBe(
      outRefLabel(thread),
    );
    const step03 = await submitStep03({
      lucid: s.h.proverLucid,
      blueprint: s.h.realBlueprint,
      deploymentInfo: s.deploymentInfo,
      network,
      signer: s.h.proverSigner,
      threadOutRef: outRefLabel(thread),
      tx1SpendInputCbors: parseSpendInputCbors(
        s.inclusion.tx1SpendInputCbors,
        "--tx1-inputs",
      ),
      nativeTxCompactCbor: parseSubmitStep01TxInclusion(
        s.inclusion.tx1.inclusion,
      ).nativeTxCompactCbor,
      doubleSpentInputIndex: 1n,
      referenceScriptUtxo: s.refs.fraudProofDoubleSpendStep03!.utxo,
      awaitConfirmation: true,
    });
    expect(step03.doubleSpentInputIndex).toBe(1);
  }, 600_000);
});
