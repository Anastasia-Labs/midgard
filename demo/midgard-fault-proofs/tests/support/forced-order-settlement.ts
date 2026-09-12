import {
  aikenSerialisedPlutusDataCbor,
  computeMidgardNativeTxId,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  generateSeedPhrase,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  toUnit,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { vi } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../../src/transition-trace/phas.js";
import { settleOldestCrossBlockHeader } from "./cross-block-settlement-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  type CompleteSignedTransactionMeasurement,
  makeFaultProofEmulatorHarness,
  makeHeader,
  makeNativeTx,
  measureCompleteSignedTransaction,
  publishPlainReferenceScriptUtxo,
  runEmulatorLifecycleStage,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

/** Publish the SDK-selected carriage through the real certificate validator.
 * Keep every signed transaction measurement, including when later order
 * construction cannot fit the L1 execution budget.
 */
export const publishForcedOrderCarriage = async (
  h: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>,
  lucid: LucidEvolution,
  submittedTxCbor: Buffer,
  onMeasurement?: (
    stage: string,
    measurement: CompleteSignedTransactionMeasurement,
  ) => void,
) => {
  const publisherAddress = await lucid.wallet().address();
  const owner = Buffer.from(
    getAddressDetails(publisherAddress).paymentCredential!.hash,
    "hex",
  );
  const material = SDK.deriveTxOrderMaterial({ submittedTxCbor, owner });
  const plan = SDK.planTxOrderMaterialCarriage({ material, owner });
  const published: UTxO[] = [];
  const publish = async (stage: string, tx: TxSignBuilder) => {
    const signed = await tx.sign.withWallet().complete();
    onMeasurement?.(stage, measureCompleteSignedTransaction(signed.toCBOR()));
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    const [utxo] = await lucid.utxosByOutRef([{ txHash, outputIndex: 0 }]);
    if (utxo === undefined)
      throw new Error("carriage publication output is missing");
    published.push(utxo);
  };
  for (const field of plan.referenced) {
    for (const publication of SDK.fieldPreimagePublicationOutputs(field.plan)) {
      await publish(
        `field-${field.fieldIndex}-publication`,
        await Effect.runPromise(
          SDK.buildUnsignedFieldPreimagePublicationProgram(lucid, {
            publication,
            publisherAddress,
          }),
        ),
      );
    }
    if (field.plan.tier === "Certified") {
      await publish(
        `field-${field.fieldIndex}-certificate`,
        await Effect.runPromise(
          SDK.buildUnsignedFieldPreimageCertificationProgram(lucid, {
            sourceKind: 1n,
            plan: field.plan,
            certificatePolicyId: h.contracts.fieldPreimageCertificate.policyId,
            certificateAddress:
              h.contracts.fieldPreimageCertificate.spendingScriptAddress,
            certificateWitness: {
              kind: "inline_emulator_only",
              certificateScript:
                h.contracts.fieldPreimageCertificate.mintingScript,
            },
            chunkUtxos: published.slice(-field.plan.publications.length),
            compactCbor: material.submitted_source.compact_cbor,
            witnessSetCompactCbor:
              material.submitted_source.witness_set_compact_cbor,
          }),
        ),
      );
    }
  }
  return { material, plan, referenceInputs: published };
};

/** Real order mint/spend, settlement mint, state-queue and PHAS validators.
 * DA attachment uses the common emulator transport boundary, as in the
 * installed settlement lifecycle. No synthetic order or settlement is seeded.
 */
export const makeForcedOrderSettlementScenario = async (
  options: {
    verdict?: SDK.OperatorVerdict;
    changeLeaf?: (leaf: SDK.ForcedInclusionTxV1) => SDK.ForcedInclusionTxV1;
    foreignOrderKey?: boolean;
    twoOrders?: boolean;
    submitted?: ReturnType<typeof materializeMidgardForcedTxFromCanonical>;
    onMeasurement?: (
      stage: string,
      measurement: CompleteSignedTransactionMeasurement,
    ) => void;
  } = {},
) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: { realTxOrder: true, realSettlement: true },
  });
  const lucid = h.proverLucid;
  const wallets = [lucid];
  if (options.twoOrders) {
    const second = await Lucid(h.emulator, "Custom");
    second.selectWallet.fromSeed(generateSeedPhrase());
    const funding = await lucid
      .newTx()
      .pay.ToAddress(await second.wallet().address(), {
        lovelace: 100_000_000n,
      })
      .complete();
    await lucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    wallets.push(second);
  }
  const submitted =
    options.submitted ??
    materializeMidgardForcedTxFromCanonical(
      makeNativeTx({ spendInputCbors: [], outputCbors: [], fee: 0n }),
    );
  const bytes = encodeMidgardForcedTxCanonical(submitted);
  const carriage = [];
  const nonces: UTxO[] = [];
  for (const wallet of wallets) {
    carriage.push(
      await publishForcedOrderCarriage(h, wallet, bytes, options.onMeasurement),
    );
    const [nonce] = (await wallet.wallet().getUtxos())
      .filter(
        (utxo) => utxo.datum === undefined && utxo.datumHash === undefined,
      )
      .sort((left, right) =>
        Number(right.assets.lovelace - left.assets.lovelace),
      );
    if (nonce === undefined)
      throw new Error(
        "forced order nonce is missing after carriage publication",
      );
    nonces.push(nonce);
  }
  const ids = nonces.map(SDK.outputReferenceFromUTxO);
  const source = deriveMidgardForcedTxProofSource(submitted);
  const verdict = options.verdict ?? "ForcedTxValid";
  const original: SDK.ForcedInclusionTxV1 = {
    tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict,
  };
  const leaf = options.changeLeaf?.(original) ?? original;
  const keys = ids.map((id) =>
    Buffer.from(
      aikenSerialisedPlutusDataCbor(
        Data.to(
          options.foreignOrderKey
            ? { ...id, outputIndex: id.outputIndex + 1n }
            : id,
          SDK.OutputReference,
        ),
      ),
      "hex",
    ),
  );
  const value = Buffer.from(
    aikenSerialisedPlutusDataCbor(Data.to(leaf, SDK.ForcedInclusionTxV1)),
    "hex",
  );
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
    keys.map((key) => ({ key, value })),
  );
  const operator = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const header = {
    ...makeHeader(
      operator.hash,
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
    ),
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: BigInt(wallets.length),
    totalEventCount: BigInt(wallets.length),
    transitionStepCount: BigInt(wallets.length),
    validationTraceCount: BigInt(wallets.length),
    transitionTraceRoot: "aa".repeat(32),
    eventToStepRoot: "bb".repeat(32),
    validationTracesRoot: "cc".repeat(32),
  };
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header,
  });
  const orders: UTxO[] = [];
  for (const [index, wallet] of wallets.entries()) {
    const refundAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(await wallet.wallet().address()),
    );
    if (
      refundAddress.stakeCredential !== null &&
      "Pointer" in refundAddress.stakeCredential
    )
      throw new Error("fixture requires a non-pointer wallet");
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const built = await Effect.runPromise(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(wallet, h.contracts, {
        nonceInput: nonces[index]!,
        submittedTxCbor: bytes.toString("hex"),
        carriageReferenceInputs: carriage[index]!.referenceInputs,
        fieldPreimageCertificatePolicyId:
          h.contracts.fieldPreimageCertificate.policyId,
        refundAddress: {
          ...refundAddress,
          stakeCredential: refundAddress.stakeCredential,
        },
        lovelace: 3_000_000n,
      }),
    ).finally(() => clock.mockRestore());
    await runEmulatorLifecycleStage("forced.order-mint", async () => {
      const signed = await built.tx.sign.withWallet().complete();
      options.onMeasurement?.(
        "order-mint",
        measureCompleteSignedTransaction(signed.toCBOR()),
      );
      await wallet.awaitTx(await signed.submit());
    });
    const order = (
      await wallet.utxosAt(h.contracts.txOrder.spendingScriptAddress)
    ).find(
      (utxo) =>
        !orders.some(
          (old) =>
            old.txHash === utxo.txHash && old.outputIndex === utxo.outputIndex,
        ),
    );
    if (order === undefined)
      throw new Error("real order mint did not create its output");
    orders.push(order);
  }
  const order = orders[0]!;
  const datum = Data.from(order.datum!, SDK.TxOrderDatum);
  await runEmulatorLifecycleStage("forced.settlement-mint", () =>
    settleOldestCrossBlockHeader(h),
  );
  const [settlement] = await lucid.utxosAtWithUnit(
    h.contracts.settlement.spendingScriptAddress,
    toUnit(h.contracts.settlement.policyId, setup.headerHash),
  );
  if (settlement === undefined)
    throw new Error("real settlement mint did not create its output");
  const phas = SDK.phasMembershipWithdrawalScriptFromBlueprint(h.realBlueprint);
  const phasAddress = SDK.phasMembershipRewardAddress("Custom", phas);
  const spendReferences: UTxO[] = [];
  for (const [label, script] of [
    ["order mint", h.contracts.txOrder.mintingScript],
    ["order spend", h.contracts.txOrder.spendingScript],
    ["PHAS membership", phas],
  ] as const) {
    spendReferences.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script,
          label,
        })
      ).utxo,
    );
  }
  const settleOrder = async (
    orderIndex = 0,
    proofIndex = orderIndex,
    claimedVerdict: SDK.OperatorVerdict = verdict,
  ) => {
    const lucid = wallets[orderIndex]!;
    const order = orders[orderIndex]!;
    const key = keys[proofIndex]!;
    const proof = await keyValuePhasProof(
      { ...root, root: root.phasRoot },
      key,
      value,
    );
    const witness: SDK.RawRootMembershipProof = {
      domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: key.toString("hex"),
      value: value.toString("hex"),
      proof,
    };
    const unit = Object.keys(order.assets).find((unit) => unit !== "lovelace")!;
    const assetName = unit.slice(56);
    const certificate =
      SDK.buildUserEventWitnessCertificateValidator(assetName);
    const spend: BuildTxWithRedeemer = (ctx) =>
      Data.to(
        {
          input_index: SDK.requireInputIndex(ctx, order, "order"),
          output_index: 0n,
          hub_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            setup.hubOracle,
            "hub",
          ),
          settlement_ref_input_index: SDK.requireReferenceInputIndex(
            ctx,
            settlement,
            "settlement",
          ),
          burn_redeemer_index: SDK.requireMintRedeemerIndex(
            ctx,
            h.contracts.txOrder.policyId,
            "order burn",
          ),
          membership_proof: witness,
          inclusion_proof_script_withdraw_redeemer_index:
            SDK.requireWithdrawalRedeemerIndex(ctx, phasAddress, "membership"),
          validity_override: claimedVerdict,
        },
        SDK.TxOrderSpendRedeemer,
      );
    const burn: BuildTxWithRedeemer = (ctx) =>
      Data.to(
        {
          event: {
            BurnEventNFT: {
              nonce_asset_name: assetName,
              witness_unregistration_redeemer_index:
                SDK.requireSinglePublishRedeemerIndex(ctx, "unregister"),
            },
          },
          material_carriage: [],
        },
        SDK.TxOrderMintRedeemer,
      );
    const proofData = Data.to([
      root.phasRoot,
      key.toString("hex"),
      value.toString("hex"),
      Data.castTo(proof, SDK.Proof),
    ]);
    const tx = await lucid
      .newTx()
      .collectFrom([order], spend)
      .readFrom([setup.hubOracle, settlement, ...spendReferences])
      .pay.ToAddress(await lucid.wallet().address(), {
        lovelace: order.assets.lovelace!,
      })
      .mintAssets({ [unit]: -1n }, burn)
      .deregister.Stake(
        SDK.scriptRewardAddress("Custom", certificate),
        SDK.encodeUserEventWitnessMintOrBurnRedeemer(
          h.contracts.txOrder.policyId,
        ),
      )
      .attach.CertificateValidator(certificate)
      .withdraw(phasAddress, 0n, proofData)
      .complete({ localUPLCEval: true });
    const signed = await tx.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
    return signed.toCBOR();
  };
  const settle = (claimedVerdict: SDK.OperatorVerdict = verdict) =>
    settleOrder(0, 0, claimedVerdict);
  return {
    h,
    original,
    leaf,
    datum,
    order,
    orders,
    settlement,
    bytes,
    settle,
    settleOrder,
  };
};
