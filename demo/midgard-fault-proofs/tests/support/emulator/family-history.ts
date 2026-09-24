/** Real list initialization/admission for complete fabricated-family fixtures. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type TxSignBuilder,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { type makeFaultProofEmulatorHarness } from "./harness.js";
import { index, same } from "./history-pair.js";
import { measureCompleteSignedTransaction } from "./measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./protocol-parameters.js";

export const recordFamilyTransaction = (
  records: unknown[],
  label: string,
  signed: TxSigned,
) => {
  const transactionCbor = signed.toCBOR();
  const measurement = measureCompleteSignedTransaction(transactionCbor);
  expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
  );
  expect(measurement.executionMemory).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
  );
  expect(measurement.executionSteps).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
  );
  records.push({
    label,
    txHash: signed.toHash(),
    transactionCbor,
    measurement,
    fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
  });
};

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
export const prepareFamilyHistory = async (h: Harness, records: unknown[]) => {
  const lucid = h.proverLucid;
  h.proverSigner.selectWallet(lucid);
  const submit = async (label: string, unsigned: TxSignBuilder) => {
    const signed = await unsigned.sign.withWallet().complete();
    const transactionCbor = signed.toCBOR();
    const measurement = measureCompleteSignedTransaction(transactionCbor);
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    expect(measurement.executionMemory).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    );
    expect(measurement.executionSteps).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
    );
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    records.push({
      label,
      txHash,
      transactionCbor,
      measurement,
      fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
    });
    return txHash;
  };
  let split = lucid.newTx();
  for (let i = 0; i < 4; i++)
    split = split.pay.ToAddress(h.proverSigner.address, {
      lovelace: 10_000_000n,
    });
  const splitHash = await submit(
    "history-nonces",
    await split.complete({ localUPLCEval: true }),
  );
  const reserved = await lucid.utxosByOutRef(
    Array.from({ length: 4 }, (_, outputIndex) => ({
      txHash: splitHash,
      outputIndex,
    })),
  );
  reserved.sort((a, b) => a.outputIndex - b.outputIndex);
  expect(reserved).toHaveLength(4);
  const funding = async () =>
    (await lucid.wallet().getUtxos()).filter(
      (u) => !reserved.some((n) => same(u, n)) && u.scriptRef == null,
    );
  const recipes = (["Deposit", "Withdrawal"] as const).map(
    (kind, i): SDK.EventHistoryRecipe => ({
      kind,
      hubPolicyId: h.contracts.hubOracle.policyId,
      initializationNonce: {
        transactionId: reserved[i]!.txHash,
        outputIndex: BigInt(reserved[i]!.outputIndex),
      },
      protectionDurationMs: 120_000n,
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
    }),
  );
  const applied = recipes.map((recipe) =>
    SDK.applyEventHistoryValidators(
      SDK.parseFaultProofBlueprint(h.realBlueprint),
      "Custom",
      recipe,
    ),
  );
  records.push({
    label: "applied-family-deployment",
    recipes,
    historyPolicies: applied.map((a) => a.policyId),
    retentionAddresses: applied.map((a) => a.retention.address),
    hubPolicy: h.contracts.hubOracle.policyId,
    queuePolicy: h.contracts.stateQueue.policyId,
    queueSpendHash: h.contracts.stateQueue.spendingScriptHash,
    proofPolicy: h.contracts.fraudProof.policyId,
    cataloguePolicy: h.contracts.fraudProofCatalogue.policyId,
    catalogueRoot: h.catalogue.root,
    protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
  });
  const scripts: UTxO[] = [];
  for (const a of applied) {
    const txHash = await submit(
      "publish-history-list",
      await lucid
        .newTx()
        .collectFrom(await funding())
        .pay.ToAddressWithData(
          h.contracts.hubOracle.spendingScriptAddress,
          undefined,
          { lovelace: 100_000_000n },
          a.validator,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    scripts.push((await lucid.utxosByOutRef([{ txHash, outputIndex: 0 }]))[0]!);
  }
  let register = lucid
    .newTx()
    .collectFrom(await funding())
    .readFrom(scripts);
  for (const a of applied)
    register = register.register.Stake(a.rewardAddress, Data.void());
  await submit(
    "register-history-observers",
    await register.complete({ coinSelection: false, localUPLCEval: true }),
  );
  const inputs = [...(await funding()), reserved[0]!, reserved[1]!];
  const validTo = h.emulator.now() + 10_000;
  let init = lucid
    .newTx()
    .collectFrom(inputs)
    .readFrom(scripts)
    .validFrom(h.emulator.now())
    .validTo(validTo);
  for (let i = 0; i < 2; i++) {
    const a = applied[i]!;
    const root: SDK.EventHistoryNode = {
      position: "Root",
      next: null,
      protected_until: BigInt(validTo - 1) + recipes[i]!.protectionDurationMs,
      payload: "RootContent",
    };
    init = init
      .withdraw(
        a.rewardAddress,
        0n,
        Data.to(
          {
            Initialize: {
              nonce_input_index: index(inputs, reserved[i]!),
              root_output_index: BigInt(i),
            },
          },
          SDK.EventHistoryObserve,
        ),
      )
      .mintAssets({ [a.policyId]: 1n }, Data.void())
      .pay.ToContract(
        a.address,
        { kind: "inline", value: Data.to(root, SDK.EventHistoryNode) },
        { lovelace: 3_000_000n, [a.policyId]: 1n },
      );
  }
  await submit(
    "initialize-history-lists",
    await init.complete({ coinSelection: false, localUPLCEval: true }),
  );
  h.emulator.awaitSlot(130);
  const validator = (i: number): SDK.AuthenticatedValidator => {
    const a = applied[i]!;
    return {
      policyId: a.policyId,
      mintingScript: a.validator,
      mintingScriptCBOR: a.validator.script,
      spendingScript: a.validator,
      spendingScriptCBOR: a.validator.script,
      spendingScriptHash: a.policyId,
      spendingScriptAddress: a.address,
    };
  };
  const contracts = {
    ...h.contracts,
    deposit: validator(0),
    withdrawal: validator(1),
  };
  return {
    contracts,
    recipes,
    applied,
    scripts,
    submit,
    nonce: (kind: SDK.EventHistoryKind) =>
      reserved[kind === "Deposit" ? 2 : 3]!,
    admit: async (
      hub: UTxO,
      payload: SDK.EventHistoryPayload,
      header: SDK.Header,
    ) => {
      const i = "DepositPayload" in payload ? 0 : 1;
      const a = applied[i]!;
      const recipe = recipes[i]!;
      const validTo = Number(header.endTime) + 1 - SDK.EVENT_WAIT_DURATION_MS;
      const context = {
        lucid,
        applied: a,
        recipe,
        hubReference: hub,
        scriptReference: scripts[i]!,
        fundingInputs: await funding(),
      };
      const reclaimAuth: SDK.CredentialD = {
        PublicKeyCredential: [h.proverSigner.paymentKeyHash],
      };
      const plan = SDK.prepareEventHistoryPayload(payload, reclaimAuth, recipe);
      let externalData: UTxO | undefined;
      if (plan.kind === "External") {
        const publication = await SDK.buildEventHistoryPublication(
          context,
          payload,
          reclaimAuth,
        );
        const txHash = await submit(
          "prepublish-family-history",
          publication.tx,
        );
        externalData = (
          await lucid.utxosByOutRef([
            { txHash, outputIndex: publication.publicationOutputIndex },
          ])
        )[0]!;
      }
      const target = validTo - 10_000;
      if (h.emulator.now() < target)
        h.emulator.awaitSlot(Math.ceil((target - h.emulator.now()) / 1000));
      expect(h.emulator.now()).toBeLessThan(validTo);
      const admission = await SDK.buildEventHistoryAdmission(
        { ...context, fundingInputs: await funding() },
        {
          payload,
          reclaimAuth,
          nonce: reserved[i + 2]!,
          assets: { lovelace: i === 0 ? 25_000_000n : 10_000_000n },
          structuralLovelace: i === 0 ? 5_000_000n : 0n,
          structuralRefundKey: h.proverSigner.paymentKeyHash,
          externalData,
          validFrom: h.emulator.now() - 60_000,
          validTo,
        },
      );
      await submit("admit-family-history", admission.tx);
      const id =
        "DepositPayload" in payload
          ? payload.DepositPayload.event.id
          : payload.WithdrawalPayload.event.id;
      const witness = await SDK.fetchEventHistoryWitness(
        { utxosAt: (address) => lucid.utxosAt(address) },
        {
          policyId: a.policyId,
          address: a.address,
          retentionAddress: a.retention.address,
          inlineLimitBytes: recipe.inlineLimitBytes,
        },
        id,
      );
      expect(witness.kind).toBe("Present");
      if (witness.kind !== "Present")
        throw new Error("Admitted event is missing");
      const captured = SDK.captureEventHistoryWitness(
        witness,
        a.policyId,
        recipe.kind,
      );
      return {
        witness,
        captured,
        openingCbor: Data.to(
          {
            RetainedEventData: {
              payload: captured.payload,
              original_assets: captured.originalAssets,
            },
          },
          SDK.FabricatedDepositAuthenticContentOpening,
        ),
        raw: {
          network: "Custom" as const,
          history: { ...recipe, retentionAddress: a.retention.address },
          hubOraclePolicyId: recipe.hubPolicyId,
          hubOracleUtxo: hub,
          anchor: witness.anchor.utxo,
          ...(externalData ? { retainedDataUtxo: externalData } : {}),
        },
      };
    },
  };
};
