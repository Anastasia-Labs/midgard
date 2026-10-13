import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  getAddressDetails,
  Lucid,
  mintingPolicyToId,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import { publishForcedOrderCarriage } from "../forced-order-settlement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarness,
  makeHeader,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "../submit-init-emulator-shared.js";

export const mintReferenceOnlyOrder = async (
  capture: (label: string, cbor: string, scope: unknown) => void,
  extra: boolean,
  independentPolicy = false,
) => {
  const h = await makeFaultProofEmulatorHarness({
    contractOptions: { realTxOrder: true },
  });
  const lucid = await Lucid(h.emulator, SELECTED_DEPLOYMENT_PROFILE.network);
  h.proverSigner.selectWallet(lucid);
  const operator = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: makeHeader(
      operator.hash,
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + 120_000,
      ) - 1,
    ),
  });
  const txOrderMinting = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.txOrder.mintingScript,
      label: "order mint",
    })
  ).utxo;
  const golden = JSON.parse(
    readFileSync(
      fileURLToPath(
        new URL(
          "../../../../midgard-core/tests/fixtures/forced-tx-admission-v1.generated.json",
          import.meta.url,
        ),
      ),
      "utf8",
    ),
  );
  const row = golden.rows.find((r: { name: string }) => r.name === "base");
  const field = (index: number) => Buffer.from(row.fields[index], "hex");
  const bytes = encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: field(0),
        referenceInputsPreimageCbor: field(1),
        outputsPreimageCbor: field(2),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: BigInt(row.validityIntervalEnd),
        requiredObserversPreimageCbor: field(3),
        requiredSignersPreimageCbor: field(4),
        mintPreimageCbor: field(5),
        scriptIntegrityHash: Buffer.from(row.scriptIntegrityHash, "hex"),
        auxiliaryDataHash: Buffer.from(row.auxiliaryDataHash, "hex"),
        networkId: BigInt(row.networkId),
      },
      witnessSet: {
        addrTxWitsPreimageCbor: field(7),
        scriptTxWitsPreimageCbor: field(6),
        redeemerTxWitsPreimageCbor: field(8),
      },
    }),
  );
  const carriage = await publishForcedOrderCarriage(h, lucid, bytes);
  const nonce = (await lucid.wallet().getUtxos())
    .filter((u) => u.datum === undefined && u.datumHash === undefined)
    .sort((l, r) => Number(r.assets.lovelace - l.assets.lovelace))[0]!;
  const refundAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(await lucid.wallet().address()),
  );
  const clock = vi
    .spyOn(Date, "now")
    .mockImplementation(() => h.emulator.now());
  const built = await Effect.runPromise(
    SDK.buildUnsignedTxOrderTxWithMetadataProgram(lucid, h.contracts, {
      nonceInput: nonce,
      submittedTxCbor: bytes.toString("hex"),
      carriageReferenceInputs: carriage.referenceInputs,
      fieldPreimageCertificatePolicyId:
        h.contracts.fieldPreimageCertificate.policyId,
      refundAddress,
      lovelace: 3_000_000n,
      referenceScripts: { txOrderMinting },
    }),
  ).finally(() => clock.mockRestore());
  // The production SDK currently attaches this per-nonce certificate witness.
  // Stage it separately and rebuild BEFORE signing; all reference indices are
  // computed from Lucid's final context and local UPLC evaluation remains on.
  const original = CML.Transaction.from_cbor_hex(built.tx.toCBOR());
  const eventOutput = coreToTxOutput(original.body().outputs().get(0));
  const datum = Data.from(eventOutput.datum!, SDK.TxOrderDatum);
  const witnessScript = SDK.buildUserEventWitnessCertificateValidator(
    built.metadata.txOrderAuthUnit.slice(56),
  );
  const witnessReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: witnessScript,
      label: "per-nonce order witness",
    })
  ).utxo;
  const hub = await Effect.runPromise(
    SDK.fetchHubOracleUTxOProgram(lucid, {
      hubOracleAddress: h.contracts.hubOracle.spendingScriptAddress,
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
    }),
  );
  expect(carriage.referenceInputs).toHaveLength(0);
  const fakeId = { transactionId: "fe".repeat(32), outputIndex: 0n };
  const fakeName = await Effect.runPromise(
    SDK.hashHexWithBlake2b(Data.to(fakeId, SDK.OutputReference), 32),
  );
  const fakeUnit = h.contracts.txOrder.policyId + fakeName;
  const fakeDatum: SDK.TxOrderDatum = {
    ...datum,
    event: { ...datum.event, id: fakeId },
    witness: SDK.userEventWitnessScriptHash(fakeName),
  };
  const minted = {
    [built.metadata.txOrderAuthUnit]: 1n,
    ...(extra ? { [fakeUnit]: 1n } : {}),
  };
  let builder = lucid
    .newTx()
    .collectFrom([nonce])
    .readFrom([hub.utxo, txOrderMinting, witnessReference])
    .mintAssets(minted, (ctx) =>
      Data.to(
        {
          event: {
            AuthenticateEvent: {
              nonce_input_index: SDK.requireInputIndex(
                ctx,
                nonce,
                "measurement nonce",
              ),
              event_output_index: SDK.requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.assets[built.metadata.txOrderAuthUnit] === 1n,
                "measurement event",
              ),
              hub_ref_input_index: SDK.requireReferenceInputIndex(
                ctx,
                hub.utxo,
                "measurement hub",
              ),
              witness_registration_redeemer_index:
                SDK.requireSinglePublishRedeemerIndex(
                  ctx,
                  "measurement certificate",
                ),
            },
          },
          material_carriage: [
            ...SDK.txOrderMaterialCarriageVector({
              plan: carriage.plan,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId:
                h.contracts.fieldPreimageCertificate.policyId,
            }),
          ],
        },
        SDK.TxOrderMintRedeemer,
      ),
    )
    .pay.ToAddressWithData(
      eventOutput.address,
      { kind: "inline", value: eventOutput.datum! },
      eventOutput.assets,
    )
    .validTo(Number(datum.inclusion_time) - SDK.EVENT_WAIT_DURATION_MS + 1)
    .register.Stake(
      SDK.scriptRewardAddress(
        SELECTED_DEPLOYMENT_PROFILE.network,
        witnessScript,
      ),
      SDK.encodeUserEventWitnessMintOrBurnRedeemer(
        h.contracts.txOrder.policyId,
      ),
    );
  if (independentPolicy) {
    const issuer = scriptFromNative({
      type: "sig",
      keyHash: getAddressDetails(await lucid.wallet().address())
        .paymentCredential!.hash,
    });
    builder = builder
      .mintAssets({ [mintingPolicyToId(issuer) + "01"]: 1n })
      .attach.MintingPolicy(issuer);
  }
  if (extra)
    builder = builder.pay.ToAddressWithData(
      eventOutput.address,
      { kind: "inline", value: Data.to(fakeDatum, SDK.TxOrderDatum) },
      { lovelace: 3_000_000n, [fakeUnit]: 1n },
    );
  const fullReference = await builder.complete({ localUPLCEval: true });
  const signed = await fullReference.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const outputs = (
    await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
  ).filter((u) => u.txHash === txHash);
  expect(outputs).toHaveLength(extra ? 2 : 1);
  const authenticated = await Effect.runPromise(
    SDK.utxosToTxOrderUTxOs(outputs, h.contracts.txOrder.policyId),
  );
  expect(authenticated).toHaveLength(extra ? 2 : 1);
  capture(
    extra ? "tx-order-extra-own-policy-token" : "tx-order-base",
    signed.toCBOR(),
    {
      events: { deposits: 0, withdrawals: 0, txOrders: extra ? 2 : 1 },
      policyId: h.contracts.txOrder.policyId,
      submittedTxCbor: bytes.toString("hex"),
    },
  );
};
