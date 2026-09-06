import { inspect } from "node:util";

import {
  computeHash28,
  decodeMidgardNativeScript,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  submitMintAuthorizationCancel,
  submitMintAuthorizationEvaluate,
  submitMintAuthorizationInit,
  submitMintAuthorizationStep01,
  submitMintAuthorizationStep02,
  submitMintAuthorizationStep03EvaluateUnsatisfied,
  submitMintAuthorizationStep05,
} from "../src/mint-authorization/index.js";
import { createRawDatumPreimageRequirement } from "../src/workflow/raw-datum-preimage-prerequisite.js";
import {
  addressWitnessItemCbors,
  buildMintAuthorizationSubject,
  directionBNativeScript,
  makeMintAuthorizationEmulatorHarness,
  mintItemCborV1,
  publishMintAuthorizationReferenceScripts,
  setupMintAuthorizationScenario,
} from "./support/mint-authorization-emulator.js";
import {
  network,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

it.each(["signers", "native", "cancel"])(
  "measures maximum %s through the real registered native-policy path",
  async (shape) => {
    try {
      const h = await makeMintAuthorizationEmulatorHarness();
      const nativePayload = Buffer.from(
        "8202991554" + "820280".repeat(5460),
        "hex",
      );
      const nativeHash = computeHash28(
        Buffer.concat([Buffer.from([0]), nativePayload]),
      );
      const policy =
        shape !== "native"
          ? directionBNativeScript()
          : {
              script: decodeMidgardNativeScript(nativePayload).script,
              scriptBytesHex: nativePayload.toString("hex"),
              mintItemCbor: mintItemCborV1({
                policyId: nativeHash,
                assetName: Buffer.from("beef", "hex"),
              }),
            };
      const sourceScript = encodeMidgardVersionedScript({
        language: "NativeCardano",
        nativeScript: policy.script,
        scriptBytes: Buffer.from(policy.scriptBytesHex, "hex"),
      });
      const witnesses = addressWitnessItemCbors(shape !== "native" ? 318 : 0);
      const subject = buildMintAuthorizationSubject({
        mintItemCbors: [policy.mintItemCbor],
        scriptWitnessItemCbors: [sourceScript.toString("hex")],
        addrWitnessItemCbors: witnesses,
      });
      const scenario = await setupMintAuthorizationScenario({
        harness: h,
        subject,
      });
      const { block, setup } = scenario;
      if (block.txInclusion === null) throw new Error("missing inclusion");
      const refs = await publishMintAuthorizationReferenceScripts({
        lucid: h.funderLucid,
        contracts: h.family,
      });
      const init = await submitMintAuthorizationInit({
        lucid: h.proverLucid,
        blueprint: h.realBlueprint,
        network,
        contracts: h.family,
        category: h.category,
        catalogue: {
          policyId: h.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            h.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: h.catalogue.root,
        },
        signer: h.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      const common = {
        lucid: h.proverLucid,
        contracts: h.family,
        categoryId: h.category.categoryId,
        signer: h.proverSigner,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        witnessSet: subject.witnessSetCompact,
      };
      const one = await submitMintAuthorizationStep01({
        ...common,
        blueprint: h.realBlueprint,
        network,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: block.txInclusion,
        referenceScriptUtxo: refs[0],
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      const two = await submitMintAuthorizationStep02({
        ...common,
        threadOutRef: one.nextThreadOutRef,
        reconstruction: block.reconstruction,
        policyIndex: 0n,
        direction: 1n,
        mintItemCbors: subject.mintItemCbors,
        referenceScriptUtxo: refs[1],
      });
      const planned = planFaultProofFieldOpening({
        fieldIndex: SDK.MIDGARD_FIELD_INDEX.addressWitnesses,
        anchorTxId: block.nativeTxId,
        nativeTxCompactCbor: block.nativeTxCompactCbor,
        itemCbors: witnesses.map((item) => Buffer.from(item, "hex")),
        owner: h.proverSigner.paymentKeyHash,
        witnessSet: subject.witnessSetCompact,
        anchorWitnessSetHash: two.step03State.bad_tx_witness_set_hash,
        label: "mint maximum signer field",
      });
      expect(planned.preimage.length).toBe(
        encodeMidgardFieldPreimage(
          witnesses.map((item) => Buffer.from(item, "hex")),
        ).length,
      );
      if (shape === "signers") expect(planned.plan.tier).toBe("Certified");
      const publishedCarriageUtxos = await publishFaultProofFieldCarriage({
        lucid: h.proverLucid,
        signer: h.proverSigner,
        planned,
        publisherAddress: h.proverSigner.address,
        label: "mint maximum signers",
      });
      const certificateReference = await publishPlainReferenceScriptUtxo({
        lucid: h.funderLucid,
        script: h.contracts.fieldPreimageCertificate.mintingScript,
        label: "mint field certificate",
      });
      const certified =
        planned.plan.tier !== "Certified"
          ? { certificateUtxo: undefined }
          : await certifyFaultProofFieldCarriage({
              lucid: h.proverLucid,
              network,
              signer: h.proverSigner,
              planned,
              certificatePolicyId:
                h.contracts.fieldPreimageCertificate.policyId,
              certificateMintingScript:
                h.contracts.fieldPreimageCertificate.mintingScript,
              certificateReferenceScriptUtxo: certificateReference.utxo,
              chunkUtxos: publishedCarriageUtxos,
              compactCbor: block.nativeTxCompactCbor,
              witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
                subject.nativeTx,
              ).witnessSetCompactCbor.toString("hex"),
            });
      const rawPreimageUtxos: UTxO[] = [];
      if (shape !== "signers") {
        for (const datum of createRawDatumPreimageRequirement({
          preimage: Buffer.from(policy.scriptBytesHex, "hex"),
        }).publicationDatums) {
          const unsigned = await h.proverLucid
            .newTx()
            .pay.ToAddressWithData(
              h.proverSigner.address,
              { kind: "inline", value: datum },
              { lovelace: 80_000_000n },
            )
            .complete();
          const txHash = await (
            await unsigned.sign.withWallet().complete()
          ).submit();
          await h.proverLucid.awaitTx(txHash);
          const utxo = (
            await h.proverLucid.utxosAt(h.proverSigner.address)
          ).find((item) => item.txHash === txHash && item.datum === datum);
          if (utxo === undefined)
            throw new Error("missing published native preimage");
          rawPreimageUtxos.push(utxo);
        }
      }
      const three = await submitMintAuthorizationStep03EvaluateUnsatisfied({
        ...common,
        threadOutRef: two.nextThreadOutRef,
        scriptBytesHex: policy.scriptBytesHex,
        ...(shape !== "signers" ? { rawPreimageUtxos } : {}),
        addrTxWitsItemCbors: witnesses,
        referenceScriptUtxo: refs[2],
        publishedCarriageUtxos,
        certificateUtxo: certified.certificateUtxo,
      });
      if (shape === "cancel") {
        const [txHash, outputIndex] = three.nextThreadOutRef.split("#");
        const [thread] = await h.proverLucid.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(outputIndex) },
        ]);
        if (thread === undefined)
          throw new Error("missing staged native thread");
        const finalDatum = Data.to(
          {
            fraud_prover: h.proverSigner.paymentKeyHash,
            data: { policy_id: two.step03State.policy_id, direction: 1n },
          },
          SDK.MintAuthorizationStep05Datum,
        );
        const forged = ((ctx) =>
          Data.to(
            {
              Continue: [
                {
                  Finalize: {
                    input_index: SDK.requireInputIndex(
                      ctx,
                      thread,
                      "forged finalize",
                    ),
                    output_index: SDK.requireUniqueOutputIndex(
                      ctx.outputs,
                      (output) =>
                        output.address ===
                        h.family.steps[4].spendingScriptAddress,
                      "forged finalize",
                    ),
                  },
                },
              ],
            },
            SDK.MintAuthorizationEvaluateSpendRedeemer,
          )) satisfies BuildTxWithRedeemer;
        await expect(
          h.proverLucid
            .newTx()
            .collectFrom([thread], forged)
            .readFrom([refs[5]])
            .pay.ToContract(
              h.family.steps[4].spendingScriptAddress,
              { kind: "inline", value: finalDatum },
              thread.assets,
            )
            .addSignerKey(h.proverSigner.paymentKeyHash)
            .complete({ localUPLCEval: true }),
        ).rejects.toBeDefined();
        await expect(
          submitMintAuthorizationEvaluate({
            ...common,
            threadOutRef: three.nextThreadOutRef,
            scriptBytesHex: "820280",
            rawPreimageUtxos,
            referenceScriptUtxo: refs[5],
          }),
        ).rejects.toThrow("preimage differs");
        const cancelled = await submitMintAuthorizationCancel({
          ...common,
          threadOutRef: three.nextThreadOutRef,
          referenceScriptUtxo: refs[5],
          witnessReferenceScripts: setup.witnessReferenceScripts,
        });
        expect(cancelled.cancelledStepIndex).toBe(5);
        expect(
          await h.proverLucid.utxosAtWithUnit(
            h.family.steps[5].spendingScriptAddress,
            cancelled.computationThreadUnit,
          ),
        ).toHaveLength(0);
        expect(
          await h.proverLucid.utxosAtWithUnit(
            h.family.fraudProof.spendingScriptAddress,
            h.family.fraudProof.policyId +
              h.category.categoryId +
              block.headerHash,
          ),
        ).toHaveLength(0);
        return;
      }
      let nextThreadOutRef = three.nextThreadOutRef;
      if (shape !== "signers") {
        for (let count = 0; count < 3000; count++) {
          const step = await submitMintAuthorizationEvaluate({
            ...common,
            threadOutRef: nextThreadOutRef,
            scriptBytesHex: policy.scriptBytesHex,
            rawPreimageUtxos,
            referenceScriptUtxo: refs[5],
          });
          nextThreadOutRef = step.nextThreadOutRef;
          if (step.terminal) break;
        }
      }
      const five = await submitMintAuthorizationStep05({
        ...common,
        threadOutRef: nextThreadOutRef,
        referenceScriptUtxo: refs[4],
        witnessReferenceScripts: setup.witnessReferenceScripts,
      });
      expect(
        await h.proverLucid.utxosAtWithUnit(
          five.fraudProofAddress,
          five.fraudProofUnit,
        ),
      ).toHaveLength(1);
    } catch (cause) {
      throw new Error(inspect(cause, { depth: 20 }));
    }
  },
  600_000,
);
