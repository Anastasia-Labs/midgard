import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join, parse } from "node:path";

import { outRefLabel } from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimage,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { assetsToValue, CML } from "@lucid-evolution/lucid";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { afterAll } from "vitest";
import { describe, expect, it, vi } from "vitest";

import {
  FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY,
  resolveProverSigner,
  submitRemoveFraudulentBlock,
  submitTransitionTraceProof,
} from "../../src/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../../src/proof-fit/van-rossem-fit-ledger.js";
import {
  type TransitionDepositOpening,
  transitionDepositOpening,
} from "../../src/transition-trace/history-opening.js";
import {
  submitTransitionTraceCancel,
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
} from "../../src/transition-trace/index.js";
import {
  resolveTransitionTraceByteCarriage,
  transitionTraceByteChunks,
} from "../../src/transition-trace/proof-carriage.js";
import * as yieldDataModule from "../../src/transition-trace/yield-data.js";
import * as structuredDataModule from "../../src/workflow/structured-data-preimage.js";
import { captureLocallyEvaluatedTransaction } from "../../src/workflow/transaction-boundary.js";
import { measureCompleteSignedTransaction } from "./emulator/measurement.js";
import { createReferenceScriptPublisher } from "./emulator/reference-script-publisher.js";
import {
  publishFaultProofWitnessReferenceScripts,
  publishOperatorLifecycleReferenceScripts,
} from "./emulator/reference-scripts.js";
import { makeIsolatedAlwaysSucceedsAuthenticatedValidator } from "./emulator/validators.js";
import { submitInit } from "./legacy-submit-emulator.js";
import { expectStateQueueHeaderOrder } from "./submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildRemovalDeploymentInfo,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  fundedProverEmulatorAccount,
  network,
  publishFraudProofChainReferenceScripts,
  publishRemovalReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import {
  buildAcceptedClaimTransitionFixture,
  buildAcceptedTransitionFixture,
  buildDepositTransitionFixture,
} from "./transition-trace-final-fixtures.js";
import { publishTransitionTraceYields } from "./transition-trace-yields.js";

export type TransitionTraceFinalCase = Readonly<{
  assetCount: number;
  outputCount: number;
  outputBytes?: number;
  fieldBytes?: number;
  honest?: boolean;
  depth?: number;
  kind?: string;
  cancelAt?: number;
  corruptAssetIndex?: boolean;
  corruptDatum?: boolean;
  corruptSourceReference?: boolean;
  datumBytes?: number;
}>;

/** Each test file registers its own suite, measurement rows and module spies. */
export const registerTransitionTraceFinalCases = (
  cases: readonly TransitionTraceFinalCase[],
  fitLedgerSuffix?: "many-assets" | "deep-deposit",
): void => {
  const fitRows: VanRossemFitMeasurement[] = [];
  const records: unknown[] = [];
  const measuredBlueprintSha256 = createHash("sha256")
    .update(readFileSync(realBlueprintPath))
    .digest("hex");
  afterAll(async () => {
    const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
    if (directory !== undefined) {
      mkdirSync(directory, { recursive: true });
      writeFileSync(
        join(directory, `transition-final-${fitLedgerSuffix ?? "cases"}.json`),
        JSON.stringify(
          {
            scope:
              "Synthetic proof-consumer stress; scaffold event policy, not genuine admission or live acceptance",
            blueprintSha256: measuredBlueprintSha256,
            protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
            records,
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
    const configuredPath = process.env.TRANSITION_TRACE_FIT_LEDGER_PATH;
    // Split files emit sibling ledgers instead of overwriting each other's rows.
    const parsed = configuredPath ? parse(configuredPath) : undefined;
    const path =
      parsed !== undefined && fitLedgerSuffix !== undefined
        ? join(parsed.dir, `${parsed.name}-${fitLedgerSuffix}${parsed.ext}`)
        : configuredPath;
    if (path)
      await writeVanRossemFitLedger(
        path,
        buildVanRossemFitLedger({
          category: "transitionTrace",
          blueprintSha256: measuredBlueprintSha256,
          compilerVersion: "aiken v1.1.23+5adf783",
          measurements: fitRows,
        }),
      );
  });
  describe("fault-proof emulator integration", () => {
    it.each(cases)(
      "publishes accepted semantic yields and removes a wrong transition root assets=$assetCount outputs=$outputCount bytes=$outputBytes field=$fieldBytes honest=$honest depth=$depth kind=$kind cancel=$cancelAt datum=$datumBytes corruptIndex=$corruptAssetIndex corruptDatum=$corruptDatum corruptSource=$corruptSourceReference",
      async ({
        assetCount,
        outputCount,
        outputBytes = 0,
        fieldBytes = 0,
        honest = false,
        depth = 0,
        kind = "accepted",
        cancelAt = -1,
        corruptAssetIndex = false,
        corruptDatum = false,
        corruptSourceReference = false,
        datumBytes = 0,
      }) => {
        const realBlueprint = readBlueprint(realBlueprintPath);
        const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
        const names = new Map<string, bigint>();
        for (let i = 0; i < assetCount; i++) {
          const name =
            i === 0
              ? Buffer.alloc(0)
              : i <= 256
                ? Buffer.from([i - 1])
                : Buffer.from([(i - 257) >> 8, (i - 257) & 255]);
          names.set(
            name.toString("hex"),
            i === assetCount - 1 ? (kind === "deposit" ? 65536n : 256n) : 1n,
          );
        }
        const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
        const prover = fundedProverEmulatorAccount(20_000_000_000n);
        const depositor = generateEmulatorAccount({ lovelace: 100_000_000n });
        if (kind === "deposit")
          for (const [name, quantity] of names)
            depositor.assets["44".repeat(28) + name] = quantity;
        const emulator = new Emulator(
          [funder, prover, depositor],
          EMULATOR_PROTOCOL_PARAMETERS,
        );
        const submit = emulator.submitTx.bind(emulator);
        let measuredIndex = 0;
        emulator.submitTx = async (cbor) => {
          const result = await submit(cbor);
          const m = measureCompleteSignedTransaction(cbor);
          expect(m.executionMemory).toBeLessThanOrEqual(13_200_000n);
          const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
          let publication = false;
          for (let i = 0; i < outputs.len(); i++)
            if (outputs.get(i).script_ref() != null) publication = true;
          const shape = `assets-${assetCount}-outputs-${outputCount}-bytes-${outputBytes}-field-${fieldBytes}-honest-${honest}-depth-${depth}-kind-${kind}-cancel-${cancelAt}-datum-${datumBytes}-corrupt-index-${corruptAssetIndex}-corrupt-datum-${corruptDatum}-corrupt-source-${corruptSourceReference}`;
          records.push({
            label: `${shape}/tx-${measuredIndex}`,
            txHash: result,
            transactionCbor: cbor,
            measurement: m,
            fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
          });
          fitRows.push({
            name: `${shape}/tx-${measuredIndex++}`,
            kind: publication ? "publication" : "lifecycle",
            maximumShape: shape,
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          });
          if (
            process.env.TRANSITION_TRACE_FIT_PROGRESS === "1" &&
            measuredIndex % 100 === 0
          )
            console.info(
              `transitionTrace ${shape}: ${measuredIndex} measured transactions; latest memory ${m.executionMemory}`,
            );
          return result;
        };
        const funderLucid = await Lucid(emulator, "Custom");
        const proverLucid = await Lucid(emulator, "Custom");
        funderLucid.selectWallet.fromSeed(funder.seedPhrase);
        const proverSigner = resolveProverSigner({
          network,
          walletSeedPhrase: prover.seedPhrase,
        });
        // Selected through the signer so the prover Lucid instance and every
        // `signer.selectWallet(lucid)` call site address the same funded wallet.
        proverSigner.selectWallet(proverLucid);

        await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
        const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
          await createReferenceScriptPublisher(funderLucid, emulator.now());
        // Explicit stress-fixture parameters, shared by every applied proof recipe.
        // This scaffold exercises proof consumers, not genuine list admission.
        const historyBounds = {
          inlineLimitBytes: 512n,
          maxPayloadBytes: 14000n,
          maxPayloadNodes: 512n,
        };
        const baseContracts = {
          ...(await buildMinimalFaultProofContracts(
            realBlueprint,
            alwaysBlueprint,
            nonceUtxo,
            {
              realTransitionTrace: true,
              eventHistoryBounds: historyBounds,
              alwaysFraudProofCatalogue: true,
              referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
            },
          )),
          deposit: makeIsolatedAlwaysSucceedsAuthenticatedValidator(),
          referenceScriptAuth,
          referenceScriptPublisher,
        };
        // Operator registration and activation source their four directory
        // validators from published reference scripts. Published from the prover
        // wallet before the header clock is sampled so the funder's nonce UTxO
        // survives and the whole fixture timeline shifts uniformly.
        const contracts = {
          ...baseContracts,
          operatorLifecycleReferenceScripts:
            await publishOperatorLifecycleReferenceScripts({
              lucid: proverLucid,
              contracts: baseContracts,
            }),
        };
        const catalogue = await buildCatalogueDeploymentInfo(
          contracts.fraudProofs,
        );
        const witnessReferenceScripts =
          await publishFaultProofWitnessReferenceScripts({
            lucid: proverLucid,
            realBlueprint,
            computationThreadMintingScript:
              contracts.computationThread.mintingScript,
            fraudProofMintingScript: contracts.fraudProof.mintingScript,
          });
        // See `publishRemovalReferenceScripts`: removal must source these seven
        // validators from reference inputs to stay inside the 16,384-byte L1
        // envelope. Published before the header clock is sampled so the whole
        // timeline shifts uniformly.
        const removalReferenceScriptPublications =
          await publishRemovalReferenceScripts({
            lucid: proverLucid,
            contracts,
          });
        const transitionTraceReferenceScripts =
          await publishFraudProofChainReferenceScripts({
            lucid: proverLucid,
            steps: contracts.fraudProofContracts.transitionTrace.steps,
            entryNames:
              FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY.transitionTrace,
            familyLabel: "transition-trace",
          });
        const yieldReferences = await publishTransitionTraceYields(
          proverLucid,
          contracts,
        );
        const funderPaymentCredential = getAddressDetails(
          await funderLucid.wallet().address(),
        ).paymentCredential;
        if (
          funderPaymentCredential === undefined ||
          funderPaymentCredential.type !== "Key"
        ) {
          throw new Error(
            "Expected funder wallet to expose a payment key hash",
          );
        }
        const headerStartTime =
          alignUnixTimeToEmulatorSlotBoundary(
            funderLucid,
            emulator.now() + (kind === "deposit" ? 240_000 : 120_000),
          ) - 1;
        const makeOutput = (padding: number, script: boolean) =>
          encodeMidgardTxOutput({
            address: Buffer.from("60" + "aa".repeat(28), "hex"),
            value: {
              lovelace:
                datumBytes > 0
                  ? 90_000_000n
                  : kind === "deposit" && assetCount > 0
                    ? 30_000_000n
                    : 2_000_000n,
              assets:
                assetCount === 0
                  ? new Map()
                  : new Map([["44".repeat(28), names]]),
            },
            ...(datumBytes > 0
              ? {
                  datum: {
                    kind: "inline" as const,
                    cbor: Buffer.from(Data.to("ab".repeat(datumBytes)), "hex"),
                  },
                }
              : {}),
            ...(script
              ? {
                  script_ref: {
                    language: "PlutusV3" as const,
                    scriptBytes: Buffer.alloc(padding),
                  },
                }
              : {}),
          });
        let padding = 0;
        while (
          outputBytes > 0 &&
          makeOutput(padding, true).length < outputBytes
        )
          padding++;
        const outputCbor = makeOutput(padding, outputBytes > 0);
        if (outputBytes > 0) expect(outputCbor.length).toBe(outputBytes);
        if (datumBytes === 15841) expect(outputCbor.length).toBe(16384);
        let outputCbors = Array.from({ length: outputCount }, () => outputCbor);
        if (fieldBytes > 0) {
          let sibling = 0;
          while (
            encodeMidgardFieldPreimage([makeOutput(sibling, true), outputCbor])
              .length < fieldBytes
          )
            sibling++;
          outputCbors = [makeOutput(sibling, true), outputCbor];
          expect(encodeMidgardFieldPreimage(outputCbors).length).toBe(
            fieldBytes,
          );
        }
        let traceFixture = await buildAcceptedTransitionFixture({
          outputCbors,
          honest,
          depth,
          operatorVkey: funderPaymentCredential.hash,
          now: headerStartTime,
        });
        if (kind === "claim")
          traceFixture = await buildAcceptedClaimTransitionFixture({
            operatorVkey: funderPaymentCredential.hash,
            now: headerStartTime,
            honest,
          });
        const additionalReferenceInputs: UTxO[] = [];
        let depositOpening: TransitionDepositOpening | undefined;
        if (kind === "deposit") {
          proverLucid.selectWallet.fromSeed(depositor.seedPhrase);
          const nonce = (await proverLucid.wallet().getUtxos()).find(
            (utxo) => utxo.datum == null,
          )!;
          const id = {
            transactionId: nonce.txHash,
            outputIndex: BigInt(nonce.outputIndex),
          };
          const info: SDK.DepositInfo = {
            l2_address: {
              paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
              stakeCredential: null,
            },
            l2_network_id: 0n,
            l2_datum: datumBytes > 0 ? "ab".repeat(datumBytes) : null,
          };
          const payload: SDK.EventHistoryPayload = {
            DepositPayload: { event: { id, info } },
          };
          const reclaimKey = getAddressDetails(
            await proverLucid.wallet().address(),
          ).paymentCredential!.hash;
          const plan = SDK.prepareEventHistoryPayload(
            payload,
            { PublicKeyCredential: [reclaimKey] },
            historyBounds,
          );
          const unit = contracts.deposit.policyId + plan.key;
          const retention = SDK.applyEventHistoryRetentionValidator(
            SDK.parseFaultProofBlueprint(realBlueprint),
            "Custom",
            contracts.hubOracle.policyId,
            "Deposit",
          );
          // Publish from the prover wallet so the event nonce remains unspent.
          // The later Order transaction references this existing output.
          if (plan.kind === "External") {
            proverSigner.selectWallet(proverLucid);
            const publication = await (
              await proverLucid
                .newTx()
                .pay.ToContract(
                  retention.address,
                  { kind: "inline", value: plan.datumCbor },
                  { lovelace: 90_000_000n },
                )
                .complete({ localUPLCEval: true })
            ).sign
              .withWallet()
              .complete();
            const txHash = await publication.submit();
            await proverLucid.awaitTx(txHash);
            additionalReferenceInputs.push(
              (
                await proverLucid.utxosByOutRef([{ txHash, outputIndex: 0 }])
              )[0]!,
            );
            proverLucid.selectWallet.fromSeed(depositor.seedPhrase);
          }
          const node: SDK.EventHistoryNode = {
            position: { Key: [plan.key] },
            next: null,
            protected_until: BigInt(headerStartTime + 120_000),
            payload: {
              Order: {
                facts: {
                  event_id: id,
                  inclusion_time: BigInt(headerStartTime + 1),
                  location: plan.location,
                  structural_lovelace: 5_000_000n,
                  structural_refund_key: reclaimKey,
                },
              },
            },
          };
          const datum = Data.to(node, SDK.EventHistoryNode);
          let admission = proverLucid.newTx().collectFrom([nonce]);
          if (additionalReferenceInputs.length > 0)
            admission = admission.readFrom(additionalReferenceInputs);
          const signed = await (
            await admission
              .mintAssets({ [unit]: 1n }, Data.void())
              .attach.MintingPolicy(contracts.deposit.mintingScript)
              .pay.ToContract(
                contracts.deposit.spendingScriptAddress,
                { kind: "inline", value: datum },
                {
                  lovelace:
                    5_000_000n +
                    (datumBytes > 0
                      ? 90_000_000n
                      : kind === "deposit" && assetCount > 0
                        ? 30_000_000n
                        : 2_000_000n),
                  [unit]: 1n,
                  ...Object.fromEntries(
                    [...names].map(([name, quantity]) => [
                      "44".repeat(28) + name,
                      quantity,
                    ]),
                  ),
                },
              )
              .complete({ localUPLCEval: true })
          ).sign
            .withWallet()
            .complete();
          await proverLucid.awaitTx(await signed.submit());
          additionalReferenceInputs.push(
            await expectSingleUtxoWithUnit(
              proverLucid,
              contracts.deposit.spendingScriptAddress,
              unit,
            ),
          );
          const witness = await SDK.fetchEventHistoryWitness(
            { utxosAt: (address) => proverLucid.utxosAt(address) },
            {
              policyId: contracts.deposit.policyId,
              address: contracts.deposit.spendingScriptAddress,
              retentionAddress: retention.address,
              inlineLimitBytes: historyBounds.inlineLimitBytes,
            },
            id,
          );
          if (witness.kind !== "Present")
            throw new Error("Missing stress fixture Order");
          depositOpening = transitionDepositOpening(
            SDK.captureEventHistoryWitness(
              witness,
              contracts.deposit.policyId,
              "Deposit",
            ),
          );
          if (fitLedgerSuffix === "deep-deposit")
            expect(
              assetsToValue(witness.anchor.utxo.assets).to_cbor_bytes().length,
            ).toBe(5000);
          proverSigner.selectWallet(proverLucid);
          traceFixture = await buildDepositTransitionFixture({
            operatorVkey: funderPaymentCredential.hash,
            now: headerStartTime,
            id,
            info,
            outputCbor,
            honest,
            depth,
          });
        }
        if (kind === "deposit")
          emulator.awaitSlot(
            funderLucid.unixTimeToSlot(headerStartTime - 120_000) -
              funderLucid.currentSlot(),
          );
        const setup = await submitSetupTx({
          lucid: funderLucid,
          contracts,
          nonceUtxo,
          catalogue,
          header: traceFixture.header,
        });
        expect(setup.headerHash).toBe(traceFixture.headerHash);
        await expectStateQueueHeaderOrder({
          lucid: funderLucid,
          contracts,
          expectedHeaderHashes: [traceFixture.headerHash],
        });

        const deploymentInfo = buildRemovalDeploymentInfo(
          contracts,
          catalogue,
          {
            removalReferenceScripts:
              removalReferenceScriptPublications.published,
            fraudProofReferenceScripts: {
              ...transitionTraceReferenceScripts,
              ...yieldReferences,
            },
          },
        );
        records.push({
          label: "deployment",
          kind,
          assetCount,
          datumBytes,
          depth,
          eventHistoryBounds: historyBounds,
          hubPolicy: contracts.hubOracle.policyId,
          depositPolicy: contracts.deposit.policyId,
          deploymentInfo,
        });
        const initialize = () =>
          submitInit({
            lucid: proverLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: proverSigner,
            fraudCategory: "transitionTrace",
            fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            witnessReferenceScripts,
            awaitConfirmation: true,
          });

        let initResult = await initialize();
        expect(initResult.txHash).toHaveLength(64);
        expect(initResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
        expect(initResult.fraudCategoryName).toBe("transitionTrace");
        expect(initResult.fraudCategoryId).toBe(
          catalogue.categories.transitionTrace.categoryId,
        );
        expect(initResult.computationThreadAssetName).toBe(
          `${catalogue.categories.transitionTrace.categoryId}${traceFixture.headerHash}`,
        );

        let firstStepUtxo = await expectSingleUtxoWithUnit(
          proverLucid,
          initResult.firstStepAddress,
          initResult.computationThreadUnit,
        );
        if (cancelAt >= 0) {
          const route = await submitTransitionTraceRoute({
            lucid: proverLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: proverSigner,
            threadOutRef: outRefLabel(firstStepUtxo),
            proof: traceFixture.proof,
          });
          for (const bytes of outputCbors)
            await resolveTransitionTraceByteCarriage({
              lucid: proverLucid,
              chunks: transitionTraceByteChunks(bytes.toString("hex")),
              publish: true,
            });
          let checkpoint = (
            await proverLucid.utxosByOutRef([
              {
                txHash: route.routeOutRef.split("#")[0]!,
                outputIndex: Number(route.routeOutRef.split("#")[1]),
              },
            ])
          )[0]!;
          for (let hop = 0; hop < 6; hop++) {
            const state = Data.from(
              checkpoint.datum!,
              SDK.TransitionTraceProofCommitmentDatum,
            ).data!;
            if (state.phase === BigInt(cancelAt)) break;
            const captured = await captureLocallyEvaluatedTransaction(
              (boundary) =>
                submitTransitionTraceFinal({
                  lucid: proverLucid,
                  blueprint: realBlueprint,
                  deploymentInfo,
                  network,
                  signer: proverSigner,
                  threadOutRef: outRefLabel(checkpoint),
                  proof: traceFixture.proof,
                  additionalReferenceInputs,
                  depositOpening,
                  witnessReferenceScripts,
                  preSubmitBoundary: boundary,
                }),
            );
            expect((await proverLucid.utxosByOutRef([checkpoint])).length).toBe(
              1,
            );
            await proverLucid.awaitTx(await captured.signed.submit());
            checkpoint = await expectSingleUtxoWithUnit(
              proverLucid,
              checkpoint.address,
              initResult.computationThreadUnit,
            );
          }
          expect(
            Data.from(
              checkpoint.datum!,
              SDK.TransitionTraceProofCommitmentDatum,
            ).data!.phase,
          ).toBe(BigInt(cancelAt));
          const final =
            contracts.fraudProofContracts.transitionTrace.finals[
              route.finalIndex
            ]!;
          const reference = Object.values(transitionTraceReferenceScripts).find(
            (entry) => entry.scriptHash === final.spendingScriptHash,
          )!.utxo;
          await submitTransitionTraceCancel({
            lucid: proverLucid,
            contracts: {
              computationThread: contracts.computationThread,
              fraudProof: contracts.fraudProof,
              transitionTrace: contracts.fraudProofContracts.transitionTrace,
            },
            categoryId: catalogue.categories.transitionTrace.categoryId,
            signer: proverSigner,
            threadOutRef: outRefLabel(checkpoint),
            referenceScriptUtxo: reference,
            witnessReferenceScripts,
          });
          expect(
            await proverLucid.utxosAtWithUnit(
              checkpoint.address,
              initResult.computationThreadUnit,
            ),
          ).toEqual([]);
          await expectStateQueueHeaderOrder({
            lucid: funderLucid,
            contracts,
            expectedHeaderHashes: [traceFixture.headerHash],
          });
          initResult = await initialize();
          firstStepUtxo = await expectSingleUtxoWithUnit(
            proverLucid,
            initResult.firstStepAddress,
            initResult.computationThreadUnit,
          );
        }
        const proverPaymentCredential = getAddressDetails(
          await proverLucid.wallet().address(),
        ).paymentCredential;
        expect(proverPaymentCredential?.type).toBe("Key");
        const proverPaymentKeyHash = proverPaymentCredential!.hash;

        const submitProof = () =>
          submitTransitionTraceProof({
            lucid: proverLucid,
            blueprint: realBlueprint,
            deploymentInfo,
            network,
            signer: proverSigner,
            threadOutRef: outRefLabel(firstStepUtxo),
            proof: traceFixture.proof,
            additionalReferenceInputs,
            depositOpening,
            witnessReferenceScripts,
            awaitConfirmation: true,
          });
        if (corruptDatum) {
          const derive = structuredDataModule.structuredDataPublicationPlan;
          const replacement = vi
            .spyOn(structuredDataModule, "structuredDataPublicationPlan")
            .mockImplementation((input) => ({
              ...derive(input),
              publicationDatums: [Data.to("00")],
            }));
          try {
            await expect(submitProof()).rejects.toThrow(
              /failed script execution/u,
            );
            await expect(
              proverLucid.utxosAtWithUnit(
                contracts.fraudProof.spendingScriptAddress,
                toUnit(
                  contracts.fraudProof.policyId,
                  initResult.computationThreadAssetName,
                ),
              ),
            ).resolves.toHaveLength(0);
          } finally {
            replacement.mockRestore();
          }
          return;
        }
        if (corruptSourceReference) {
          const derive = yieldDataModule.transitionTraceYieldData;
          const replacement = vi
            .spyOn(yieldDataModule, "transitionTraceYieldData")
            .mockImplementation((input) =>
              derive(input).map((entry) => {
                if (entry.depositSourceCbor === undefined) return entry;
                const source = Data.from(
                  entry.depositSourceCbor,
                  SDK.EventHistoryCommitment,
                );
                return {
                  ...entry,
                  depositSourceCbor: Data.to(
                    {
                      ...source,
                      event_id: {
                        transactionId: "00".repeat(32),
                        outputIndex: 0n,
                      },
                    },
                    SDK.EventHistoryCommitment,
                  ),
                };
              }),
            );
          try {
            await expect(submitProof()).rejects.toThrow(
              /failed script execution/u,
            );
            await expect(
              proverLucid.utxosAtWithUnit(
                contracts.fraudProof.spendingScriptAddress,
                toUnit(
                  contracts.fraudProof.policyId,
                  initResult.computationThreadAssetName,
                ),
              ),
            ).resolves.toHaveLength(0);
          } finally {
            replacement.mockRestore();
          }
          return;
        }
        if (corruptAssetIndex) {
          const derive = yieldDataModule.transitionTraceYieldData;
          const replacement = vi
            .spyOn(yieldDataModule, "transitionTraceYieldData")
            .mockImplementation((input) =>
              derive(input).map((entry) => ({
                ...entry,
                ...(entry.depositAssetIndexes === undefined
                  ? {}
                  : {
                      depositAssetIndexes: Object.fromEntries(
                        Object.keys(entry.depositAssetIndexes).map((key) => [
                          key,
                          -1,
                        ]),
                      ),
                    }),
              })),
            );
          try {
            await expect(submitProof()).rejects.toThrow(
              /failed script execution/u,
            );
            await expect(
              proverLucid.utxosAtWithUnit(
                contracts.fraudProof.spendingScriptAddress,
                toUnit(
                  contracts.fraudProof.policyId,
                  initResult.computationThreadAssetName,
                ),
              ),
            ).resolves.toHaveLength(0);
          } finally {
            replacement.mockRestore();
          }
          return;
        }
        if (honest) {
          await expect(submitProof()).rejects.toThrow(
            /failed script execution/u,
          );
          await expect(
            proverLucid.utxosAtWithUnit(
              contracts.fraudProof.spendingScriptAddress,
              toUnit(
                contracts.fraudProof.policyId,
                initResult.computationThreadAssetName,
              ),
            ),
          ).resolves.toHaveLength(0);
          return;
        }
        const proofResult = await submitProof();

        expect(proofResult.txHash).toHaveLength(64);
        expect(proofResult.fraudulentHeaderHash).toBe(traceFixture.headerHash);
        expect(proofResult.fraudProofAssetName).toBe(
          initResult.computationThreadAssetName,
        );
        expect(proofResult.fraudProofUnit).toBe(
          toUnit(
            contracts.fraudProof.policyId,
            initResult.computationThreadAssetName,
          ),
        );
        expect(proofResult.fraudProofMintRedeemerIndex).not.toBe(
          proofResult.computationThreadMintRedeemerIndex,
        );
        await expect(
          proverLucid.utxosAtWithUnit(
            initResult.firstStepAddress,
            initResult.computationThreadUnit,
          ),
        ).resolves.toHaveLength(0);

        const fraudProofUtxo = await expectSingleUtxoWithUnit(
          proverLucid,
          proofResult.fraudProofAddress,
          proofResult.fraudProofUnit,
        );
        expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
          fraud_prover: proverPaymentKeyHash,
        });

        const removeNow = BigInt(emulator.now());
        const removeResult = await submitRemoveFraudulentBlock({
          lucid: proverLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: proverSigner,
          fraudCategory: "transitionTrace",
          fraudulentHeaderHash: traceFixture.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
          validTo: removeNow + 300_000n,
        });

        expect(removeResult.fraudCategory).toBe("transitionTrace");
        expect(removeResult.fraudCategoryId).toBe(
          catalogue.categories.transitionTrace.categoryId,
        );
        expect(removeResult.stateQueueMutationLease).toBeNull();
        expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
          "remove-target",
        ]);
        expect(
          removeResult.transactions.map((tx) => tx.removedHeaderHash),
        ).toEqual([traceFixture.headerHash]);
        expect(
          removeResult.transactions.map((tx) => tx.slashingApproach),
        ).toEqual(["SlashActiveOperator"]);
        await expectStateQueueHeaderOrder({
          lucid: funderLucid,
          contracts,
          expectedHeaderHashes: [],
        });
        await expect(
          funderLucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            setup.stateQueueBlockUnit,
          ),
        ).resolves.toHaveLength(0);
        const retainedFraudProof = await expectSingleUtxoWithUnit(
          proverLucid,
          proofResult.fraudProofAddress,
          proofResult.fraudProofUnit,
        );
        expect(outRefLabel(retainedFraudProof)).toBe(
          outRefLabel(fraudProofUtxo),
        );
        expect(retainedFraudProof.assets[proofResult.fraudProofUnit]).toBe(1n);
      },
      1_800_000,
    );
  });
};
