import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { decodeMidgardTxOutput, outRefLabel } from "@al-ft/midgard-core";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import { submitInit } from "../src/submit-init.js";
import { transitionDepositOpening } from "../src/transition-trace/history-opening.js";
import {
  resolveTransitionTraceByteCarriage,
  transitionTraceByteChunks,
  transitionTraceProofChunks,
} from "../src/transition-trace/proof-carriage.js";
import {
  makeTransitionProofMaterial,
  transitionProofHistorySource,
} from "../src/transition-trace/proof-material.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { deriveTransitionTraceReplayEvidence } from "../src/transition-trace/replay.js";
import {
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
} from "../src/transition-trace/submit.js";
import {
  buildTransitionFaultProof,
  buildValidDepositTransitionWitness,
} from "../src/transition-trace/witnesses.js";
import { transitionTraceYieldData } from "../src/transition-trace/yield-data.js";
import { captureLocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  awaitHeaderCommitWindow,
  FAMILY_HISTORY_HEADER_LEAD_MS,
  prepareFamilyHistory,
} from "./support/emulator/family-history.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { fixtureLedgerAfter } from "./support/fixture-ledger.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  RAW_HISTORY_ROWS,
  rawDatum,
} from "./support/transition-raw-history-datums.js";
import { depositEventsRetainedBlock } from "./support/transition-trace-retained.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

const records: unknown[] = [];
const originalAssets = { lovelace: 20_000_000n };
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "transition-raw-history.json"),
    JSON.stringify(
      {
        scope:
          "Applied public raw Deposit admission and transition proof consumers; fixture catalogue governance and retained header construction, not node-produced settlement or live acceptance.",
        blueprintSha256: createHash("sha256")
          .update(readFileSync(realBlueprintPath))
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        rawDatum,
        originalAssets,
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

it.each(RAW_HISTORY_ROWS)(
  "proves a raw Deposit datum through applied transition stages: $name; honest=$honest",
  async ({ name, datum: datumRow, honest, bounds }) => {
    const h = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realTransitionTrace: true,
        alwaysFraudProofCatalogue: true,
        ...(bounds === undefined ? {} : { eventHistoryBounds: bounds }),
      },
    });
    const submit = h.emulator.submitTx.bind(h.emulator);
    const signedDatums: string[] = [];
    const signedOpenings: string[] = [];
    h.emulator.submitTx = async (cbor) => {
      const transaction = CML.Transaction.from_cbor_hex(cbor);
      const measurement = measureCompleteSignedTransaction(cbor);
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
      );
      expect(measurement.executionMemory).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
      );
      expect(measurement.executionSteps).toBeLessThanOrEqual(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
      );
      const txHash = await submit(cbor);
      const outputs = transaction.body().outputs();
      for (let index = 0; index < outputs.len(); index++) {
        const datum = outputs.get(index).datum()?.as_datum();
        if (datum !== undefined) signedDatums.push(datum.to_cbor_hex());
      }
      const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
      for (let index = 0; index < (redeemers?.len() ?? 0); index++) {
        const raw = redeemers!.get(index).data().to_cbor_hex();
        // Other scripts have different redeemer schemas; only inspect a decoded
        // transition Continue's optional opening without reserializing its Data.
        let decoded: SDK.TransitionTraceYieldFinalSpendRedeemer;
        try {
          decoded = Data.from(raw, SDK.TransitionTraceYieldFinalSpendRedeemer);
        } catch {
          continue;
        }
        if (
          "Continue" in decoded &&
          decoded.Continue[0].deposit_opening !== null
        )
          signedOpenings.push(plutusConstrFieldCbor(raw, [0, 9, 0]));
      }
      records.push({
        label: `${name}/honest-${honest}/accepted`,
        txHash,
        transactionCbor: cbor,
        measurement,
        fee: transaction.body().fee(),
      });
      return txHash;
    };
    const history = await prepareFamilyHistory(h, records, bounds);
    const contracts = history.contracts;
    const yields = await publishTransitionTraceYields(h.proverLucid, contracts);
    const removal = await publishRemovalReferenceScripts({
      lucid: h.proverLucid,
      contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, h.catalogue, {
      removalReferenceScripts: removal.published,
      fraudProofReferenceScripts: {
        ...h.faultProofReferenceScripts,
        ...yields,
      },
    });
    const nonce = history.nonce("Deposit");
    const id = {
      transactionId: nonce.txHash,
      outputIndex: BigInt(nonce.outputIndex),
    };
    const depositPayload = (datum: string) => {
      const event = aikenSerialisedPlutusDataCborPreservingMapOrder(
        replacePlutusConstrFieldCbor(
          Data.to(
            {
              id,
              info: {
                l2_address: {
                  paymentCredential: { PublicKeyCredential: ["aa".repeat(28)] },
                  stakeCredential: null,
                },
                l2_network_id: 0n,
                l2_datum: 0n,
              },
            },
            SDK.DepositEvent,
          ),
          [1, 2, 0],
          datum,
        ),
      );
      return {
        eventCbor: event,
        payloadCbor: replacePlutusConstrFieldCbor("d8799f00ff", [0], event),
      };
    };
    // The committed bytes are serialiseData form.
    const datum = aikenSerialisedPlutusDataCborPreservingMapOrder(datumRow);
    const { eventCbor, payloadCbor } = depositPayload(datum);
    const now =
      alignUnixTimeToEmulatorSlotBoundary(
        h.funderLucid,
        h.emulator.now() + FAMILY_HISTORY_HEADER_LEAD_MS,
      ) - 1;
    const retained = await depositEventsRetainedBlock({
      operatorVkey: await funderPaymentKeyHash(h.funderLucid),
      startTime: BigInt(now),
      endTime: BigInt(now + 1000),
      blockSlot: 0n,
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      priorLedger: [],
      events: [{ eventCbor, originalAssets, honest }],
    });
    const recipe = history.recipes[0]!;
    const applied = history.applied[0]!;
    const setup = await submitSetupTx({
      lucid: h.funderLucid,
      contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: retained.header,
      beforeHeaderCommit: async (hubReference) => {
        const validTo =
          Number(retained.header.endTime) + 1 - SDK.EVENT_WAIT_DURATION_MS;
        const reserved = new Set(
          [nonce, history.nonce("Withdrawal")].map(outRefLabel),
        );
        const funding = async () =>
          (await h.proverLucid.wallet().getUtxos()).filter(
            (utxo) =>
              !reserved.has(outRefLabel(utxo)) &&
              utxo.datum == null &&
              utxo.scriptRef == null,
          );
        const context = {
          lucid: h.proverLucid,
          applied,
          recipe,
          hubReference,
          scriptReference: history.scripts[0]!,
          fundingInputs: await funding(),
        };
        const reclaimAuth: SDK.CredentialD = {
          PublicKeyCredential: [h.proverSigner.paymentKeyHash],
        };
        // A payload above the inline limit is published to the retention
        // address first and admitted against that output.
        let externalData: UTxO | undefined;
        if (
          SDK.prepareEventHistoryPayloadCbor(payloadCbor, reclaimAuth, recipe)
            .kind === "External"
        ) {
          const publication = await SDK.buildEventHistoryPublication(
            context,
            payloadCbor,
            reclaimAuth,
            validTo,
          );
          const txHash = await history.submit(
            "raw-deposit-publication",
            publication.tx,
          );
          [externalData] = await h.proverLucid.utxosByOutRef([
            { txHash, outputIndex: publication.publicationOutputIndex },
          ]);
          context.fundingInputs = await funding();
        }
        const target = validTo - 10_000;
        if (h.emulator.now() < target)
          h.emulator.awaitSlot(Math.ceil((target - h.emulator.now()) / 1000));
        expect(h.emulator.now()).toBeLessThan(validTo);
        const admission = await SDK.buildEventHistoryAdmission(context, {
          payloadCbor,
          reclaimAuth,
          ...(externalData === undefined ? {} : { externalData }),
          nonce,
          assets: { lovelace: 25_000_000n },
          structuralLovelace: 5_000_000n,
          structuralRefundKey: h.proverSigner.paymentKeyHash,
          validFrom: h.emulator.now() - 60_000,
          validTo,
        });
        await history.submit("raw-deposit-admission", admission.tx);
        awaitHeaderCommitWindow(h.emulator, retained.header);
      },
    });
    const witness = await SDK.fetchEventHistoryWitness(
      { utxosAt: (address) => h.proverLucid.utxosAt(address) },
      {
        policyId: applied.policyId,
        address: applied.address,
        retentionAddress: applied.retention.address,
        inlineLimitBytes: recipe.inlineLimitBytes,
      },
      id,
    );
    if (witness.kind !== "Present")
      throw new Error("Missing admitted raw Deposit");
    const captured = SDK.captureEventHistoryWitness(
      witness,
      applied.policyId,
      "Deposit",
    );
    expect(captured.payloadCbor).toBe(payloadCbor);
    expect(SDK.valueToAssets(captured.originalAssets)).toEqual(originalAssets);
    expect(plutusConstrFieldCbor(captured.payloadCbor, [0, 1, 2, 0])).toBe(
      datum,
    );
    const opening = transitionDepositOpening(captured);
    const current = await reconstructDaPayload({
      payloadEnvelopeCbor: retained.payloadEnvelopeCbor,
      expectedHeaderHash: retained.headerHash,
      committedHeader: retained.header,
    });
    const replay = await deriveTransitionTraceReplayEvidence({
      current,
      deposits: [{ history: opening }],
      network: "Custom",
      depositPolicyId: applied.policyId,
    });
    expect(replay.depositTransitions).toHaveLength(1);
    const typedProof = buildTransitionFaultProof({
      reconstruction: current,
      fault: {
        InvalidOneStepTransition: {
          witness: await buildValidDepositTransitionWitness({
            reconstruction: current,
            stepIndex: 0n,
            evidence: replay.depositTransitions![0]!,
          }),
        },
      },
    });
    const proof = makeTransitionProofMaterial(current, typedProof);
    const sourceCbor = plutusConstrFieldCbor(eventCbor, [1]);
    expect(transitionProofHistorySource(proof)?.valueCbor).toBe(sourceCbor);
    // The typed proof carries the committed leaf bytes, so even a datum the
    // typed deposit view would collapse reaches the material unchanged.
    expect(proof.proofCbor).toBe(Data.to(typedProof, SDK.TransitionFaultProof));
    expect(
      decodeMidgardTxOutput(
        Buffer.from(fixtureLedgerAfter(retained.header)[0]![1], "hex"),
      ).datum,
    ).toEqual({ kind: "inline", cbor: Buffer.from(datum, "hex") });
    const common = {
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo,
      network,
      signer: h.proverSigner,
      witnessReferenceScripts: h.witnessReferenceScripts,
    };
    const init = await submitInit({
      ...common,
      fraudCategory: "transitionTrace",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    });
    const route = await submitTransitionTraceRoute({
      ...common,
      threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
      proof,
    });
    const outputCbors = transitionTraceYieldData({
      proof,
      network: "Custom",
      depositPolicyId: applied.policyId,
      depositOpening: opening,
    }).find((entry) => entry.outputCbors !== undefined)!.outputCbors!;
    expect(outputCbors).toHaveLength(1);
    expect(
      decodeMidgardTxOutput(Buffer.from(outputCbors[0]!, "hex")).datum,
    ).toEqual({
      kind: "inline",
      cbor: Buffer.from(datum, "hex"),
    });
    for (const outputCbor of outputCbors)
      await resolveTransitionTraceByteCarriage({
        lucid: h.proverLucid,
        chunks: transitionTraceByteChunks(outputCbor),
        publish: true,
      });
    const [routeHash, routeIndex] = route.routeOutRef.split("#");
    let checkpoint = (
      await h.proverLucid.utxosByOutRef([
        { txHash: routeHash!, outputIndex: Number(routeIndex) },
      ])
    )[0]!;
    let terminal = false;
    for (let hop = 0; hop < 64; hop++) {
      const state = Data.from(
        checkpoint.datum!,
        SDK.TransitionTraceProofCommitmentDatum,
      ).data!;
      expect(state.proof_commitment.hash).toBe(
        transitionTraceProofChunks(proof).hash,
      );
      if (state.phase === 5n) {
        terminal = true;
        break;
      }
      const tx = await captureLocallyEvaluatedTransaction((boundary) =>
        submitTransitionTraceFinal({
          ...common,
          threadOutRef: outRefLabel(checkpoint),
          proof,
          depositOpening: opening,
          preSubmitBoundary: boundary,
        }),
      );
      await h.proverLucid.awaitTx(await tx.signed.submit());
      checkpoint = await expectSingleUtxoWithUnit(
        h.proverLucid,
        checkpoint.address,
        init.computationThreadUnit,
      );
    }
    expect(terminal).toBe(true);
    expect(signedOpenings.length).toBeGreaterThan(0);
    for (const signedOpening of signedOpenings) {
      expect(
        aikenSerialisedPlutusDataCborPreservingMapOrder(signedOpening),
      ).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(opening.openingCbor),
      );
      // Redeemer Data carries no encoding, so compare serialiseData forms.
      expect(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(signedOpening, [0, 0, 1, 2, 0]),
        ),
      ).toBe(datum);
      expect(
        Data.from(plutusConstrFieldCbor(signedOpening, [1]), SDK.Value),
      ).toEqual(captured.originalAssets);
    }
    for (const chunk of transitionTraceProofChunks(proof).chunks)
      expect(
        signedDatums.some((datum) => {
          try {
            return Data.from(datum) === chunk;
          } catch {
            return false;
          }
        }),
      ).toBe(true);
    const finish = () =>
      submitTransitionTraceFinal({
        ...common,
        threadOutRef: outRefLabel(checkpoint),
        proof,
        depositOpening: opening,
      });
    if (honest) {
      const queueBefore = await h.proverLucid.utxosByOutRef([
        {
          txHash: setup.fraudulentBlockOutRef.split("#")[0]!,
          outputIndex: Number(setup.fraudulentBlockOutRef.split("#")[1]),
        },
      ]);
      expect(queueBefore).toHaveLength(1);
      await expect(finish()).rejects.toThrow();
      expect(await h.proverLucid.utxosByOutRef([checkpoint])).toHaveLength(1);
      expect(await h.proverLucid.utxosByOutRef(queueBefore)).toEqual(
        queueBefore,
      );
      expect(
        await h.proverLucid.utxosAtWithUnit(
          contracts.fraudProof.spendingScriptAddress,
          contracts.fraudProof.policyId + init.computationThreadAssetName,
        ),
      ).toHaveLength(0);
    } else {
      const result = await finish();
      expect(result.fraudProofUnit).toBe(
        contracts.fraudProof.policyId + init.computationThreadAssetName,
      );
      expect(
        await h.proverLucid.utxosAtWithUnit(
          contracts.fraudProof.spendingScriptAddress,
          result.fraudProofUnit,
        ),
      ).toHaveLength(1);
    }
    expect(
      (await h.proverLucid.utxosByOutRef([witness.anchor.utxo]))[0]!.assets,
    ).toEqual(witness.anchor.utxo.assets);
    records.push({
      label: `${name}/honest-${honest}/raw-material`,
      deploymentInfo,
      eventCbor,
      payloadCbor,
      sourceCbor,
      opening,
      proof,
      signedOpenings,
      payloadEnvelopeCbor: retained.payloadEnvelopeCbor.toString("hex"),
      header: retained.header,
      headerHash: retained.headerHash,
      originalAssets,
    });
  },
  60_000,
);
