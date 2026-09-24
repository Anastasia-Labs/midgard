import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { encodeMidgardTxOutput, outRefLabel } from "@al-ft/midgard-core";
import * as PlutusDataCbor from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  mintingPolicyToId,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it, vi } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/index.js";
import { submitInit } from "../src/submit-init.js";
import { transitionDepositOpening } from "../src/transition-trace/history-opening.js";
import {
  resolveTransitionTraceByteCarriage,
  transitionTraceByteChunks,
} from "../src/transition-trace/proof-carriage.js";
import {
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
} from "../src/transition-trace/submit.js";
import { captureLocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import { prepareFamilyHistory } from "./support/emulator/family-history.js";
import { insertHistoryFillerAfter } from "./support/emulator/history-pair.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
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
import { buildDepositTransitionFixture } from "./support/transition-trace-final-fixtures.js";
import { publishTransitionTraceYields } from "./support/transition-trace-yields.js";

const records: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "transition-history.json"),
    JSON.stringify(
      {
        scope:
          "Applied history admission and staged transition projection, pointer churn and serialized reopening; fixture catalogue governance, not live acceptance",
        blueprintSha256: createHash("sha256")
          .update(
            readFileSync(
              new URL("../../../onchain/aiken/plutus.json", import.meta.url),
            ),
          )
          .digest("hex"),
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ) + "\n",
  );
});

describe("applied transition history", () => {
  it.each([
    { mode: "inline", honest: false, assetCount: 0 },
    { mode: "external", honest: false, assetCount: 0 },
    { mode: "inline", honest: true, assetCount: 0 },
    { mode: "external", honest: true, assetCount: 0 },
    { mode: "inline", honest: false, assetCount: 3 },
    { mode: "external", honest: false, assetCount: 3 },
    { mode: "inline", honest: true, assetCount: 3 },
    { mode: "external", honest: true, assetCount: 3 },
    { mode: "inline", honest: false, assetCount: 9 },
    { mode: "external", honest: false, assetCount: 9 },
    { mode: "inline", honest: true, assetCount: 9 },
    { mode: "external", honest: true, assetCount: 9 },
  ] as const)(
    "reopens $mode original funds after pointer churn; honest=$honest assets=$assetCount",
    async ({ mode, honest, assetCount }) => {
      const h = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realTransitionTrace: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const submit = h.emulator.submitTx.bind(h.emulator);
      h.emulator.submitTx = async (cbor) => {
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
        records.push({
          label: `${mode}/honest-${honest}/assets-${assetCount}/transaction`,
          txHash,
          transactionCbor: cbor,
          measurement,
          fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
        });
        return txHash;
      };
      const assetPolicy = scriptFromNative({
        type: "sig",
        keyHash: h.proverSigner.paymentKeyHash,
      });
      const assetPolicyId = mintingPolicyToId(assetPolicy);
      const names = new Map(
        [
          ["", 17n],
          ["00", 18n],
          ["0000", 19n],
          ...Array.from({ length: 6 }, (_, i) => [
            (i + 1).toString(16).padStart(2, "0").repeat(32),
            i === 5 ? 9_223_372_036_854_775_807n : BigInt(i + 20),
          ]),
        ].slice(0, assetCount) as [string, bigint][],
      );
      const nativeAssets = Object.fromEntries(
        [...names].map(([name, quantity]) => [assetPolicyId + name, quantity]),
      );
      if (assetCount > 0) {
        const mint = await h.proverLucid
          .newTx()
          .mintAssets(nativeAssets)
          .attach.MintingPolicy(assetPolicy)
          .addSignerKey(h.proverSigner.paymentKeyHash)
          .complete({ localUPLCEval: true });
        await h.proverLucid.awaitTx(
          await (await mint.sign.withWallet().complete()).submit(),
        );
      }
      const history = await prepareFamilyHistory(h, records);
      const contracts = history.contracts;
      const yields = await publishTransitionTraceYields(
        h.proverLucid,
        contracts,
      );
      const removal = await publishRemovalReferenceScripts({
        lucid: h.proverLucid,
        contracts,
      });
      const deploymentInfo = buildRemovalDeploymentInfo(
        contracts,
        h.catalogue,
        {
          removalReferenceScripts: removal.published,
          fraudProofReferenceScripts: {
            ...h.faultProofReferenceScripts,
            ...yields,
          },
        },
      );
      records.push({
        label: `${mode}/honest-${honest}/assets-${assetCount}/deployment`,
        deploymentInfo,
      });
      const nonce = history.nonce("Deposit");
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
        l2_datum: mode === "external" ? "ab".repeat(2000) : null,
      };
      const outputCbor = encodeMidgardTxOutput({
        address: Buffer.from("60" + "aa".repeat(28), "hex"),
        value: {
          lovelace: 20_000_000n,
          assets:
            assetCount === 0 ? new Map() : new Map([[assetPolicyId, names]]),
        },
        ...(info.l2_datum === null
          ? {}
          : {
              datum: {
                kind: "inline" as const,
                cbor: Buffer.from(Data.to(info.l2_datum), "hex"),
              },
            }),
      });
      const now =
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 240_000,
        ) - 1;
      const fixture = await buildDepositTransitionFixture({
        operatorVkey: await funderPaymentKeyHash(h.funderLucid),
        now,
        id,
        info,
        outputCbor,
        honest,
      });
      let admitted: Awaited<ReturnType<typeof history.admit>> | undefined;
      let hub: UTxO | undefined;
      const setup = await submitSetupTx({
        lucid: h.funderLucid,
        contracts,
        nonceUtxo: h.nonceUtxo,
        catalogue: h.catalogue,
        header: fixture.header,
        beforeHeaderCommit: async (oracle) => {
          hub = oracle;
          admitted = await history.admit(
            oracle,
            { DepositPayload: { event: { id, info } } },
            fixture.header,
            { lovelace: 25_000_000n, ...nativeAssets },
          );
        },
      });
      if (admitted === undefined || hub === undefined)
        throw new Error("History admission did not run");
      expect(admitted.captured.originalAssets.get("")?.get("")).toBe(
        20_000_000n,
      );
      expect(admitted.witness.retainedDataUtxo !== undefined).toBe(
        mode === "external",
      );
      const opening = transitionDepositOpening(admitted.captured);
      // Journal-equivalent serialization retains only immutable preimages, not a live out-ref.
      const persisted = JSON.parse(JSON.stringify(opening)) as typeof opening;
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
        proof: fixture.proof,
      });
      await resolveTransitionTraceByteCarriage({
        lucid: h.proverLucid,
        chunks: transitionTraceByteChunks(outputCbor.toString("hex")),
        publish: true,
      });
      let checkpoint = (
        await h.proverLucid.utxosByOutRef([
          {
            txHash: route.routeOutRef.split("#")[0]!,
            outputIndex: Number(route.routeOutRef.split("#")[1]),
          },
        ])
      )[0]!;
      let captured = false;
      for (let stage = 0; stage < 10; stage++) {
        const state = Data.from(
          checkpoint.datum!,
          SDK.TransitionTraceProofCommitmentDatum,
        ).data!;
        if (state.deposit_source_cbor !== "") {
          captured = true;
          break;
        }
        const transaction = await captureLocallyEvaluatedTransaction(
          (boundary) =>
            submitTransitionTraceFinal({
              ...common,
              threadOutRef: outRefLabel(checkpoint),
              proof: fixture.proof,
              depositOpening: persisted,
              preSubmitBoundary: boundary,
            }),
        );
        await h.proverLucid.awaitTx(await transaction.signed.submit());
        checkpoint = await expectSingleUtxoWithUnit(
          h.proverLucid,
          checkpoint.address,
          init.computationThreadUnit,
        );
      }
      expect(captured).toBe(true);
      const state = Data.from(
        checkpoint.datum!,
        SDK.TransitionTraceProofCommitmentDatum,
      ).data!;
      expect(
        Data.from(state.deposit_source_cbor, SDK.EventHistoryCommitment),
      ).toEqual(admitted.captured.commitment);
      // After capture, permissionless successor insertion spends the exact Order.
      // Its immutable facts and funds survive; the old pointer is no longer usable.
      const protectedUntil = admitted.witness.anchor.node.protected_until;
      const targetTime = Number(protectedUntil) + 61_000;
      if (h.emulator.now() < targetTime)
        h.emulator.awaitSlot(Math.ceil((targetTime - h.emulator.now()) / 1000));
      const fillerKey = "ff".repeat(32);
      const churn = await insertHistoryFillerAfter(
        {
          applied: history.applied,
          scripts: history.scripts,
          lucid: h.proverLucid,
          hub,
          owner: h.proverSigner.paymentKeyHash,
          funding: async () =>
            (await h.proverLucid.wallet().getUtxos()).filter(
              (utxo) => utxo.datum == null && utxo.scriptRef == null,
            ),
          bounds: () => {
            const lower = h.emulator.now() - 60_000;
            const validTo = h.emulator.now() + 10_000;
            return {
              lower,
              validTo,
              protectedUntil:
                BigInt(validTo - 1) + history.recipes[0]!.protectionDurationMs,
            };
          },
        },
        "Deposit",
        admitted.witness,
        fillerKey,
        [],
      );
      await history.submit("churn-captured-deposit-pointer", churn);
      expect(
        await h.proverLucid.utxosByOutRef([admitted.witness.anchor.utxo]),
      ).toHaveLength(0);
      const finish = () =>
        submitTransitionTraceFinal({
          ...common,
          threadOutRef: outRefLabel(checkpoint),
          proof: fixture.proof,
          depositOpening: persisted,
        });
      if (honest) {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
        expect(
          await h.proverLucid.utxosAtWithUnit(
            contracts.fraudProof.spendingScriptAddress,
            contracts.fraudProof.policyId + init.computationThreadAssetName,
          ),
        ).toHaveLength(0);
        return;
      }
      // Bypass the client's opening preflight only at the final redeemer encoding.
      // The genuine deployed yield must reject a changed Value itself.
      const encode = Data.to;
      let substituted = 0;
      const replaceField = PlutusDataCbor.replacePlutusConstrFieldCbor;
      const corrupt = vi
        .spyOn(PlutusDataCbor, "replacePlutusConstrFieldCbor")
        .mockImplementation((cbor, path, replacement) => {
          const encoded = replaceField(cbor, path, replacement);
          if (
            path.length !== 3 ||
            path[0] !== 0 ||
            path[1] !== 9 ||
            path[2] !== 0
          )
            return encoded;
          // This splice follows typed encoding and the opening preflight. Change
          // only the final opening's Value, preserving all other raw fields.
          const opening = Data.from(replacement, SDK.EventHistoryOpening);
          opening.original_assets.get("")!.set("", 25_000_000n);
          substituted++;
          return replaceField(
            encoded,
            [...path, 1],
            encode(opening.original_assets),
          );
        });
      try {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
      } finally {
        corrupt.mockRestore();
      }
      expect(substituted).toBeGreaterThan(0);
      checkpoint = await expectSingleUtxoWithUnit(
        h.proverLucid,
        checkpoint.address,
        init.computationThreadUnit,
      );
      let omittedTerminalWitness = 0;
      const omitWitness = vi
        .spyOn(Data, "to")
        .mockImplementation((data, schema, options) => {
          const cbor = encode(data, schema, options);
          if (schema !== SDK.TransitionTraceYieldFinalSpendRedeemer)
            return cbor;
          const redeemer = Data.from(
            cbor,
            SDK.TransitionTraceYieldFinalSpendRedeemer,
          );
          if (
            !("Continue" in redeemer) ||
            redeemer.Continue[0].completed_fraud_witness === null
          )
            return cbor;
          redeemer.Continue[0].completed_fraud_witness = null;
          omittedTerminalWitness++;
          return encode(redeemer, SDK.TransitionTraceYieldFinalSpendRedeemer);
        });
      try {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
      } finally {
        omitWitness.mockRestore();
      }
      expect(omittedTerminalWitness).toBeGreaterThan(0);
      checkpoint = await expectSingleUtxoWithUnit(
        h.proverLucid,
        checkpoint.address,
        init.computationThreadUnit,
      );
      expect(
        Data.from(checkpoint.datum!, SDK.TransitionTraceProofCommitmentDatum)
          .data!.phase,
      ).toBe(5n);
      const queueBefore = await expectSingleUtxoWithUnit(
        h.proverLucid,
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      );
      const queueNodeBefore = Effect.runSync(
        SDK.getStateQueueNodeFromStateQueueDatum(
          await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(queueBefore),
          ),
        ),
      );
      expect(queueNodeBefore.proven_fraud).toBeNull();
      // Bypass client deadline planning to exercise the deployed terminal check.
      // Keep a short finite interval that straddles earliest merge maturity.
      const newTx = h.proverLucid.newTx.bind(h.proverLucid);
      const deadline = Number(
        fixture.proof.header.endTime + SDK.MATURITY_DURATION_MS,
      );
      const expire = vi.spyOn(h.proverLucid, "newTx").mockImplementation(() => {
        const tx = newTx();
        const validFrom = tx.validFrom.bind(tx);
        const validTo = tx.validTo.bind(tx);
        tx.validFrom = () => validFrom(deadline - 1000);
        tx.validTo = () => validTo(deadline + 1000);
        return tx;
      });
      try {
        await expect(finish()).rejects.toThrow(/failed script execution/u);
      } finally {
        expire.mockRestore();
      }
      const proof = await finish();
      const queueAfter = await expectSingleUtxoWithUnit(
        h.proverLucid,
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      );
      const queueNodeAfter = Effect.runSync(
        SDK.getStateQueueNodeFromStateQueueDatum(
          await Effect.runPromise(
            SDK.getLinkedListNodeViewFromUTxO(queueAfter),
          ),
        ),
      );
      expect(queueNodeAfter.proven_fraud).toBe(init.computationThreadAssetName);
      expect(proof.fraudProofUnit).toBe(
        contracts.fraudProof.policyId + init.computationThreadAssetName,
      );
      const removeNow = BigInt(h.emulator.now());
      await submitRemoveFraudulentBlock({
        ...common,
        fraudCategory: "transitionTrace",
        fraudulentHeaderHash: fixture.headerHash,
        requireReferenceScripts: true,
        validFrom: removeNow - 120_000n,
        validTo: removeNow + 300_000n,
      });
      expect(
        await h.proverLucid.utxosAtWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          setup.stateQueueBlockUnit,
        ),
      ).toHaveLength(0);
    },
    180_000,
  );
});
