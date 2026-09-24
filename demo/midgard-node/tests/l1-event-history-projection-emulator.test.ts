import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../src/l1-event-history-source.js";
import { decodeEventHistoryTransition } from "../src/l1-event-history-transition.js";
import {
  historyOutputObservation,
  submitHistoryObservation,
} from "./helpers/history-projection-observations.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "./helpers/published-workflow-deployment.js";

const ref = (output: { txHash: string; outputIndex: number }) => ({
  txHash: output.txHash,
  outputIndex: output.outputIndex,
});
const label = (output: { txHash: string; outputIndex: number }) =>
  `${output.txHash}#${output.outputIndex}`;
const ordinary = (output: UTxO) =>
  output.datum == null &&
  output.datumHash == null &&
  output.scriptRef == null &&
  Object.keys(output.assets).every((unit) => unit === "lovelace");
const syntheticPointId = (transactionHash: string) =>
  createHash("sha256")
    .update(`projection-emulator-point:${transactionHash}`)
    .digest("hex");

/** Real accepted L1 admissions and exact historical outputs; only chain-point,
 * endpoint and genesis labels are synthetic. No ChainSync/finality authority or
 * L2 withdrawal validity is established by this consumer fixture. */
it("projects accepted deposit and withdrawal admissions and an Order pointer continuation", async () => {
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const deployment = await publishWorkflowDeployment({ accounts });
  const { operatorLucid: lucid, emulator, contracts, manifest } = deployment;
  vi.useFakeTimers({ toFake: ["Date"] });
  try {
    vi.setSystemTime(emulator.now());
    const histories = SDK.requireEventHistoryContracts(contracts);
    const binding = await Effect.runPromise(
      makeEventHistorySourceBinding({
        contracts,
        identity: {
          kind: "manifest",
          manifest,
          manifestId: manifest.manifestId,
          consensusProfile: manifest.consensusProfile,
        },
        network: "Custom",
        ogmiosUrl: "http://projection-emulator.invalid:1337",
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
          scope: "synthetic emulator transport only",
          startTime: emulator.now(),
        }),
      }),
    );
    const address = await lucid.wallet().address();
    lucid.overrideUTxOs((await lucid.utxosAt(address)).filter(ordinary));
    let split = lucid.newTx();
    for (let i = 0; i < 3; i++)
      split = split.pay.ToAddress(address, { lovelace: 30_000_000n });
    split = split.pay.ToAddress(address, { lovelace: 10_000_000n });
    const splitSigned = await (
      await split.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const splitHash = await splitSigned.submit();
    expect(await lucid.awaitTx(splitHash)).toBe(true);
    const nonceOutputs = (await lucid.utxosAt(address)).filter(
      (output) =>
        output.txHash === splitHash && output.assets.lovelace === 30_000_000n,
    );
    expect(nonceOutputs).toHaveLength(3);
    const key = (nonce: UTxO) =>
      Effect.runSync(
        SDK.eventHistoryKey({
          transactionId: nonce.txHash,
          outputIndex: BigInt(nonce.outputIndex),
        }),
      );
    nonceOutputs.sort((a, b) => key(a).localeCompare(key(b)));
    const reserved = new Set(nonceOutputs.map(label));
    const addresses = [
      ...new Set([
        binding.hubAddress,
        ...Object.values(binding.deployments).flatMap((history) => [
          history.address,
          history.retentionAddress,
        ]),
      ]),
    ];
    const readCapture = async (point: { slot: number; id: string }) =>
      Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { slot: point.slot, id: point.id },
            addresses,
            outputs: (
              await Promise.all(
                addresses.map((address) => lucid.utxosAt(address)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          },
          binding,
        ),
      );
    let capture = await readCapture({
      slot: emulator.slot,
      id: syntheticPointId(splitHash),
    });
    expect(capture.history.deposits).toHaveLength(0);
    expect(capture.history.withdrawals).toHaveLength(0);
    const ownerKey = CML.PrivateKey.from_bech32(
      walletFromSeed(accounts.operator.seedPhrase, { network: "Custom" })
        .paymentKey,
    );
    const ownerAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(address),
    );
    let firstDeposit: SDK.DepositUTxO | undefined;
    const receipts: unknown[] = [];
    for (const [ordinal, kind] of (
      ["deposit", "deposit", "withdrawal"] as const
    ).entries()) {
      const nonce = nonceOutputs[ordinal]!;
      const history = histories[kind];
      const nodes = SDK.authenticateHistoryNodes(
        await lucid.utxosAt(history.list.spendingScriptAddress),
        SDK.eventHistoryDeploymentFromContracts(history),
      );
      const protectedUntil = nodes.reduce(
        (latest, { node }) =>
          node.protected_until > latest ? node.protected_until : latest,
        0n,
      );
      await deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
      vi.setSystemTime(emulator.now());
      lucid.overrideUTxOs(
        (await lucid.utxosAt(address)).filter(
          (output) =>
            ordinary(output) &&
            (!reserved.has(label(output)) || label(output) === label(nonce)),
        ),
      );
      const built =
        kind === "deposit"
          ? await Effect.runPromise(
              SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, {
                nonceInput: nonce,
                l2Address: address,
                l2Datum: null,
                lovelace: 10_000_000n,
                additionalAssets: {},
                referenceScripts: {
                  depositMinting: deployment.references.get("depositMint")!,
                },
              }),
            )
          : await Effect.runPromise(
              SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
                lucid,
                contracts,
                (() => {
                  const body: SDK.WithdrawalBody = {
                    l2_outref: firstDeposit!.event.id,
                    l2_owner: ownerKey.to_public().hash().to_hex(),
                    l2_value: SDK.assetsToValue(firstDeposit!.originalAssets),
                    l1_address: ownerAddress,
                    l1_datum: "NoDatum",
                  };
                  return {
                    nonceInput: nonce,
                    body,
                    signature: SDK.signWithdrawalBody(ownerKey, body),
                    refundAddress: ownerAddress,
                    referenceScripts: {
                      withdrawalMinting:
                        deployment.references.get("withdrawalMint")!,
                    },
                  };
                })(),
              ),
            );
      expect(built.metadata.nonceInput).toEqual(ref(nonce));
      const accepted = await submitHistoryObservation(lucid, built.tx);
      reserved.delete(label(nonce));
      const archive = new Map(
        accepted.historical.map((output) => [label(output), output]),
      );
      const direct = decodeEventHistoryTransition({
        transaction: accepted.transaction,
        kind,
        history,
        binding,
        currentNodes: capture.history.ledger.outputs.filter(
          (output) => output.address === history.list.spendingScriptAddress,
        ),
        resolveReference: (input) => archive.get(label(input)),
        slotToUnixTime: lucid.slotToUnixTime,
      });
      expect(direct?.operation).toBe("InsertOrder");
      const point = {
        slot: emulator.slot,
        id: syntheticPointId(accepted.transaction.txHash),
        height: emulator.blockHeight,
      };
      const projected = await projectEventHistoryBlock({
        previous: capture,
        block: {
          parent: capture.history.ledger.point.id,
          point,
          transactions: [accepted.transaction],
        },
        binding,
        histories,
        resolveReference: (transactionHash, input) => {
          expect(transactionHash).toBe(accepted.transaction.txHash);
          return archive.get(label(input));
        },
        slotToUnixTime: lucid.slotToUnixTime,
      });
      expect(projected.transitions).toEqual([
        { transactionIndex: 0, transition: direct },
      ]);
      const actual = await readCapture(point);
      expect(projected.capture.snapshotDigest).toBe(actual.snapshotDigest);
      expect(projected.capture.history.deposits).toEqual(
        actual.history.deposits,
      );
      expect(projected.capture.history.withdrawals).toEqual(
        actual.history.withdrawals,
      );
      const orders =
        kind === "deposit"
          ? actual.history.deposits
          : actual.history.withdrawals;
      const admitted = orders.find((order) => order.assetName === key(nonce))!;
      expect(admitted).toBeDefined();
      expect(direct!.admission).toMatchObject({
        key: key(nonce),
        idCbor: admitted.idCbor.toString("hex"),
        factsCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(admitted.utxo.datum!, [3, 0]),
        ),
        originalAssetsCbor: Data.to(
          SDK.assetsToValue(admitted.originalAssets),
          SDK.Value,
        ),
        inclusionTime: admitted.facts.inclusion_time,
        outRef: ref(admitted.utxo),
      });
      expect(admitted.event.id).toEqual({
        transactionId: nonce.txHash,
        outputIndex: BigInt(nonce.outputIndex),
      });
      const limits = manifest.cardanoProtocolParameters.snapshot;
      expect(accepted.measurement.completeSignedBytes).toBeLessThanOrEqual(
        Number(limits.maxTxSize),
      );
      expect(accepted.measurement.executionMemory).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.memory),
      );
      expect(accepted.measurement.executionSteps).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.steps),
      );
      receipts.push({
        kind,
        txHash: accepted.transaction.txHash,
        signedCbor: accepted.signedCbor,
        ...accepted.measurement,
        projectedSnapshotDigest: projected.capture.snapshotDigest,
        providerSnapshotDigest: actual.snapshotDigest,
        transition: direct,
      });
      if (ordinal === 0) firstDeposit = actual.history.deposits[0]!;
      if (ordinal === 1) {
        const continued = actual.history.deposits.find(
          (order) => order.assetName === firstDeposit!.assetName,
        )!;
        expect(direct!.continuations).toEqual([
          {
            key: firstDeposit!.assetName,
            before: ref(firstDeposit!.utxo),
            after: ref(continued.utxo),
          },
        ]);
        expect(continued.facts).toEqual(firstDeposit!.facts);
        expect(continued.originalAssets).toEqual(firstDeposit!.originalAssets);
        expect(continued.utxo.assets).toEqual(firstDeposit!.utxo.assets);
      }
      capture = projected.capture;
    }
    expect(capture.history.deposits).toHaveLength(2);
    expect(capture.history.withdrawals).toHaveLength(1);
    const evidencePath = process.env.MIDGARD_HISTORY_PROJECTION_EVIDENCE_PATH;
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          {
            manifestId: manifest.manifestId,
            blueprintSha256: manifest.artifacts.blueprintHash,
            protocolParameters: manifest.cardanoProtocolParameters,
            transport: {
              scope:
                "Accepted emulator transactions; synthetic point, endpoint and genesis labels; no live/canonical/L2 withdrawal validity claim",
              endpoint: "http://projection-emulator.invalid:1337",
              pointIdDomain: "projection-emulator-point:",
              sourceBindingDigest: binding.digest,
            },
            receipts,
          },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  } finally {
    vi.useRealTimers();
  }
});
