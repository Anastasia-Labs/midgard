import { createHash, randomUUID } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  CML,
  Data,
  Emulator,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import {
  historyInitializationAddresses,
  replayEventHistoryInitialization,
} from "../src/l1-event-history-initialization.js";
import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import { stageHistoryProvenance } from "../src/l1-event-history-provenance.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import {
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
  submitHistoryObservation,
} from "./helpers/history-projection-observations.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./helpers/mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeploymentOnChain,
} from "./helpers/published-workflow-deployment.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./helpers/reference-publication-chain.js";
import { provideDatabaseLayers } from "./utils.js";

const label = (ref: { txHash: string; outputIndex: number }) =>
  `${ref.txHash}#${ref.outputIndex}`;
const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const ordinary = (output: UTxO) =>
  output.datum == null &&
  output.datumHash == null &&
  output.scriptRef == null &&
  Object.keys(output.assets).every((unit) => unit === "lovelace");

/** Accepted atomic initialization, then public admissions after protection,
 * with contemporaneous complete provider captures and exact historical refs.
 * Point hashes/endpoint/genesis labels are synthetic. No canonical ChainSync
 * authority, live-chain finality, or L2 withdrawal validity is asserted. */
it("bootstraps both histories from accepted atomic initialization and subsequent public admissions", async () => {
  // The standard test configuration supplies a disposable, migrated worker
  // database. Reset only this fixture's history state before its fresh emulator
  // deployment; the singleton authority must not inherit another test's owner.
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority`;
      }),
    ),
  );
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const emulator = new Emulator(
    [accounts.operator, accounts.publisher],
    MAINNET_PROTOCOL_PARAMETERS,
  );
  const lucid = await createMainnetEmulatorLucid(emulator, "Custom");
  const publisherLucid = await createMainnetEmulatorLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  // The manifest snapshot's rational fee representation is shared with existing
  // publication fixtures. Bind every represented quantity to the actual pinned
  // protocol snapshot instead of claiming default emulator costs are mainnet.
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const snapshot = {
    ...TEST_CARDANO_PROTOCOL_PARAMETERS,
    minFeeA: String(p.minFeeA),
    minFeeB: String(p.minFeeB),
    coinsPerUtxoByte: String(p.coinsPerUtxoByte),
    collateralPercentage: String(p.collateralPercentage),
    maxCollateralInputs: String(p.maxCollateralInputs),
    maxTxSize: String(p.maxTxSize),
    maxValueSize: String(p.maxValSize),
    maxTxExUnits: {
      memory: String(p.maxTxExMem),
      steps: String(p.maxTxExSteps),
    },
  };
  expect(
    Number(snapshot.priceMemory.numerator) /
      Number(snapshot.priceMemory.denominator),
  ).toBe(p.priceMem);
  expect(
    Number(snapshot.priceSteps.numerator) /
      Number(snapshot.priceSteps.denominator),
  ).toBe(p.priceStep);
  expect(
    Number(snapshot.referenceScriptFee.base.numerator) /
      Number(snapshot.referenceScriptFee.base.denominator),
  ).toBe(p.minFeeRefScriptCostPerByte);
  const receipts: (AcceptedHistoryObservation & {
    observedSlot: number;
    observedHeight: number;
  })[] = [];
  const batches: {
    observations: readonly AcceptedHistoryObservation[];
    slot: number;
    height: number;
    outputs: LedgerSnapshotOutput[];
  }[] = [];
  let observer:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  let parentOutputs: LedgerSnapshotOutput[] | undefined;
  let parentSlot: number | undefined;
  let initializationCbor: string | undefined;
  const evidencePath = process.env.MIDGARD_HISTORY_INITIALIZATION_EVIDENCE_PATH;
  let evidence: Record<string, unknown> = {
    status: "incomplete",
    protocolParametersSource: MAINNET_PROTOCOL_PARAMETERS_SOURCE,
    protocolParameters: p,
  };
  try {
    const deployment = await publishWorkflowDeploymentOnChain({
      network: "Custom",
      accounts,
      operatorLucid: lucid,
      publisherLucid,
      chain: {
        now: () => emulator.now(),
        delaySlots: (slots) => emulator.awaitSlot(slots),
        awaitLedgerTime: (time) => {
          const slots = Math.ceil((time - emulator.now()) / 1000);
          if (slots > 0) emulator.awaitSlot(slots);
        },
      },
      protocolParameters: snapshot,
      publicationJournalPath: join(
        await mkdtemp(join(tmpdir(), "midgard-history-origin-")),
        "transactions.ndjson",
      ),
      publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
      publicationSynchronize: async () => emulator.slot,
      onInitialization: (signedCbor) => {
        expect(observer).toBeUndefined();
        initializationCbor = signedCbor;
        parentSlot = emulator.slot;
        // Capture the real complete ledger before activation, then select the
        // five deployment addresses once its finalized manifest is available.
        parentOutputs = Object.values(emulator.ledger)
          .filter(({ spent }) => !spent)
          .map(({ utxo }) => historyOutputObservation(utxo));
        observer = captureConfirmedHistoryObservations(
          lucid,
          emulator,
          async (observations) => {
            batches.push({
              observations,
              slot: emulator.slot,
              height: emulator.blockHeight,
              outputs: Object.values(emulator.ledger)
                .filter(({ spent }) => !spent)
                .map(({ utxo }) => historyOutputObservation(utxo)),
            });
            receipts.push(
              ...observations.map((observation) => ({
                ...observation,
                observedSlot: emulator.slot,
                observedHeight: emulator.blockHeight,
              })),
            );
          },
        );
      },
    });
    const { contracts, manifest } = deployment;
    evidence = {
      ...evidence,
      manifestId: manifest.manifestId,
      blueprintSha256: manifest.artifacts.blueprintHash,
      manifestProtocolParameters: manifest.cardanoProtocolParameters,
    };
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
        ogmiosUrl: "http://history-initialization-emulator.invalid:1337",
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
          scope: "synthetic emulator transport",
          initialization: deployment.initialization.txHash,
        }),
      }),
    );
    expect(initializationCbor).toBeDefined();
    expect(parentOutputs).toBeDefined();
    expect(parentSlot).toBeDefined();
    expect(receipts[0]!.transaction.txHash).toBe(
      deployment.initialization.txHash,
    );
    expect(receipts[0]!.signedCbor).toBe(initializationCbor);
    const addresses = historyInitializationAddresses(binding);
    const ledger = {
      point: {
        slot: parentSlot!,
        id: hash(
          `synthetic-history-origin-parent:${deployment.initialization.txHash}`,
        ),
      },
      addresses,
      outputs: parentOutputs!.filter((output) =>
        addresses.includes(output.address),
      ),
    };
    expect(ledger.addresses).toHaveLength(5);
    expect(ledger.outputs).toEqual([]);
    const nonce = histories.deposit.recipe.initializationNonce;
    expect(histories.withdrawal.recipe.initializationNonce).toEqual(nonce);
    expect(
      parentOutputs!.filter(
        (output) =>
          output.txHash === nonce.transactionId &&
          BigInt(output.outputIndex) === nonce.outputIndex,
      ),
    ).toHaveLength(1);
    expect(
      await lucid.utxosByOutRef([
        { txHash: nonce.transactionId, outputIndex: Number(nonce.outputIndex) },
      ]),
    ).toHaveLength(0);
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(emulator.now());
    const address = await lucid.wallet().address();
    lucid.overrideUTxOs((await lucid.utxosAt(address)).filter(ordinary));
    const split = await lucid
      .newTx()
      .pay.ToAddress(address, { lovelace: 30_000_000n })
      .pay.ToAddress(address, { lovelace: 30_000_000n })
      .pay.ToAddress(address, { lovelace: 10_000_000n })
      .complete({ localUPLCEval: true });
    const funding = await submitHistoryObservation(lucid, split);
    const nonces = (await lucid.utxosAt(address)).filter(
      (output) =>
        output.txHash === funding.transaction.txHash &&
        output.assets.lovelace === 30_000_000n,
    );
    expect(nonces).toHaveLength(2);
    const reserved = new Set(nonces.map(label));
    const ownerKey = CML.PrivateKey.from_bech32(
      walletFromSeed(accounts.operator.seedPhrase, { network: "Custom" })
        .paymentKey,
    );
    const ownerAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(address),
    );
    const admissions: {
      kind: "deposit" | "withdrawal";
      receipt: AcceptedHistoryObservation;
      order: SDK.DepositUTxO | SDK.WithdrawalUTxO;
    }[] = [];
    let deposit: SDK.DepositUTxO | undefined;
    for (const [ordinal, kind] of (
      ["deposit", "withdrawal"] as const
    ).entries()) {
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
      const eventNonce = nonces[ordinal]!;
      lucid.overrideUTxOs(
        (await lucid.utxosAt(address)).filter(
          (output) =>
            ordinary(output) &&
            (!reserved.has(label(output)) ||
              label(output) === label(eventNonce)),
        ),
      );
      const built =
        kind === "deposit"
          ? await Effect.runPromise(
              SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, {
                nonceInput: eventNonce,
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
                    l2_outref: deposit!.event.id,
                    l2_owner: ownerKey.to_public().hash().to_hex(),
                    l2_value: SDK.assetsToValue(deposit!.originalAssets),
                    l1_address: ownerAddress,
                    l1_datum: "NoDatum",
                  };
                  return {
                    nonceInput: eventNonce,
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
      const accepted = await submitHistoryObservation(lucid, built.tx);
      reserved.delete(label(eventNonce));
      const view = await Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: {
              slot: emulator.slot,
              id: hash(`admission:${accepted.transaction.txHash}`),
            },
            addresses,
            outputs: (
              await Promise.all(
                addresses.map((tracked) => lucid.utxosAt(tracked)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          },
          binding,
        ),
      );
      const orders =
        kind === "deposit" ? view.history.deposits : view.history.withdrawals;
      expect(orders).toHaveLength(1);
      const order = orders[0]!;
      expect(order.event.id).toEqual({
        transactionId: eventNonce.txHash,
        outputIndex: BigInt(eventNonce.outputIndex),
      });
      admissions.push({ kind, receipt: accepted, order });
      if (kind === "deposit") deposit = view.history.deposits[0]!;
    }
    await observer!.flush();
    expect(observer!.pendingCount()).toBe(0);
    const archives = new Map(
      receipts.map((receipt) => [
        receipt.transaction.txHash,
        new Map(receipt.historical.map((output) => [label(output), output])),
      ]),
    );
    const resolveReference = (
      transactionHash: string,
      ref: { txHash: string; outputIndex: number },
    ) => archives.get(transactionHash)?.get(label(ref));
    const pointFor = (batch: (typeof batches)[number]) => ({
      slot: batch.slot,
      height: batch.height,
      id: hash(
        `synthetic-history-point:${batch.observations.map(({ transaction }) => transaction.txHash).join(":")}`,
      ),
    });
    const providerAt = (batch: (typeof batches)[number]) =>
      Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { slot: batch.slot, id: pointFor(batch).id },
            addresses,
            outputs: batch.outputs.filter((output) =>
              addresses.includes(output.address),
            ),
          },
          binding,
        ),
      );
    const initialBatch = batches[0]!;
    expect(initialBatch.observations).toHaveLength(1);
    expect(initialBatch.observations[0]!.transaction.txHash).toBe(
      deployment.initialization.txHash,
    );
    const input = {
      ledger,
      captureBindingDigest: binding.digest,
      binding,
      histories,
      block: {
        parent: ledger.point.id,
        point: pointFor(initialBatch),
        transactions: initialBatch.observations.map(
          ({ transaction }) => transaction,
        ),
      },
      resolveReference,
      slotToUnixTime: lucid.slotToUnixTime,
    };
    const replay = await replayEventHistoryInitialization(input);
    const initializedProvider = await providerAt(initialBatch);
    expect(replay.activationIndex).toBe(0);
    expect(replay.activationTransactionHash).toBe(
      deployment.initialization.txHash,
    );
    expect(replay.capture.snapshotDigest).toBe(
      initializedProvider.snapshotDigest,
    );
    expect(replay.capture.history.deposits).toHaveLength(0);
    expect(replay.capture.history.withdrawals).toHaveLength(0);
    expect(replay.incarnations).toEqual([]);
    expect(
      replay.transitions.map(({ transactionIndex, transition }) => [
        transactionIndex,
        transition.kind,
        transition.operation,
      ]),
    ).toEqual([
      [0, "deposit", "Initialize"],
      [0, "withdrawal", "Initialize"],
    ]);
    // Persist exact accepted initialization evidence and its paired capture in
    // one real recovery transaction. The transport remains explicitly modeled;
    // this does not grant the archived receipt production source authority.
    const persistedOrigin = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const token = yield* Authority.acquire({
            deploymentIdentity: binding.manifestId,
            ownerToken: randomUUID(),
            leaseDurationMs: 60_000,
          });
          yield* Authority.withRecovery(
            token,
            Journal.seed({
              binding,
              capture: replay.capture,
              height: input.block.point.height,
              originReceipt: replay.originReceipt,
              originReceiptDigest: replay.originReceiptDigest,
              incarnations: replay.incarnations,
            }),
          );
          return yield* Journal.load(binding);
        }),
      ),
    );
    expect(persistedOrigin?.originReceipt).toBe(replay.originReceipt);
    expect(persistedOrigin?.originReceiptDigest).toBe(
      replay.originReceiptDigest,
    );
    expect(persistedOrigin?.capture.snapshotDigest).toBe(
      replay.capture.snapshotDigest,
    );
    expect(persistedOrigin?.anchor).toEqual(input.block.point);
    expect(persistedOrigin?.incarnations).toEqual([]);
    evidence = {
      ...evidence,
      originReceipt: replay.originReceipt,
      originReceiptDigest: replay.originReceiptDigest,
      persistedOriginAnchor: persistedOrigin!.anchor,
    };
    let capture = replay.capture;
    const incarnations = new Map(
      replay.incarnations.map((event) => [event.id, event]),
    );
    const blockEvidence: unknown[] = [
      {
        block: input.block,
        transitions: replay.transitions,
        projectedSnapshotDigest: replay.capture.snapshotDigest,
        providerSnapshotDigest: initializedProvider.snapshotDigest,
      },
    ];
    for (const batch of batches.slice(1)) {
      expect(batch.slot).toBeGreaterThan(capture.history.ledger.point.slot);
      const block = {
        parent: capture.history.ledger.point.id,
        point: pointFor(batch),
        transactions: batch.observations.map(({ transaction }) => transaction),
      };
      const projected = await projectEventHistoryBlock({
        previous: capture,
        block,
        binding,
        histories,
        resolveReference,
        slotToUnixTime: lucid.slotToUnixTime,
      });
      const provider = await providerAt(batch);
      expect(projected.capture.snapshotDigest).toBe(provider.snapshotDigest);
      expect(projected.capture.history.deposits).toEqual(
        provider.history.deposits,
      );
      expect(projected.capture.history.withdrawals).toEqual(
        provider.history.withdrawals,
      );
      for (const change of stageHistoryProvenance({
        bindingDigest: binding.digest,
        block,
        transitions: projected.transitions,
        incarnations: [...incarnations.values()],
      }))
        incarnations.set(change.after.id, change.after);
      blockEvidence.push({
        block,
        transitions: projected.transitions,
        projectedSnapshotDigest: projected.capture.snapshotDigest,
        providerSnapshotDigest: provider.snapshotDigest,
      });
      capture = projected.capture;
    }
    expect(capture.history.deposits).toHaveLength(1);
    expect(capture.history.withdrawals).toHaveLength(1);
    expect(incarnations.size).toBe(2);
    for (const { kind, receipt, order } of admissions) {
      const batch = batches.find(({ observations }) =>
        observations.some(
          ({ transaction }) =>
            transaction.txHash === receipt.transaction.txHash,
        ),
      )!;
      const index = batch.observations.findIndex(
        ({ transaction }) => transaction.txHash === receipt.transaction.txHash,
      );
      const point = pointFor(batch);
      expect(point.slot).toBeGreaterThan(input.block.point.slot);
      const incarnation = [...incarnations.values()].find(
        (event) => event.kind === kind,
      )!;
      expect(incarnation.bindingDigest).toBe(binding.digest);
      expect(incarnation.event).toMatchObject({
        key: order.assetName,
        idCbor: order.idCbor.toString("hex"),
        factsCbor: aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(order.utxo.datum!, [3, 0]),
        ),
        originalAssetsCbor: Data.to(
          SDK.assetsToValue(order.originalAssets),
          SDK.Value,
        ),
        inclusionTime: order.facts.inclusion_time,
        outRef: {
          txHash: order.utxo.txHash,
          outputIndex: order.utxo.outputIndex,
        },
      });
      const placement = {
        blockHash: point.id,
        slot: point.slot,
        height: point.height,
        transactionHash: receipt.transaction.txHash,
        transactionIndex: index,
      };
      expect(incarnation.placement).toEqual({
        admission: placement,
        current: {
          outRef: {
            txHash: order.utxo.txHash,
            outputIndex: order.utxo.outputIndex,
          },
          at: placement,
        },
        retirement: null,
      });
    }
    // Refusals alter source transport inputs only, never transaction execution
    // or accepted signed CBOR. No malformed transaction is submitted.
    await expect(
      replayEventHistoryInitialization({
        ...input,
        captureBindingDigest: hash("wrong capture binding"),
      }),
    ).rejects.toThrow("parent capture belongs to another source binding");
    await expect(
      replayEventHistoryInitialization({
        ...input,
        ledger: { ...ledger, addresses: addresses.slice(1) },
      }),
    ).rejects.toThrow("exact complete history scope");
    await expect(
      replayEventHistoryInitialization({
        ...input,
        block: { ...input.block, transactions: [] },
      }),
    ).rejects.toThrow("History capture requires exactly one authenticated hub");
    await expect(
      replayEventHistoryInitialization({
        ...input,
        block: {
          parent: ledger.point.id,
          point: pointFor(batches.at(-1)!),
          transactions: [admissions[0]!.receipt.transaction],
        },
      }),
    ).rejects.toThrow(
      "Historical reference contradicts the complete tracked scope",
    );
    expect(ledger.outputs).toEqual([]);
    expect(
      (await replayEventHistoryInitialization(input)).capture.snapshotDigest,
    ).toBe(initializedProvider.snapshotDigest);
    for (const receipt of receipts) {
      expect(receipt.measurement.completeSignedBytes).toBeLessThanOrEqual(
        p.maxTxSize,
      );
      expect(receipt.measurement.executionMemory).toBeLessThanOrEqual(
        p.maxTxExMem,
      );
      expect(receipt.measurement.executionSteps).toBeLessThanOrEqual(
        p.maxTxExSteps,
      );
    }
    evidence = {
      ...evidence,
      status: "passed",
      transport: {
        scope:
          "Actual accepted emulator transactions with observed per-confirmation slot/height and provider state; synthetic point/endpoint/genesis labels; no canonical ChainSync or L2 withdrawal validity claim",
        sourceBindingDigest: binding.digest,
      },
      parent: ledger,
      blocks: blockEvidence,
      activationTransactionHash: replay.activationTransactionHash,
      incarnations: [...incarnations.values()],
      finalSnapshotDigest: capture.snapshotDigest,
      refusalCount: 4,
    };
  } finally {
    observer?.restore();
    vi.useRealTimers();
    if (evidencePath !== undefined) {
      mkdirSync(dirname(evidencePath), { recursive: true });
      writeFileSync(
        evidencePath,
        JSON.stringify(
          { ...evidence, receipts },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  }
});
