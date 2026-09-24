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
  coreToTxOutput,
  Data,
  Emulator,
  generateEmulatorAccount,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import * as ReplayReceipts from "../src/database/eventHistoryReplayReceipts.js";
import { historyInitializationAddresses } from "../src/l1-event-history-initialization.js";
import {
  advanceEventHistoryListReplay,
  beginEventHistoryListReplay,
  joinEventHistoryListReplay,
} from "../src/l1-event-history-list-replay.js";
import {
  type BoundHistoryChainBlock,
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
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
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
type Receipt = AcceptedHistoryObservation & {
  observedSlot: number;
  observedHeight: number;
};

/** Real accepted transactions and observed slots; synthetic transport hashes,
 * contiguous replay heights, endpoint and genesis. Receipt batches are NOT a
 * complete live ChainSync branch. No raw pre-activation capture is supplied. */
it("replays actual initialized lists and joins current external orders whose retained bodies predate activation", async () => {
  // Standard globalSetup owns this disposable migrated worker database. Keep
  // this scenario's journal and singleton authority independent of prior tests.
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`TRUNCATE event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_replay_receipts, event_history_authority`;
      }),
    ),
  );
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const user = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator(
    [accounts.operator, accounts.publisher, user],
    p,
  );
  const lucid = await createMainnetEmulatorLucid(emulator, "Custom");
  const publisher = await createMainnetEmulatorLucid(emulator, "Custom");
  const owner = await createMainnetEmulatorLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  owner.selectWallet.fromSeed(user.seedPhrase);
  const ownerAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(user.address),
  );
  const ownerKey = CML.PrivateKey.from_bech32(
    walletFromSeed(user.seedPhrase, { network: "Custom" }).paymentKey,
  );
  const ownerHash = ownerKey.to_public().hash().to_hex();
  const reclaimAuth: SDK.CredentialD = { PublicKeyCredential: [ownerHash] };
  const l2Datum = Data.to("ab".repeat(600));
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
  const creatingBodies = new Map<string, string>();
  const rememberBody = (txHash: string, signedCbor: string) => {
    const body = CML.Transaction.from_cbor_hex(signedCbor).body();
    expect(CML.hash_transaction(body).to_hex()).toBe(txHash);
    const cbor = body.to_cbor_hex();
    expect(cbor.length / 2).toBeLessThanOrEqual(p.maxTxSize);
    expect(CML.TransactionBody.from_cbor_hex(cbor).to_cbor_hex()).toBe(cbor);
    const previous = creatingBodies.get(txHash);
    if (previous !== undefined) expect(previous).toBe(cbor);
    creatingBodies.set(txHash, cbor);
  };
  const receipts: Receipt[] = [];
  const batches: {
    observations: readonly AcceptedHistoryObservation[];
    observedSlot: number;
    observedHeight: number;
    outputs: LedgerSnapshotOutput[];
  }[] = [];
  const record = (observation: AcceptedHistoryObservation): Receipt => {
    rememberBody(observation.transaction.txHash, observation.signedCbor);
    const receipt = {
      ...observation,
      observedSlot: emulator.slot,
      observedHeight: emulator.blockHeight,
    };
    receipts.push(receipt);
    return receipt;
  };
  let preparedContracts: SDK.MidgardValidators | undefined;
  let nonces: UTxO[] = [];
  let retained: UTxO[] = [];
  let publication: Receipt | undefined;
  let withdrawalBody: SDK.WithdrawalBody | undefined;
  let signature: SDK.WithdrawalSignature | undefined;
  const plans = new Map<
    "deposit" | "withdrawal",
    Extract<
      ReturnType<typeof SDK.prepareEventHistoryPayload>,
      { kind: "External" }
    >
  >();
  let observer:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  const evidencePath = process.env.MIDGARD_HISTORY_LIST_REPLAY_EVIDENCE_PATH;
  let evidence: Record<string, unknown> = {
    status: "incomplete",
    protocolParameters: p,
    protocolParametersSource: MAINNET_PROTOCOL_PARAMETERS_SOURCE,
  };
  try {
    const deployment = await publishWorkflowDeploymentOnChain({
      network: "Custom",
      accounts,
      operatorLucid: lucid,
      publisherLucid: publisher,
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
        await mkdtemp(join(tmpdir(), "midgard-list-replay-")),
        "transactions.ndjson",
      ),
      publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
      publicationSynchronize: async () => emulator.slot,
      onPublication: ({ signedCbor, outRef }) =>
        rememberBody(outRef.txHash, signedCbor),
      onPrepared: async ({ nonce, authPolicy }) => {
        preparedContracts = await loadRealMidgardContractsForTest(
          nonce,
          authPolicy,
        );
        const histories = SDK.requireEventHistoryContracts(preparedContracts);
        expect(
          await lucid.utxosAt(
            preparedContracts.hubOracle.spendingScriptAddress,
          ),
        ).toEqual([]);
        const split = await owner
          .newTx()
          .pay.ToAddress(user.address, { lovelace: 30_000_000n })
          .pay.ToAddress(user.address, { lovelace: 30_000_000n })
          .pay.ToAddress(user.address, { lovelace: 10_000_000n })
          .complete({ localUPLCEval: true });
        const funding = record(await submitHistoryObservation(owner, split));
        nonces = (await owner.utxosAt(user.address)).filter(
          (output) =>
            output.txHash === funding.transaction.txHash &&
            output.assets.lovelace === 30_000_000n,
        );
        expect(nonces).toHaveLength(2);
        const eventId = (index: number) => ({
          transactionId: nonces[index]!.txHash,
          outputIndex: BigInt(nonces[index]!.outputIndex),
        });
        withdrawalBody = {
          l2_outref: eventId(0),
          l2_owner: ownerHash,
          l2_value: SDK.assetsToValue({ lovelace: 10_000_000n }),
          l1_address: ownerAddress,
          l1_datum: { InlineDatum: { data: "cd".repeat(600) } },
        };
        signature = SDK.signWithdrawalBody(ownerKey, withdrawalBody);
        const payloads: ["deposit" | "withdrawal", SDK.EventHistoryPayload][] =
          [
            [
              "deposit",
              {
                DepositPayload: {
                  event: {
                    id: eventId(0),
                    info: {
                      l2_address: ownerAddress,
                      l2_network_id: 0n,
                      l2_datum: Data.from(l2Datum),
                    },
                  },
                },
              },
            ],
            [
              "withdrawal",
              {
                WithdrawalPayload: {
                  event: {
                    id: eventId(1),
                    info: {
                      body: withdrawalBody,
                      signature,
                      validity: "WithdrawalIsValid",
                    },
                  },
                  refund_address: ownerAddress,
                  refund_datum: "NoDatum",
                },
              },
            ],
          ];
        // Normal output creation is permissionless. The production publication
        // wrapper authenticates an existing hub, so cannot precede activation.
        // Prepare with the actual SDK recipe and make real owner-funded outputs;
        // later public admission validates those exact retained datums.
        const fundingInputs = (await owner.utxosAt(user.address)).filter(
          (output) =>
            ordinary(output) && output.assets.lovelace! > 100_000_000n,
        );
        expect(fundingInputs).toHaveLength(1);
        let tx = owner.newTx().collectFrom(fundingInputs);
        for (const [kind, payload] of payloads) {
          const plan = SDK.prepareEventHistoryPayload(
            payload,
            reclaimAuth,
            histories[kind].recipe,
          );
          if (plan.kind !== "External")
            throw new Error("Fixture must publish actual external data");
          expect(plan.payloadBytes).toBeGreaterThan(
            histories[kind].recipe.inlineLimitBytes,
          );
          expect(plan.payloadBytes).toBeLessThanOrEqual(
            histories[kind].recipe.maxPayloadBytes,
          );
          plans.set(kind, plan);
          tx = tx.pay.ToContract(
            histories[kind].retention.spendingScriptAddress,
            { kind: "inline", value: plan.datumCbor },
            {},
          );
        }
        publication = record(
          await submitHistoryObservation(
            owner,
            await tx.complete({ coinSelection: false, localUPLCEval: true }),
          ),
        );
        retained = await owner.utxosByOutRef(
          [0, 1].map((outputIndex) => ({
            txHash: publication!.transaction.txHash,
            outputIndex,
          })),
        );
        expect(retained).toHaveLength(2);
        for (const [index, kind] of (
          ["deposit", "withdrawal"] as const
        ).entries()) {
          expect(retained[index]!.address).toBe(
            histories[kind].retention.spendingScriptAddress,
          );
          expect(retained[index]!.datum).toBe(plans.get(kind)!.datumCbor);
        }
        expect(await owner.utxosByOutRef(nonces)).toHaveLength(2);
        expect(
          await lucid.utxosAt(
            preparedContracts.hubOracle.spendingScriptAddress,
          ),
        ).toEqual([]);
      },
      onInitialization: () => {
        expect(observer).toBeUndefined();
        expect(preparedContracts).toBeDefined();
        const histories = SDK.requireEventHistoryContracts(preparedContracts!);
        const addresses = [
          preparedContracts!.hubOracle.spendingScriptAddress,
          ...Object.values(histories).flatMap((history) => [
            history.list.spendingScriptAddress,
            history.retention.spendingScriptAddress,
          ]),
        ];
        observer = captureConfirmedHistoryObservations(
          lucid,
          emulator,
          async (observations) => {
            batches.push({
              observations,
              observedSlot: emulator.slot,
              observedHeight: emulator.blockHeight,
              outputs: (
                await Promise.all(
                  addresses.map((address) => lucid.utxosAt(address)),
                )
              )
                .flat()
                .map(historyOutputObservation),
            });
            observations.forEach(record);
          },
        );
      },
    });
    const { contracts, manifest } = deployment;
    const histories = SDK.requireEventHistoryContracts(contracts);
    const before = SDK.requireEventHistoryContracts(preparedContracts!);
    for (const kind of ["deposit", "withdrawal"] as const) {
      expect(histories[kind].recipe).toEqual(before[kind].recipe);
      expect(histories[kind].list.policyId).toBe(before[kind].list.policyId);
      expect(histories[kind].retention.spendingScriptAddress).toBe(
        before[kind].retention.spendingScriptAddress,
      );
    }
    evidence = {
      ...evidence,
      manifestId: manifest.manifestId,
      blueprintSha256: manifest.artifacts.blueprintHash,
      manifestProtocolParameters: manifest.cardanoProtocolParameters,
    };
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
        ogmiosUrl: "http://history-list-replay-emulator.invalid:1337",
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
          scope: "synthetic emulator transport",
          initialization: deployment.initialization.txHash,
        }),
      }),
    );
    const addresses = historyInitializationAddresses(binding);
    expect(batches[0]!.observations).toHaveLength(1);
    expect(batches[0]!.observations[0]!.transaction.txHash).toBe(
      deployment.initialization.txHash,
    );
    expect(publication!.observedSlot).toBeLessThan(batches[0]!.observedSlot);
    expect(
      batches
        .flatMap(({ observations }) => observations)
        .some(
          ({ transaction }) =>
            transaction.txHash === publication!.transaction.txHash,
        ),
    ).toBe(false);
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(emulator.now());
    const reserved = new Set(nonces.map(label));
    const admissions: {
      kind: "deposit" | "withdrawal";
      receipt: AcceptedHistoryObservation;
      order: SDK.DepositUTxO | SDK.WithdrawalUTxO;
    }[] = [];
    for (const [index, kind] of (
      ["deposit", "withdrawal"] as const
    ).entries()) {
      const history = histories[kind];
      const nodes = SDK.authenticateHistoryNodes(
        await owner.utxosAt(history.list.spendingScriptAddress),
        SDK.eventHistoryDeploymentFromContracts(history),
      );
      const protectedUntil = nodes.reduce(
        (latest, { node }) =>
          node.protected_until > latest ? node.protected_until : latest,
        0n,
      );
      await deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
      vi.setSystemTime(emulator.now());
      const nonce = nonces[index]!;
      owner.overrideUTxOs(
        (await owner.utxosAt(user.address)).filter(
          (output) =>
            ordinary(output) &&
            (!reserved.has(label(output)) || label(output) === label(nonce)),
        ),
      );
      const built =
        kind === "deposit"
          ? await Effect.runPromise(
              SDK.buildUnsignedDepositTxWithMetadataProgram(owner, contracts, {
                nonceInput: nonce,
                externalData: retained[index]!,
                l2Address: user.address,
                l2Datum,
                lovelace: 10_000_000n,
                additionalAssets: {},
                referenceScripts: {
                  depositMinting: deployment.references.get("depositMint")!,
                },
              }),
            )
          : await Effect.runPromise(
              SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
                owner,
                contracts,
                {
                  nonceInput: nonce,
                  externalData: retained[index]!,
                  body: withdrawalBody!,
                  signature: signature!,
                  refundAddress: ownerAddress,
                  referenceScripts: {
                    withdrawalMinting:
                      deployment.references.get("withdrawalMint")!,
                  },
                },
              ),
            );
      const receipt = await submitHistoryObservation(owner, built.tx);
      reserved.delete(label(nonce));
      const orders =
        kind === "deposit"
          ? await Effect.runPromise(
              SDK.fetchDepositUTxOsProgram(
                owner,
                SDK.eventHistoryDeploymentFromContracts(history),
              ),
            )
          : await Effect.runPromise(
              SDK.fetchWithdrawalUTxOsProgram(
                owner,
                SDK.eventHistoryDeploymentFromContracts(history),
              ),
            );
      expect(orders).toHaveLength(1);
      const order = orders[0]!;
      expect(order.facts.location).toEqual(plans.get(kind)!.location);
      expect(order.event.id).toEqual({
        transactionId: nonce.txHash,
        outputIndex: BigInt(nonce.outputIndex),
      });
      expect(order.history.retainedDataUtxo).toEqual(retained[index]);
      expect(receipt.transaction.references).toContainEqual({
        txHash: retained[index]!.txHash,
        outputIndex: retained[index]!.outputIndex,
      });
      admissions.push({ kind, receipt, order });
    }
    await observer!.flush();
    expect(observer!.pendingCount()).toBe(0);
    const requests: string[] = [];
    const getCreatingBody = (txHash: string) => {
      requests.push(txHash);
      return creatingBodies.get(txHash);
    };
    const common = {
      binding,
      histories,
      slotToUnixTime: lucid.slotToUnixTime,
      getCreatingBody,
      maximumBodyBytes: p.maxTxSize,
    };
    const pointFor = (index: number) => ({
      slot: batches[index]!.observedSlot,
      // Receipt batches omit empty emulator blocks. These are deliberately
      // synthetic contiguous replay heights, never observed ChainSync heights.
      height: index + 1,
      id: hash(
        `synthetic-list-replay-point:${batches[index]!.observations.map(({ transaction }) => transaction.txHash).join(":")}`,
      ),
    });
    const initialBlock = {
      parent: hash(
        `synthetic-list-replay-parent:${deployment.initialization.txHash}`,
      ),
      point: pointFor(0),
      transactions: batches[0]!.observations.map(
        ({ transaction }) => transaction,
      ),
    };
    const initialized = beginEventHistoryListReplay({
      ...common,
      block: initialBlock,
    });
    expect(initialized.state.outputs).toHaveLength(3);
    expect(initialized.state.incarnations).toEqual([]);
    expect(initialized.state.activation.transactionHash).toBe(
      deployment.initialization.txHash,
    );
    expect(
      initialized.transitions.map(({ transition }) => [
        transition.kind,
        transition.operation,
      ]),
    ).toEqual([
      ["deposit", "Initialize"],
      ["withdrawal", "Initialize"],
    ]);
    let state = initialized.state;
    const refusalResults: {
      kind: "deposit" | "withdrawal";
      missingBody: boolean;
      wrongBody: boolean;
    }[] = [];
    const blocks: {
      block: BoundHistoryChainBlock;
      receipt: string;
      replay: ReturnType<typeof beginEventHistoryListReplay>["state"];
      transitions: ReturnType<
        typeof beginEventHistoryListReplay
      >["transitions"];
      observedSlot: number;
      observedHeight: number;
    }[] = [
      {
        block: initialBlock,
        receipt: initialized.receipt,
        replay: initialized.state,
        transitions: initialized.transitions,
        observedSlot: batches[0]!.observedSlot,
        observedHeight: batches[0]!.observedHeight,
      },
    ];
    for (const [offset, batch] of batches.slice(1).entries()) {
      const block = {
        parent: state.point.id,
        point: pointFor(offset + 1),
        transactions: batch.observations.map(({ transaction }) => transaction),
      };
      const admission = admissions.find(({ receipt }) =>
        block.transactions.some(
          ({ txHash }) => txHash === receipt.transaction.txHash,
        ),
      );
      if (admission !== undefined) {
        const preserved = state.replayDigest;
        expect(() =>
          advanceEventHistoryListReplay({
            ...common,
            previous: state,
            block,
            getCreatingBody: (txHash) =>
              txHash === publication!.transaction.txHash
                ? undefined
                : creatingBodies.get(txHash),
          }),
        ).toThrow("referenced creating body is unavailable");
        expect(() =>
          advanceEventHistoryListReplay({
            ...common,
            previous: state,
            block,
            getCreatingBody: (txHash) =>
              txHash === publication!.transaction.txHash
                ? creatingBodies.get(deployment.initialization.txHash)
                : creatingBodies.get(txHash),
          }),
        ).toThrow(
          "creating body does not match its referenced transaction hash",
        );
        expect(state.replayDigest).toBe(preserved);
        refusalResults.push({
          kind: admission.kind,
          missingBody: true,
          wrongBody: true,
        });
      }
      const requestStart = requests.length;
      const advanced = advanceEventHistoryListReplay({
        ...common,
        previous: state,
        block,
      });
      if (admission !== undefined)
        expect(requests.slice(requestStart)).toContain(
          publication!.transaction.txHash,
        );
      const trackedAddresses = [
        binding.hubAddress,
        histories.deposit.list.spendingScriptAddress,
        histories.withdrawal.list.spendingScriptAddress,
      ];
      expect(
        advanced.state.outputs.every((output) =>
          trackedAddresses.includes(output.address),
        ),
      ).toBe(true);
      expect(
        advanced.state.outputs.some(
          (output) => output.txHash === publication!.transaction.txHash,
        ),
      ).toBe(false);
      state = advanced.state;
      blocks.push({
        block,
        receipt: advanced.receipt,
        replay: advanced.state,
        transitions: advanced.transitions,
        observedSlot: batch.observedSlot,
        observedHeight: batch.observedHeight,
      });
    }
    expect(refusalResults.map(({ kind }) => kind)).toEqual([
      "deposit",
      "withdrawal",
    ]);
    expect(state.outputs).toHaveLength(5);
    expect(state.incarnations).toHaveLength(2);
    const currentOutputs = (
      await Promise.all(addresses.map((address) => lucid.utxosAt(address)))
    )
      .flat()
      .map(historyOutputObservation);
    const capture = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          point: { slot: state.point.slot, id: state.point.id },
          addresses,
          outputs: currentOutputs,
        },
        binding,
      ),
    );
    const joined = joinEventHistoryListReplay({ state, capture, binding });
    expect(joined.capture.snapshotDigest).toBe(capture.snapshotDigest);
    expect(joined.capture.history.deposits).toHaveLength(1);
    expect(joined.capture.history.withdrawals).toHaveLength(1);
    expect(joined.height).toBe(state.point.height);
    expect(joined.incarnations).toEqual(state.incarnations);
    expect(joined.originReceiptDigest).toBe(hash(joined.originReceipt));
    for (const { kind, receipt, order } of admissions) {
      const incarnation = joined.incarnations.find(
        (event) => event.kind === kind,
      )!;
      const batchIndex = batches.findIndex(({ observations }) =>
        observations.some(
          ({ transaction }) =>
            transaction.txHash === receipt.transaction.txHash,
        ),
      );
      const transactionIndex = batches[batchIndex]!.observations.findIndex(
        ({ transaction }) => transaction.txHash === receipt.transaction.txHash,
      );
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
        outRef: {
          txHash: order.utxo.txHash,
          outputIndex: order.utxo.outputIndex,
        },
      });
      expect(incarnation.placement?.admission).toEqual({
        blockHash: pointFor(batchIndex).id,
        slot: pointFor(batchIndex).slot,
        height: pointFor(batchIndex).height,
        transactionHash: receipt.transaction.txHash,
        transactionIndex,
      });
      expect(incarnation.placement?.current?.outRef).toEqual(
        incarnation.event.outRef,
      );
      expect(incarnation.placement?.retirement).toBeNull();
    }
    const initialCapture = await Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          point: { slot: initialBlock.point.slot, id: initialBlock.point.id },
          addresses,
          outputs: batches[0]!.outputs,
        },
        binding,
      ),
    );
    expect(() =>
      joinEventHistoryListReplay({ state, capture: initialCapture, binding }),
    ).toThrow("capture and replay do not share their bound point");
    expect(
      joinEventHistoryListReplay({ state, capture, binding })
        .originReceiptDigest,
    ).toBe(joined.originReceiptDigest);
    expect(await lucid.utxosByOutRef(retained)).toEqual(retained);
    // Independently compare resolved creating-body bytes to actual provider
    // references; production replay itself owns hash/index validation.
    const publicationBody = CML.TransactionBody.from_cbor_hex(
      creatingBodies.get(publication!.transaction.txHash)!,
    );
    for (const output of retained) {
      expect(
        historyOutputObservation({
          ...coreToTxOutput(publicationBody.outputs().get(output.outputIndex)),
          txHash: output.txHash,
          outputIndex: output.outputIndex,
        }),
      ).toEqual(historyOutputObservation(output));
      expect(currentOutputs).toContainEqual(historyOutputObservation(output));
    }
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
    // This resource bound covers local replay receipts, not Cardano transaction
    // size or execution. Every immutable frontier commits separately; the final
    // join is seeded only after all seven parent-linked receipts are durable.
    const maximumReceiptBytes = 16 * 1024 * 1024;
    expect(blocks).toHaveLength(7);
    const persisted = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const token = yield* Authority.acquire({
            deploymentIdentity: binding.manifestId,
            ownerToken: randomUUID(),
            leaseDurationMs: 60_000,
          });
          for (const { block, receipt, replay } of blocks) {
            expect(Buffer.byteLength(receipt, "utf8")).toBeLessThanOrEqual(
              maximumReceiptBytes,
            );
            yield* Authority.withRecovery(
              token,
              ReplayReceipts.put({
                binding,
                block,
                receipt,
                replay,
                maximumReceiptBytes,
              }),
            );
          }
          yield* Authority.withRecovery(
            token,
            Journal.seed({ ...joined, binding }),
          );
          const journal = yield* Journal.load(binding);
          const sql = yield* SqlClient.SqlClient;
          const count = yield* sql<{
            count: number;
          }>`SELECT count(*)::integer AS count FROM event_history_replay_receipts WHERE binding_digest = ${Buffer.from(binding.digest, "hex")}`;
          return { journal, receiptCount: count[0]!.count };
        }),
      ),
    );
    expect(persisted.receiptCount).toBe(7);
    expect(persisted.journal).not.toBeNull();
    expect(persisted.journal!.originReceipt).toBe(joined.originReceipt);
    expect(persisted.journal!.originReceiptDigest).toBe(
      joined.originReceiptDigest,
    );
    expect(persisted.journal!.capture.snapshotDigest).toBe(
      capture.snapshotDigest,
    );
    expect(persisted.journal!.capture.history.deposits).toEqual(
      capture.history.deposits,
    );
    expect(persisted.journal!.capture.history.withdrawals).toEqual(
      capture.history.withdrawals,
    );
    expect(persisted.journal!.anchor).toEqual(state.point);
    expect(persisted.journal!.head).toEqual(state.point);
    expect(persisted.journal!.incarnations).toEqual(
      [...joined.incarnations].sort((a, b) => a.id.localeCompare(b.id)),
    );
    evidence = {
      ...evidence,
      status: "passed",
      transport: {
        scope:
          "Actual accepted transactions and observed slots/heights retained separately; synthetic point hashes, contiguous replay heights, endpoint/genesis; no live ChainSync, canonical-source authority, or L2 classification claim",
        sourceBindingDigest: binding.digest,
      },
      preactivationPublication: publication,
      retainedInputs: retained,
      historyRecipes: {
        deposit: histories.deposit.recipe,
        withdrawal: histories.withdrawal.recipe,
      },
      blocks,
      creatingBodies: [...creatingBodies].map(([txHash, bodyCbor]) => ({
        txHash,
        bodyCbor,
      })),
      resolvedCreatingBodies: requests,
      currentCapture: capture,
      authenticatedListState: state,
      originReceipt: joined.originReceipt,
      originReceiptDigest: joined.originReceiptDigest,
      refusalResults,
      pointMismatchRefused: true,
      persistence: {
        maximumReceiptBytes,
        receiptCount: persisted.receiptCount,
        originReceipt: persisted.journal!.originReceipt,
        originReceiptDigest: persisted.journal!.originReceiptDigest,
        snapshotDigest: persisted.journal!.capture.snapshotDigest,
        anchor: persisted.journal!.anchor,
        head: persisted.journal!.head,
        incarnations: persisted.journal!.incarnations,
      },
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
