import "./helpers/follower-emulator-installed.js";
import "@al-ft/midgard-core/codec";
import "da-committee-node/da/payload";
import "vitest";
import "../src/commands/command-utils.js";
import "../src/commands/transfer-build-core.make-static-midgard-provider.js";
import "./deposit-flow-emulator-shared.js";
import "./deposit-flow-emulator-merge-payout.admit-l2-tx.js";

import {
  DaPayloadValidationError,
  verifyDaPayloadAgainstHeader,
} from "da-committee-node/da/payload";
import { describe, expect, it, vi } from "vitest";

import { type NodeUtxo } from "../src/commands/command-utils.js";
import {
  makeTransferMidgard,
  privateKeyHash,
  toMidgardUtxo,
} from "../src/commands/transfer-build-core.make-static-midgard-provider.js";
import { admitAndAcceptL2Tx } from "./deposit-flow-emulator-merge-payout.admit-l2-tx.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  CML,
  commitConfirmRecoverAndMerge,
  configureEmulatorDaRuntimeManifest,
  DaPayloadsDB,
  Data,
  decodeNodeUtxo,
  Effect,
  expectDaCommitteeAcceptsPersistedPayload,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
  MempoolLedgerDB,
  Option,
  resetActiveRuntimePaths,
  runNodeDatabaseEffect,
  SDK,
  submitDepositWithDiagnostics,
  walletFromSeed,
} from "./deposit-flow-emulator-shared.js";

const OUTPUT_LOVELACE = 3_000_000n;

/**
 * Every input of a normal transaction resolves against the ledger state
 * immediately before its own transaction, as on Cardano. The node commits a
 * block's normal transactions in the order it validated them, and the DA
 * committee replays exactly that order from the parent block's post-state.
 */
describe(
  "input resolution at each transaction's position",
  { concurrent: false },
  () => {
    it("commits a referencer before the later spender of its reference and the committee attests the node's payload", async () => {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      await configureEmulatorDaRuntimeManifest();

      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      const l2Address = await fixture.depositorLucid.wallet().address();
      const signer = CML.PrivateKey.from_bech32(
        walletFromSeed(fixture.depositorAccount.seedPhrase, {
          network: "Custom",
        }).paymentKey,
      );
      await submitDepositWithDiagnostics(fixture, {
        l2Address,
        l2Datum: null,
        lovelace: 20_000_000n,
        additionalAssets: {},
      });
      const [depositUtxo] = await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(fixture.depositorLucid, {
          ...SDK.eventHistoryDeploymentFromContracts(
            SDK.requireEventHistoryContracts(fixture.contracts).deposit,
          ),
        }),
      );
      fixture.emulator.awaitSlot(
        fixture.operatorLucid.unixTimeToSlot(
          Number(depositUtxo!.facts.inclusion_time),
        ) + 1,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const depositBlock = await commitConfirmRecoverAndMerge({
        fixture,
        lucidService,
        globals,
      });
      await expectDaCommitteeAcceptsPersistedPayload({
        headerHash: depositBlock.queuedHeaderHash,
        l1Header: depositBlock.queuedHeader,
      });

      const spendable = async (): Promise<readonly NodeUtxo[]> =>
        (
          await runNodeDatabaseEffect(
            MempoolLedgerDB.retrieveSpendableByAddress(l2Address),
          )
        ).map((entry) =>
          decodeNodeUtxo({
            outref: entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
            outputCbor: entry[MempoolLedgerDB.Columns.OUTPUT].toString("hex"),
          }),
        );
      const outputsOf = async (txIdHex: string): Promise<readonly NodeUtxo[]> =>
        (await spendable())
          .filter((utxo) => utxo.txHash === txIdHex)
          .sort((left, right) => left.outputIndex - right.outputIndex);
      const build = async ({
        spend,
        reference = [],
        outputs,
      }: {
        readonly spend: readonly NodeUtxo[];
        readonly reference?: readonly NodeUtxo[];
        readonly outputs: number;
      }) => {
        const midgard = await makeTransferMidgard({
          senderAddress: l2Address,
          signer,
          utxos: spend,
          network: "Custom",
          networkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
        });
        let builder = midgard
          .newTx()
          .collectFrom(spend.map(toMidgardUtxo))
          .addSigner(privateKeyHash(signer));
        if (reference.length > 0) {
          builder = builder.readFrom(reference.map(toMidgardUtxo));
        }
        for (let index = 0; index < outputs; index += 1) {
          builder = builder.pay.ToAddress(l2Address, {
            lovelace: OUTPUT_LOVELACE,
          });
        }
        const signed = await (await builder.complete({ fee: 0n })).sign();
        return { txId: signed.txId, txCbor: signed.txCbor };
      };

      // Block 2 splits the deposit into the outputs the shapes below use.
      const [depositOutput] = await spendable();
      const split = await build({ spend: [depositOutput!], outputs: 5 });
      let admittedAt = Number(depositBlock.queuedHeader.endTime) + 1;
      await admitAndAcceptL2Tx(fixture, split, new Date(admittedAt));
      const splitBlock = await commitConfirmRecoverAndMerge({
        fixture,
        lucidService,
        globals,
        expectedL2TxIds: [split.txId],
      });
      await expectDaCommitteeAcceptsPersistedPayload({
        headerHash: splitBlock.queuedHeaderHash,
        l1Header: splitBlock.queuedHeader,
      });
      const splitOutputs = await outputsOf(split.txId.toString("hex"));
      const [x, a, b, c, d] = splitOutputs.filter(
        (utxo) => utxo.assets.lovelace === OUTPUT_LOVELACE,
      );
      expect([x, a, b, c, d].every((utxo) => utxo !== undefined)).toBe(true);

      // Block 3: R references X and a later S spends X; P produces Y and a
      // later Q references Y without spending anything P produced; an
      // independent I arrives last.
      const referencer = await build({
        spend: [a!],
        reference: [x!],
        outputs: 1,
      });
      const spender = await build({ spend: [x!], outputs: 1 });
      const producer = await build({ spend: [b!], outputs: 1 });
      admittedAt = Number(splitBlock.queuedHeader.endTime) + 1;
      for (const tx of [referencer, spender, producer]) {
        await admitAndAcceptL2Tx(fixture, tx, new Date(admittedAt));
        admittedAt += 1;
      }
      const [y] = await outputsOf(producer.txId.toString("hex"));
      const producedReferencer = await build({
        spend: [c!],
        reference: [y!],
        outputs: 1,
      });
      await admitAndAcceptL2Tx(
        fixture,
        producedReferencer,
        new Date(admittedAt),
      );
      admittedAt += 1;
      const independent = await build({ spend: [d!], outputs: 1 });
      await admitAndAcceptL2Tx(fixture, independent, new Date(admittedAt));

      // Validation applies Q after its reference's producer P, so I, which
      // depends on nothing in the block, is applied before Q; the block
      // commits exactly the order validation applied.
      const committedOrder = [
        referencer,
        spender,
        producer,
        independent,
        producedReferencer,
      ];
      const shapesBlock = await commitConfirmRecoverAndMerge({
        fixture,
        lucidService,
        globals,
        expectedL2TxIds: committedOrder.map((tx) => tx.txId),
      });
      const verified = await expectDaCommitteeAcceptsPersistedPayload({
        headerHash: shapesBlock.queuedHeaderHash,
        l1Header: shapesBlock.queuedHeader,
      });
      expect(verified.counts.l2TransactionCount).toBe(5n);

      // The committed order is the event_to_step order of the L2 events.
      const stepOfTx = new Map(
        verified.payload.block_body.event_to_step.flatMap(
          ([keyHex, valueHex]) => {
            const key = Data.from(keyHex, SDK.EventKey) as SDK.EventKey;
            if (!("L2TransactionEventKey" in key)) return [];
            const value = Data.from(
              valueHex,
              SDK.EventToStepValue,
            ) as SDK.EventToStepValue;
            return [
              [key.L2TransactionEventKey.tx_id, value.step_index] as const,
            ];
          },
        ),
      );
      expect(
        [...stepOfTx.entries()]
          .sort(([, left], [, right]) => (left < right ? -1 : 1))
          .map(([txId]) => txId),
      ).toEqual(committedOrder.map((tx) => tx.txId.toString("hex")));

      // Replayed from the block's own post-state instead of its pre-state, the
      // referencer's input is gone at its position, so the same payload is
      // refused: the committee's verdict depends on the state before each
      // transaction.
      const stored = await runNodeDatabaseEffect(
        DaPayloadsDB.retrieveByHeaderHash(
          Buffer.from(shapesBlock.queuedHeaderHash, "hex"),
        ),
      );
      if (Option.isNone(stored)) throw new Error("missing persisted payload");
      const postState = verified.payload.block_body.utxos.map(
        ([outRefHex, outputHex]) =>
          [outRefHex, Buffer.from(outputHex, "hex")] as const,
      );
      const refusal = await verifyDaPayloadAgainstHeader(
        stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR],
        shapesBlock.queuedHeaderHash,
        shapesBlock.queuedHeader,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: `emulator:${shapesBlock.queuedHeaderHash}`,
          preBlockUtxos: postState,
        },
      ).then(
        () => undefined,
        (error: unknown) => error,
      );
      expect(refusal).toBeInstanceOf(DaPayloadValidationError);
      expect(refusal).toMatchObject({ code: "malformed_transaction" });
      expect(String((refusal as Error).message)).toMatch(
        /is absent from the state immediately before the transaction/u,
      );
    });
  },
);
