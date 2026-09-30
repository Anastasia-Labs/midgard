import { join } from "node:path";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  buildTransferTxWithMinFee,
  fetchNodeUtxos,
  submitNativeTransferTx,
} from "midgard-node/commands/submit-l2-transfer";
import type { ResolvedTxStatus } from "midgard-node/commands/tx-status";

import {
  readJsonIfPresent,
  type StackJournal,
  writeDurableJson,
} from "./journal.js";
import {
  assertDepositCredit,
  assertDepositFunding,
  assertTransferDeltas,
  assertWithdrawalDebit,
} from "./journey-balances.js";
import { awaitExactPayout, settlementEvidence } from "./payout.js";
import { getJson, poll, type StackProcesses } from "./process.js";
import { verifyPublicDa } from "./public-da.js";
import { confirmRuntimeReadiness } from "./runtime.js";
import { assertHostDatabaseIsStackDatabase } from "./storage.js";
import type { StackStep } from "./workflow.js";

type TransferIntent = {
  txId: string;
  signedTxCbor: string;
  fee: string;
  senderBalanceBefore: string;
  recipientBalanceBefore: string;
};
type WithdrawalIntent = {
  l2OutRef: string;
  address: string;
  assets: Record<string, string>;
  recipientBalanceBefore: string;
};
type NodeUtxos = Effect.Effect.Success<ReturnType<typeof fetchNodeUtxos>>;
/** The node's L2 ledger as the journey reads and writes it. */
export type JourneyLedger = {
  utxos(address: string): Promise<NodeUtxos>;
  submitTransfer(signedTxCbor: string, txId: string): Promise<unknown>;
};
export const nodeLedger = (endpoint: string): JourneyLedger => ({
  utxos: (address) =>
    Effect.runPromise(fetchNodeUtxos(endpoint, address, 10_000)),
  submitTransfer: (signedTxCbor, txId) =>
    Effect.runPromise(
      submitNativeTransferTx(endpoint, signedTxCbor, txId, 30_000),
    ),
});
const lovelaceOf = (utxos: NodeUtxos) =>
  utxos.reduce((sum, utxo) => sum + (utxo.assets.lovelace ?? 0n), 0n);

export function journeySteps(
  processes: StackProcesses,
  ledger: JourneyLedger = nodeLedger(processes.config.endpoint),
): StackStep[] {
  const { config, env } = processes;
  const user = walletFromSeed(env[config.wallets.user!.seedEnv]!, {
    network: "Preprod",
  });
  const recipient = walletFromSeed(env[config.wallets.recipient!.seedEnv]!, {
    network: "Preprod",
  });
  const utxos = (address: string) => ledger.utxos(address);
  const balance = async (address: string) => lovelaceOf(await utxos(address));
  const journal = () =>
    readJsonIfPresent(
      join(config.runDirectory, "stack-journal.json"),
    ) as Promise<StackJournal>;
  const steps: StackStep[] = [];
  for (let cycle = 0; cycle < config.journey.cycles; cycle++) {
    const prefix = `cycle-${cycle}`;
    const file = (name: string) =>
      join(config.runDirectory, `${prefix}-${name}.json`);
    steps.push({
      id: `${prefix}-deposit`,
      reconcile: async () => {
        const receipt = (await readJsonIfPresent(file("deposit"))) as
          | { metadata: { depositEventId: string } }
          | undefined;
        if (!receipt) return { status: "retry" };
        const eventId = receipt.metadata?.depositEventId;
        if (!eventId || !/^[0-9a-f]+$/.test(eventId))
          throw new Error("Deposit receipt lacks its exact event identity");
        const observed = await poll(
          "automatic deposit absorption",
          config.timeoutMs,
          async () => {
            const evidence = await settlementEvidence(
              processes,
              "deposit",
              eventId,
            );
            return evidence.jobs?.length === 1 &&
              evidence.jobs[0]!.phase === "complete"
              ? evidence
              : undefined;
          },
        );
        if ((await readJsonIfPresent(file("deposit-balance"))) === undefined) {
          const before = (await readJsonIfPresent(file("deposit-intent"))) as {
            balance: string;
          };
          const after = await balance(user.address);
          assertDepositCredit(
            BigInt(before.balance),
            after,
            BigInt(config.journey.depositLovelace),
          );
          await writeDurableJson(file("deposit-balance"), {
            before: before.balance,
            after: String(after),
            credited: config.journey.depositLovelace,
          });
        }
        return { status: "complete", data: { eventId, observed } };
      },
      execute: async () => {
        if ((await readJsonIfPresent(file("deposit-intent"))) === undefined) {
          // A resend after a lost response reuses the intent; only a first send checks funds.
          const funds = (await processes.node(`${prefix}-deposit-funds`, [
            "l1-utxos",
            "--address",
            user.address,
          ])) as { totals: { lovelace: string } };
          assertDepositFunding(
            BigInt(funds.totals.lovelace),
            BigInt(config.journey.depositLovelace),
          );
          await writeDurableJson(file("deposit-intent"), {
            balance: String(await balance(user.address)),
          });
        }
        const run = await journal();
        await assertHostDatabaseIsStackDatabase(processes);
        const receipt = await processes.node(`${prefix}-deposit-submit`, [
          "submit-deposit",
          "--submission-id",
          `${run.runId}:${prefix}:deposit`,
          "--wallet-seed-phrase-env",
          config.wallets.user!.seedEnv,
          "--l2-address",
          user.address,
          "--lovelace",
          config.journey.depositLovelace,
        ]);
        await writeDurableJson(file("deposit"), receipt);
        return receipt;
      },
    });
    steps.push({
      id: `${prefix}-transfer`,
      reconcile: async () => {
        const intent = (await readJsonIfPresent(file("transfer"))) as
          | TransferIntent
          | undefined;
        if (!intent) return { status: "retry" };
        const status = (await getJson(
          `${config.endpoint}/tx-status?tx_hash=${intent.txId}`,
        )) as ResolvedTxStatus | undefined;
        if (status?.status === "rejected")
          throw new Error(`Transfer rejected: ${status.reasonCode}`);
        if (!status || status.status === "not_found")
          return { status: "retry" };
        const committed = await poll(
          "transfer commitment",
          config.timeoutMs,
          async () => {
            const value = (await getJson(
              `${config.endpoint}/tx-status?tx_hash=${intent.txId}`,
            )) as ResolvedTxStatus | undefined;
            if (value?.status === "rejected")
              throw new Error(`Transfer rejected: ${value.reasonCode}`);
            return value?.status === "committed" && value.headerHash
              ? value
              : undefined;
          },
        );
        const da = await verifyPublicDa(processes, committed.headerHash!);
        const merged = await poll(
          "automatic transfer merge",
          config.timeoutMs,
          async () => {
            const value = (await getJson(
              `${config.endpoint}/tx-status?tx_hash=${intent.txId}`,
            )) as ResolvedTxStatus | undefined;
            return value?.status === "committed" &&
              value.confirmedLedgerFinalized === true
              ? value
              : undefined;
          },
        );
        if ((await readJsonIfPresent(file("transfer-balance"))) === undefined) {
          const actual = await balance(user.address);
          const recipientUtxos = await utxos(recipient.address);
          const recipientBalanceAfter = lovelaceOf(recipientUtxos);
          assertTransferDeltas({
            amount: BigInt(config.journey.transferLovelace),
            fee: BigInt(intent.fee),
            senderBefore: BigInt(intent.senderBalanceBefore),
            senderAfter: actual,
            recipientBefore: BigInt(intent.recipientBalanceBefore),
            recipientAfter: recipientBalanceAfter,
            received: recipientUtxos
              .filter((utxo) => utxo.txHash === intent.txId)
              .map((utxo) => utxo.assets.lovelace ?? 0n),
          });
          await writeDurableJson(file("transfer-balance"), {
            senderBalance: String(actual),
            recipientBalance: String(recipientBalanceAfter),
            fee: intent.fee,
            received: config.journey.transferLovelace,
          });
        }
        return { status: "complete", data: { merged, da } };
      },
      execute: async () => {
        let intent = (await readJsonIfPresent(file("transfer"))) as
          | TransferIntent
          | undefined;
        if (!intent) {
          const available = await utxos(user.address);
          const built = await buildTransferTxWithMinFee({
            senderAddress: user.address,
            destinationAddress: recipient.address,
            signer: user.paymentKey,
            availableUtxos: available,
            requestedAssets: {
              lovelace: BigInt(config.journey.transferLovelace),
            },
            network: "Preprod",
            networkId: 0n,
            minFeeA: BigInt(env.MIN_FEE_A!),
            minFeeB: BigInt(env.MIN_FEE_B!),
          });
          intent = {
            txId: built.txIdHex,
            signedTxCbor: built.txHex,
            fee: String(built.fee),
            recipientBalanceBefore: String(await balance(recipient.address)),
            senderBalanceBefore: String(lovelaceOf(available)),
          };
          // This checkpoint precedes the first send, including a lost response.
          await writeDurableJson(file("transfer"), intent);
        }
        const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
          Buffer.from(intent.signedTxCbor, "hex"),
        );
        if (
          computeMidgardNativeTxId(decoded).toString("hex") !== intent.txId ||
          decoded.body.fee !== BigInt(intent.fee)
        )
          throw new Error(
            "Saved transfer differs from its exact transaction identity or fee",
          );
        return ledger.submitTransfer(intent.signedTxCbor, intent.txId);
      },
    });
    steps.push({
      id: `${prefix}-withdrawal`,
      reconcile: async () => {
        const receipt = (await readJsonIfPresent(file("withdrawal"))) as
          | { withdrawalEventId: string }
          | undefined;
        if (!receipt) return { status: "retry" };
        const intent = (await readJsonIfPresent(
          file("withdrawal-intent"),
        )) as WithdrawalIntent;
        const payout = await awaitExactPayout(
          processes,
          receipt.withdrawalEventId,
          intent.address,
          intent.assets,
        );
        const available = await utxos(recipient.address);
        if (
          available.some(
            (utxo) => `${utxo.txHash}#${utxo.outputIndex}` === intent.l2OutRef,
          )
        )
          throw new Error("Paid withdrawal remains spendable on L2");
        if (
          (await readJsonIfPresent(file("withdrawal-balance"))) === undefined
        ) {
          const after = lovelaceOf(available);
          assertWithdrawalDebit(
            BigInt(intent.recipientBalanceBefore),
            after,
            BigInt(intent.assets.lovelace!),
          );
          await writeDurableJson(file("withdrawal-balance"), {
            before: intent.recipientBalanceBefore,
            after: String(after),
            debited: intent.assets.lovelace,
          });
        }
        await confirmRuntimeReadiness(processes);
        return { status: "complete", data: payout };
      },
      execute: async () => {
        let intent = (await readJsonIfPresent(file("withdrawal-intent"))) as
          | WithdrawalIntent
          | undefined;
        if (!intent) {
          const transfer = (await readJsonIfPresent(
            file("transfer"),
          )) as TransferIntent;
          const selected = (await utxos(recipient.address)).filter(
            (utxo) => utxo.txHash === transfer.txId,
          );
          if (selected.length !== 1)
            throw new Error(
              "Cannot identify the exact transferred output for withdrawal",
            );
          const output = selected[0]!;
          intent = {
            l2OutRef: `${output.txHash}#${output.outputIndex}`,
            address: recipient.address,
            recipientBalanceBefore: String(await balance(recipient.address)),
            assets: Object.fromEntries(
              Object.entries(output.assets).map(([unit, value]) => [
                unit,
                String(value),
              ]),
            ),
          };
          await writeDurableJson(file("withdrawal-intent"), intent);
        }
        const run = await journal();
        await assertHostDatabaseIsStackDatabase(processes);
        const receipt = await processes.node(`${prefix}-withdrawal-submit`, [
          "submit-withdrawal",
          "--submission-id",
          `${run.runId}:${prefix}:withdrawal`,
          "--wallet-seed-phrase-env",
          config.wallets.recipient!.seedEnv,
          "--l2-out-ref",
          intent.l2OutRef,
          "--l1-address",
          intent.address,
          "--endpoint",
          config.endpoint,
        ]);
        await writeDurableJson(file("withdrawal"), receipt);
        return receipt;
      },
    });
  }
  return steps;
}
