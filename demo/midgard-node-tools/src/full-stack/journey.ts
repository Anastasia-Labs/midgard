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
import { awaitExactPayout, settlementEvidence } from "./payout.js";
import { getJson, poll, type StackProcesses } from "./process.js";
import { verifyPublicDa } from "./public-da.js";
import { confirmRuntimeReadiness } from "./runtime.js";
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
export function journeySteps(processes: StackProcesses): StackStep[] {
  const { config, env } = processes;
  const user = walletFromSeed(env[config.wallets.user!.seedEnv]!, {
    network: "Preprod",
  });
  const recipient = walletFromSeed(env[config.wallets.recipient!.seedEnv]!, {
    network: "Preprod",
  });
  const utxos = (address: string) =>
    Effect.runPromise(fetchNodeUtxos(config.endpoint, address, 10_000));
  const balance = async (address: string) =>
    (await utxos(address)).reduce(
      (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
      0n,
    );
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
          if (
            after !==
            BigInt(before.balance) + BigInt(config.journey.depositLovelace)
          )
            throw new Error("Deposit did not credit the exact L2 balance");
          await writeDurableJson(file("deposit-balance"), {
            before: before.balance,
            after: String(after),
            credited: config.journey.depositLovelace,
          });
        }
        return { status: "complete", data: { eventId, observed } };
      },
      execute: async () => {
        if ((await readJsonIfPresent(file("deposit-intent"))) === undefined)
          await writeDurableJson(file("deposit-intent"), {
            balance: String(await balance(user.address)),
          });
        const run = await journal();
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
          const expected =
            BigInt(intent.senderBalanceBefore) -
            BigInt(config.journey.transferLovelace) -
            BigInt(intent.fee);
          const actual = await balance(user.address);
          if (actual !== expected)
            throw new Error("L2 transfer balance or fee does not match");
          const recipientBalanceAfter = await balance(recipient.address);
          if (
            recipientBalanceAfter !==
            BigInt(intent.recipientBalanceBefore) +
              BigInt(config.journey.transferLovelace)
          )
            throw new Error("Recipient L2 balance does not match the transfer");
          const received = (await utxos(recipient.address)).filter(
            (utxo) => utxo.txHash === intent.txId,
          );
          if (
            received.length !== 1 ||
            received[0]!.assets.lovelace !==
              BigInt(config.journey.transferLovelace)
          )
            throw new Error("Recipient did not receive the exact transfer");
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
            senderBalanceBefore: String(
              available.reduce(
                (sum, value) => sum + (value.assets.lovelace ?? 0n),
                0n,
              ),
            ),
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
        return Effect.runPromise(
          submitNativeTransferTx(
            config.endpoint,
            intent.signedTxCbor,
            intent.txId,
            30_000,
          ),
        );
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
          const after = available.reduce(
            (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
            0n,
          );
          if (
            after !==
            BigInt(intent.recipientBalanceBefore) -
              BigInt(intent.assets.lovelace!)
          )
            throw new Error(
              "Withdrawal did not debit the exact recipient L2 balance",
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
