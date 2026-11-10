import { mkdir, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";

import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";
import { vi } from "vitest";

import { type JourneyLedger, journeySteps } from "../src/full-stack/journey.js";
import {
  RecordingProcesses,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

/**
 * The e2e-stack wallet journey (`journey.ts`) over a recording stack: the
 * node's L2 ledger as scripted holdings, /tx-status as a stubbed fetch, and
 * public DA as a recorded check.
 */
export async function journey() {
  const { config } = await stackFixture();
  const env: Record<string, string> = {
    ...stackEnvironment(config),
    MIN_FEE_A: "44",
    MIN_FEE_B: "155381",
  };
  const processes = new RecordingProcesses(config, env);
  const user = walletFromSeed(env[config.wallets.user!.seedEnv]!, {
    network: "Preprod",
  }).address;
  const recipient = walletFromSeed(env[config.wallets.recipient!.seedEnv]!, {
    network: "Preprod",
  }).address;
  const reads: string[] = [];
  const sent: { cbor: string; txId: string }[] = [];
  let submit = async (): Promise<unknown> => {
    throw new Error("response lost");
  };
  // Each address's spendable L2 outputs, as the node's /utxos serves them.
  const holdings = new Map<string, { txHash: string; lovelace: bigint }[]>([
    [user, [{ txHash: "44".repeat(32), lovelace: 50_000_000n }]],
  ]);
  const ledger: JourneyLedger = {
    utxos: async (address) => {
      reads.push(address);
      return (holdings.get(address) ?? []).map(({ txHash, lovelace }) => {
        const assets = { lovelace };
        return {
          txHash,
          outputIndex: 0,
          outrefCbor: makeOutRefCbor(txHash, 0),
          outputCbor: Buffer.from(
            makeMidgardTxOutput(
              CML.Address.from_bech32(address),
              assetsToValue(assets),
            ).to_cbor_bytes(),
          ),
          address,
          assets,
        };
      });
    },
    submitTransfer: async (cbor, txId) => {
      sent.push({ cbor, txId });
      return submit();
    },
  };
  // Public DA as the stack serves it: each verified header's saved record.
  const verifiedDa: string[] = [];
  const verifyDa = async (_processes: unknown, headerHash: string) => {
    verifiedDa.push(headerHash);
    return {
      headerHash,
      sha256: "5a".repeat(32),
      deploymentId: "ab".repeat(32),
    };
  };
  const steps = journeySteps(processes, ledger, verifyDa);
  const byId = (id: string) => steps.find((step) => step.id === id)!;
  const context = {
    directory: config.runDirectory,
    intentDigest: "a".repeat(64),
  };
  const transferFile = join(config.runDirectory, "cycle-0-transfer.json");
  const txStatus = (body: unknown) =>
    vi.stubGlobal("fetch", async () => new Response(JSON.stringify(body)));
  const file = (name: string) =>
    join(config.runDirectory, `cycle-0-${name}.json`);
  return {
    byId,
    config,
    context,
    file,
    /** Sets an address's spendable L2 outputs. */
    hold: (address: string, outputs: { txHash: string; lovelace: bigint }[]) =>
      holdings.set(address, outputs),
    processes,
    reads,
    recipient,
    sent,
    setSubmit: (next: typeof submit) => (submit = next),
    steps,
    transferFile,
    txStatus,
    user,
    verifiedDa,
    /** The deployment manifest the stack's settlement reads name. */
    writeManifest: async () => {
      const path = join(
        config.nodeRoot,
        "deploymentInfo/contract-deployment-info.json",
      );
      await mkdir(dirname(path), { recursive: true });
      await writeFile(
        path,
        JSON.stringify({
          manifestId: "ab".repeat(32),
          contracts: { payoutMint: { scriptHash: "cd".repeat(28) } },
        }),
      );
    },
  };
}

export const DEPOSIT_EVENT = "de".repeat(32);
export const HEADER_HASH = "cd".repeat(28);
export const TRANSFER_TX = "7a".repeat(32);

/** A saved deposit: its intent (the user's L2 balance before) and its receipt. */
export const savedDeposit = async (
  value: Awaited<ReturnType<typeof journey>>,
) => {
  await mkdir(value.config.runDirectory, { recursive: true });
  await writeFile(
    value.file("deposit-intent"),
    JSON.stringify({ balance: "50000000" }),
  );
  await writeFile(
    value.file("deposit"),
    JSON.stringify({ kind: "Deposit", eventId: DEPOSIT_EVENT }),
  );
};

/**
 * A saved transfer from the user's 50 ADA, and the L2 balances it leaves:
 * the user's change and the recipient's one received output.
 */
export const savedTransfer = async (
  value: Awaited<ReturnType<typeof journey>>,
) => {
  const fee = 200_000n;
  const amount = BigInt(value.config.journey.transferLovelace);
  await mkdir(value.config.runDirectory, { recursive: true });
  await writeFile(
    value.transferFile,
    JSON.stringify({
      txId: TRANSFER_TX,
      signedTxCbor: "00",
      fee: String(fee),
      senderBalanceBefore: "50000000",
      recipientBalanceBefore: "0",
    }),
  );
  value.hold(value.user, [
    { txHash: TRANSFER_TX, lovelace: 50_000_000n - amount - fee },
  ]);
  value.hold(value.recipient, [{ txHash: TRANSFER_TX, lovelace: amount }]);
};

/** The node's /tx-status of the saved transfer, committed in `HEADER_HASH`. */
export const committedTransfer = (confirmedLedgerFinalized: boolean) => ({
  txId: TRANSFER_TX,
  status: "committed",
  committedMeaning: "immutable_db_inclusion_not_confirmed_ledger_merge",
  headerHash: HEADER_HASH,
  confirmedLedgerFinalized,
});
