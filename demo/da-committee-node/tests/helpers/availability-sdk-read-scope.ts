import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import type { DaAvailabilityOperationContext } from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";
export const dirs: string[] = [];
export const journals: ReturnType<typeof openAvailabilityOperationJournal>[] =
  [];
/** Real signed CML bytes and SQLite reservations; only the wallet's async
 * signing boundary and provider observations are controlled. */
export const scene = () => {
  const dir = mkdtempSync(join(tmpdir(), "availability-read-scope-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(
    join(dir, "operations.sqlite"),
  );
  journals.push(journal);
  const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
  const actor = key.to_public().hash().to_hex();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("ab".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(
        credentialToAddress("Preprod", { type: "Key", hash: actor }),
      ),
      CML.Value.from_coin(5_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 100_000n);
  body.set_validity_interval_start(0n);
  body.set_ttl(1000n);
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), key));
  witnesses.set_vkeywitnesses(vkeys);
  const signed = CML.Transaction.new(body, witnesses, true);
  const sign = vi.fn(async () => ({ toCBOR: () => signed.to_cbor_hex() }));
  const tx = {
    toTransaction: () => signed,
    sign: { withWallet: () => ({ complete: sign }) },
  } as unknown as TxSignBuilder;
  const context: DaAvailabilityOperationContext = {
    actor,
    deploymentIdentity: "11".repeat(32),
    stateQueuePolicyId: "22".repeat(28),
    journal,
    minimumConfirmationDepth: 10,
    transactionLimits: {
      maxTxSize: 16_384,
      maxTxExMem: 14_000_000n,
      maxTxExSteps: 10_000_000_000n,
      coinsPerUtxoByte: 4310n,
      feeCeilings: { prepare: 500_000n },
    },
    assertActuationCurrent: vi.fn(async () => {}),
    observe: vi.fn(async () => ({
      status: "unspent" as const,
      currentSlot: 0,
    })),
    submit: vi.fn(async () => CML.hash_transaction(body).to_hex()),
    nowMs: Date.now,
    monotonicMs: Date.now,
  };
  const build = vi.fn(async (_signal: AbortSignal) => tx);
  const operation = {
    action: "prepare" as const,
    headerHash: "33".repeat(28),
    unsignedDeadlineMs: 1100,
    build,
  };
  return {
    context,
    journal,
    tx,
    sign,
    build,
    operation,
    path: join(dir, "operations.sqlite"),
  };
};
