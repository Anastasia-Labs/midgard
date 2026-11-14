import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "history-bootstrap.json"),
    JSON.stringify(
      {
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_key, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      2,
    ),
  );
});

const setup = async () => {
  const wallet = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const submit = async (label: string, tx: TxSignBuilder) => {
    const signed = await tx.sign.withWallet().complete();
    const transactionCbor = signed.toCBOR();
    const measurement = measureCompleteSignedTransaction(transactionCbor);
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
    );
    expect(measurement.executionMemory).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
    );
    expect(measurement.executionSteps).toBeLessThanOrEqual(
      EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
    );
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);
    records.push({
      label,
      measurement,
      transactionCbor,
      txHash,
      fee: CML.Transaction.from_cbor_hex(transactionCbor).body().fee(),
    });
    return txHash;
  };
  const split = await submit(
    "reserve-shared-nonce",
    await lucid
      .newTx()
      .pay.ToAddress(wallet.address, { lovelace: 2_000_000n })
      .complete({ localUPLCEval: true }),
  );
  const [nonce] = await lucid.utxosByOutRef([
    { txHash: split, outputIndex: 0 },
  ]);
  if (nonce === undefined) throw new Error("Missing reserved nonce");
  const contracts = SDK.buildEventHistoryDeployments({
    blueprint,
    network: "Custom",
    hubOraclePolicyId: "ab".repeat(28),
    initializationNonce: nonce,
    protectionDurationMs: 2_000n,
    bounds: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
    },
  });
  records.push({
    label: "deployment",
    deposit: contracts.deposit.recipe,
    withdrawal: contracts.withdrawal.recipe,
    policies: [
      contracts.deposit.list.policyId,
      contracts.withdrawal.list.policyId,
    ],
  });
  const funding = async () =>
    (await lucid.wallet().getUtxos()).filter(
      (u) =>
        u.scriptRef == null &&
        !(u.txHash === nonce.txHash && u.outputIndex === nonce.outputIndex),
    );
  const scripts = {} as Record<"deposit" | "withdrawal", typeof nonce>;
  for (const kind of ["deposit", "withdrawal"] as const) {
    const hash = await submit(
      `publish-${kind}-observer`,
      await lucid
        .newTx()
        .collectFrom(await funding())
        .pay.ToAddressWithData(
          wallet.address,
          undefined,
          { lovelace: 100_000_000n },
          contracts[kind].list.mintingScript,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    const [script] = await lucid.utxosByOutRef([
      { txHash: hash, outputIndex: 0 },
    ]);
    if (script === undefined) throw new Error("Missing list reference script");
    scripts[kind] = script;
  }
  const params = () => ({
    contracts,
    nonce,
    referenceScripts: scripts,
    validFrom: emulator.now() - 60_000 + 123,
    validTo: emulator.now() + 10_000 + 123,
  });
  const register = async () => {
    const tx = lucid.newTx().collectFrom(await funding());
    for (const { list } of Object.values(contracts))
      tx.register.Stake(
        SDK.scriptRewardAddress("Custom", list.withdrawalScript),
      );
    await submit(
      "register-before-bootstrap",
      await tx.complete({ coinSelection: false, localUPLCEval: true }),
    );
    expect(await lucid.utxosByOutRef([nonce])).toHaveLength(1);
  };
  return {
    lucid,
    emulator,
    wallet,
    submit,
    nonce,
    contracts,
    funding,
    scripts,
    params,
    register,
  };
};

it("initializes both genuine lists with one nonce after composition and wallet coin selection", async () => {
  const h = await setup();
  await h.register();
  const params = h.params();
  const tx = SDK.appendEventHistoryInitialization(
    h.lucid,
    h.lucid
      .newTx()
      .collectFrom([h.nonce])
      .pay.ToAddress(h.wallet.address, { lovelace: 3_000_000n }),
    params,
  );
  // The nonce cannot fund the outputs alone: completion must add wallet inputs.
  const signed = await tx.complete({ localUPLCEval: true });
  await h.submit("atomic-both-history-roots", signed);
  expect(await h.lucid.utxosByOutRef([h.nonce])).toHaveLength(0);
  expect(h.contracts.deposit.list.policyId).not.toBe(
    h.contracts.withdrawal.list.policyId,
  );
  for (const name of ["deposit", "withdrawal"] as const) {
    const { list, retention, recipe } = h.contracts[name];
    const nodes = SDK.authenticateHistoryNodes(
      await h.lucid.utxosAt(list.spendingScriptAddress),
      {
        policyId: list.policyId,
        address: list.spendingScriptAddress,
        retentionAddress: retention.spendingScriptAddress,
        inlineLimitBytes: recipe.inlineLimitBytes,
      },
    );
    expect(nodes).toHaveLength(1);
    expect(nodes[0]!.node).toEqual({
      position: "Root",
      next: null,
      payload: "RootContent",
      protected_until:
        BigInt(h.lucid.slotToUnixTime(h.lucid.unixTimeToSlot(params.validTo))) -
        1n +
        recipe.protectionDurationMs,
    });
    expect(nodes[0]!.utxo.outputIndex).toBe(name === "deposit" ? 1 : 2);
    expect(nodes[0]!.utxo.assets.lovelace).toBeGreaterThanOrEqual(
      SDK.eventHistoryMinimumNodeLovelace(
        nodes[0]!.utxo.assets,
        nodes[0]!.node,
      ),
    );
  }
}, 180_000);

it("requires the configured shared nonce for both list recipes", async () => {
  const h = await setup();
  const params = h.params();
  const contracts = {
    ...params.contracts,
    withdrawal: {
      ...params.contracts.withdrawal,
      recipe: {
        ...params.contracts.withdrawal.recipe,
        initializationNonce: {
          transactionId: "cd".repeat(32),
          outputIndex: 0n,
        },
      },
    },
  };
  expect(() =>
    SDK.appendEventHistoryInitialization(h.lucid, h.lucid.newTx(), {
      ...params,
      contracts,
    }),
  ).toThrow("declared shared nonce");
  expect(await h.lucid.utxosByOutRef([h.nonce])).toHaveLength(1);
}, 180_000);

it("refuses bootstrap when the reserved nonce is absent from final inputs", async () => {
  const h = await setup();
  await h.register();
  const tx = SDK.appendEventHistoryInitialization(
    h.lucid,
    h.lucid.newTx().collectFrom(await h.funding()),
    h.params(),
  );
  await expect(
    tx.complete({ coinSelection: false, localUPLCEval: true }),
  ).rejects.toThrow(/missing from final tx inputs/u);
  expect(await h.lucid.utxosByOutRef([h.nonce])).toHaveLength(1);
}, 180_000);
