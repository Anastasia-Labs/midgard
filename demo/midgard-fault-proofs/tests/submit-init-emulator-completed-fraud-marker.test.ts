/** Applied queue spend authority. Queue issuance and completed-proof issuance
 * are fixture native policies; terminal fraud semantics and merge races need
 * their separate production integration scenarios. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  Emulator,
  fromText,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  mintingPolicyToId,
  scriptFromNative,
  toUnit,
  type TxSignBuilder,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import { prepareFabricatedCompletedFraud } from "../src/fabricated-completed-fraud.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
type Fault =
  | "missing-mint"
  | "foreign-policy"
  | "other-header"
  | "clear-marker"
  | "rewrite-marker"
  | "change-link"
  | "change-status"
  | "change-address"
  | "redirect-input";

const setup = async (previouslyMarked = false) => {
  const wallet = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const owner = getAddressDetails(wallet.address).paymentCredential!.hash;
  const queueIssuer = scriptFromNative({ type: "sig", keyHash: owner });
  const proofIssuer = scriptFromNative({
    type: "all",
    scripts: [{ type: "sig", keyHash: owner }],
  });
  const queuePolicy = mintingPolicyToId(queueIssuer);
  const proofPolicy = mintingPolicyToId(proofIssuer);
  const spend = {
    type: "PlutusV3" as const,
    script: SDK.applyBlueprintParams(blueprint, "state_queue.spend.spend", [
      queuePolicy,
      "aa".repeat(28),
      "bb".repeat(28),
      proofPolicy,
    ]),
  };
  const address = validatorToAddress("Custom", spend);
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  // Keep the production 60-second lower-bound backoff after emulator genesis.
  emulator.awaitSlot(100);
  const header = makeHeader(owner, emulator.now() - 10_000);
  const headerHash = Effect.runSync(SDK.hashBlockHeader(header));
  const proofAsset = "00000001" + headerHash;
  const unit = toUnit(queuePolicy, fromText("MBLC") + headerHash);
  const node: SDK.StateQueueNode = {
    header,
    da_attestation: SDK.NO_DA_ATTESTATION,
    proven_fraud: previouslyMarked ? proofAsset : null,
  };
  const view: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next: { Key: { key: "ff".repeat(28) } },
    data: SDK.castStateQueueNodeToData(node) as SDK.LinkedListNodeView["data"],
  };
  const datum = SDK.encodeLinkedListNodeView(view);
  const submit = async (label: string, tx: TxSignBuilder) => {
    const signed = await tx.sign.withWallet().complete();
    const cbor = signed.toCBOR();
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
    const txHash = await signed.submit().catch((cause: unknown) => {
      throw new Error(`${label}: ${inspect(cause, { depth: 12 })}`);
    });
    await lucid.awaitTx(txHash);
    records.push({
      label,
      queuePolicy,
      proofPolicy,
      address,
      headerHash,
      previouslyMarked,
      measurement,
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee(),
      txHash,
      transactionCbor: cbor,
    });
    return txHash;
  };
  await submit(
    "fixture-queue-issuance",
    await lucid
      .newTx()
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(queueIssuer)
      .pay.ToContract(
        address,
        { kind: "inline", value: datum },
        { lovelace: 10_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true }),
  );
  const hubAddress = validatorToAddress("Custom", queueIssuer);
  const hubUnit = toUnit(queuePolicy, SDK.HUB_ORACLE_ASSET_NAME);
  const addr = Effect.runSync(SDK.addressDataFromBech32(address));
  const hubDatum: SDK.HubOracleDatum = {
    registered_operators: queuePolicy,
    active_operators: queuePolicy,
    retired_operators: queuePolicy,
    scheduler: queuePolicy,
    state_queue: queuePolicy,
    fraud_proof_catalogue: queuePolicy,
    fraud_proof: proofPolicy,
    deposit: queuePolicy,
    withdrawal: queuePolicy,
    tx_order: queuePolicy,
    settlement: queuePolicy,
    payout: queuePolicy,
    registered_operators_addr: addr,
    active_operators_addr: addr,
    retired_operators_addr: addr,
    scheduler_addr: addr,
    state_queue_addr: addr,
    fraud_proof_catalogue_addr: addr,
    fraud_proof_addr: addr,
    deposit_addr: addr,
    withdrawal_addr: addr,
    tx_order_addr: addr,
    settlement_addr: addr,
    reserve_addr: addr,
    payout_addr: addr,
    reserve_observer: queuePolicy,
  };
  await submit(
    "fixture-terminal-hub-and-reference",
    await lucid
      .newTx()
      .mintAssets({ [hubUnit]: 1n })
      .attach.MintingPolicy(queueIssuer)
      .pay.ToContract(
        hubAddress,
        { kind: "inline", value: Data.to(hubDatum, SDK.HubOracleDatum) },
        { lovelace: 10_000_000n, [hubUnit]: 1n },
      )
      .pay.ToAddressWithData(
        wallet.address,
        undefined,
        { lovelace: 50_000_000n },
        spend,
      )
      .complete({ localUPLCEval: true }),
  );
  const queueReferenceScript = (await lucid.utxosAt(wallet.address)).find(
    (u) => u.scriptRef?.script === spend.script,
  )!;
  expect(queueReferenceScript).toBeDefined();
  const prepare = (
    overrides: Partial<
      Parameters<typeof prepareFabricatedCompletedFraud>[0]
    > = {},
  ) =>
    prepareFabricatedCompletedFraud({
      lucid,
      hubOraclePolicyId: queuePolicy,
      stateQueuePolicyId: queuePolicy,
      headerHash,
      headerEnd: header.endTime,
      proofAsset,
      queueReferenceScript,
      now: emulator.now(),
      ...overrides,
    });
  const input = await lucid.utxoByUnit(unit);
  const build = async (fault?: Fault) => {
    const proofName =
      fault === "other-header" ? "00000001" + "ee".repeat(28) : proofAsset;
    const continued: SDK.StateQueueNode = {
      ...node,
      proven_fraud: fault === "clear-marker" ? null : proofName,
      da_attestation:
        fault === "change-status"
          ? { Attested: { da_bond_asset_name: "ab".repeat(32) } }
          : node.da_attestation,
    };
    const continuedDatum = SDK.encodeLinkedListNodeView({
      ...view,
      next: fault === "change-link" ? "Empty" : view.next,
      data: SDK.castStateQueueNodeToData(
        continued,
      ) as SDK.LinkedListNodeView["data"],
    });
    const redeemer = ((ctx) =>
      Data.to(
        {
          RecordCompletedFraud: {
            state_queue_input_index:
              fault === "redirect-input"
                ? 100n
                : SDK.requireInputIndex(ctx, input, "completed fraud"),
            state_queue_output_index: 0n,
            fraud_proof_asset_name: proofName,
          },
        },
        SDK.StateQueueSpendRedeemer,
      )) satisfies BuildTxWithRedeemer;
    let tx = lucid
      .newTx()
      .collectFrom([input], redeemer)
      .attach.SpendingValidator(spend)
      .validFrom(emulator.now())
      .validTo(emulator.now() + 20_000)
      .pay.ToContract(
        fault === "change-address"
          ? validatorToAddress("Custom", queueIssuer)
          : address,
        { kind: "inline", value: continuedDatum },
        input.assets,
      );
    if (fault !== "missing-mint")
      tx = tx
        .mintAssets({
          [toUnit(
            fault === "foreign-policy" ? queuePolicy : proofPolicy,
            proofName,
          )]: 1n,
        })
        .attach.MintingPolicy(
          fault === "foreign-policy" ? queueIssuer : proofIssuer,
        );
    return tx.complete({ localUPLCEval: true });
  };
  return {
    lucid,
    input,
    unit,
    submit,
    build,
    proofAsset,
    prepare,
    emulator,
    proofIssuer,
    proofPolicy,
    header,
    headerHash,
  };
};

afterAll(() => {
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "completed-fraud-marker.json"),
    JSON.stringify(
      {
        scope:
          "Applied state-queue spend; fixture queue/proof native issuers; terminal policy coupling and production merge race incomplete",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

describe("applied completed-fraud queue marker", () => {
  it("consumes the exact queue node and permanently marks its continuation", async () => {
    const h = await setup();
    const txHash = await h.submit("record-completed-fraud", await h.build());
    const continued = await h.lucid.utxoByUnit(h.unit);
    expect(continued.txHash).toBe(txHash);
    expect(await h.lucid.utxosByOutRef([h.input])).toHaveLength(0);
    const view = SDK.linkedListDatumToNodeView(
      Data.from(continued.datum!, SDK.LinkedListDatum),
      h.unit.slice(56),
    );
    expect(
      Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(view))
        .proven_fraud,
    ).toBe(h.proofAsset);
  });

  it.each([false, true])(
    "production queue preparation preserves an existing marker=%s",
    async (previouslyMarked) => {
      const h = await setup(previouslyMarked);
      const terminalAsset = previouslyMarked
        ? "00000002" + h.headerHash
        : h.proofAsset;
      const prepared = await h.prepare({ proofAsset: terminalAsset });
      expect(prepared.previouslyRecorded).toBe(previouslyMarked);
      const tx = prepared.apply(
        h.lucid
          .newTx()
          .mintAssets({ [toUnit(h.proofPolicy, terminalAsset)]: 1n })
          .attach.MintingPolicy(h.proofIssuer),
      );
      const txHash = await h.submit(
        `production-queue-prepare-${previouslyMarked}`,
        await tx.complete({ localUPLCEval: true }),
      );
      const continued = await h.lucid.utxoByUnit(h.unit);
      expect(continued.txHash).toBe(previouslyMarked ? h.input.txHash : txHash);
      const view = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(continued),
      );
      expect(
        Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(view))
          .proven_fraud,
      ).toBe(h.proofAsset);
      expect(continued.assets).toEqual(h.input.assets);
    },
  );

  it("production queue preparation refreshes a raced marker and rejects the stale spend", async () => {
    const h = await setup();
    const old = await h.prepare();
    const stale = await (
      await old
        .apply(
          h.lucid
            .newTx()
            .mintAssets({ [toUnit(h.proofPolicy, h.proofAsset)]: 1n })
            .attach.MintingPolicy(h.proofIssuer),
        )
        .complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    await h.submit(
      "competing-marker-before-production-terminal",
      await h.build(),
    );
    await expect(stale.submit()).rejects.toThrow();
    const refreshed = await h.prepare();
    expect(refreshed.previouslyRecorded).toBe(true);
    expect(refreshed.input.txHash).not.toBe(old.input.txHash);
  });

  it("production queue preparation refuses wrong identity, script, and expired deadline", async () => {
    const h = await setup();
    await expect(
      h.prepare({ stateQueuePolicyId: "ee".repeat(28) }),
    ).rejects.toThrow("differs from the hub");
    await expect(
      h.prepare({ proofAsset: "00000001" + "ff".repeat(28) }),
    ).rejects.toThrow("does not name");
    await expect(
      h.prepare({ queueReferenceScript: undefined }),
    ).rejects.toThrow("queue spending reference script");
    await expect(
      h.prepare({ now: Number(h.header.endTime + SDK.MATURITY_DURATION_MS) }),
    ).rejects.toThrow("before merge");
    expect((await h.lucid.utxoByUnit(h.unit)).txHash).toBe(h.input.txHash);
  });

  it.each([
    "missing-mint",
    "foreign-policy",
    "other-header",
    "clear-marker",
    "rewrite-marker",
    "change-link",
    "change-status",
    "change-address",
    "redirect-input",
  ] as const)("refuses %s", async (fault) => {
    const h = await setup(fault === "rewrite-marker");
    let refusal: unknown;
    try {
      await h.build(fault);
    } catch (error) {
      refusal = error;
    }
    expect(inspect(refusal, { depth: 10 })).toMatch(/failed script execution/);
    expect((await h.lucid.utxoByUnit(h.unit)).txHash).toBe(h.input.txHash);
    records.push({
      label: `refused-${fault}`,
      reason: inspect(refusal, { depth: 10 }),
    });
  });

  it("invalidates a competing prepared spend through the consumed queue out-ref", async () => {
    const h = await setup();
    const first = await h.build();
    const competing = await h.build();
    const stale = await competing.sign.withWallet().complete();
    await h.submit("record-wins-input-race", first);
    await expect(stale.submit()).rejects.toThrow();
    records.push({ label: "stale-competing-spend-refused", input: h.input });
  });
});
