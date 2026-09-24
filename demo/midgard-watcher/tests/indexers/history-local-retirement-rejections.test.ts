import {
  ForcedInclusionTxV1,
  HubOracleDatum,
  MerkleRoot,
  Proof,
  RootDomain,
  SettlementDatum,
  TxOrderDatum,
  TxOrderMintRedeemer,
  TxOrderSpendRedeemer,
  UserEventWitnessPublishRedeemer,
} from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import type { WatcherIndexedUserEvent } from "../../src/indexers/user-event-indexer.js";
import type { WatcherUserEventOriginFacts } from "../../src/indexers/user-event-origin.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import {
  durableFixture,
  historyLifecycle,
  ledgerReferenceIndex,
  openOrigin,
  ordinaryLocalOrderCreation,
  syntheticUserEventTransaction,
  transactionInput,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Semantic transactions in synthetic native blocks, through the production local
// publisher. This file makes no Cardano ledger acceptance claim.
const adjacentConstructor = (cbor: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cbor).as_constr_plutus_data()!;
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(
      constructor.alternative() + 1n,
      constructor.fields(),
    ),
  ).to_cbor_hex();
};
const truncatedConstructor = (cbor: string): string => {
  const constructor =
    CML.PlutusData.from_cbor_hex(cbor).as_constr_plutus_data()!;
  const fields = constructor.fields();
  const truncated = CML.PlutusDataList.new();
  for (let index = 0; index + 1 < fields.len(); index++)
    truncated.add(fields.get(index));
  return CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(constructor.alternative(), truncated),
  ).to_cbor_hex();
};
const redeemersOf = (cbor: string) => {
  const entries = CML.Transaction.from_cbor_hex(cbor)
    .witness_set()
    .redeemers()!
    .as_arr_legacy_redeemer()!;
  return Array.from({ length: entries.len() }, (_, index) => {
    const entry = entries.get(index);
    return {
      tag: entry.tag(),
      index: entry.index(),
      cbor: entry.data().to_cbor_hex(),
    };
  });
};
const replaceRedeemer = (
  cbor: string,
  index: number,
  replacement: string,
  variant: number,
): string => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const entries = redeemersOf(cbor);
  entries[index] = { ...entries[index]!, cbor: replacement };
  const result = CML.Transaction.from_cbor_hex(
    syntheticUserEventTransaction(transaction.body(), entries),
  );
  const body = result.body();
  // Bind each hostile witness to distinct transaction bytes: sibling branches
  // must not collide in the local fixture's creating-transaction body cache.
  const hash = Buffer.alloc(32, 0xc8);
  hash.writeUInt32BE(variant, 28);
  body.set_script_data_hash(CML.ScriptDataHash.from_raw_bytes(hash));
  return CML.Transaction.new(
    body,
    result.witness_set(),
    true,
    undefined,
  ).to_canonical_cbor_hex();
};

const openScenario = async () => {
  const fixture = await createSyntheticUserEventOriginFixture();
  const initial = await openOrigin(fixture);
  const durable = await durableFixture(
    readWatcherLocalBackfillFinality(initial.pair.finality).policy,
  );
  const publisher = await createWatcherLocalUserEventPublisher({
    ...initial.input,
    origin: initial.origin,
    runtime: durable.runtime,
    archive: durable.archive,
  });
  const publish = async (
    block: Parameters<typeof fixture.openFinalizedBlock>[0],
  ) => {
    const pair = await fixture.openFinalizedBlock(block);
    try {
      await publisher.publish(pair);
    } finally {
      await pair.close();
    }
  };
  try {
    await publisher.publish(initial.pair);
    await initial.pair.close();
    await publish(fixture.emptySuccessorBlock);
    return {
      fixture,
      initial,
      durable,
      publisher,
      publish,
      close: async () => {
        publisher.close();
        await fixture.close();
      },
    };
  } catch (cause) {
    publisher.close();
    await initial.pair.close();
    await fixture.close();
    throw cause;
  }
};

const rejectWithoutPublication = async (
  scenario: Awaited<ReturnType<typeof openScenario>>,
  parent: Parameters<typeof scenario.fixture.openFinalizedBlock>[0],
  transaction: string,
  creatingBodies: string[],
) => {
  const { fixture, publisher, durable, publish } = scenario;
  await fixture.selectCanonicalBranch(parent.point);
  const snapshot = publisher.read().snapshot;
  const cas = durable.casCount();
  const before = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(durable.runtime),
  );
  const hostile = await fixture.makeBlock({
    parent,
    transactions: [transaction],
    creatingBodies,
  });
  await expect(publish(hostile)).rejects.toThrow(
    "whole-block event semantics differ",
  );
  const after = readWatcherProtectedUserEventCheckpointReceipt(
    await readWatcherProtectedUserEventCheckpoint(durable.runtime),
  );
  expect(after.checkpoint).toEqual(before.checkpoint);
  expect(after.trustedHead).toEqual(before.trustedHead);
  expect(
    Buffer.compare(Buffer.from(after.payload!), Buffer.from(before.payload!)),
  ).toBe(0);
  expect(durable.casCount()).toBe(cas);
  expect(publisher.read().snapshot).toEqual(snapshot);
};

// Ports the former nonDepositSpendBundle forced-order branch, using the current
// local origin's deployed hub and a referenced settlement creating transaction.
const forcedTerminal = (
  facts: WatcherUserEventOriginFacts,
  event: WatcherIndexedUserEvent,
) => {
  if (event.witnessScriptHash === undefined)
    throw new Error("forced witness missing");
  const hub = Data.from(facts.activation.hubDatumCbor, HubOracleDatum);
  const datum = Data.from(event.datumCborHex, TxOrderDatum);
  const phasRoot = "a4".repeat(32);
  const domain = "ForcedTransactionsV1RootDomain" as const;
  const root = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardRootCountV1"),
        Buffer.from(Data.to(domain, RootDomain), "hex"),
        Buffer.from(phasRoot, "hex"),
        Buffer.from(Data.to(1n), "hex"),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");
  if (!("ScriptCredential" in hub.settlement_addr.paymentCredential))
    throw new Error("settlement script missing");
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(hub.settlement),
    CML.AssetName.from_hex(""),
    1n,
  );
  const settlementOutputs = CML.TransactionOutputList.new();
  settlementOutputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(
        `70${hub.settlement_addr.paymentCredential.ScriptCredential[0]}`,
      ),
      CML.Value.new(5_000_000n, assets),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(
          Data.to(
            {
              deposits_root: "a5".repeat(32),
              withdrawals_root: "a6".repeat(32),
              forced_transactions_root: root,
              transactions_root: "a7".repeat(32),
              resolution_claim: null,
            },
            SettlementDatum,
          ),
        ),
      ),
    ),
  );
  const settlementInputs = CML.TransactionInputList.new();
  settlementInputs.add(transactionInput(`${"a8".repeat(32)}#0`));
  const settlement = CML.TransactionBody.new(
    settlementInputs,
    settlementOutputs,
    200_000n,
  );
  const settlementBody = settlement.to_canonical_cbor_hex();
  const settlementRef = `${CML.hash_transaction(
    CML.TransactionBody.from_cbor_hex(settlementBody),
  ).to_hex()}#0`;
  const refs = CML.TransactionInputList.new();
  refs.add(transactionInput(facts.activation.hubOutRef));
  refs.add(transactionInput(settlementRef));
  const inputs = CML.TransactionInputList.new();
  inputs.add(transactionInput(event.outRef));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_hex(`60${"88".repeat(28)}`),
      CML.Value.from_coin(3_000_000n),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  body.set_reference_inputs(refs);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(event.policyId),
    CML.AssetName.from_hex(event.assetNameHex),
    -1n,
  );
  body.set_mint(mint);
  const certificates = CML.CertificateList.new();
  certificates.add(
    CML.Certificate.new_unreg_cert(
      CML.Credential.new_script(
        CML.ScriptHash.from_hex(event.witnessScriptHash),
      ),
      0n,
    ),
  );
  body.set_certs(certificates);
  const proof = {
    domain,
    root,
    phas_root: phasRoot,
    count: 1n,
    key: CML.PlutusData.from_cbor_hex(event.eventCborHex)
      .as_constr_plutus_data()!
      .fields()
      .get(0)
      .to_cbor_hex(),
    value: Data.to(
      {
        tx_id: datum.event.tx.tx_id,
        submitted_source: datum.event.tx.submitted_source,
        verdict: "ForcedTxValid",
      },
      ForcedInclusionTxV1,
    ),
    proof: [],
  };
  const membership = CML.PlutusDataList.new();
  membership.add(CML.PlutusData.from_cbor_hex(Data.to(phasRoot, MerkleRoot)));
  membership.add(CML.PlutusData.new_bytes(Buffer.from(proof.key, "hex")));
  membership.add(CML.PlutusData.new_bytes(Buffer.from(proof.value, "hex")));
  membership.add(CML.PlutusData.from_cbor_hex(Data.to([], Proof)));
  return {
    settlementBody,
    consume: syntheticUserEventTransaction(body, [
      {
        tag: CML.RedeemerTag.Spend,
        index: 0n,
        cbor: Data.to(
          {
            input_index: 0n,
            output_index: 0n,
            hub_ref_input_index: ledgerReferenceIndex(
              refs,
              facts.activation.hubOutRef,
            ),
            settlement_ref_input_index: ledgerReferenceIndex(
              refs,
              settlementRef,
            ),
            burn_redeemer_index: 1n,
            membership_proof: proof,
            inclusion_proof_script_withdraw_redeemer_index: 3n,
            validity_override: "ForcedTxValid",
          },
          TxOrderSpendRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Mint,
        index: 0n,
        cbor: Data.to(
          {
            event: {
              BurnEventNFT: {
                nonce_asset_name: event.assetNameHex,
                witness_unregistration_redeemer_index: 2n,
              },
            },
            material_carriage: [],
          },
          TxOrderMintRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Cert,
        index: 0n,
        cbor: Data.to(
          { MintOrBurn: { targetPolicy: event.policyId } },
          UserEventWitnessPublishRedeemer,
        ),
      },
      {
        tag: CML.RedeemerTag.Reward,
        index: 0n,
        cbor: CML.PlutusData.new_list(membership).to_cbor_hex(),
      },
    ]),
  };
};

describe("local publication rejects malformed history and forced terminal redeemers", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "rejects adjacent/truncated %s admission observers without archive/CAS mutation",
    async (kind) => {
      const scenario = await openScenario();
      try {
        const { fixture, initial, publisher, publish } = scenario;
        const lifecycle = historyLifecycle(initial.facts, false, { kind });
        const entries = redeemersOf(lifecycle.create);
        const observer = entries.findIndex(
          ({ tag }) => tag === CML.RedeemerTag.Reward,
        );
        expect(observer).toBeGreaterThanOrEqual(0);
        for (const [index, mutate] of [
          adjacentConstructor,
          truncatedConstructor,
        ].entries()) {
          await rejectWithoutPublication(
            scenario,
            fixture.emptySuccessorBlock,
            replaceRedeemer(
              lifecycle.create,
              observer,
              mutate(entries[observer]!.cbor),
              index + 1,
            ),
            [fixture.initializationBodyCbor],
          );
        }
        await fixture.selectCanonicalBranch(fixture.emptySuccessorBlock.point);
        const valid = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [lifecycle.create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(valid);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        expect(publisher.read().snapshot.terminalEvents).toHaveLength(0);
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );

  it.each(["deposit", "withdrawal", "forced_order"] as const)(
    "rejects each malformed %s terminal redeemer, then accepts the unchanged valid terminal",
    async (kind) => {
      const scenario = await openScenario();
      try {
        const { fixture, initial, publisher, publish } = scenario;
        const history =
          kind === "forced_order"
            ? null
            : historyLifecycle(initial.facts, false, {
                kind,
                withdrawalPayout: kind === "withdrawal",
              });
        const create =
          history?.create ??
          ordinaryLocalOrderCreation(
            initial.facts,
            "forced_order",
            makeNativeTx().txCbor,
          ).cbor;
        const admission = await fixture.makeBlock({
          parent: fixture.emptySuccessorBlock,
          transactions: [create],
          creatingBodies: [fixture.initializationBodyCbor],
        });
        await publish(admission);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(1);
        const event = publisher.read().snapshot.activeEvents[0]!;
        const terminal = history ?? forcedTerminal(initial.facts, event);
        const entries = redeemersOf(terminal.consume);
        const orderIndex = ledgerReferenceIndex(
          CML.Transaction.from_cbor_hex(terminal.consume).body().inputs(),
          event.outRef,
        );
        let mutations = 0;
        for (const [index, entry] of entries.entries()) {
          if (
            kind !== "forced_order" &&
            entry.tag === CML.RedeemerTag.Mint &&
            Data.from(entry.cbor) === 0n
          )
            continue;
          if (
            kind !== "forced_order" &&
            entry.tag === CML.RedeemerTag.Spend &&
            entry.index !== orderIndex
          )
            continue;
          const mutators =
            kind !== "forced_order" && entry.tag === CML.RedeemerTag.Spend
              ? [
                  () => Data.to(99n),
                  () =>
                    CML.PlutusData.new_constr_plutus_data(
                      CML.ConstrPlutusData.new(0n, CML.PlutusDataList.new()),
                    ).to_cbor_hex(),
                ]
              : CML.PlutusData.from_cbor_hex(entry.cbor).as_list() !== undefined
                ? [() => Data.to([]), () => Data.to(0n)]
                : [adjacentConstructor, truncatedConstructor];
          for (const mutate of mutators) {
            mutations++;
            await rejectWithoutPublication(
              scenario,
              admission,
              replaceRedeemer(
                terminal.consume,
                index,
                mutate(entry.cbor),
                mutations,
              ),
              [fixture.initializationBodyCbor, terminal.settlementBody],
            );
          }
        }
        expect(mutations).toBe(kind === "deposit" ? 6 : 8);
        await fixture.selectCanonicalBranch(admission.point);
        const valid = await fixture.makeBlock({
          parent: admission,
          transactions: [terminal.consume],
          creatingBodies: [
            fixture.initializationBodyCbor,
            terminal.settlementBody,
          ],
        });
        await publish(valid);
        expect(publisher.read().snapshot.activeEvents).toHaveLength(0);
        expect(publisher.read().snapshot.terminalEvents).toHaveLength(1);
        expect(publisher.read().snapshot.terminalEvents[0]).toMatchObject({
          eventId: event.eventId,
          terminalStatus:
            kind === "deposit"
              ? "absorbed"
              : kind === "withdrawal"
                ? "payout_initialized"
                : "processed",
        });
      } finally {
        await scenario.close();
      }
    },
    120_000,
  );
});
