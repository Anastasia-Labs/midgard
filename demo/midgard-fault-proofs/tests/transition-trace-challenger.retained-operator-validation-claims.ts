import "./transition-trace-challenger.transition-trace-challenger-tooling.js";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxProofCommitment,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { placeholderValidationTraceDescriptorV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildRetainedValidationClaimWitness,
  detectTransitionTraceFaults,
} from "../src/transition-trace/index.js";
import {
  buildPayloadFixture,
  depositEventKey,
  forcedEventKey,
  reconstruct,
  sdkProof,
  withdrawalEventKey,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  eventToStepEntry,
  forcedTx,
  LEDGER_OUTPUT_CBOR,
  ledgerTrieValue,
  nativeMaterial,
  outRef,
  spendInputItem,
  withdrawalInfo,
} from "./transition-trace-challenger.native-material.js";

describe("transition trace single ledger mutation replay", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "authenticates %s keys and mutation roots in both polarities",
    async (kind) => {
      const id = outRef(81);
      const key = spendInputItem(id.transactionId, Number(id.outputIndex));
      const output = Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
      const value = ledgerTrieValue(key, output);
      const tree = await Trie.fromList([{ key, value }]);
      const proof = sdkProof(await tree.prove(key));
      const occupied = tree.hash.toString("hex");
      const inserting = kind === "deposit";
      const before = inserting ? SDK.EMPTY_MERKLE_TREE_ROOT : occupied;
      const after = inserting ? occupied : SDK.EMPTY_MERKLE_TREE_ROOT;
      const eventKey = inserting ? depositEventKey(id) : withdrawalEventKey(id);
      const mutation = inserting
        ? {
            key: key.toString("hex"),
            value: value.toString("hex"),
            insert_proof: proof,
            non_membership_proof: proof,
          }
        : {
            key: key.toString("hex"),
            value: value.toString("hex"),
            delete_proof: proof,
            opening: "",
          };
      const evidence = inserting
        ? {
            depositTransitions: [
              {
                stepIndex: 0n,
                projectedUtxo: mutation as SDK.LedgerInsertWitness,
              },
            ],
          }
        : {
            withdrawalTransitions: [
              { stepIndex: 0n, spentUtxo: mutation as SDK.LedgerDeleteWitness },
            ],
          };
      const make = async (honest: boolean) =>
        reconstruct(
          await buildPayloadFixture({
            prevUtxosRoot: before,
            utxos: inserting
              ? [[key.toString("hex"), output.toString("hex")]]
              : [],
            deposits: inserting
              ? [
                  encodedEntry({
                    key: id,
                    keySchema: SDK.OutputReferenceSchema,
                    value: depositInfo(81),
                    valueSchema: SDK.DepositInfoSchema,
                  }),
                ]
              : [],
            withdrawals: inserting
              ? []
              : [
                  encodedEntry({
                    key: id,
                    keySchema: SDK.OutputReferenceSchema,
                    value: withdrawalInfo(81, "WithdrawalIsValid"),
                    valueSchema: SDK.WithdrawalInfoSchema,
                  }),
                ],
            steps: [
              {
                schema_version: 1n,
                step_index: 0n,
                event_key: eventKey,
                phase: inserting ? "Deposit" : "Withdrawal",
                pre_utxos_root: before,
                post_utxos_root: honest ? after : h32(83),
              },
            ],
            eventToStep: [
              eventToStepEntry(eventKey, {
                step_index: 0n,
                phase: inserting ? "Deposit" : "Withdrawal",
              }),
            ],
          }),
        );
      const invariant = `${kind}_transition_matches_authenticated_replay`;
      expect(
        (await detectTransitionTraceFaults(await make(true), evidence)).filter(
          (d) => d.invariant === invariant,
        ),
      ).toHaveLength(0);
      const bad = await make(false);
      expect(
        (await detectTransitionTraceFaults(bad, evidence)).filter(
          (d) => d.invariant === invariant,
        ),
      ).toHaveLength(1);
      mutation.key = spendInputItem(h32(84), 0).toString("hex");
      await expect(detectTransitionTraceFaults(bad, evidence)).rejects.toThrow(
        "key differs from authenticated source",
      );
      mutation.key = key.toString("hex");
      mutation.value = `${value.toString("hex")}00`;
      await expect(
        detectTransitionTraceFaults(bad, evidence),
      ).rejects.toThrow();
    },
  );
});

describe("retained operator validation claims", () => {
  it.each(["normal", "forced"] as const)(
    "refuses a retained %s validation claim when the block carries only a placeholder",
    async (kind) => {
      const material = nativeMaterial(88);
      const id = outRef(80);
      const eventKey: SDK.EventKey =
        kind === "normal"
          ? { L2TransactionEventKey: { tx_id: material.txId } }
          : forcedEventKey(id);
      const source: SDK.L2TransactionSource = {
        tx_id: material.txId,
        source: material.source,
      };
      const forced = forcedTx(88, "ForcedTxValid");
      const submitted =
        kind === "normal" ? material.source : forced.submitted_source;
      const transactionCommitment = (
        kind === "normal"
          ? computeMidgardNativeTxProofCommitment
          : computeMidgardForcedTxProofCommitment
      )({
        compactCbor: Buffer.from(submitted.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          submitted.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          submitted.field_preimage_lengths_cbor,
          "hex",
        ),
      });
      const descriptor = SDK.validationTraceDescriptorDataFromCore(
        placeholderValidationTraceDescriptorV1(
          {
            eventKeyCbor: Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
            transactionCommitment,
          },
          "accepted",
          null,
        ),
      );
      const fixture = await buildPayloadFixture({
        transactions:
          kind === "normal"
            ? [[material.txId, Data.to(source, SDK.L2TransactionSource)]]
            : [],
        transactionPreimages:
          kind === "normal"
            ? [[material.txId, material.canonicalCbor.toString("hex")]]
            : [],
        forcedTransactions:
          kind === "forced"
            ? [
                [
                  Data.to(id, SDK.OutputReference),
                  Data.to(forced, SDK.ForcedInclusionTxV1),
                ],
              ]
            : [],
        validationDescriptors: [
          [
            Data.to(eventKey, SDK.EventKey),
            Data.to(descriptor, SDK.ValidationTraceDescriptor),
          ],
        ],
        retainedWitnesses: [],
        steps: [
          {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: kind === "normal" ? "L2Transaction" : "ForcedTransaction",
            pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
            post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
          },
        ],
        eventToStep: [
          eventToStepEntry(eventKey, {
            step_index: 0n,
            phase: kind === "normal" ? "L2Transaction" : "ForcedTransaction",
          }),
        ],
      });
      const reconstruction = await reconstruct(fixture);
      await expect(
        buildRetainedValidationClaimWitness({ reconstruction, eventKey }),
      ).rejects.toThrow("Retained validation initial endpoint is absent");
    },
  );
});
