import "./transition-trace-challenger.transition-trace-challenger-tooling.js";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import { detectTransitionTraceFaults } from "../src/transition-trace/index.js";
import {
  buildPayloadFixture,
  depositEventKey,
  reconstruct,
  sdkProof,
  withdrawalEventKey,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  depositInfo,
  encodedEntry,
  eventToStepEntry,
  LEDGER_OUTPUT_CBOR,
  ledgerTrieValue,
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
