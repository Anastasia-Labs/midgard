import "./transition-trace-challenger.transition-trace-challenger-tooling.js";

import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationContext,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
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
    "opens %s counted sources and exact operator endpoint bytes",
    async (kind) => {
      const material = nativeMaterial(88);
      const id = outRef(80);
      const eventKey: SDK.EventKey =
        kind === "normal"
          ? { L2TransactionEventKey: { tx_id: material.txId } }
          : forcedEventKey(id);
      const context = Buffer.from("80", "hex");
      const terminalWork = Buffer.from("8100", "hex");
      const initial: MidgardValidationMachineState = {
        machineVersion: 1,
        eventKeyHash: Buffer.alloc(32, 1),
        transactionId: Buffer.from(material.txId, "hex"),
        transactionCommitment: Buffer.alloc(32, 2),
        validationContextHash: hashMidgardValidationContext(context),
        sourceKind: kind,
        priorLedgerRoot: Buffer.from(SDK.EMPTY_MERKLE_TREE_ROOT, "hex"),
        phase: "canonicalDecode",
        programCounter: 0,
        workRoot: Buffer.alloc(32, 3),
        executionCpu: 0n,
        executionMemory: 0n,
        verdict: "pending",
        rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        ledgerDeltaRoot: Buffer.alloc(32, 4),
      };
      const terminal: MidgardValidationMachineState = {
        ...initial,
        phase: "terminal",
        programCounter: 1,
        verdict: "accepted",
        workRoot: hashMidgardValidationWorkWitness({
          phase: "terminal",
          programCounter: 1,
          witnessCbor: terminalWork,
        }),
      };
      const tree = buildMidgardValidationTraceTree(
        [initial, terminal].map(hashMidgardValidationMachineState),
        "accepted",
        MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      );
      const descriptor = SDK.validationTraceDescriptorDataFromCore(
        tree.descriptor,
      );
      const records: SDK.DaPayloadEntry[] = (
        ["initial", "terminal"] as const
      ).map((endpoint) => {
        const coordinate = SDK.retainedValidationEndpointCoordinate(
          1n,
          endpoint,
        );
        const terminalEndpoint = endpoint === "terminal";
        const record = {
          phase: terminalEndpoint ? 14n : -1n,
          program_counter: terminalEndpoint ? 1n : 0n,
          machine_state: SDK.validationMachineStateDataFromCore(
            terminalEndpoint ? terminal : initial,
          ),
          trace_proof: SDK.validationTraceProofDataFromCore(
            tree.proofs[terminalEndpoint ? 1 : 0]!,
          ),
          witness_cbor: (terminalEndpoint ? terminalWork : context).toString(
            "hex",
          ),
          auxiliary: "NoAuxiliaryWitness" as const,
        };
        return [
          SDK.encodeRetainedValidationWitnessKey({
            event_key: eventKey,
            execution_index: coordinate,
          }).toString("hex"),
          SDK.encodeRetainedValidationWitness(record).toString("hex"),
        ];
      });
      const source: SDK.L2TransactionSource = {
        tx_id: material.txId,
        source: material.source,
      };
      const forced = forcedTx(88, "ForcedTxValid");
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
        retainedWitnesses: records,
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
      const opened = await buildRetainedValidationClaimWitness({
        reconstruction,
        eventKey,
      });
      expect(opened.claim.descriptor_membership.root).toBe(
        reconstruction.header.validationTracesRoot,
      );
      expect(opened.claim.source_membership).toHaveProperty(
        kind === "normal" ? "NormalValidationSource" : "ForcedValidationSource",
      );
      expect(opened.claim.validation_context_cbor).toBe(
        context.toString("hex"),
      );
      expect(opened.terminalWorkWitnessCbor).toBe(terminalWork.toString("hex"));
      const original =
        reconstruction.payload.block_body.validation_trace_witnesses[1]!;
      const record = SDK.decodeRetainedValidationWitness(
        Buffer.from(original[1], "hex"),
      );
      reconstruction.payload.block_body.validation_trace_witnesses[1] = [
        original[0],
        SDK.encodeRetainedValidationWitness({
          ...record,
          witness_cbor: "8101",
        }).toString("hex"),
      ];
      await expect(
        buildRetainedValidationClaimWitness({ reconstruction, eventKey }),
      ).rejects.toThrow();
    },
  );
});
