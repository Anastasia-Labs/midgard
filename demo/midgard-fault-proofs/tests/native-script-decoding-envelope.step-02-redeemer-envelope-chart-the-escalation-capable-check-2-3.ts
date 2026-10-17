import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { NativeScriptDecodingStep02SpendRedeemer } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ADVERSARY_LOG2_WORK,
  dataBytes,
  L1_ENVELOPE_BYTES,
  printChart,
  proofFromCbor,
  STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
} from "./native-script-decoding-envelope.step-01-redeemer-envelope-chart-both-carriages-q4.js";
import {
  ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
  insertAdversarialMembershipSiblings,
  membershipProofBranchLevelsReachableWithWork,
  outputReferenceCbor,
} from "./support/submit-init-emulator-fixtures.js";
import {
  h32,
  makeHeader,
  makeNativeTx,
} from "./support/submit-init-emulator-shared.js";

describe("step-02 redeemer envelope chart (THE escalation-capable check, §2.3)", () => {
  /**
   * Worst admissible step-02 instance: forced-source thread (both extra
   * openings live), forced leaf carrying a real §3 proof-source triple and the
   * worst `OperatorVerdictV1` arm, and all three MPF proofs at the grinded
   * adversarial branch depth.
   */
  const buildStep02WorstRedeemerBytes = async (
    branchLevels: number,
  ): Promise<number> => {
    const txOrderId = { transactionId: h32("c1"), outputIndex: 0n };
    const forcedKey = outputReferenceCbor({
      transactionId: txOrderId.transactionId,
      outputIndex: txOrderId.outputIndex,
    });
    const eventToStepKey = Buffer.alloc(40, 0xe2);
    const transitionStepKey = Buffer.from("03", "hex");

    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(forcedKey, Buffer.from("f0", "hex"));
    await trie.insert(eventToStepKey, Buffer.from("f1", "hex"));
    await trie.insert(transitionStepKey, Buffer.from("f2", "hex"));
    await insertAdversarialMembershipSiblings({
      trie,
      targets: [
        { key: forcedKey, domain: 0x0b01 },
        { key: eventToStepKey, domain: 0x0b02 },
        { key: transitionStepKey, domain: 0x0b03 },
      ],
      branchLevels,
    });
    const proveData = async (key: Buffer) =>
      proofFromCbor(
        Buffer.from((await trie.prove(key)).toCBOR()).toString("hex"),
      );
    const forcedProof = await proveData(forcedKey);
    const eventToStepProof = await proveData(eventToStepKey);
    const transitionStepProof = await proveData(transitionStepKey);

    // Real commitment-bounded proof source, derived from a real native tx —
    // not synthesized bytes, so a widening of the §3 compact encoding shows up
    // here as a measured regression.
    const forcedNativeTx = makeNativeTx({
      spendInputCbors: [
        { transactionId: h32("c3"), outputIndex: 0n },
        { transactionId: h32("c4"), outputIndex: 1n },
        { transactionId: h32("c5"), outputIndex: 2n },
      ].map(outputReferenceCbor),
      fee: 1_000_000n,
      referenceByte: "c6",
      outputByte: "c7",
      witnessByte: "c8",
    });
    const source = deriveMidgardForcedTxProofSource(forcedNativeTx);

    const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
    const header = makeHeader(
      "77".repeat(28),
      1_700_000_000_000,
      h32("d1"),
      2n,
    );
    const redeemer: NativeScriptDecodingStep02SpendRedeemer = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          header,
          event_to_step_membership: {
            domain: "EventToStepRootDomain",
            root: h32("d2"),
            phas_root: h32("d3"),
            count: 65_535n,
            key: eventKey,
            value: { step_index: 65_535n, phase: "ForcedTransaction" },
            proof: eventToStepProof,
          },
          transition_step_membership: {
            domain: "TransitionTraceRootDomain",
            root: h32("d4"),
            phas_root: h32("d5"),
            count: 65_535n,
            key: 65_535n,
            value: {
              schema_version: 1n,
              step_index: 65_535n,
              event_key: eventKey,
              phase: "ForcedTransaction",
              pre_utxos_root: h32("d6"),
              post_utxos_root: h32("d7"),
            },
            proof: transitionStepProof,
          },
          forced_membership: {
            domain: "ForcedTransactionsV1RootDomain",
            root: h32("d8"),
            phas_root: h32("d9"),
            count: 65_535n,
            key: txOrderId,
            value: {
              tx_id: h32("da"),
              submitted_source: {
                compact_cbor: Buffer.from(source.compactCbor).toString("hex"),
                witness_set_compact_cbor: Buffer.from(
                  source.witnessSetCompactCbor,
                ).toString("hex"),
                field_preimage_lengths_cbor: Buffer.from(
                  source.fieldPreimageLengthsCbor,
                ).toString("hex"),
              },
              // Worst verdict arm: two integer payloads (the 47-arm catalogue
              // carries at most two small integers, #633).
              verdict: {
                ForcedTxInvalid: {
                  reason: {
                    OutputAssetAccumulationLimit: {
                      output_index: 65_535n,
                      asset_index: 65_535n,
                    },
                  },
                },
              },
            },
            proof: forcedProof,
          },
          chosen_outpoint_source_kind: 0n,
          chosen_outpoint_cursor: 65_535n,
        },
      ],
    };
    return dataBytes(
      Data.to(redeemer, NativeScriptDecodingStep02SpendRedeemer),
    );
  };

  it("fits the worst forced-leaf instance at adversarial depth — or escalates to the wave branch", async () => {
    const shallowBytes = await buildStep02WorstRedeemerBytes(0);
    const deepBytes = await buildStep02WorstRedeemerBytes(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );

    // THE gate. A failure here is a wave-branch on-chain format finding
    // (design §2.3/§9): the step-02 argument layout would need to move
    // openings out of the single redeemer. It must be escalated, not absorbed
    // by shrinking the fixture or the overhead allowance.
    expect(
      deepBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES,
      "ESCALATE(#635 → wave branch): worst-case step-02 redeemer no longer fits the L1 fault-proof envelope",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);
    // "Fits" alone is not the design's claim — §2.3 expects real margin, so a
    // creeping regression surfaces before it becomes an escalation.
    expect(
      deepBytes + STEP_TX_OVERHEAD_ALLOWANCE_BYTES + 1_024,
      "worst-case step-02 margin dropped below 1 KiB — investigate before the next widening lands",
    ).toBeLessThanOrEqual(L1_ENVELOPE_BYTES);

    // Exhaustion arithmetic over the one adversary-movable axis. Step 02
    // stacks THREE proofs, so its per-level cost is three times step-01's and
    // its exhaustion depth proportionally shallower — still work-bounded, and
    // recorded under the same Q1X-F5 convention.
    const perLevelBytes =
      (deepBytes - shallowBytes) / ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS;
    expect(perLevelBytes).toBeGreaterThan(0);
    const exhaustionDepth =
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS +
      Math.floor(
        (L1_ENVELOPE_BYTES - STEP_TX_OVERHEAD_ALLOWANCE_BYTES - deepBytes) /
          perLevelBytes,
      );
    printChart("native-script-decoding step-02 chart", {
      shallowBytes,
      deepBytes,
      perLevelBytes,
      exhaustionDepth,
    });
    expect(exhaustionDepth).toBeGreaterThanOrEqual(
      ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS,
    );
    // Measured 2026-08-25: per-level 333.6 bytes (three stacked proofs),
    // exhaustion depth 37 against the work-reachable level 32. This is the
    // SHARP number of the whole suite: step 02 has no published-chunk
    // fallback, so if the exhaustion depth ever drops to 32 or below, a
    // work-feasible adversarial block exists whose worst forced-leaf opening
    // cannot reach L1 at all — that is the §2.3 wave-branch escalation, with
    // only ~5 levels of margin today. Watch it, do not absorb it.
    expect(perLevelBytes).toBeGreaterThanOrEqual(250);
    expect(perLevelBytes).toBeLessThanOrEqual(450);
    expect(
      exhaustionDepth,
      "ESCALATE(#635 → wave branch): a work-feasible (2^128) membership depth no longer fits the step-02 redeemer, and step 02 has no alternative carriage",
    ).toBeGreaterThan(
      membershipProofBranchLevelsReachableWithWork(ADVERSARY_LOG2_WORK),
    );
  }, 600_000);
});
