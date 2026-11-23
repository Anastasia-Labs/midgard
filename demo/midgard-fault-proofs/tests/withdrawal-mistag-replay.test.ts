import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.transition-trace-reconstruction.js";
import {
  detectWithdrawalMistagReplay,
  prepareWithdrawalMistagReplay,
} from "../src/withdrawal-mistag/replay.js";
import { WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import {
  checkedLedgerAfter,
  ledgerAfter,
  ledgerEntriesAfter,
} from "./support/predecessor-ledger.js";
import { buildDuplicateOutRefMistagMaterial } from "./support/withdrawal-mistag-duplicate-outref.js";
import {
  retainedBlock,
  withdrawalMistagRetainedFixture,
} from "./support/withdrawal-mistag-retained.js";

describe("withdrawal mistag retained replay", () => {
  it.each(["valid-marked-invalid", "invalid-marked-valid"] as const)(
    "authenticates the prior ledger and refuses honest %s tags",
    async (direction) => {
      for (const honest of [false, true]) {
        const fixture = await withdrawalMistagRetainedFixture({
          direction,
          honest,
        });
        const result = await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
          fixture.evidence,
          fixture.context,
        );
        expect(result.detections).toHaveLength(honest ? 0 : 1);
        await expect(
          prepareWithdrawalMistagReplay({
            current: fixture.evidence.reconstruction,
            index: 0,
          }),
        ).rejects.toThrow("the block starts from a non-empty ledger");
        await expect(
          prepareWithdrawalMistagReplay({
            current: fixture.evidence.reconstruction,
            predecessor: await ledgerAfter(fixture.predecessor),
            index: 1,
          }),
        ).rejects.toThrow("source index is absent");
      }
    },
  );
});

/**
 * The ledger evidence for withdrawal `index` of `block`: the ledger a
 * from-scratch replay of the block's earlier steps gives (the test
 * reference, not the family's walk), with a proof of the withdrawn out-ref
 * from its trie.
 */
const expectedLedgerEvidence = async (
  block: CanonicalBlockEvidence,
  predecessor: CheckedLedger,
  index: number,
) => {
  const { key: withdrawalId, value } = block.reconstruction.withdrawals[index]!;
  const eventKey = { WithdrawalEventKey: { withdrawal_id: withdrawalId } };
  const ledger = await referenceLedgerBefore({
    block,
    predecessor,
    eventKey,
    stepIndex: block.reconstruction.eventToStepByFingerprint.get(
      eventKeyFingerprint(eventKey),
    )!.value.step_index,
  });
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(value.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(value.body.l2_outref.outputIndex),
  });
  const leaf = ledger.trie.entries.find((entry) => entry.key.equals(key));
  return leaf === undefined
    ? {
        AbsentLedgerOutput: {
          non_membership_proof: await keyValuePhasNonMembershipProof(
            ledger.trie,
            key,
          ),
        },
      }
    : {
        PresentLedgerOutput: {
          descriptor_cbor: leaf.value.toString("hex"),
          membership_proof: await keyValuePhasProof(
            ledger.trie,
            key,
            leaf.value,
          ),
        },
      };
};

describe("withdrawal mistag ledger evidence", () => {
  it.each(["valid-marked-invalid", "invalid-marked-valid"] as const)(
    "proves the %s withdrawal's present output against the ledger before its step",
    async (direction) => {
      const fixture = await withdrawalMistagRetainedFixture({ direction });
      const predecessor = await checkedLedgerAfter(fixture.predecessor);
      const findings = await detectWithdrawalMistagReplay({
        block: fixture.evidence,
        predecessor,
      });
      expect(findings).toHaveLength(1);
      const expected = await expectedLedgerEvidence(
        fixture.evidence,
        predecessor,
        findings[0]!.index,
      );
      expect(expected).toHaveProperty("PresentLedgerOutput");
      expect(findings[0]!.prepared.ledgerEvidence).toStrictEqual(expected);
    },
  );

  it("proves an output an earlier step of the block spent absent", async () => {
    // Step 0 drains the out-ref both withdrawals name; the second, tagged
    // valid, reads the ledger step 0 leaves.
    const material =
      await buildDuplicateOutRefMistagMaterial("WithdrawalIsValid");
    const { outputs } = material;
    const fixture = await retainedBlock({
      predecessorUtxos: [outputs.shared, outputs.other],
      blockUtxos: [outputs.other],
      source: material.source,
      event: material.event,
      trace: material.trace,
    });
    const predecessor = await checkedLedgerAfter(fixture.predecessor);
    const findings = await detectWithdrawalMistagReplay({
      block: fixture.evidence,
      predecessor,
    });
    expect(findings.map(({ index }) => index)).toEqual([1]);
    const expected = await expectedLedgerEvidence(
      fixture.evidence,
      predecessor,
      1,
    );
    expect(expected).toHaveProperty("AbsentLedgerOutput");
    expect(findings[0]!.prepared.ledgerEvidence).toStrictEqual(expected);
  });

  it("is this process's local fault when the store no longer holds the predecessor", async () => {
    // The challenged withdrawal is step 0, so its evidence reads the
    // predecessor itself, which `retain` of nothing deletes.
    const fixture = await withdrawalMistagRetainedFixture({
      direction: "valid-marked-invalid",
    });
    const store = createLedgerStore(createMemoryLedgerStorage());
    const predecessor = await store.admit(
      ledgerEntriesAfter(fixture.predecessor),
      fixture.predecessor.header.utxosRoot,
    );
    await store.retain(new Set());
    const error: unknown = await prepareWithdrawalMistagReplay({
      current: fixture.evidence.reconstruction,
      predecessor,
      index: 0,
    }).catch((cause: unknown) => cause);
    expect(isReplayLocalFault(error)).toBe(true);
    expect((error as { cause: unknown }).cause).toBeInstanceOf(
      LedgerNodeMissing,
    );
  });
});
