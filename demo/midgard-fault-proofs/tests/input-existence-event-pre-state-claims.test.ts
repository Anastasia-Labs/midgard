/** Claims about the target event's ledger on the registered four-step
 * input-existence route, over committed chronology blocks. A claim that does
 * not hold for the target event's own transition step is refused at the step
 * that checks it, and the honest step then proceeds on the same thread. The
 * selected field also travels as a published UTxO through the whole route. */
import { computeHash32 } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import type { PreparedInputExistence } from "../src/input-existence/prepare.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { buildIndexedTraceProof } from "../src/transition-trace/witnesses.js";
import type { InputChronology } from "./helpers/input-existence-chronology-fixture.js";
import {
  expectOnchainRefusal,
  type RefusalPin,
} from "./support/emulator/expect-onchain-refusal.js";
import { startChronologyJourney } from "./support/input-existence-chronology-journey.js";

const TIMEOUT = 600_000;
const FIELDS = [0, 1] as const;
/** The physical step validator that holds the refusing check. */
const refusedAt = (field: 0 | 1, step: 3 | 4, check: RegExp): RefusalPin =>
  field === 0
    ? step === 3
      ? { refusedBy: "fraud_proofs/no_input/step_03", check }
      : { refusedBy: "fraud_proofs/no_input/step_04", check }
    : step === 3
      ? { refusedBy: "fraud_proofs/no_reference_input/step_03", check }
      : { refusedBy: "fraud_proofs/no_reference_input/step_04", check };
/** Checks, as their verbose traces name them. */
const CHECK = {
  // The transition-step door of the pre-state step.
  transitionStep:
    /^expect and \{ verify_carried_counted_membership_v1\( step_membership, transition_trace\.TransitionTraceRootDomain,/u,
  // The pre-state step's output ledger state.
  ledgerState: /^Validator returned false$/u,
  // The terminal step's membership check of a present input.
  membership: /^expect mpf_proof_v1\.has_value_hash\( state\.pre_utxos_root,/u,
  // The terminal step's verdict.
  verdict: /^Validator returned false$/u,
};
const withAbsence = async (
  prepared: PreparedInputExistence,
  ledger: Parameters<typeof keyValuePhasNonMembershipProof>[0],
  key: Buffer,
): Promise<PreparedInputExistence> => ({
  ...prepared,
  ledgerEvidence: {
    Absent: { proof: await keyValuePhasNonMembershipProof(ledger, key) },
  },
});

describe("input-existence claims about the target event's ledger", () => {
  it.each(FIELDS)(
    "field %s: refuses another event's transition step at the pre-state step",
    async (field) => {
      // Step 0 creates the input; the target, step 1, spends it.
      const j = await startChronologyJourney({
        field,
        scenario: "forced_creator",
      });
      const { history, prepared } = j;
      expect(history.targetStep).toBe(1n);
      expect(prepared.ledgerEvidence).toHaveProperty("Present");
      await j.send(0);
      await j.send(1);
      const creatorStep = await buildIndexedTraceProof({
        reconstruction: history.evidence.reconstruction,
        stepIndex: 0n,
      });
      // The creator's own pre-root lacks the input it creates.
      expect(creatorStep.value.pre_utxos_root).toBe(
        history.predecessor.utxosRoot,
      );
      const carried = await withAbsence(
        { ...prepared, transitionMembership: creatorStep },
        history.predecessor.trie,
        history.selected,
      );
      await expectOnchainRefusal(
        () => j.send(2, { raw: true, prepared: carried }),
        refusedAt(field, 3, CHECK.transitionStep),
      );
      await j.expectThreadKept();
      const honest = await j.send(2);
      expect(honest.eventPreUtxosRoot).toBe(history.targetPreRoot);
    },
    TIMEOUT,
  );

  it.each(FIELDS)(
    "field %s: refuses a membership claim for an input absent at a forced rejection's pre-root",
    async (field) => {
      const j = await startChronologyJourney({
        field,
        scenario: "absent_input_rejection",
      });
      const { history, prepared } = j;
      expect(prepared.source.subject.direction).toBe(1n);
      expect(prepared.ledgerEvidence).toHaveProperty("Absent");
      for (const stepIndex of [0, 1, 2] as const) await j.send(stepIndex);
      // A genuine membership witness for the input, from another ledger.
      const value = Buffer.from("a0", "hex");
      const elsewhere = await keyValuePhasRootWithCount([
        { key: history.selected, value },
      ]);
      expect(elsewhere.root).not.toBe(history.targetPreRoot);
      const crafted: PreparedInputExistence = {
        ...prepared,
        ledgerEvidence: {
          Present: {
            value: value.toString("hex"),
            value_hash: computeHash32(value).toString("hex"),
            proof: await keyValuePhasProof(elsewhere, history.selected, value),
          },
        },
      };
      await expectOnchainRefusal(
        () => j.send(3, { raw: true, prepared: crafted }),
        refusedAt(field, 4, CHECK.membership),
      );
      await j.expectThreadKept();
    },
    TIMEOUT,
  );

  it.each(
    FIELDS.flatMap((field) =>
      (
        [
          { scenario: "normal_spend", convicts: true },
          { scenario: "forced_target", convicts: true },
          { scenario: "prior_ledger_present_rejection", convicts: true },
          { scenario: "prior_deposit", convicts: false },
          { scenario: "absent_input_rejection", convicts: false },
        ] satisfies readonly {
          scenario: InputChronology;
          convicts: boolean;
        }[]
      ).map((row) => ({ field, ...row })),
    ),
  )(
    "field $field: carries the selected field as a published UTxO for $scenario",
    async ({ field, scenario, convicts }) => {
      const j = await startChronologyJourney({
        field,
        scenario,
        publishedFieldCarriage: true,
      });
      expect(j.fieldTier).toBe("RawUtxo");
      for (const stepIndex of [0, 1, 2] as const) await j.send(stepIndex);
      if (convicts) await j.expectConviction(await j.send(3));
      else {
        await expectOnchainRefusal(
          () => j.send(3),
          refusedAt(field, 4, CHECK.verdict),
        );
        await j.expectThreadKept();
      }
    },
    TIMEOUT,
  );

  // A spent input leaves the target's own post-root. A reference input stays
  // in it, so this claim has no field-1 counterpart.
  it.each(["prior_deposit", "forced_prior_deposit"] as const)(
    "field 0: refuses the target's post-root as its pre-root for %s",
    async (scenario) => {
      const j = await startChronologyJourney({ field: 0, scenario });
      const { history, prepared } = j;
      expect(prepared.ledgerEvidence).toHaveProperty("Present");
      await j.send(0);
      await j.send(1);
      const post = await checkLedgerAgainstRoot({
        entries: history.targetPostEntries,
        utxosRoot: history.targetPostRoot,
        label: "target post-state",
      });
      expect(
        post.entries.some(({ outRef }) => outRef.equals(history.selected)),
      ).toBe(false);
      const carried = await withAbsence(prepared, post.trie, history.selected);
      await expectOnchainRefusal(
        () =>
          j.send(2, {
            raw: true,
            prepared: carried,
            claimedPreUtxosRoot: history.targetPostRoot,
          }),
        refusedAt(0, 3, CHECK.ledgerState),
      );
      await j.expectThreadKept();
      const honest = await j.send(2);
      expect(honest.eventPreUtxosRoot).toBe(history.targetPreRoot);
    },
    TIMEOUT,
  );
});
