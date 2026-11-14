/** Input existence after a submitted L2 withdrawal. The seed owner signs a
 * withdrawal of a deposit-derived seed output, the public SDK builder admits
 * it to the applied withdrawal list, and the node's own ingestion and
 * classification derive the committed event and its ledger removal. The
 * target then spends (field 0) or references (field 1) the withdrawn output,
 * and the registered four-step route convicts on its absence. The control
 * commits the same withdrawal while the target touches another seed output,
 * and step 04 refuses it on chain. The seed deposits themselves are derived
 * by the canonical deposit effect but are not admitted on L1. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import * as WithdrawalsDB from "midgard-node/database/withdrawals";
import { describe, expect, it } from "vitest";

import {
  admitInputExistenceArtifact,
  inputExistenceArtifact,
} from "../src/input-existence/artifact.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { detectLedgerRelativeMissingInputs } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";
import {
  chronologySeedDeposits,
  chronologySeedEntries,
} from "./helpers/input-existence-chronology-seed.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  awaitHeaderCommitWindow,
  FAMILY_HISTORY_HEADER_LEAD_MS,
  prepareFamilyHistory,
} from "./support/emulator/family-history.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { runInputExistenceRoute } from "./support/emulator/input-existence-route.js";
import {
  commitChronologyWithdrawal,
  submitChronologyWithdrawal,
} from "./support/emulator/real-chronology-withdrawal.js";
import { submitSecondHeaderTx } from "./support/emulator/setup-tx.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./support/transition-trace-retained.deposit-events-retained-block.js";

/** The control's step 04 holds a ledger proof that its selected input is
 * present at the target's pre-root, so the verdict finds no fault. The
 * verdict's no-fault branch returns a plain false, with no trace of its own,
 * so the traced run names the module and a false result rather than one
 * check; a failed `expect` anywhere in step 04 would carry its own trace. */
const CONTROL_REFUSAL = {
  0: {
    refusedBy: "fraud_proofs/no_input/step_04",
    check: /^Validator returned false$/u,
  },
  1: {
    refusedBy: "fraud_proofs/no_reference_input/step_04",
    check: /^Validator returned false$/u,
  },
} as const;

/** The seed output the withdrawal names: deposit 0x51…51#0. */
const WITHDRAWN: SDK.OutputReference = {
  transactionId: "51".repeat(32),
  outputIndex: 0n,
};

const CASES = [
  [0, "withdrawal_consumed"],
  [0, "withdrawal_untouched"],
  [1, "withdrawal_consumed"],
  [1, "withdrawal_untouched"],
] as const;

describe("input existence on a submitted withdrawal", () => {
  it.each(CASES)(
    "runs field %i for %s",
    async (field, scenario) => {
      const fault = scenario === "withdrawal_consumed";
      const h = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realNonExistentInput: field === 0,
          realNoReferenceInput: field === 1,
          alwaysFraudProofCatalogue: true,
        },
        registerAdditionalRewardAccounts:
          registerPexcludesExclusionRewardAccount,
      });
      const family = await prepareFamilyHistory(h, []);
      const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
      // The prior block opens at the widest lead its setup admits; the
      // target follows it and holds the withdrawal's inclusion time.
      const priorStart = BigInt(
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + FAMILY_HISTORY_HEADER_LEAD_MS,
        ) - 1,
      );
      const startTime = priorStart + 30_000n;
      const endTime = startTime + 30_000n;
      const priorDeposit = await depositEventsRetainedBlock({
        operatorVkey,
        startTime: priorStart,
        endTime: startTime,
        blockSlot: 0n,
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        priorLedger: [],
        events: chronologySeedDeposits(),
      });
      let order: SDK.WithdrawalUTxO | undefined;
      const committedPrior = await submitSetupTx({
        lucid: h.funderLucid,
        contracts: family.contracts,
        nonceUtxo: h.nonceUtxo,
        catalogue: h.catalogue,
        header: priorDeposit.header,
        beforeHeaderCommit: async () => {
          order = await submitChronologyWithdrawal({
            h,
            family,
            l2OutRef: WITHDRAWN,
            validTo: Number(endTime) + 1 - SDK.EVENT_WAIT_DURATION_MS,
          });
          awaitHeaderCommitWindow(h.emulator, priorDeposit.header);
        },
      });
      expect(committedPrior.headerHash).toBe(priorDeposit.headerHash);
      if (order === undefined) throw new Error("withdrawal was not admitted");
      // The target's base ledger is the prior block's: the seed deposits.
      const withdrawal = await commitChronologyWithdrawal(
        order,
        chronologySeedEntries(),
      );
      expect(withdrawal.validity).toBe(
        WithdrawalsDB.Validity.WithdrawalIsValid,
      );
      expect(withdrawal.inclusionTime.getTime()).toBeGreaterThan(
        Number(startTime),
      );
      expect(withdrawal.inclusionTime.getTime()).toBeLessThanOrEqual(
        Number(endTime),
      );
      expect(withdrawal.effect.operations).toEqual([
        { type: "delete", outRefCbor: outRefCbor(81, 0n) },
      ]);
      const history = await inputExistenceChronologyFixture(
        scenario,
        field,
        {
          operatorVkey,
          startTime,
          predecessorHeaderHash: priorDeposit.headerHash,
        },
        withdrawal,
      );
      expect(history.predecessor.utxosRoot).toBe(priorDeposit.header.utxosRoot);
      expect(history.evidence.header.withdrawalCount).toBe(1n);
      awaitHeaderCommitWindow(h.emulator, history.evidence.header);
      const committed = await submitSecondHeaderTx({
        lucid: h.funderLucid,
        contracts: family.contracts,
        header: history.evidence.header,
      });
      expect(committed.headerHash).toBe(history.evidence.headerHash);
      const targetFingerprint = Data.to(history.targetEvent, SDK.EventKey);
      const detections = (
        await detectLedgerRelativeMissingInputs({
          evidence: history.evidence,
          context: history.context,
          kind: field === 0 ? "spend" : "reference",
        })
      ).filter((d) => d.subjectEventKeyCbors.includes(targetFingerprint));
      expect(detections.map((d) => d.violationId)).toEqual(
        fault
          ? [field === 0 ? "non-existent-input" : "no-reference-input"]
          : [],
      );
      const prepared = await prepareInputExistence({
        block: history.evidence,
        eventKey: history.targetEvent,
        field,
        inputIndex: 0n,
        predecessor: history.predecessor,
        owner: h.proverSigner.paymentKeyHash,
      });
      expect(prepared.source.subject.direction).toBe(0n);
      expect(prepared.source.subject.source_kind).toBe(0n);
      expect(prepared.eventMembership.value.step_index).toBe(
        history.targetStep,
      );
      expect(prepared.transitionMembership.value.pre_utxos_root).toBe(
        history.targetPreRoot,
      );
      expect(prepared.ledgerEvidence).toHaveProperty(
        fault ? "Absent" : "Present",
      );
      const admitted = admitInputExistenceArtifact(
        JSON.parse(JSON.stringify(inputExistenceArtifact(prepared))),
        h.proverSigner.paymentKeyHash,
      );
      expect(admitted.ledgerEvidence).toEqual(prepared.ledgerEvidence);
      expect(Data.to(admitted.source.eventKey, SDK.EventKey)).toBe(
        targetFingerprint,
      );
      await runInputExistenceRoute({
        h,
        field,
        prepared,
        fraudulentBlockOutRef: committed.blockOutRef,
        outcome: fault
          ? { kind: "conviction" }
          : {
              kind: "refusal",
              refuse: (step04) =>
                expectOnchainRefusal(step04, CONTROL_REFUSAL[field]),
            },
      });
    },
    900_000,
  );
});
