/** Honest-commitment polarity of the zero-input proof, from a fresh key. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import { zeroInputEvidenceCloses } from "../src/zero-input/family.js";
import { submitZeroInputStep02V1 } from "../src/zero-input/submit-step-02.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  stageZeroInputAcceptance,
  stageZeroInputHonestRejection,
  zeroInputHonestSpendInput,
  type ZeroInputStagedThread,
} from "./support/zero-input-honest-commitment.js";
import { submitRawZeroInputStep02 } from "./support/zero-input-raw-step02.js";

const expectUnlistedProver = (staged: ZeroInputStagedThread) =>
  expect(
    Object.values(SELECTED_DEPLOYMENT_PROFILE.allow_lists).flatMap(
      (keys) => keys ?? [],
    ),
  ).not.toContain(staged.harness.proverSigner.paymentKeyHash);

const expectBuilderRefusal = async (staged: ZeroInputStagedThread) => {
  expect(zeroInputEvidenceCloses(staged.evidence)).toBe(false);
  await expect(
    submitZeroInputStep02V1({
      lucid: staged.harness.proverLucid,
      contracts: staged.contracts,
      categoryId: staged.categoryId,
      signer: staged.harness.proverSigner,
      threadOutRef: staged.terminalThreadOutRef,
      evidence: staged.evidence,
      nativeTxCompactCbor: staged.nativeTxCompactCbor,
      referenceScriptUtxo: staged.stepReferences[1],
      witnessReferenceScripts: staged.harness.witnessReferenceScripts,
    }),
  ).rejects.toThrow(/terminal evidence is honest/u);
};

describe("zero-input proof against an honest commitment", () => {
  it("lands the hand-built proof against an accepted transaction that spends nothing", async () => {
    const staged = await stageZeroInputAcceptance({ spendInputs: [] });
    expectUnlistedProver(staged);
    expect(zeroInputEvidenceCloses(staged.evidence)).toBe(true);
    const { fraudProofUnit } = await submitRawZeroInputStep02({
      staged,
      spendInputs: staged.committedSpendInputs,
    });
    const minted = await staged.harness.proverLucid.utxosAtWithUnit(
      staged.contracts.fraudProof.spendingScriptAddress,
      fraudProofUnit,
    );
    expect(minted).toHaveLength(1);
  }, 600_000);

  it("refuses a fresh key's proof against an accepted transaction that spends an input", async () => {
    const staged = await stageZeroInputAcceptance({
      spendInputs: [zeroInputHonestSpendInput],
    });
    expectUnlistedProver(staged);
    expect(staged.committedSpendInputs).toHaveLength(1);
    await expectBuilderRefusal(staged);

    // Built by hand, the committed field 0 opens to one item, so the terminal
    // rule finds no fault.
    await expectOnchainRefusal(
      () =>
        submitRawZeroInputStep02({
          staged,
          spendInputs: staged.committedSpendInputs,
        }),
      {
        refusedBy: "fraud_proofs/zero_input/step_02",
        check: /^Validator returned false$/u,
      },
    );
    // An empty field 0 is not the one the transaction commits.
    await expectOnchainRefusal(
      () => submitRawZeroInputStep02({ staged, spendInputs: [] }),
      {
        refusedBy: "fraud_proofs/zero_input/step_02",
        check: /^expect field_commitment\(preimage\) == expected_hash/u,
      },
    );
  }, 600_000);

  it("refuses a fresh key's proof against a forced transaction with no inputs recorded as EmptyInputs", async () => {
    const staged = await stageZeroInputHonestRejection();
    expectUnlistedProver(staged);
    expect(staged.committedSpendInputs).toHaveLength(0);
    await expectBuilderRefusal(staged);

    // Built by hand, the committed field 0 opens empty, so the recorded
    // rejection stands and the terminal rule finds no contradiction.
    await expectOnchainRefusal(
      () =>
        submitRawZeroInputStep02({
          staged,
          spendInputs: staged.committedSpendInputs,
        }),
      {
        refusedBy: "fraud_proofs/zero_input/step_02",
        check: /^Validator returned false$/u,
      },
    );
    // A field 0 holding an input is not the one the transaction commits.
    await expectOnchainRefusal(
      () =>
        submitRawZeroInputStep02({
          staged,
          spendInputs: [zeroInputHonestSpendInput],
        }),
      {
        refusedBy: "fraud_proofs/zero_input/step_02",
        check: /^expect field_commitment\(preimage\) == expected_hash/u,
      },
    );
  }, 600_000);
});
