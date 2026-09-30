import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { requireLinearFaultThreadUtxo } from "../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../src/linear-fault-finalize.js";
import { missingScriptSourceEvidenceCloses } from "../src/missing-script-source/family.js";
import { ExecutionSourceStep06RedeemerSchema } from "../src/missing-script-source/schemas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  buildMissingScriptSourceFixture,
  buildMissingScriptSourceUniverse,
  claimMissingScriptSourcePrefix,
  commitMissingScriptSourceBlock,
  makeMissingScriptSourceHarness,
  makeMissingScriptSourceStages,
  missingScriptSourceEvidence,
  type MissingScriptSourceFixture,
  type MissingScriptSourceHarness,
  missingScriptSourceReason,
  runMissingScriptSourceThread,
} from "./support/missing-script-source-emulator.js";

/**
 * A forced ScriptSourceMissing reason names a purpose by kind and its index in
 * that kind's namespace, and missingScriptSource reopens exactly that purpose.
 * Spends are indexed in sorted out-ref order, not field order: the fixture
 * puts the sourceless spend first in the field and second when sorted. The
 * verdict is the one the node's classifier writes, so the suite fails if the
 * writer and the proof disagree on the namespace: one purpose early names the
 * spend whose source is present and convicts; the written purpose is refused
 * on chain.
 */

const shape = {
  purposeKind: 0,
  presentAt: "inline",
  inlineDecoys: 0,
  referenceDecoys: 0,
  direction: "forced",
  absentSecondSpend: true,
} as const;

const writtenPurposeIndex = async (
  fixture: MissingScriptSourceFixture,
): Promise<bigint> => {
  const forced = materializeMidgardForcedTxFromCanonical(
    fixture.transaction.tx,
  );
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: fixture.ledgerWitnessEntries.map(({ outRef, output }) => [
      outRef,
      output,
    ]),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: missingScriptSourceReason(0, 1n) },
  });
  return 1n;
};

/** The terminal step submitted past the builder's local admission. */
const submitRawStep06 = async (
  { harness, contracts, category, references }: MissingScriptSourceHarness,
  threadOutRef: string,
) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    family: "missing-script-source",
    stepIndex: 5,
    threadOutRef,
  });
  return submitLinearFaultFinalize({
    lucid: harness.proverLucid,
    family: "missing-script-source",
    stepIndex: 5,
    step: contracts.steps[5],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer: harness.proverSigner,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ExecutionSourceStep06RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: references[5]!,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/**
 * A block committing the fixture under `ScriptSourceMissing { 0, written +
 * offset }`, and the prover's evidence for that purpose. When the purpose has
 * no source the evidence is the adversary's: it claims the scan stopped at
 * the only source, the other spend's.
 */
const setupScenario = async (offset: bigint) => {
  const fixture = await buildMissingScriptSourceFixture(shape);
  const index = (await writtenPurposeIndex(fixture)) + offset;
  const reason = missingScriptSourceReason(0, index);
  const context = await makeMissingScriptSourceHarness();
  const block = await commitMissingScriptSourceBlock({
    harness: context.harness,
    catalogue: context.catalogue,
    fixture,
    committedReason: reason,
  });
  const present = offset !== 0n;
  const complete = await buildMissingScriptSourceUniverse(
    fixture,
    present,
    Number(index),
  );
  const universe = present
    ? complete
    : claimMissingScriptSourcePrefix({
        fixture,
        universe: complete,
        sourceCursor: 0,
      });
  const evidence = missingScriptSourceEvidence({
    fixture,
    universe,
    nativeTxId: block.nativeTxId,
    reason,
  });
  return {
    context,
    universe,
    evidence,
    stages: makeMissingScriptSourceStages(context, block),
  };
};

describe("forced ScriptSourceMissing coordinate the node writes", () => {
  it("convicts a coordinate one purpose early, where the source is present", async () => {
    const s = await setupScenario(-1n);
    expect(missingScriptSourceEvidenceCloses(s.evidence)).toBe(true);
    await runMissingScriptSourceThread(
      s.stages,
      s.evidence,
      s.universe.authentication,
    );
    await s.stages.remove();
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupScenario(0n);
    expect(missingScriptSourceEvidenceCloses(s.evidence)).toBe(false);
    const { stages, evidence } = s;
    const authentication = s.universe.authentication;
    const opened = await stages.step04(
      await stages.step03(
        await stages.step02(
          await stages.step01(await stages.init(), evidence),
          evidence,
          authentication,
        ),
        evidence,
        authentication,
      ),
      evidence,
    );
    const scanned = await stages.scan(opened, evidence);
    // The chain walked the claimed prefix and recorded no source for the
    // written purpose, so the terminal contradiction has nothing to convict.
    await expectOnchainRefusal(() =>
      submitRawStep06(s.context, scanned.threadOutRef),
    );
  }, 600_000);
});
