import { InputSetUniquenessStep04SpendRedeemerSchema } from "@al-ft/midgard-sdk/fraud-proof";
import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { requireInputSetUniquenessThreadUtxo } from "../../src/input-set-uniqueness/submit-common.js";
import { buildLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { parseOutRef } from "../../src/runtime.js";
import type { InputSetUniquenessHarness } from "./input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
import { buildAndSubmitUnjournaled } from "./submit-unjournaled.js";

/**
 * Input-set-uniqueness forced-scan step-04 Finalize built by hand: it spends
 * the scan thread at whatever cursor it holds and mints the proof. No builder
 * check runs on whether the scan is complete or the accused pair names no
 * duplicate.
 */
export const submitRawInputSetUniquenessScanFinalize = async ({
  harness,
  threadOutRef,
  referenceScriptUtxo,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const stepIndex = 3 as const;
  const lucid = harness.proverLucid;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    },
  );
  return await buildAndSubmitUnjournaled(buildLinearFaultFinalize, {
    lucid,
    family: "input-set-uniqueness",
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer: harness.proverSigner,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: InputSetUniquenessStep04SpendRedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      Finalize: {
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      },
    }),
    referenceScriptUtxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/**
 * Hands the raw scan finalize to `refuse`, which checks its on-chain refusal,
 * then checks that the refused finalize left the scan thread in place.
 */
export const expectRawScanFinalizeRefused = async ({
  refuse,
  ...finalize
}: Parameters<typeof submitRawInputSetUniquenessScanFinalize>[0] & {
  readonly refuse: (finalize: () => Promise<unknown>) => Promise<unknown>;
}) => {
  await refuse(() => submitRawInputSetUniquenessScanFinalize(finalize));
  expect(
    await finalize.harness.proverLucid.utxosByOutRef([
      parseOutRef(finalize.threadOutRef, "scan thread"),
    ]),
  ).toHaveLength(1);
};
