/**
 * Step 02 at a Certified-tier input-field opening, driven turn by turn
 * through the workflow capture boundary: once the field certificate is on
 * chain, the next captured transaction is the step itself, and only one
 * certificate is ever minted.
 */
import { describe, expect, it } from "vitest";

import { submitResolvedOutputNonCanonicalStep02 } from "../src/resolved-output-non-canonical/index.js";
import {
  driveCapturedStep,
  fieldPreimageCertificateCount,
} from "./support/captured-step-turns.js";
import {
  buildPriorLedger,
  commitBlock,
  makeResolvedOutputContext,
  makeResolvedOutputStages,
  MAXIMUM_INPUT_ITEM_COUNT,
  resolvedOutputEvidence,
  smallMalformedOutput,
  subjectTransactionFor,
} from "./support/resolved-output-non-canonical-emulator.js";

describe("resolvedOutputNonCanonical step 02 under the workflow capture boundary", () => {
  it("reuses the field certificate already on chain, so the next captured transaction is the step", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: smallMalformedOutput(),
      adversarialDepth: false,
    });
    const block = await commitBlock({
      context,
      nativeTx: subjectTransactionFor({
        sourceKind: 0,
        outRefBytes: prior.outRefBytes,
        count: MAXIMUM_INPUT_ITEM_COUNT,
      }),
      priorRoot: prior.priorRoot,
    });
    const evidence = resolvedOutputEvidence({
      block,
      prior,
      coordinate: { sourceKind: 0, inputIndex: 0 },
    });
    expect(evidence.carriage).toBe("Certified");
    const stages = await makeResolvedOutputStages(context, block);
    const bound = await stages.step01Accepted(await stages.init(), evidence);
    const threadOutRef = bound.result.nextThreadOutRef;
    const { harness, contracts, category } = context;
    const drive = await driveCapturedStep({
      emulator: harness.emulator,
      threadOutRef,
      maxTurns: 6,
      invoke: async (boundary) =>
        await submitResolvedOutputNonCanonicalStep02({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          referenceScriptUtxo: stages.references[1]!,
          evidence,
          nativeTxCompactCbor: block.compactCborHex,
          witnessSetCompactCbor: block.witnessSetCompactCborHex,
          publishCarriage: evidence.carriage === "RawUtxo",
          certificateReferenceScriptUtxo: stages.certificateReference,
          publicationPreSubmitBoundary: boundary,
          certificatePreSubmitBoundary: boundary,
          preSubmitBoundary: boundary,
        }),
    });
    expect(drive.prerequisites.length).toBeGreaterThan(1);
    expect(
      await fieldPreimageCertificateCount({
        lucid: harness.proverLucid,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      }),
    ).toBe(1n);
  }, 900_000);
});
