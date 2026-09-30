import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { buildUnusedRedeemerMaterialFromRetainedDa } from "../src/unused-redeemer/replay.js";
import { makeStage } from "./forced-reason-coordinate-unused-redeemer.stage.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  buildMaterial,
  buildRetainedDa,
  programMaterialSidecarCbor,
} from "./unused-redeemer-lifecycle.build-material.js";

/**
 * A forced UnusedRedeemer reason names a field-8 item by its index, and
 * unusedRedeemer reopens exactly that item at the stage-12 audit and reads its
 * bit in the used-redeemer bitmap. The fixture spends two outputs of one
 * script under two spend redeemers and carries a third, mint redeemer that no
 * purpose selects. The verdict is the one the node's classifier writes, so the
 * suite fails if the writer names any item but the unused one: one item early
 * is a used spend redeemer and convicts; the written item is refused on chain.
 */

const SPEND_COUNT = 2;

type RetainedDa = Awaited<ReturnType<typeof buildRetainedDa>>;
type Material = Awaited<ReturnType<typeof buildMaterial>>["material"];

const writtenRedeemerIndex = async (da: RetainedDa): Promise<number> => {
  const forced = materializeMidgardForcedTxFromCanonical(da.transaction.tx);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: da.ledgerWitnessEntries.map(({ outRef, output }) => [
      outRef,
      output,
    ]),
    programMaterialSidecarCbor,
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: { UnusedRedeemer: { redeemer_index: 2n } } },
  });
  return 2;
};

const forcedSubject = (da: RetainedDa, redeemerIndex: number) =>
  SDK.forcedVerdictSubject({
    transactionId: da.transaction.txId.toString("hex"),
    sourceKey: da.sourceKey,
    rejectionReason: {
      UnusedRedeemer: { redeemer_index: BigInt(redeemerIndex) },
    },
  });

/**
 * The material with every bound state carrying `subject`. The prover's replay
 * refuses to build a forced proof at an unused item, so the negative replays
 * the retained DA under the accepted subject of the same transaction and
 * rebinds it, which changes only the subject the validators check.
 */
const underSubject = (
  material: Material,
  subject: SDK.VerdictSubject,
): Material => {
  const a = material.authentication;
  const bound = { ...a.descriptorState.bound, subject };
  const controlState = { ...a.controlState, bound };
  return {
    evidence: {
      ...material.evidence,
      finding: { ...material.evidence.finding, subject },
    },
    authentication: {
      ...a,
      descriptorState: { ...a.descriptorState, bound },
      controlState,
      headerState: { ...a.headerState, authenticated: controlState },
      authenticatedState: { ...a.authenticatedState, bound },
    },
  };
};

describe("forced UnusedRedeemer coordinate the node writes", () => {
  it("convicts a coordinate one item early, where the redeemer is used", async () => {
    const written = await writtenRedeemerIndex(
      await buildRetainedDa("forced", false, false, false, SPEND_COUNT),
    );
    const early = await buildMaterial(
      "forced",
      false,
      written - 1,
      false,
      false,
      SPEND_COUNT,
    );
    expect(early.material.evidence.unused).toBe(false);
    const stage = await makeStage(early, early.material);
    let outRef = await stage.bind();
    for (let offset = 0; offset < stage.stepCount; offset += 1)
      outRef = await stage.linearStep(offset, outRef);
    expect((await stage.finalize(outRef)).fraudProofUnit).toBeTruthy();
    await stage.remove();
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const da = await buildRetainedDa(
      "forced",
      false,
      false,
      false,
      SPEND_COUNT,
    );
    const written = await writtenRedeemerIndex(da);
    const replay = (subject: SDK.VerdictSubject) =>
      buildUnusedRedeemerMaterialFromRetainedDa({
        block: da.block,
        eventKey: da.eventKey,
        subject,
        redeemerIndex: written,
        txCbor: da.txCbor,
      });
    // The written item is selected by no purpose: the rejection holds, and
    // the prover's own replay finds no contradiction.
    await expect(replay(forcedSubject(da, written))).rejects.toThrow(
      "selection frontier contradicts proof direction",
    );
    const accepted = await replay(
      SDK.acceptedVerdictSubject(da.transaction.txId.toString("hex")),
    );
    expect(accepted.evidence.unused).toBe(true);
    const material = underSubject(accepted, forcedSubject(da, written));
    const stage = await makeStage(da, material);
    let outRef = await stage.bind();
    // Step 02 authenticates the descriptor, 02a the stage-12 audit at the
    // cited item, and 02b the item header.
    for (let offset = 0; offset < 3; offset += 1)
      outRef = await stage.linearStep(offset, outRef);
    // Step 02c reads the cited item's bit in the used-redeemer bitmap, which
    // a forced rejection may cite only when it is set.
    await expectOnchainRefusal(
      () => stage.linearStep(3, outRef),
      /^expect if authenticated\.bound\.subject\.direction == substrate\.direction_wrongful_acceptance \{.*\} else \{ authenticated\.used_redeemer_bitmap \/ bit\(header\.item_index\) % 2 == 1 \}$/u,
    );
  }, 900_000);
});
