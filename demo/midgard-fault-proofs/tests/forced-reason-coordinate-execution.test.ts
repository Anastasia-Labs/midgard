import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { prepareReceivePurposeLanguageArtifact } from "../src/receive-purpose-language/authenticated-replay.js";
import { prepareReceivePurposeLanguageEvidence } from "../src/receive-purpose-language/family.js";
import {
  buildReceivePurposeLanguageAuthenticationFromRetainedDa,
  receivePurposeLanguageDescriptorFromAuthentication,
} from "../src/receive-purpose-language/retained-witness.js";
import { submitReceivePurposeLanguageStep01Forced } from "../src/receive-purpose-language/submit-step-01.js";
import { submitReceivePurposeLanguageStep02 } from "../src/receive-purpose-language/submit-step-02.js";
import { submitReceivePurposeLanguageStep03 } from "../src/receive-purpose-language/submit-step-03.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { makeHarness } from "./receive-purpose-language-lifecycle.make-harness.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  buildReceivePurposeFixture,
  submitReceiveStep03Raw,
} from "./support/receive-purpose-language-emulator.js";
import {
  PLUTUS_V3_RECEIVE_SIDECAR,
  receivePurposeReason,
} from "./support/receive-purpose-language-emulator.receive-purpose-fixture-spec.js";
import { funderPaymentKeyHash } from "./support/submit-init-emulator-shared.js";

/**
 * A forced ReceivePurposePlutusV3Forbidden reason names an execution by its
 * index over every script execution, native ones included, and
 * receivePurposeLanguage reopens exactly that execution. The fixture runs a
 * passing native receive at execution 0 before the PlutusV3 receive the
 * machine refuses. The verdict is the one the node's classifier writes, so
 * the suite fails if the writer and the proof disagree on how executions are
 * counted: one execution early names the native receive and convicts; the
 * written execution is refused on chain.
 */

/** The execution the node writes: the PlutusV3 receive, after the native. */
const WRITTEN = 1n;

const setupScenario = async (offset: bigint) => {
  const h = await makeHarness();
  const fixture = await buildReceivePurposeFixture({
    direction: "forced",
    language: "plutusV3",
    claimedVerdict: "rejected",
    purposeCount: 1,
    leadingNativeReceive: true,
    committedExecutionIndex: Number(WRITTEN + offset),
    inputByte: 0x6c,
    operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
    startTime: h.startTime(),
  });
  const forced = materializeMidgardForcedTxFromCanonical(
    fixture.transaction.tx,
  );
  expect(
    await nodeForcedVerdict({
      transactionId: computeMidgardNativeTxId(forced),
      forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
      ledger: fixture.ledgerEntries.map(({ outRef, output }) => [
        outRef,
        output,
      ]),
      programMaterialSidecarCbor: PLUTUS_V3_RECEIVE_SIDECAR,
    }),
  ).toStrictEqual({
    ForcedTxInvalid: { reason: receivePurposeReason(Number(WRITTEN)) },
  });
  expect(fixture.executionIndex).toBe(Number(WRITTEN));
  const setup = await h.setupBlock(fixture);
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: fixture.block.reconstruction,
    eventKey: fixture.eventKey,
  });
  const initialized = await h.init(setup);
  const bound = await submitReceivePurposeLanguageStep01Forced({
    ...h.common(initialized.result.nextThreadOutRef, 0),
    header: fixture.header,
    membership,
    executionIndex: WRITTEN + offset,
  });
  return { h, fixture, setup, bound };
};

describe("forced ReceivePurposePlutusV3Forbidden coordinate the node writes", () => {
  it("convicts a coordinate one execution early, where the native receive ran", async () => {
    const { h, fixture, setup, bound } = await setupScenario(-1n);
    const artifact = await prepareReceivePurposeLanguageArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.evidence.finding.executionIndex).toBe(0);
    expect(artifact.authentication.language_tag).toBe(0n);
    const authenticated = await submitReceivePurposeLanguageStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence: artifact.evidence,
      authentication: artifact.authentication,
    });
    const final = await submitReceivePurposeLanguageStep03({
      ...h.common(authenticated.nextThreadOutRef, 2),
      evidence: artifact.evidence,
      witnessReferenceScripts: h.harness.witnessReferenceScripts,
    });
    expect(
      (
        await h.harness.proverLucid.utxosAt(
          h.harness.contracts.fraudProof.spendingScriptAddress,
        )
      ).some(({ txHash }) => txHash === final.txHash),
    ).toBe(true);
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const { h, fixture, bound } = await setupScenario(0n);
    const rebuilt =
      await buildReceivePurposeLanguageAuthenticationFromRetainedDa({
        eventKey: fixture.eventKey,
        executionIndex: Number(WRITTEN),
        ...fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.header.validationTracesRoot,
        expectedLanguageTag: 3,
      });
    const evidence = prepareReceivePurposeLanguageEvidence({
      finding: { subject: fixture.subject, executionIndex: Number(WRITTEN) },
      descriptor: receivePurposeLanguageDescriptorFromAuthentication(
        rebuilt.authentication,
        Number(WRITTEN),
      ),
    });
    const authenticated = await submitReceivePurposeLanguageStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence,
      authentication: rebuilt.authentication,
    });
    // The written execution is the PlutusV3 receive: the rejection holds.
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep03Raw({
          ...h.common(authenticated.nextThreadOutRef, 2),
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
  }, 600_000);
});
