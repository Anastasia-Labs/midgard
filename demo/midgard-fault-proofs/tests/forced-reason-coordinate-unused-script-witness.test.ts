import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it, vi } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { prepareUnusedScriptWitnessArtifact } from "../src/unused-script-witness/replay.js";
import { buildUnusedScriptWitnessDirectionControlFromRetainedDa } from "../src/unused-script-witness/retained-stage-twelve.js";
import { submitUnusedScriptWitnessStep01Forced } from "../src/unused-script-witness/submit-step-01.js";
import {
  submitUnusedScriptWitnessStep02,
  type UnusedScriptWitnessAuthentication,
} from "../src/unused-script-witness/submit-step-02.js";
import { submitUnusedScriptWitnessStep03 } from "../src/unused-script-witness/submit-step-03.js";
import { submitUnusedScriptWitnessStep04 } from "../src/unused-script-witness/submit-step-04.js";
import { submitUnusedScriptWitnessStep05 } from "../src/unused-script-witness/submit-step-05.js";
import { submitUnusedScriptWitnessStep06 } from "../src/unused-script-witness/submit-step-06.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  funderPaymentKeyHash,
  network,
} from "./support/submit-init-emulator-shared.js";
import {
  buildUnusedScriptWitnessFixture,
  submitUnusedStep02Raw,
} from "./support/unused-script-witness-emulator.js";
import {
  type Harness,
  makeHarness,
} from "./unused-script-witness-lifecycle.make-harness.js";

/**
 * A forced UnusedScriptWitness reason names a field-6 inline script by its
 * index. The fixture carries three always-true native scripts and spends an
 * output under the first alone, so scripts 1 and 2 are both unused; the node
 * names the first of them, where the machine's stage-11 source audit stops.
 *
 * unusedScriptWitness convicts a forced rejection only by the machine's
 * stage-12 terminal, which a transaction reaches only when every inline script
 * is used. So the suite runs the written reason in both polarities: committed
 * against the twin transaction whose every script is used (the node writes
 * that one valid) it is convicted and the block removed; committed against the
 * transaction the node wrote it for, it is refused on chain at step 02.
 */

const SOURCE_COUNT = 3;
const WRITTEN_INDEX = 1;

const fixtureFor = async (
  h: Harness,
  accusedUnused: boolean,
  inputByte: number,
) =>
  await buildUnusedScriptWitnessFixture({
    direction: "forced",
    claimedVerdict: "rejected",
    accusedUnused,
    sourceCount: SOURCE_COUNT,
    accusedIndex: WRITTEN_INDEX,
    inputByte,
    operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
    startTime: h.startTime(),
  });

type Fixture = Awaited<ReturnType<typeof fixtureFor>>;

/** The verdict the node's classifier writes for the fixture's transaction. */
const writtenVerdict = async (fixture: Fixture) => {
  const forced = materializeMidgardForcedTxFromCanonical(
    fixture.transaction.tx,
  );
  return await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: fixture.ledgerEntries.map(({ outRef, output }) => [outRef, output]),
  });
};

/** Run each self-looping scan step until it reports completion. */
const scanToEnd = async (
  step: (threadOutRef: string) => Promise<{
    readonly nextThreadOutRef: string;
    readonly complete: boolean;
  }>,
  threadOutRef: string,
): Promise<string> => {
  let current = threadOutRef;
  for (;;) {
    const result = await step(current);
    current = result.nextThreadOutRef;
    if (result.complete) return current;
  }
};

describe("forced UnusedScriptWitness coordinate the node writes", () => {
  it("convicts the written reason against a transaction whose every script is used", async () => {
    const h = await makeHarness();
    const unused = await fixtureFor(h, true, 0x6a);
    expect(await writtenVerdict(unused)).toStrictEqual({
      ForcedTxInvalid: {
        reason: {
          UnusedScriptWitness: { script_index: BigInt(WRITTEN_INDEX) },
        },
      },
    });
    const fixture = await fixtureFor(h, false, 0x6b);
    expect(await writtenVerdict(fixture)).toBe("ForcedTxValid");
    const setup = await h.setupBlock(fixture);
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.evidence.unused).toBe(false);
    const initialized = await h.init(setup);
    const bound = await submitUnusedScriptWitnessStep01Forced({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      header: fixture.header,
      membership: artifact.forcedMembership!,
      scriptIndex: BigInt(WRITTEN_INDEX),
    });
    const authenticated = await submitUnusedScriptWitnessStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence: artifact.evidence,
      authentication: artifact.authentication,
    });
    const walking = await submitUnusedScriptWitnessStep03(
      h.common(authenticated.nextThreadOutRef, 2),
    );
    const sourcesWalked = await scanToEnd(
      async (threadOutRef) =>
        await submitUnusedScriptWitnessStep04({
          ...h.common(threadOutRef, 3),
          evidence: artifact.evidence,
        }),
      walking.nextThreadOutRef,
    );
    const purposesWalked = await scanToEnd(
      async (threadOutRef) =>
        await submitUnusedScriptWitnessStep05({
          ...h.common(threadOutRef, 4),
          evidence: artifact.evidence,
        }),
      sourcesWalked,
    );
    await submitUnusedScriptWitnessStep06({
      ...h.common(purposesWalked, 5),
      evidence: artifact.evidence,
      witnessReferenceScripts: h.harness.witnessReferenceScripts,
    });
    const deploymentInfo = await h.removalDeployment();
    vi.setSystemTime(h.harness.emulator.now());
    const now = BigInt(h.harness.emulator.now());
    const removal = await submitRemoveFraudulentBlock({
      lucid: h.harness.proverLucid,
      blueprint: h.harness.realBlueprint,
      deploymentInfo,
      network,
      signer: h.harness.proverSigner,
      fraudCategory: "unusedScriptWitness",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: h.leaseCoordinator(
        "unused-script-witness-coordinate",
      ),
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
    expect(removal.fraudulentHeaderHash).toBe(setup.headerHash);
    expect(removal.transactions.map(({ kind }) => kind)).toEqual([
      "remove-target",
    ]);
  }, 1_800_000);

  it("refuses the written reason on chain", async () => {
    const h = await makeHarness();
    const fixture = await fixtureFor(h, true, 0x6c);
    expect(await writtenVerdict(fixture)).toStrictEqual({
      ForcedTxInvalid: {
        reason: {
          UnusedScriptWitness: { script_index: BigInt(WRITTEN_INDEX) },
        },
      },
    });
    const setup = await h.setupBlock(fixture);
    // The rejection holds, so the prover's own replay finds no contradiction.
    await expect(
      prepareUnusedScriptWitnessArtifact(
        fixture.canonicalBlock(setup.headerHash),
      ),
    ).rejects.toThrow(/no contradiction/u);
    // The only retained ScriptSources seam is the stage-11 audit state at the
    // written script; hand exactly that to the forced-direction step 02.
    const base = await buildUnusedScriptWitnessDirectionControlFromRetainedDa({
      eventKey: fixture.eventKey,
      transactionId: fixture.nativeTxId,
      direction: 0n,
      scriptIndex: WRITTEN_INDEX,
      ...fixture.retainedEntries,
      expectedValidationTracesRoot: fixture.header.validationTracesRoot,
    });
    expect(base.control.stage).toBe(11n);
    const target = base.sources[WRITTEN_INDEX]!;
    const authentication: UnusedScriptWitnessAuthentication = {
      trace_membership: base.traceMembership,
      machine_state: base.machineState,
      trace_proof: base.traceProof,
      control: { witness_cbor: base.witnessCbor },
      language_tag: BigInt(target.languageTag),
      script_hash: target.scriptHashHex,
      total_length: BigInt(target.scriptTotalLength),
      item_commitment: target.itemCommitmentHex,
      source_siblings: [...target.siblings],
    };
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: fixture.block.reconstruction,
      eventKey: fixture.eventKey,
    });
    const initialized = await h.init(setup);
    const bound = await submitUnusedScriptWitnessStep01Forced({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      header: fixture.header,
      membership,
      scriptIndex: BigInt(WRITTEN_INDEX),
    });
    // A forced proof authenticates the machine's stage-12 terminal, which a
    // transaction with an unused inline script never reaches. The trace names
    // step 02's one authentication expect; the retained state is the genuine
    // stage-11 audit, so its stage clause is the one that fails.
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(bound.nextThreadOutRef, 1),
          authentication,
          frontiers: {
            source_count: base.control.source_count,
            source_peaks: base.control.source_peaks,
            purpose_count: base.control.purpose_count,
            purpose_peaks: base.control.purpose_peaks,
          },
        }),
      {
        refusedBy: "fraud_proofs/unused_script_witness/step_02",
        check:
          /^expect and \{ event_key_matches_subject\(trace_membership\.key, bound\.subject\),.* else \{ and \{ control\.stage == 12, control\.source_cursor == control\.source_count, \} \},.*\}$/u,
      },
    );
  }, 900_000);
});
