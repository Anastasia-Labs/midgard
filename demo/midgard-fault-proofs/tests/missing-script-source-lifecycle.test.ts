/**
 * missingScriptSource (`0000002d`) — complete Lucid lifecycle over the six
 * applied reference scripts.
 *
 * Every thread starts from the generic computation-thread `Init` and walks
 * step 01 (purpose binding) → step 02 (trace authentication) → step 03
 * (purpose and transaction-source frontiers) → step 04 (resolved partition,
 * scan opening) → step 05 (the resumable universal-source scan) → step 06
 * (permanent mint) → state-queue target-and-descendant removal. Evidence is
 * reconstructed from the retained DA rows the block commits, never from a
 * fabricated mid-thread datum. The suite covers the §5.3 items the family
 * owns: both directions over every purpose kind and both source locations,
 * honest refusals at the exact on-chain predicate, reason/coordinate and
 * seam substitutions, cancel from every nonterminal physical step, restart
 * from a real scan checkpoint, permanent mint plus removal, the scan's own
 * budget bound, and the maximum consensus-bounded frontier. Every positive
 * transaction is measured against the Van Rossem envelope with the
 * repository's reserves; no oversized route exists here.
 */
import {
  encodeMidgardFieldPreimage,
  encodeMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifyMissingScriptSourceFinding,
  missingScriptSourceEvidenceCloses,
} from "../src/missing-script-source/family.js";
import { discoverRetainedMissingScriptSourceCoordinates } from "../src/missing-script-source/retained-script-universe.js";
import {
  ExecutionSourceStep05DatumSchema,
  ScriptSourcesControlSchema,
} from "../src/missing-script-source/schemas.js";
import {
  MISSING_SCRIPT_SOURCE_SCAN_BUDGET,
  missingScriptSourceDriverBatch,
  missingScriptSourceOnchainCheckpoint,
} from "../src/missing-script-source/universe-scan.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  buildMissingScriptSourceFixture,
  buildMissingScriptSourceUniverse,
  claimMissingScriptSourcePrefix,
  commitMissingScriptSourceBlock,
  makeMissingScriptSourceHarness,
  makeMissingScriptSourceStages,
  missingScriptSourceEvidence,
  type MissingScriptSourceLocation,
  type MissingScriptSourcePurposeKind,
  missingScriptSourceReason,
  runMissingScriptSourceThread,
} from "./support/missing-script-source-emulator.js";
import {
  decoyMissingScriptSourceScript,
  largestInlineSourceCountUnderConsensus,
  largestReferenceSourceCountUnderConsensus,
  MAXIMUM_INLINE_SOURCE_COUNT,
  MAXIMUM_REFERENCE_SOURCE_COUNT,
  MAXIMUM_SUPPORTED_SOURCE_COUNT,
  RESUMABLE_INLINE_SOURCE_COUNT,
  RESUMABLE_REFERENCE_SOURCE_COUNT,
} from "./support/missing-script-source-shapes.js";

const PURPOSE_KINDS: readonly MissingScriptSourcePurposeKind[] = [0, 1, 2, 3];
const LOCATIONS: readonly MissingScriptSourceLocation[] = [
  "inline",
  "reference",
];
const SMALL_INLINE = 3;
const SMALL_REFERENCE = 2;

const measuredFit = createMeasuredFitRecorder(
  "missing-script-source",
  "lifecycle",
  "all purpose/source directions; maximum retained source universe and resumed batched traversal",
);

describe("missingScriptSource retained fixtures", () => {
  it.each(PURPOSE_KINDS)(
    "reconstructs the universe for purpose kind %i in every direction and location",
    async (purposeKind) => {
      const absent = await buildMissingScriptSourceFixture({
        purposeKind,
        presentAt: "absent",
        inlineDecoys: SMALL_INLINE,
        referenceDecoys: SMALL_REFERENCE,
        direction: "accepted",
      });
      expect(
        discoverRetainedMissingScriptSourceCoordinates({
          eventKey: absent.eventKey,
          retainedValidationWitnessEntries: absent.retainedEntries,
        }),
      ).toEqual([{ purposeKind, purposeIndex: 0 }]);
      const universe = await buildMissingScriptSourceUniverse(absent);
      expect(universe.purpose.purposeKind).toBe(purposeKind);
      expect(universe.purpose.requiredScriptHashHex).toBe(
        absent.requiredHashHex,
      );
      expect(universe.sources.map(({ originKind }) => originKind)).toEqual([
        0, 0, 0, 1, 1,
      ]);
      expect(universe.transactionSourceCount).toBe(SMALL_INLINE);
      const evidence = missingScriptSourceEvidence({
        fixture: absent,
        universe,
        nativeTxId: absent.transaction.txId.toString("hex"),
      });
      expect(evidence.foundAtSourceIndex).toBeNull();
      expect(missingScriptSourceEvidenceCloses(evidence)).toBe(true);
      // The complete universe cannot be read as a presence prefix.
      await expect(
        buildMissingScriptSourceUniverse(absent, true),
      ).rejects.toThrow(/absent or duplicated/u);
      for (const location of LOCATIONS) {
        const present = await buildMissingScriptSourceFixture({
          purposeKind,
          presentAt: location,
          presentPosition: "last",
          inlineDecoys: SMALL_INLINE,
          referenceDecoys: SMALL_REFERENCE,
          direction: "forced",
        });
        const prefix = await buildMissingScriptSourceUniverse(present);
        expect(prefix.sources).toHaveLength(present.presentSourceIndex! + 1);
        expect(prefix.sources.at(-1)?.scriptHashHex).toBe(
          present.requiredHashHex,
        );
        const presence = missingScriptSourceEvidence({
          fixture: present,
          universe: prefix,
          nativeTxId: present.transaction.txId.toString("hex"),
        });
        expect(presence.foundAtSourceIndex).toBe(present.presentSourceIndex);
        expect(missingScriptSourceEvidenceCloses(presence)).toBe(true);
        // A matched purpose is not a terminal absence coordinate.
        expect(
          discoverRetainedMissingScriptSourceCoordinates({
            eventKey: present.eventKey,
            retainedValidationWitnessEntries: present.retainedEntries,
          }),
        ).toEqual([]);
      }
    },
    120_000,
  );
});

describe("missingScriptSource real lifecycle", () => {
  it.each(
    PURPOSE_KINDS.flatMap((purposeKind) => [
      { purposeKind, presentAt: "absent" as const },
      ...LOCATIONS.map((presentAt) => ({ purposeKind, presentAt })),
    ]),
  )(
    "corrects purpose kind $purposeKind with the required source $presentAt",
    async ({ purposeKind, presentAt }) => {
      const fixture = await buildMissingScriptSourceFixture({
        purposeKind,
        presentAt,
        presentPosition: "last",
        inlineDecoys: SMALL_INLINE,
        referenceDecoys: SMALL_REFERENCE,
        direction: presentAt === "absent" ? "accepted" : "forced",
      });
      const context = await makeMissingScriptSourceHarness();
      const block = await commitMissingScriptSourceBlock({
        harness: context.harness,
        catalogue: context.catalogue,
        fixture,
      });
      const universe = await buildMissingScriptSourceUniverse(fixture);
      const evidence = missingScriptSourceEvidence({
        fixture,
        universe,
        nativeTxId: block.nativeTxId,
      });
      expect(missingScriptSourceEvidenceCloses(evidence)).toBe(true);
      const stages = makeMissingScriptSourceStages(context, block);
      const { final, batches } = await runMissingScriptSourceThread(
        stages,
        evidence,
        universe.authentication,
      );
      expect(batches).toBe(1);
      const [permanentProof] =
        await context.harness.proverLucid.utxosAtWithUnit(
          context.harness.contracts.fraudProof.spendingScriptAddress,
          final.fraudProofUnit,
        );
      expect(permanentProof?.txHash).toBe(final.txHash);
      await stages.remove();
      stages.assertFit(`kind-${purposeKind.toString()}-${presentAt}`);
      stages.measurements.forEach(({ stage, measurement }, index) =>
        measuredFit.record(
          `${`kind-${purposeKind.toString()}-${presentAt}`}/${index}-${stage}`,
          measurement,
          measurement.executionMemory === 0n ? "publication" : "lifecycle",
        ),
      );
    },
    300_000,
  );

  it("refuses the honest accepted block off chain and at the purpose frontier", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 0,
      presentAt: "inline",
      inlineDecoys: 0,
      referenceDecoys: 0,
      direction: "accepted",
      honest: true,
    });
    // Off chain: no terminal absence coordinate exists, and the matched
    // prefix does not close a wrongful-acceptance claim.
    expect(
      discoverRetainedMissingScriptSourceCoordinates({
        eventKey: fixture.eventKey,
        retainedValidationWitnessEntries: fixture.retainedEntries,
      }),
    ).toEqual([]);
    await expect(
      buildMissingScriptSourceUniverse(fixture, false),
    ).rejects.toThrow(/absent or duplicated/u);
    const context = await makeMissingScriptSourceHarness();
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
    });
    const matched = await buildMissingScriptSourceUniverse(fixture, true);
    const evidence = missingScriptSourceEvidence({
      fixture,
      universe: matched,
      nativeTxId: block.nativeTxId,
    });
    expect(evidence.foundAtSourceIndex).toBe(0);
    expect(missingScriptSourceEvidenceCloses(evidence)).toBe(false);
    // On chain: the bind and trace authentication admit the honest block,
    // and step 03 refuses because the retained discovery never reached the
    // end of the source frontier for a wrongful-acceptance claim.
    const stages = makeMissingScriptSourceStages(context, block);
    const bound = await stages.step02(
      await stages.step01(await stages.init(), evidence),
      evidence,
      matched.authentication,
    );
    await expectOnchainRefusal(() =>
      stages.step03(bound, evidence, matched.authentication),
    );
    await stages.cancel(bound, 2);
  }, 300_000);

  it("refuses the honest forced rejection off chain and at the terminal contradiction", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 0,
      presentAt: "absent",
      inlineDecoys: SMALL_INLINE,
      referenceDecoys: SMALL_REFERENCE,
      direction: "forced",
    });
    await expect(
      buildMissingScriptSourceUniverse(fixture, true),
    ).rejects.toThrow(/absent or duplicated/u);
    const complete = await buildMissingScriptSourceUniverse(fixture, false);
    const context = await makeMissingScriptSourceHarness();
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
    });
    expect(
      missingScriptSourceEvidenceCloses(
        missingScriptSourceEvidence({
          fixture,
          universe: complete,
          nativeTxId: block.nativeTxId,
        }),
      ),
    ).toBe(false);
    // A lying prover claims the scan stopped at source 2. Every row is the
    // operator's retained DA, so the chain admits the prefix and the scan
    // walks it; the terminal contradiction refuses because no source in the
    // prefix carries the required hash.
    const prefix = claimMissingScriptSourcePrefix({
      fixture,
      universe: complete,
      sourceCursor: 2,
    });
    const evidence = missingScriptSourceEvidence({
      fixture,
      universe: prefix,
      nativeTxId: block.nativeTxId,
    });
    expect(evidence.sources).toHaveLength(3);
    expect(missingScriptSourceEvidenceCloses(evidence)).toBe(false);
    const stages = makeMissingScriptSourceStages(context, block);
    const opened = await stages.step04(
      await stages.step03(
        await stages.step02(
          await stages.step01(await stages.init(), evidence),
          evidence,
          prefix.authentication,
        ),
        evidence,
        prefix.authentication,
      ),
      evidence,
    );
    const scanned = await stages.scan(opened, evidence);
    expect(scanned.batches).toBe(1);
    // The chain walked the whole claimed prefix and committed the terminal
    // scan state with `found = false`; the finalizer's builder refuses that
    // state as a wrongful-rejection contradiction before signing, and the
    // validator's own `terminal_contradiction_v1` refuses it too
    // (`honest_rejection_absence_refuses`).
    const [scannedTxHash] = scanned.threadOutRef.split("#");
    const terminal = (
      await context.harness.proverLucid.utxosAt(
        context.contracts.steps[5].spendingScriptAddress,
      )
    ).find(({ txHash }) => txHash === scannedTxHash);
    expect(terminal).toBeDefined();
    const terminalState = (
      Data.from(
        terminal!.datum!,
        ExecutionSourceStep05DatumSchema as never,
      ) as {
        data: NonNullable<
          Data.Static<typeof ExecutionSourceStep05DatumSchema>["data"]
        >;
      }
    ).data;
    expect(terminalState.cursor).toBe(3n);
    expect(terminalState.authenticated.purpose.scan_limit).toBe(3n);
    expect(terminalState.found).toBe(false);
    await expect(stages.step06(scanned.threadOutRef, evidence)).rejects.toThrow(
      /not the retained contradiction/u,
    );
    // The step-06 thread is nonterminal until the mint: it cancels.
    await stages.cancel(scanned.threadOutRef, 5);
  }, 300_000);

  it("refuses a mutated typed reason and a mutated purpose coordinate", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 1,
      presentAt: "reference",
      presentPosition: "last",
      inlineDecoys: SMALL_INLINE,
      referenceDecoys: SMALL_REFERENCE,
      direction: "forced",
    });
    const universe = await buildMissingScriptSourceUniverse(fixture);
    const context = await makeMissingScriptSourceHarness();
    // Another reason constructor cannot enter the family at all.
    expect(() =>
      classifyMissingScriptSourceFinding({
        subject: {
          ...missingScriptSourceEvidence({
            fixture,
            universe,
            nativeTxId: fixture.transaction.txId.toString("hex"),
          }).finding.subject,
          rejection_reason: "FeeBelowMinimum",
        },
        purposeKind: 1,
        purposeIndex: 0,
        executionIndex: 0,
      }),
    ).toThrow(/outside/u);
    // The operator committed the rejection under purpose (1, 1) while the
    // retained discovery names (1, 0).
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
      committedReason: missingScriptSourceReason(1, 1n),
    });
    const stages = makeMissingScriptSourceStages(context, block);
    const claimed = missingScriptSourceEvidence({
      fixture,
      universe,
      nativeTxId: block.nativeTxId,
    });
    const thread = await stages.init();
    // Claiming the retained coordinate against the committed leaf is refused
    // before anything is signed: the leaf's reason names another purpose.
    await expect(stages.step01(thread, claimed)).rejects.toThrow(
      /purpose coordinate differs/u,
    );
    // Claiming the committed coordinate binds. The retained evidence names
    // (1, 0), so the trace builder refuses the bound subject before signing;
    // a prover who also lies about the evidence coordinate authenticates the
    // trace and is refused on chain at the purpose frontier, whose
    // discovery control names purpose (1, 0).
    const boundToLeaf = await stages.step01(thread, claimed, {
      purposeIndex: 1n,
    });
    await expect(
      stages.step02(boundToLeaf, claimed, universe.authentication),
    ).rejects.toThrow(/differs from bound subject/u);
    const lying = {
      ...claimed,
      finding: { ...claimed.finding, purposeIndex: 1 },
    };
    const lyingAuthentication = {
      ...universe.authentication,
      purpose_index: 1n,
    };
    const traced = await stages.step02(boundToLeaf, lying, lyingAuthentication);
    await expectOnchainRefusal(() =>
      stages.step03(traced, lying, lyingAuthentication),
    );
    await stages.cancel(traced, 2);
  }, 300_000);

  it("refuses substitution at every authentication seam and still completes", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 2,
      presentAt: "reference",
      presentPosition: "last",
      inlineDecoys: SMALL_INLINE,
      referenceDecoys: SMALL_REFERENCE,
      direction: "forced",
    });
    const universe = await buildMissingScriptSourceUniverse(fixture);
    const context = await makeMissingScriptSourceHarness();
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
    });
    const evidence = missingScriptSourceEvidence({
      fixture,
      universe,
      nativeTxId: block.nativeTxId,
    });
    const authentication = universe.authentication;
    const stages = makeMissingScriptSourceStages(context, block);
    const bound = await stages.step01(await stages.init(), evidence);
    // Reference-script substitution is refused before signing.
    await expect(
      stages.step02(bound, evidence, authentication, context.references[2]),
    ).rejects.toThrow(/reference script/iu);
    // Trace seam: a proof for another retained state, and a machine state
    // whose hash the descriptor never committed.
    const earlier = claimMissingScriptSourcePrefix({
      fixture,
      universe,
      sourceCursor: 0,
    }).authentication;
    await expectOnchainRefusal(() =>
      stages.step02(bound, evidence, {
        ...authentication,
        trace_proof: earlier.trace_proof,
      }),
    );
    await expectOnchainRefusal(() =>
      stages.step02(bound, evidence, {
        ...authentication,
        machine_state: {
          ...authentication.machine_state,
          prior_ledger_root: "44".repeat(32),
        },
      }),
    );
    const traced = await stages.step02(bound, evidence, authentication);
    // Control seam: a control whose bytes differ from the retained witness
    // (here its resolved-input accumulator) does not hash to the
    // authenticated work root; substituted purpose siblings do not reach the
    // authenticated purpose frontier.
    const substitutedControl = {
      ...authentication.control,
      resolved_inputs_accumulator: "66".repeat(32),
    };
    await expectOnchainRefusal(() =>
      stages.step03(traced, evidence, {
        ...authentication,
        control: substitutedControl,
        control_data: Data.from(
          Data.to(substitutedControl as never, ScriptSourcesControlSchema),
        ),
      }),
    );
    await expectOnchainRefusal(() =>
      stages.step03(traced, evidence, {
        ...authentication,
        purpose_subject: "55".repeat(28),
      }),
    );
    await expectOnchainRefusal(() =>
      stages.step03(traced, evidence, {
        ...authentication,
        purpose_siblings: [...authentication.purpose_siblings, "55".repeat(32)],
      }),
    );
    const staged = await stages.step03(traced, evidence, authentication);
    // Partition seam: a resolved-reference count that does not complete the
    // authenticated scan limit.
    await expectOnchainRefusal(() =>
      stages.step04(staged, {
        ...evidence,
        sources: evidence.sources.slice(0, -1),
        sourceCount: evidence.sources.length - 1,
      }),
    );
    const opened = await stages.step04(staged, evidence);
    // Scan seam: an alternate hash claimed for an authenticated source, a
    // substituted membership path, and a budget above the frozen bound.
    const [first, ...rest] = evidence.sources;
    await expectOnchainRefusal(() =>
      stages.step05(opened, {
        ...evidence,
        sources: [
          { ...first!, scriptHashHex: fixture.requiredHashHex },
          ...rest,
        ],
      }),
    );
    await expectOnchainRefusal(() =>
      stages.step05(opened, {
        ...evidence,
        sources: [
          {
            ...first!,
            sourceMembership: {
              ...first!.sourceMembership,
              siblings: first!.sourceMembership.siblings.map(() =>
                Buffer.alloc(32, 0x66),
              ),
            },
          },
          ...rest,
        ],
      }),
    );
    await expectOnchainRefusal(() =>
      stages.step05(opened, evidence, {
        itemBudget: MISSING_SCRIPT_SOURCE_SCAN_BUDGET + 1,
      }),
    );
    await expect(
      stages.step05(opened, evidence, { itemBudget: 0 }),
    ).rejects.toThrow(/cannot make progress/u);
    // The thread survives every refusal and completes.
    const scanned = await stages.scan(opened, evidence);
    const final = await stages.step06(scanned.threadOutRef, evidence);
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("seams");
    stages.measurements.forEach(({ stage, measurement }, index) =>
      measuredFit.record(
        `${"seams"}/${index}-${stage}`,
        measurement,
        measurement.executionMemory === 0n ? "publication" : "lifecycle",
      ),
    );
  }, 600_000);

  it("cancels from every nonterminal physical step and resumes after a real checkpoint", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 3,
      presentAt: "absent",
      inlineDecoys: RESUMABLE_INLINE_SOURCE_COUNT,
      referenceDecoys: RESUMABLE_REFERENCE_SOURCE_COUNT,
      direction: "accepted",
    });
    const universe = await buildMissingScriptSourceUniverse(fixture);
    const context = await makeMissingScriptSourceHarness();
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
    });
    const evidence = missingScriptSourceEvidence({
      fixture,
      universe,
      nativeTxId: block.nativeTxId,
    });
    const authentication = universe.authentication;
    const stages = makeMissingScriptSourceStages(context, block);
    const sourceCount =
      RESUMABLE_INLINE_SOURCE_COUNT + RESUMABLE_REFERENCE_SOURCE_COUNT;
    expect(evidence.sources).toHaveLength(sourceCount);
    expect(sourceCount).toBeGreaterThan(MISSING_SCRIPT_SOURCE_SCAN_BUDGET);
    // Item 7: cancel from each nonterminal physical step, including the
    // self-looping scan in both of its live states and the step-06 thread
    // before the mint.
    await stages.cancel(await stages.init(), 0);
    await stages.cancel(await stages.step01(await stages.init(), evidence), 1);
    const through02 = async () =>
      await stages.step02(
        await stages.step01(await stages.init(), evidence),
        evidence,
        authentication,
      );
    await stages.cancel(await through02(), 2);
    const through03 = async () =>
      await stages.step03(await through02(), evidence, authentication);
    await stages.cancel(await through03(), 3);
    const through04 = async () =>
      await stages.step04(await through03(), evidence);
    await stages.cancel(await through04(), 4);
    const midWalk = await stages.step05(await through04(), evidence);
    expect(midWalk.closed).toBe(false);
    await stages.cancel(midWalk.nextThreadOutRef, 4);
    const walked = await stages.scan(await through04(), evidence);
    expect(walked.batches).toBe(2);
    await stages.cancel(walked.threadOutRef, 5);
    // Item 8: interrupt after the first batch's checkpoint, discard every
    // builder result, and resume from the live step-05 output alone.
    const interrupted = await stages.step05(await through04(), evidence);
    expect(interrupted.closed).toBe(false);
    const stepAddress = context.contracts.steps[4].spendingScriptAddress;
    const live = (await context.harness.proverLucid.utxosAt(stepAddress)).find(
      ({ txHash }) => txHash === interrupted.txHash,
    );
    expect(live).toBeDefined();
    const checkpoint = (
      Data.from(live!.datum!, ExecutionSourceStep05DatumSchema as never) as {
        data: NonNullable<
          Data.Static<typeof ExecutionSourceStep05DatumSchema>["data"]
        >;
      }
    ).data;
    expect(checkpoint.cursor).toBe(BigInt(MISSING_SCRIPT_SOURCE_SCAN_BUDGET));
    expect(checkpoint.found).toBe(false);
    expect(checkpoint.next_expected_script_hash).toBe(
      context.contracts.steps[4].spendingScriptHash,
    );
    expect(checkpoint.checkpoint_hash).toBe(
      missingScriptSourceOnchainCheckpoint({
        sourceIdentityHex: checkpoint.authenticated.source_identity_hash,
        cursor: checkpoint.cursor,
        found: false,
        nextExpectedScriptHashHex: checkpoint.next_expected_script_hash,
      }),
    );
    const resumedOutRef = `${live!.txHash}#${live!.outputIndex.toString()}`;
    expect(resumedOutRef).toBe(interrupted.nextThreadOutRef);
    const resumed = await stages.step05(resumedOutRef, evidence, {
      label: "step05-scan-resumed",
    });
    expect(resumed.closed).toBe(true);
    const final = await stages.step06(resumed.nextThreadOutRef, evidence);
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("resumable");
    stages.measurements.forEach(({ stage, measurement }, index) =>
      measuredFit.record(
        `${"resumable"}/${index}-${stage}`,
        measurement,
        measurement.executionMemory === 0n ? "publication" : "lifecycle",
      ),
    );
  }, 600_000);

  it("pins the maximum supported frontier to the consensus field bounds", () => {
    expect(largestInlineSourceCountUnderConsensus()).toBe(
      MAXIMUM_INLINE_SOURCE_COUNT,
    );
    expect(largestReferenceSourceCountUnderConsensus()).toBe(
      MAXIMUM_REFERENCE_SOURCE_COUNT,
    );
    // Item 11: one more inline source than the pinned bound is not a
    // canonical transaction at all; its field exceeds the consensus preimage
    // bound and is refused by the field-length rules, not by this family.
    expect(
      encodeMidgardFieldPreimage(
        Array.from({ length: MAXIMUM_INLINE_SOURCE_COUNT + 1 }, (_v, i) =>
          encodeMidgardVersionedScript(decoyMissingScriptSourceScript(i)),
        ),
      ).length,
    ).toBeGreaterThan(MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes);
  });

  it.each([
    { direction: "accepted" as const, presentAt: "absent" as const },
    { direction: "forced" as const, presentAt: "reference" as const },
  ])(
    "walks the maximum supported frontier in the $direction direction",
    async ({ direction, presentAt }) => {
      const fixture = await buildMissingScriptSourceFixture({
        purposeKind: 0,
        presentAt,
        presentPosition: "last",
        inlineDecoys: MAXIMUM_INLINE_SOURCE_COUNT,
        referenceDecoys:
          MAXIMUM_REFERENCE_SOURCE_COUNT - (presentAt === "absent" ? 0 : 1),
        direction,
      });
      expect(fixture.sourceCount).toBe(MAXIMUM_SUPPORTED_SOURCE_COUNT);
      const universe = await buildMissingScriptSourceUniverse(fixture);
      expect(universe.sources).toHaveLength(MAXIMUM_SUPPORTED_SOURCE_COUNT);
      const context = await makeMissingScriptSourceHarness();
      const block = await commitMissingScriptSourceBlock({
        harness: context.harness,
        catalogue: context.catalogue,
        fixture,
      });
      const evidence = missingScriptSourceEvidence({
        fixture,
        universe,
        nativeTxId: block.nativeTxId,
      });
      expect(missingScriptSourceEvidenceCloses(evidence)).toBe(true);
      const stages = makeMissingScriptSourceStages(context, block);
      const { final, batches } = await runMissingScriptSourceThread(
        stages,
        evidence,
        universe.authentication,
      );
      expect(batches).toBe(
        Math.ceil(
          MAXIMUM_SUPPORTED_SOURCE_COUNT /
            missingScriptSourceDriverBatch(MAXIMUM_SUPPORTED_SOURCE_COUNT),
        ),
      );
      expect(final.fraudProofUnit).toBeTruthy();
      await stages.remove();
      stages.assertFit(`maximum-${direction}`);
      stages.measurements.forEach(({ stage, measurement }, index) =>
        measuredFit.record(
          `${`maximum-${direction}`}/${index}-${stage}`,
          measurement,
          measurement.executionMemory === 0n ? "publication" : "lifecycle",
        ),
      );
    },
    1_800_000,
  );
});
