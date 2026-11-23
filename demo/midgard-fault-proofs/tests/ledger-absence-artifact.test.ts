import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { checkedLedgerAsLedger } from "../src/checked-ledger-store.js";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import {
  admitInputExistenceArtifact,
  inputExistenceArtifact,
} from "../src/input-existence/artifact.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import type { CheckedLedger } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import { detectNoReferenceInputWrongfulRejections } from "../src/no-reference-input/wrongful-rejection.js";
import {
  detectNonExistentInputWrongfulRejections,
  prepareNonExistentInputWrongfulRejection,
} from "../src/non-existent-input/wrongful-rejection.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { detectLedgerRelativeMissingInputs } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.detect-double-withdraws.js";
import {
  admitLedgerAbsenceArtifact,
  type LedgerAbsenceCategory,
  prepareLedgerAbsenceArtifact,
} from "../src/workflow/ledger-absence-artifact.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { withdrawalOf } from "./helpers/event-ledger-seeded-blocks.js";
import {
  type InputChronology,
  inputExistenceChronologyFixture,
} from "./helpers/input-existence-chronology-fixture.js";

const OWNER = "91".repeat(28);

type Fixture = Awaited<ReturnType<typeof inputExistenceChronologyFixture>>;

const context = async () => {
  const transaction = buildFixtureTransaction({
    spendInputs: [outRefCbor(12, 0n)],
    referenceInputs: [outRefCbor(13, 1n)],
    fee: 1n,
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: { L2TransactionEventKey: { tx_id: transaction.txId } },
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/ledger-absence-artifact-test",
      grade: "security",
    },
  });
  return { evidence };
};

const prepare = async (category: LedgerAbsenceCategory) => {
  const { evidence } = await context();
  const replayer =
    category === "nonExistentInput"
      ? NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY
      : NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY;
  const decision = await replayer.replay(evidence);
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections: decision.detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected") {
    throw new Error("fixture did not classify a ledger-absence fault");
  }
  return await prepareLedgerAbsenceArtifact({
    category,
    evidence,
    classification,
    owner: OWNER,
  });
};

describe("production ledger-absence artifacts V1", () => {
  it.each(["nonExistentInput", "noReferenceInput"] as const)(
    "replays %s source, event/transition opening, and event-relative ledger proof",
    async (category) => {
      const artifact = await prepare(category);
      const admitted = admitLedgerAbsenceArtifact(artifact, OWNER);

      expect(admitted.artifact.category).toBe(category);
      expect(admitted.prepared.selectedInput).toEqual(
        category === "nonExistentInput"
          ? { tx_id: "0c".repeat(32), output_index: 0n }
          : { tx_id: "0d".repeat(32), output_index: 1n },
      );
      expect(admitted.prepared.fieldPlan.commitment).toMatch(/^[0-9a-f]{64}$/u);
      expect(admitted.prepared.eventMembership.value.step_index).toBe(0n);
      expect(admitted.prepared.transitionMembership.value.pre_utxos_root).toBe(
        SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      expect(
        admitLedgerAbsenceArtifact(JSON.parse(JSON.stringify(artifact)), OWNER)
          .artifact,
      ).toEqual(artifact);
    },
  );

  it("rejects an unrelated ledger proof at the authenticated event pre-root", async () => {
    const artifact = await prepare("nonExistentInput");

    expect(() =>
      admitLedgerAbsenceArtifact(
        {
          ...artifact,
          evidence: {
            ...artifact.evidence,
            ledger: {
              kind: "absent",
              proof: Data.to(
                [
                  {
                    Leaf: {
                      skip: 0n,
                      key: "ff".repeat(32),
                      value: "ee".repeat(32),
                    },
                  },
                ],
                SDK.Proof,
              ),
            },
          },
        },
        OWNER,
      ),
    ).toThrow(/MPF root changed/u);
  });

  it("rejects a domain substitution without changing the authenticated MPF claim", async () => {
    const artifact = await prepare("nonExistentInput");
    const { prepared } = admitLedgerAbsenceArtifact(artifact, OWNER);
    expect(() =>
      admitLedgerAbsenceArtifact(
        {
          ...artifact,
          evidence: {
            ...artifact.evidence,
            event: Data.to(
              {
                ...prepared.eventMembership,
                domain: SDK.ROOT_DOMAINS.transactionsV1,
              },
              SDK.EventToStepMembershipProof,
            ),
          },
        },
        OWNER,
      ),
    ).toThrow(/counted root changed/u);
  });

  it("rejects the retired creator-exclusion authority on a valid event-relative artifact", async () => {
    const artifact = await prepare("nonExistentInput");
    expect(() =>
      admitLedgerAbsenceArtifact(
        { ...artifact, txsNonMembershipProofCbor: Data.to([], SDK.Proof) },
        OWNER,
      ),
    ).toThrow(/unknown fields/u);
  });

  it("rejects a detection identity whose selected input differs", async () => {
    const artifact = await prepare("nonExistentInput");
    expect(() =>
      admitLedgerAbsenceArtifact(
        {
          ...artifact,
          detectionId: artifact.detectionId.replace(":0:", ":1:"),
        },
        OWNER,
      ),
    ).toThrow(/detection identity/u);
  });
});

// The withdrawal chronology needs a submitted withdrawal; its detection and
// artifact checks run on the emulator in input-existence-real-withdrawal.test.ts.
describe("input-existence event chronology", () => {
  const scenarios: readonly InputChronology[] = [
    "forced_creator",
    "normal_creator",
    "later_creator",
    "forced_spend",
    "normal_spend",
    "rejected_creator",
    "forced_target",
    "same_block_deposit",
    "prior_deposit",
    "forced_prior_deposit",
  ];
  for (const field of [0, 1] as const) {
    it.each(scenarios)(
      `authenticates field ${field} at %s rather than the block-initial root or an arbitrary creator`,
      async (scenario) => {
        const f = await inputExistenceChronologyFixture(scenario, field);
        const detections = await detectLedgerRelativeMissingInputs({
          evidence: f.evidence,
          context: f.context,
          kind: field === 0 ? "spend" : "reference",
        });
        const targetFingerprint = Data.to(f.targetEvent, SDK.EventKey);
        const targetDetections = detections.filter((d) =>
          d.subjectEventKeyCbors.includes(targetFingerprint),
        );
        const present =
          scenario === "forced_creator" ||
          scenario === "normal_creator" ||
          scenario === "prior_deposit" ||
          scenario === "forced_prior_deposit";
        expect(targetDetections).toHaveLength(present ? 0 : 1);
        if (!present)
          expect(targetDetections[0]!.violationId).toBe(
            field === 0 ? "non-existent-input" : "no-reference-input",
          );
        const prepared = await prepareInputExistence({
          block: f.evidence,
          eventKey: f.targetEvent,
          field,
          inputIndex: 0n,
          predecessor: await checkedLedgerAsLedger(f.predecessor),
          owner: OWNER,
        });
        expect(prepared.eventMembership.value.step_index).toBe(f.targetStep);
        expect(prepared.transitionMembership.value.pre_utxos_root).toBe(
          f.targetPreRoot,
        );
        expect(prepared.source.subject.direction).toBe(0n);
        expect(prepared.source.subject.source_kind).toBe(
          scenario === "forced_target" || scenario === "forced_prior_deposit"
            ? 1n
            : 0n,
        );
        expect(prepared.ledgerEvidence).toHaveProperty(
          present ? "Present" : "Absent",
        );
        const admitted = admitInputExistenceArtifact(
          JSON.parse(JSON.stringify(inputExistenceArtifact(prepared))),
          OWNER,
        );
        expect(admitted.ledgerEvidence).toEqual(prepared.ledgerEvidence);
        expect(Data.to(admitted.source.eventKey, SDK.EventKey)).toBe(
          targetFingerprint,
        );
        // These cases deliberately distinguish the event root from predecessor
        // state while retaining the exact canonical source's selected outref.
        if (
          scenario === "forced_creator" ||
          scenario === "normal_creator" ||
          scenario === "forced_spend" ||
          scenario === "normal_spend"
        ) {
          expect(f.targetPreRoot).not.toBe(f.evidence.header.prevUtxosRoot);
        }
        // A later event of the block creates the input: the block's final
        // ledger has it, the target event's ledger does not.
        if (scenario === "later_creator" || scenario === "same_block_deposit")
          expect(f.targetPostRoot).not.toBe(f.evidence.header.utxosRoot);
      },
    );
  }
});

/**
 * The leaf and proof of `key` in the ledger before `eventKey`, step
 * `stepIndex` of `block`: the ledger a from-scratch replay of the block's
 * earlier steps gives (the test reference, not the family's walk), proved
 * from its trie.
 */
const referenceEvidence = async (
  block: CanonicalBlockEvidence,
  predecessor: CheckedLedger,
  eventKey: SDK.EventKey,
  stepIndex: bigint,
  key: Buffer,
) => {
  const { trie } = await referenceLedgerBefore({
    block,
    predecessor,
    eventKey,
    stepIndex,
  });
  const leaf = trie.entries.find((entry) => entry.key.equals(key));
  return leaf === undefined
    ? { proof: await keyValuePhasNonMembershipProof(trie, key) }
    : {
        value: leaf.value,
        proof: await keyValuePhasProof(trie, key, leaf.value),
      };
};

describe("input-existence ledger evidence", () => {
  it.each([
    // Step 0 withdraws the seed 0x51…51#0; the target names 0x53…53#0,
    // which no step touches.
    ["withdrawal_untouched", 0, withdrawalOf(0x51, true), "Present"],
    // Step 0 spends the input the target names.
    ["normal_spend", 0, undefined, "Absent"],
    // Step 0 creates the output the target references.
    ["normal_creator", 1, undefined, "Present"],
  ] as const)(
    "proves the %s target's field %s input against the ledger before its step",
    async (scenario, field, withdrawal, variant) => {
      const f = await inputExistenceChronologyFixture(
        scenario,
        field,
        undefined,
        withdrawal?.committed,
      );
      expect(f.targetPreRoot).not.toBe(f.evidence.header.prevUtxosRoot);
      const prepared = await prepareInputExistence({
        block: f.evidence,
        eventKey: f.targetEvent,
        field,
        inputIndex: 0n,
        predecessor: await checkedLedgerAsLedger(f.predecessor),
        owner: OWNER,
      });
      const { value, proof } = await referenceEvidence(
        f.evidence,
        f.predecessor,
        f.targetEvent,
        f.targetStep,
        f.selected,
      );
      expect(prepared.ledgerEvidence).toStrictEqual(
        value === undefined
          ? { Absent: { proof } }
          : {
              Present: {
                value: value.toString("hex"),
                value_hash: computeHash32(value).toString("hex"),
                proof,
              },
            },
      );
      expect(prepared.ledgerEvidence).toHaveProperty(variant);
    },
  );

  it.each([
    [0, detectNonExistentInputWrongfulRejections],
    [1, detectNoReferenceInputWrongfulRejections],
  ] as const)(
    "proves the field %s input of a wrongful rejection present at the ledger before its step",
    async (field, detect) => {
      // Step 0 withdraws the seed 0x52…52#0; the forced target, rejected
      // as InputNotFound, names 0x51…51#0, which no step touches.
      const f = await inputExistenceChronologyFixture(
        "prior_ledger_present_rejection",
        field,
        undefined,
        withdrawalOf(0x52, true).committed,
      );
      expect(f.targetPreRoot).not.toBe(f.evidence.header.prevUtxosRoot);
      const detections = await detect({
        block: f.evidence,
        predecessor: f.predecessor,
      });
      expect(detections).toHaveLength(1);
      const { value, proof } = await referenceEvidence(
        f.evidence,
        f.predecessor,
        f.targetEvent,
        f.targetStep,
        f.selected,
      );
      expect(value).toBeDefined();
      expect(detections[0]!.prepared.ledgerMembership).toStrictEqual({
        value: value!.toString("hex"),
        proof,
      });
    },
  );

  it.each([
    [
      "an input",
      "later_creator",
      async (f: Fixture, predecessor: Ledger) =>
        await prepareInputExistence({
          block: f.evidence,
          eventKey: f.targetEvent,
          field: 0,
          inputIndex: 0n,
          predecessor,
          owner: OWNER,
        }),
    ],
    [
      "a wrongful rejection",
      "prior_ledger_present_rejection",
      async (f: Fixture, predecessor: Ledger) =>
        await prepareNonExistentInputWrongfulRejection({
          block: f.evidence,
          forcedIndex: 0,
          predecessor,
        }),
    ],
  ] as const)(
    "is this process's local fault for %s when the store no longer holds the predecessor",
    async (_, scenario, prepare) => {
      // The target is step 0, so its evidence reads the predecessor itself,
      // which `retain` of nothing deletes.
      const f = await inputExistenceChronologyFixture(scenario, 0);
      expect(f.targetStep).toBe(0n);
      const store = createLedgerStore(createMemoryLedgerStorage());
      const predecessor = await store.admit(
        f.predecessorEntries,
        f.predecessor.utxosRoot,
      );
      await store.retain(new Set());
      const error: unknown = await prepare(f, predecessor).catch(
        (cause: unknown) => cause,
      );
      expect(isReplayLocalFault(error)).toBe(true);
      expect((error as { cause: unknown }).cause).toBeInstanceOf(
        LedgerNodeMissing,
      );
    },
  );
});
