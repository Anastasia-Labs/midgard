import { expect } from "vitest";

import {
  advanceUnusedScriptWitnessSources,
  UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
} from "../src/unused-script-witness/checkpoint.js";
import { prepareUnusedScriptWitnessArtifact } from "../src/unused-script-witness/replay.js";
import { type UnusedScriptWitnessAuthentication } from "../src/unused-script-witness/submit-step-02.js";
import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  buildUnusedScriptWitnessFixture,
  readUnusedScanState,
  submitUnusedStep02Raw,
  submitUnusedStep04Raw,
  type UnusedScriptWitnessFixture,
} from "./support/unused-script-witness-emulator.js";
import {
  coverage,
  type Harness,
  hex,
  MAXIMUM_DECOY_TRANSACTION_COUNT,
  MAXIMUM_PURPOSE_COUNT,
  MAXIMUM_SOURCE_COUNT,
  progress,
} from "./unused-script-witness-lifecycle.make-harness.js";

/**
 * Builds the maximum-shape fixture: a probe without widening spends reveals
 * the baseline purpose count the machine derives from the shape (one per
 * used script, plus the mint, observer and receive purposes), and the real
 * fixture widens it to exactly `MAXIMUM_PURPOSE_COUNT`.
 */
export const buildMaximumFixture = async (
  spec: Omit<
    Parameters<typeof buildUnusedScriptWitnessFixture>[0],
    | "sourceCount"
    | "allPurposeKinds"
    | "extraSpendPurposes"
    | "decoyTransactionCount"
  >,
) => {
  const probe = await buildUnusedScriptWitnessFixture({
    ...spec,
    sourceCount: MAXIMUM_SOURCE_COUNT,
    allPurposeKinds: true,
  });
  expect(probe.purposeCount).toBeLessThan(MAXIMUM_PURPOSE_COUNT);
  const fixture = await buildUnusedScriptWitnessFixture({
    ...spec,
    sourceCount: MAXIMUM_SOURCE_COUNT,
    allPurposeKinds: true,
    extraSpendPurposes: MAXIMUM_PURPOSE_COUNT - probe.purposeCount,
    decoyTransactionCount: MAXIMUM_DECOY_TRANSACTION_COUNT,
  });
  expect(fixture.purposeCount).toBe(MAXIMUM_PURPOSE_COUNT);
  expect(fixture.header.validationTraceCount).toBe(
    BigInt(MAXIMUM_DECOY_TRANSACTION_COUNT + 1),
  );
  return fixture;
};

export type Artifact = Awaited<
  ReturnType<typeof prepareUnusedScriptWitnessArtifact>
>;

export const shapeLabel = (fixture: UnusedScriptWitnessFixture) =>
  `${fixture.spec.sourceCount.toString()} inline scripts (accused at ${fixture.scriptIndex.toString()}), ${fixture.purposeCount.toString()} purposes, ${fixture.header.validationTraceCount.toString()} validation traces`;

export const frontiersOf = (artifact: Artifact) => ({
  source_count: BigInt(artifact.evidence.sources[0]!.membership.frontier.count),
  source_peaks: artifact.evidence.sources[0]!.membership.frontier.peaks.map(
    ({ height, hash }) => ({ height: BigInt(height), hash: hex(hash) }),
  ),
  purpose_count: BigInt(
    artifact.evidence.purposes[0]!.membership.frontier.count,
  ),
  purpose_peaks: artifact.evidence.purposes[0]!.membership.frontier.peaks.map(
    ({ height, hash }) => ({ height: BigInt(height), hash: hex(hash) }),
  ),
});

const sourceOpenings = (artifact: Artifact, start: number, end: number) =>
  artifact.evidence.sources.slice(start, end).map((opening) => ({
    source_index: BigInt(opening.sourceIndex),
    language_tag: BigInt(opening.languageTag),
    script_hash: opening.scriptHashHex,
    total_length: BigInt(opening.scriptTotalLength),
    item_commitment: opening.itemCommitmentHex,
    siblings: opening.membership.siblings.map(hex),
  }));

export const purposeOpenings = (
  artifact: Artifact,
  start: number,
  end: number,
) =>
  artifact.evidence.purposes.slice(start, end).map((opening) => ({
    frontier_index: BigInt(opening.frontierIndex),
    purpose_kind: BigInt(opening.purposeKind),
    purpose_index: BigInt(opening.purposeIndex),
    script_hash: opening.scriptHashHex,
    purpose_subject: opening.purposeSubjectHex,
    siblings: opening.membership.siblings.map(hex),
  }));

/** Every step-02 authentication seam, mutated one at a time against a bound thread. */
export const refuseEveryStep02Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const authentication = artifact.authentication;
  const frontiers = frontiersOf(artifact);
  const attempt = async (
    seam: string,
    mutated: UnusedScriptWitnessAuthentication,
    overrides: Partial<{
      frontiers: typeof frontiers;
      nextStepIndex: number;
    }> = {},
  ) => {
    progress(`step-02 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(threadOutRef, 1),
          authentication: mutated,
          frontiers: overrides.frontiers ?? frontiers,
          nextStepIndex: overrides.nextStepIndex,
        }),
    );
    coverage.seams.add(seam);
  };
  const membership = authentication.trace_membership;
  await attempt("validation_traces_root", {
    ...authentication,
    trace_membership: { ...membership, root: "ff".repeat(32) },
  });
  await attempt("trace_descriptor", {
    ...authentication,
    trace_membership: {
      ...membership,
      value: {
        ...membership.value,
        step_count: membership.value.step_count + 1n,
      },
    },
  });
  await attempt("subject_event_key", {
    ...authentication,
    trace_membership: {
      ...membership,
      key: { L2TransactionEventKey: { tx_id: "aa".repeat(32) } },
    },
  });
  await attempt("machine_state", {
    ...authentication,
    machine_state: {
      ...authentication.machine_state,
      prior_ledger_root: "ee".repeat(32),
    },
  });
  expect(authentication.trace_proof.siblings.length).toBeGreaterThan(0);
  await attempt("trace_proof", {
    ...authentication,
    trace_proof: {
      ...authentication.trace_proof,
      siblings: [
        "dd".repeat(32),
        ...authentication.trace_proof.siblings.slice(1),
      ],
    },
  });
  await attempt("retained_control", {
    ...authentication,
    control: {
      witness_cbor: `${authentication.control.witness_cbor.slice(0, -2)}00`,
    },
  });
  await attempt("source_item", {
    ...authentication,
    script_hash: artifact.evidence.sources[0]!.scriptHashHex,
  });
  await attempt("source_language", {
    ...authentication,
    language_tag: authentication.language_tag === 0n ? 3n : 0n,
  });
  await attempt("source_length", {
    ...authentication,
    total_length: authentication.total_length + 1n,
  });
  await attempt("source_commitment", {
    ...authentication,
    item_commitment: "cc".repeat(32),
  });
  expect(authentication.source_siblings.length).toBeGreaterThan(0);
  await attempt("source_membership", {
    ...authentication,
    source_siblings: [
      "99".repeat(32),
      ...authentication.source_siblings.slice(1),
    ],
  });
  await attempt("source_frontier", authentication, {
    frontiers: {
      ...frontiers,
      purpose_peaks: frontiers.purpose_peaks.map((peak) => ({
        ...peak,
        hash: "bb".repeat(32),
      })),
    },
  });
  await attempt("wrong_successor", authentication, { nextStepIndex: 3 });
};

/** Every step-04 seam against a thread that just entered the alternate walk. */
export const refuseEveryStep04Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const state = await readUnusedScanState(h.common(threadOutRef, 3), 3);
  expect(state.alternate_cursor).toBe(0n);
  const budget = UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH;
  const legit = sourceOpenings(artifact, 0, budget);
  expect(legit.length).toBe(budget);
  const next = advanceUnusedScriptWitnessSources({
    state,
    evidence: artifact.evidence,
    itemBudget: budget,
  });
  const attempt = async (
    seam: string,
    input: Partial<Parameters<typeof submitUnusedStep04Raw>[0]>,
  ) => {
    progress(`step-04 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep04Raw({
          ...h.common(threadOutRef, 3),
          openings: legit,
          itemBudget: BigInt(budget),
          nextState: next,
          nextStepIndex: 3,
          ...input,
        }),
    );
    coverage.seams.add(seam);
  };
  await attempt("alternate_source_item", {
    openings: [
      { ...legit[0]!, script_hash: artifact.evidence.targetScriptHashHex },
      ...legit.slice(1),
    ],
  });
  await attempt("alternate_source_membership", {
    openings: [
      {
        ...legit[0]!,
        siblings: ["77".repeat(32), ...legit[0]!.siblings.slice(1)],
      },
      ...legit.slice(1),
    ],
  });
  await attempt("alternate_source_order", {
    openings: [legit[1]!, legit[0]!, ...legit.slice(2)],
  });
  await attempt("alternate_batch_short", {
    openings: legit.slice(0, budget - 1),
    nextState: advanceUnusedScriptWitnessSources({
      state,
      evidence: artifact.evidence,
      itemBudget: budget - 1,
    }),
  });
  await attempt("alternate_budget_over_bound", {
    openings: sourceOpenings(artifact, 0, budget + 1),
    itemBudget: BigInt(budget + 1),
    nextState: {
      ...advanceUnusedScriptWitnessSources({
        state,
        evidence: artifact.evidence,
        itemBudget: budget,
      }),
      alternate_cursor: BigInt(budget + 1),
    },
  });
};
