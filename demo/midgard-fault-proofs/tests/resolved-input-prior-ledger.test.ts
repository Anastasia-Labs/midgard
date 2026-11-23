/**
 * The prior ledger resolved-output-non-canonical and
 * spend-input-signer-missing read their inputs' outputs from. Their findings,
 * the complete replay's detections and the recovery material are those a view
 * holding every predecessor entry gives, each with its proof from the
 * predecessor's own trie: the same outputs, descriptors and proof bytes.
 */
import { decodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { Proof } from "@al-ft/midgard-sdk/core";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import type { CheckedLedger } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import { prepareResolvedOutputNonCanonicalRecoveryMaterial } from "../src/resolved-output-non-canonical/authenticated-workflow.create-resolved-output-non-canonical-recovery-ports.js";
import {
  deriveResolvedOutputPriorLedgerReplay,
  detectResolvedOutputNonCanonicalCompleteReplay,
  readResolvedOutputPriorLedger,
  resolvedOutputEvidenceIdentity,
  type ResolvedOutputPriorLedgerReplay,
} from "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import { prepareSpendInputSignerMissingRecoveryMaterial } from "../src/spend-input-signer-missing/authenticated-workflow.derive-spend-input-signer-missing-authenticated-source.js";
import { detectSpendInputSignerMissingCompleteReplay } from "../src/spend-input-signer-missing/spend-input-signer-missing.js";
import { keyValuePhasResolutionProofs } from "../src/transition-trace/phas.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import type { CompleteCanonicalReplay } from "../src/workflow/complete-replay.replay-context-identity.js";
import {
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import { forcedRejection } from "./helpers/event-ledger-forced-step.js";

type Fixture = Readonly<{
  evidence: Awaited<ReturnType<typeof block>>["evidence"];
  context: Awaited<ReturnType<typeof block>>["context"];
}>;

/** Every entry of `checked`, each with its proof from `checked`'s own trie. */
const wholeLedgerView = async (
  checked: CheckedLedger,
): Promise<ResolvedOutputPriorLedgerReplay> => {
  const entries = checked.trie.entries;
  const proofs = await keyValuePhasResolutionProofs(
    checked.trie,
    entries.map(({ key }) => key),
  );
  return {
    priorRoot: checked.utxosRoot,
    outputs: new Map(
      entries.map(({ key, value }, index) => {
        const { txId, outputIndex } = decodeMidgardSpendInputItem(key);
        const transactionId = Buffer.from(txId).toString("hex");
        const membershipProofCborHex = proofs[index]!.toString("hex");
        return [
          `${transactionId}#${outputIndex.toString()}`,
          {
            transactionId,
            outputIndex,
            descriptorCborHex: value.toString("hex"),
            outputCborHex: checked.outputs
              .get(key.toString("hex"))!
              .toString("hex"),
            membershipProofCborHex,
            membershipProof: Data.from(membershipProofCborHex, Proof),
          },
        ] as const;
      }),
    ),
  };
};

const families = {
  resolvedOutput: {
    detect: detectResolvedOutputNonCanonicalCompleteReplay,
    replay: RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    material: prepareResolvedOutputNonCanonicalRecoveryMaterial,
    detectionId: (finding: unknown) =>
      `resolved-output-non-canonical:${resolvedOutputEvidenceIdentity(
        finding as never,
      )}`,
  },
  spendSigner: {
    detect: detectSpendInputSignerMissingCompleteReplay,
    replay: SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
    material: prepareSpendInputSignerMissingRecoveryMaterial,
    detectionId: undefined,
  },
} as const;

/** The family's findings from the whole-ledger view, after checking that the
 * production view, replay and (unless `withMaterial` is false) recovery
 * material give exactly them. */
const sameAsWholeLedger = async (
  family: (typeof families)[keyof typeof families],
  fixture: Fixture,
  withMaterial = true,
) => {
  const checked = completeCanonicalReplayPredecessorLedger(fixture)!;
  const detect = family.detect as (input: {
    block: Fixture["evidence"];
    priorLedger: ResolvedOutputPriorLedgerReplay;
  }) => readonly unknown[];
  const expected = detect({
    block: fixture.evidence,
    priorLedger: await wholeLedgerView(checked),
  });
  const view = await deriveResolvedOutputPriorLedgerReplay({
    block: fixture.evidence,
    predecessor: checked,
  });
  expect(detect({ block: fixture.evidence, priorLedger: view })).toStrictEqual(
    expected,
  );
  const { detections } = await (
    family.replay as CompleteCanonicalReplay
  ).replay(fixture.evidence, fixture.context);
  expect(detections).toHaveLength(expected.length);
  if (family.detectionId !== undefined)
    expect(detections.map(({ detectionId }) => detectionId)).toStrictEqual(
      expected.map(family.detectionId),
    );
  if (withMaterial && expected.length === 1) {
    const material = await family.material(
      fixture.evidence,
      detections[0]!.detectionId,
      checked,
    );
    expect(material.evidence).toStrictEqual(expected[0]);
  }
  return expected;
};

const spending = (outRef: Buffer, lovelace: bigint) => ({
  spendInputs: [outRef],
  outputs: [makeOutput(lovelace)],
  fee: 7n,
});

describe("resolved-output-non-canonical reads the predecessor's outputs as the whole ledger gives them", () => {
  it("for an accepted spend of an output whose native script does not decode", async () => {
    const fixture = await block([
      {
        input: {
          ...spending(NATIVE.outRef, 3_000_000n),
          referenceInputs: [OTHER.outRef],
        },
        applied: true,
      },
    ]);
    expect(
      await sameAsWholeLedger(families.resolvedOutput, fixture),
    ).toHaveLength(1);
  });

  it("for a forced rejection naming a canonical spent output non-canonical", async () => {
    const fixture = await forcedRejection({
      input: spending(SPENT.outRef, 1_999_993n),
      reason: {
        InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
      },
    });
    expect(
      await sameAsWholeLedger(families.resolvedOutput, fixture),
    ).toHaveLength(1);
  });

  it("for an accepted reference to that output beside a spend of an output the predecessor does not hold", async () => {
    const fixture = await block([
      {
        input: {
          ...spending(outRefCbor(0xa9, 0n), 1_000_000n),
          referenceInputs: [NATIVE.outRef],
        },
        applied: true,
      },
    ]);
    expect(
      await sameAsWholeLedger(families.resolvedOutput, fixture),
    ).toHaveLength(1);
  });

  it("for a forced rejection naming a canonical reference output non-canonical", async () => {
    const fixture = await forcedRejection({
      input: {
        ...spending(SPENT.outRef, 1_999_993n),
        referenceInputs: [OTHER.outRef],
      },
      reason: {
        InputSpentOutputNonCanonical: { source_kind: 1n, input_index: 0n },
      },
    });
    expect(
      await sameAsWholeLedger(families.resolvedOutput, fixture),
    ).toHaveLength(1);
  });
});

describe("spend-input-signer-missing reads the predecessor's outputs as the whole ledger gives them", () => {
  it("for an accepted spend of a key-locked output its owner did not sign", async () => {
    const fixture = await block([
      { input: spending(SPENT.outRef, 1_999_993n), applied: true },
    ]);
    expect(await sameAsWholeLedger(families.spendSigner, fixture)).toHaveLength(
      1,
    );
  });

  it("for that spend in a transaction whose reference field does not decode", async () => {
    const fixture = await block([
      {
        input: {
          ...spending(SPENT.outRef, 1_999_993n),
          referenceInputs: [Buffer.from([0])],
        },
        applied: true,
      },
    ]);
    // Its recovery material decodes the whole transaction and refuses it.
    expect(
      await sameAsWholeLedger(families.spendSigner, fixture, false),
    ).toHaveLength(1);
  });

  it("for a forced rejection of an unsigned spend of a key-locked output", async () => {
    const fixture = await forcedRejection({
      input: spending(SPENT.outRef, 1_999_993n),
      reason: { SpendInputSignerMissing: { input_index: 0n } },
    });
    expect(await sameAsWholeLedger(families.spendSigner, fixture)).toEqual([]);
  });
});

describe("the prior view of a predecessor ledger the store no longer holds", () => {
  it("is this process's local fault", async () => {
    const fixture = await block([
      { input: spending(SPENT.outRef, 1_999_993n), applied: true },
    ]);
    const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
    const store = createLedgerStore(createMemoryLedgerStorage());
    const ledger = await store.admit(
      predecessor.entries,
      predecessor.utxosRoot,
    );
    await store.retain(new Set());
    const error: unknown = await readResolvedOutputPriorLedger({
      block: fixture.evidence,
      predecessor: ledger,
    }).catch((cause: unknown) => cause);
    expect(isReplayLocalFault(error)).toBe(true);
    expect((error as { cause: unknown }).cause).toBeInstanceOf(
      LedgerNodeMissing,
    );
  });
});
