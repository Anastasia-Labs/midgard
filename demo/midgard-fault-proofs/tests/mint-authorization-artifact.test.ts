/**
 * The mint authorization artifact's predecessor witness: a block whose first
 * transaction spends `SPENT` and creates an output, and whose second spends
 * `OTHER`, references that output and mints under a policy no resolved
 * output carries, over a predecessor holding outputs neither touches.
 */
import { encodeMidgardMintPolicyItem } from "@al-ft/midgard-core/codec";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import {
  admitMintAuthorizationWorkflowArtifact,
  prepareMintAuthorizationWorkflowArtifact,
} from "../src/mint-authorization/artifact.js";
import {
  mintAuthorizationInputProof,
  prepareMintAuthorizationReplay,
} from "../src/mint-authorization/replay.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import type { WorkflowJsonObject } from "../src/workflow/identity.js";
import { buildFixtureTransaction } from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
} from "./helpers/event-ledger-committed-steps.js";
import { outRef } from "./helpers/event-ledger-seeded-blocks.js";
import { testLedgerEntries } from "./support/ledger-store-conformance.js";
import {
  droppedWitnesses,
  expectWitnessRefused,
  predecessorWitnessOf,
  tamperedWitnesses,
  withPredecessorWitness,
} from "./support/predecessor-witness-artifact.js";

const unauthorizedMint = (spent: Buffer, referenceInputs: Buffer[] = []) => ({
  spendInputs: [spent],
  referenceInputs,
  outputs: [makeOutput(2_999_993n)],
  mintPolicyItems: [
    encodeMidgardMintPolicyItem({
      policyId: Buffer.alloc(28, 0x11),
      assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
    }),
  ],
  fee: 7n,
});

const creator = {
  spendInputs: [SPENT.outRef],
  outputs: [makeOutput(1_999_993n)],
  fee: 7n,
};
const created = outRef(buildFixtureTransaction(creator).txId, 0);

const predecessorWith = (others: number): LedgerEntry[] => [
  NATIVE,
  SPENT,
  OTHER,
  ...testLedgerEntries(2000, others),
];

/** The committed block and the artifact the workflow writes for its one
 * detection. */
const scene = async (
  steps: Parameters<typeof block>[0],
  predecessor: readonly LedgerEntry[],
) => {
  const fixture = await block(steps, predecessor);
  const { classification } = await fixture.replay([
    MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY,
  ]);
  if (classification.decision !== "fault_detected")
    throw new Error("the scene has no detection");
  const artifact = await prepareMintAuthorizationWorkflowArtifact({
    evidence: fixture.evidence,
    classification,
    replayContext: fixture.context,
  });
  return {
    fixture,
    // What the workflow store holds: JSON written and read back.
    artifact: JSON.parse(JSON.stringify(artifact)) as WorkflowJsonObject,
  };
};

const REFERENCING = [
  { input: creator, applied: true },
  { input: unauthorizedMint(OTHER.outRef, [created]), applied: true },
] as const;

type Prepared = Awaited<
  ReturnType<typeof prepareMintAuthorizationReplay>
>[number];

/** A prepared finding with every resolved input's step-04 proof taken. */
const comparable = async ({
  referenceLedger: _,
  current,
  ...rest
}: Prepared) => {
  const proofs = [];
  for (const input of [...rest.references, ...rest.spends])
    proofs.push(
      await mintAuthorizationInputProof({ referenceLedger: _ }, input),
    );
  return { ...rest, headerHash: current.headerHash, proofs };
};

describe("mint authorization artifact predecessor witness", () => {
  it.each([0, 40])(
    "admits and prepares what the full ledger prepares, over %s other outputs",
    async (others) => {
      const { fixture, artifact } = await scene(
        REFERENCING,
        predecessorWith(others),
      );
      const admitted = await admitMintAuthorizationWorkflowArtifact(artifact);
      const [expected] = await prepareMintAuthorizationReplay({
        current: fixture.evidence.reconstruction,
        predecessor: await completeCanonicalReplayPredecessorStoreLedger({
          evidence: fixture.evidence,
          context: fixture.context,
        }),
        sourceIndex: Number(fixture.position(1)),
      });
      expect(admitted.references).toHaveLength(1);
      expect(admitted.spends).toHaveLength(1);
      expect(await comparable(admitted)).toEqual(await comparable(expected!));
    },
  );

  it("refuses every tampered and every dropped node", async () => {
    const { artifact } = await scene(REFERENCING, predecessorWith(40));
    const witness = predecessorWitnessOf(artifact);
    expect(witness.nodes.length).toBeGreaterThan(1);
    // Admission, then the proofs step 04 takes.
    const use = async (changed: typeof witness) =>
      await comparable(
        await admitMintAuthorizationWorkflowArtifact(
          withPredecessorWitness(artifact, changed),
        ),
      );
    for (const [index, tampered] of tamperedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () => await use(tampered),
        `tampered node ${index.toString()}`,
      );
    for (const [index, dropped] of droppedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () => await use(dropped),
        `dropped node ${index.toString()}`,
      );
  });

  it("refuses the witness a sibling block over the same predecessor wrote", async () => {
    const predecessor = predecessorWith(40);
    const own = await scene(
      [{ input: unauthorizedMint(OTHER.outRef), applied: true }],
      predecessor,
    );
    const sibling = await scene(
      [{ input: unauthorizedMint(SPENT.outRef), applied: true }],
      predecessor,
    );
    expect(sibling.fixture.evidence.headerHash).not.toBe(
      own.fixture.evidence.headerHash,
    );
    expect(sibling.fixture.evidence.header.prevUtxosRoot).toBe(
      own.fixture.evidence.header.prevUtxosRoot,
    );
    await comparable(
      await admitMintAuthorizationWorkflowArtifact(own.artifact),
    );
    await expectWitnessRefused(
      async () =>
        await comparable(
          await admitMintAuthorizationWorkflowArtifact(
            withPredecessorWitness(
              own.artifact,
              predecessorWitnessOf(sibling.artifact),
            ),
          ),
        ),
      "sibling witness",
    );
  });

  it("refuses the previous schema version", async () => {
    const { artifact } = await scene(REFERENCING, predecessorWith(0));
    const { predecessorWitness: _, ...rest } = artifact;
    await expect(
      admitMintAuthorizationWorkflowArtifact({
        ...rest,
        schemaVersion: "midgard-mint-authorization-workflow-artifact-v2",
        predecessorLedger: [],
      } as WorkflowJsonObject),
    ).rejects.toThrow(
      "mint authorization artifact has missing or unknown fields",
    );
    await expect(
      admitMintAuthorizationWorkflowArtifact({
        ...artifact,
        schemaVersion: "midgard-mint-authorization-workflow-artifact-v2",
      }),
    ).rejects.toThrow("mint authorization: artifact schema changed");
  });

  it("writes a witness that does not grow with the predecessor ledger", async () => {
    const nodes = async (others: number) =>
      predecessorWitnessOf(
        (await scene(REFERENCING, predecessorWith(others))).artifact,
      ).nodes.length;
    const small = await nodes(64);
    const large = await nodes(1024);
    // Sixteen times the outputs add one trie level to each path the
    // preparation reads and proves: the witness grows by that level, a whole
    // ledger in the artifact would grow sixteenfold.
    expect(large).toBeLessThan(small * 4);
  });
});
