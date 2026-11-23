/**
 * The direct purpose artifact's predecessor witness, through the
 * missing-script-source family: a forced transaction committed as admitted
 * whose only Plutus observer has no source, over a predecessor holding its
 * spent input and outputs it does not touch.
 */
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  admitDirectPurposeArtifact,
  prepareDirectPurposeArtifact,
} from "../src/direct-purpose-family/artifact.js";
import type { DirectPurposeReplay } from "../src/direct-purpose-family/artifact-family.js";
import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import { MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY } from "../src/missing-script-source/replay.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  completeCanonicalReplayPredecessorStoreLedger,
} from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import type { WorkflowJsonObject } from "../src/workflow/identity.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { testLedgerEntries } from "./support/ledger-store-conformance.js";
import {
  buildMissingRedeemerBlock,
  honestForcedObserverShape,
  type MissingRedeemerSubjectShape,
  plutusScript,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  droppedWitnesses,
  expectWitnessRefused,
  predecessorWitnessOf,
  tamperedWitnesses,
  withPredecessorWitness,
} from "./support/predecessor-witness-artifact.js";

const FAMILY = MISSING_SCRIPT_SOURCE_ARTIFACT_FAMILY;
const admit = (artifact: WorkflowJsonObject) =>
  admitDirectPurposeArtifact(FAMILY, artifact);

const signed = honestForcedObserverShape(5);
/** The observer has no source anywhere. */
const SOURCELESS: MissingRedeemerSubjectShape = {
  ...signed,
  observers: [{ script: plutusScript(5), witness: false, redeemer: false }],
  reference: [],
};
/** The same transaction spending another output the signer owns. */
const SIBLING: MissingRedeemerSubjectShape = {
  ...SOURCELESS,
  spend: [
    {
      outRef: encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x41),
        outputIndex: 0,
      }),
      output: signed.spend![0]!.output,
    },
  ],
};

const filler = (count: number) => testLedgerEntries(2000, count);

/** The committed block, its replay context and the artifact the workflow
 * writes for its one detection. */
const scene = async (
  shape: MissingRedeemerSubjectShape,
  ledger: readonly LedgerEntry[],
) => {
  const built = await buildMissingRedeemerBlock({
    shape,
    source: { kind: "forced", verdict: "ForcedTxValid" },
    ledger,
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/direct-purpose-artifact",
      grade: "security",
    },
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, ledger),
      currentEvidence: evidence,
    }),
  };
  const { detections } =
    await MISSING_SCRIPT_SOURCE_COMPLETE_CANONICAL_REPLAY.replay(
      evidence,
      context,
    );
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected")
    throw new Error("the scene has no detection");
  const artifact = await prepareDirectPurposeArtifact(FAMILY, {
    evidence,
    classification,
    replayContext: context,
  });
  return {
    evidence,
    context,
    // What the workflow store holds: JSON written and read back.
    artifact: JSON.parse(JSON.stringify(artifact)) as WorkflowJsonObject,
  };
};

/** A prepared finding with its resolved inputs' proofs taken. */
const comparable = async ({
  material,
  ...rest
}: DirectPurposeReplay & Record<string, unknown>) => {
  const { resolvedInputs, ...fields } = material;
  const proofs = [];
  for (const key of resolvedInputs.byKey.keys())
    proofs.push(await resolvedInputs.proof(Buffer.from(key, "hex")));
  return {
    ...rest,
    material: { ...fields, byKey: resolvedInputs.byKey, proofs },
  };
};

describe("direct purpose artifact predecessor witness", () => {
  it.each([0, 40])(
    "admits and prepares what the full ledger prepares, over %s other outputs",
    async (others) => {
      const { evidence, context, artifact } = await scene(SOURCELESS, [
        ...SOURCELESS.spend!,
        ...filler(others),
      ]);
      const { current, ...admitted } = await admit(artifact);
      const expected = await FAMILY.prepare({
        current: evidence.reconstruction,
        coordinate: admitted.coordinate,
        predecessor: await completeCanonicalReplayPredecessorStoreLedger({
          evidence,
          context,
        }),
      });
      expect(expected).not.toBeNull();
      expect(current.headerHash).toBe(evidence.headerHash);
      expect(admitted.material.resolvedInputs.byKey.size).toBe(1);
      expect(await comparable(admitted)).toEqual(await comparable(expected!));
    },
  );

  it("refuses every tampered and every dropped node", async () => {
    const { artifact } = await scene(SOURCELESS, [
      ...SOURCELESS.spend!,
      ...filler(40),
    ]);
    const witness = predecessorWitnessOf(artifact);
    expect(witness.nodes.length).toBeGreaterThan(1);
    // Admission, then the proofs the convicting proof takes.
    const use = async (changed: typeof witness) => {
      const admitted = await admit(withPredecessorWitness(artifact, changed));
      await comparable(admitted);
    };
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
    const ledger = [...SOURCELESS.spend!, ...SIBLING.spend!, ...filler(40)];
    const own = await scene(SOURCELESS, ledger);
    const sibling = await scene(SIBLING, ledger);
    expect(sibling.evidence.headerHash).not.toBe(own.evidence.headerHash);
    expect(sibling.evidence.header.prevUtxosRoot).toBe(
      own.evidence.header.prevUtxosRoot,
    );
    await admit(own.artifact);
    await expectWitnessRefused(
      async () =>
        await admit(
          withPredecessorWitness(
            own.artifact,
            predecessorWitnessOf(sibling.artifact),
          ),
        ),
      "sibling witness",
    );
  });

  it("refuses the previous artifact shape", async () => {
    const { artifact } = await scene(SOURCELESS, SOURCELESS.spend!);
    const { predecessorWitness: _, ...rest } = artifact;
    await expect(
      admit({ ...rest, predecessorLedger: [] } as WorkflowJsonObject),
    ).rejects.toThrow("missingScriptSource artifact shape changed");
  });

  it("refuses a null witness for a block that starts from a non-empty ledger", async () => {
    const { artifact } = await scene(SOURCELESS, SOURCELESS.spend!);
    await expect(
      admit({ ...artifact, predecessorWitness: null }),
    ).rejects.toThrow("the block starts from a non-empty ledger");
  });

  it("writes a witness that does not grow with the predecessor ledger", async () => {
    const nodes = async (others: number) =>
      predecessorWitnessOf(
        (await scene(SOURCELESS, [...SOURCELESS.spend!, ...filler(others)]))
          .artifact,
      ).nodes.length;
    const small = await nodes(64);
    const large = await nodes(1024);
    // Sixteen times the outputs add one trie level to each path the
    // preparation reads and proves: the witness grows by that level, a whole
    // ledger in the artifact would grow sixteenfold.
    expect(large).toBeLessThan(small * 4);
  });
});
