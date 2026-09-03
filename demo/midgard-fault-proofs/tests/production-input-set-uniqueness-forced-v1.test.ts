import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  InputSetUniquenessVerdictSubjectSchema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, expectTypeOf, it } from "vitest";

import { INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID } from "../src/input-set-uniqueness/replay-v1.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  admitInputSetUniquenessForcedArtifact,
  INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
  InputSetUniquenessForcedSourceSchema,
  type ManifestBoundInputSetUniquenessWorkflowConfig,
} from "../src/workflow/production-input-set-uniqueness-v1.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";
import {
  buildInputSetUniquenessFixture,
  isuOutRef,
} from "./support/input-set-uniqueness-emulator-v1.js";

const makeArtifact = async () => {
  const fixture = await buildInputSetUniquenessFixture({
    spendInputs: [isuOutRef("11", 0)],
    referenceInputs: [isuOutRef("22", 0)],
    validity: "TxIsInvalid",
  });
  const key = { transactionId: "33".repeat(32), outputIndex: 0n };
  const reason = {
    DuplicateInput: {
      first_field_index: 0n,
      first_item_index: 0n,
      second_field_index: 1n,
      second_item_index: 0n,
    },
  } as const;
  const leaf = {
    tx_id: fixture.nativeTxId,
    source: fixture.forcedSource,
    verdict: { ForcedTxInvalid: { reason } },
  } as const;
  const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(leaf as never, ForcedInclusionTxSchema as never),
    "hex",
  );
  const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const proof = await trie.prove(keyBytes);
  const header = {
    ...makeHeader("44".repeat(28), 1_000),
    forcedTransactionsRoot: root.root,
  };
  const subject = forcedVerdictSubject({
    transactionId: fixture.nativeTxId,
    sourceKey: key,
    rejectionReason: reason,
  });
  return {
    schemaVersion: INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
    headerHash: "55".repeat(28),
    detectionId: `${INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID}:forced:0:${fixture.nativeTxId}`,
    position: 0,
    forcedIndex: 0,
    transactionId: fixture.nativeTxId,
    subjectCbor: Data.to(
      subject as never,
      InputSetUniquenessVerdictSubjectSchema as never,
    ),
    nativeTxCompactCbor: fixture.nativeTxCompactCbor,
    spendInputItemCbors: fixture.spendInputItemCbors,
    referenceInputItemCbors: fixture.referenceInputItemCbors,
    forcedSourceCbor: Data.to(
      {
        header,
        membership: {
          domain: root.domain,
          root: root.root,
          phas_root: root.phasRoot,
          count: root.count,
          key,
          value: leaf,
          proof: Data.from(proof.toCBOR().toString("hex"), Proof),
        },
      } as never,
      InputSetUniquenessForcedSourceSchema as never,
    ),
  } as const;
};

describe("production input-set-uniqueness forced authority", () => {
  it("admits a journal artifact only after re-deriving the complete forced subject and unique union", async () => {
    const artifact = await makeArtifact();
    const admitted = admitInputSetUniquenessForcedArtifact(artifact);
    expect(admitted.sourceKind).toBe("forced");
    expect(admitted.spendPlan).not.toBeNull();
    expect(admitted.referencePlan).not.toBeNull();
    expect(() =>
      admitInputSetUniquenessForcedArtifact({
        ...artifact,
        referenceInputItemCbors: artifact.spendInputItemCbors,
      }),
    ).toThrow(/not unique/);
    expect(() =>
      admitInputSetUniquenessForcedArtifact({
        ...artifact,
        nativeTxCompactCbor: "00",
      }),
    ).toThrow(/subject\/source changed/);
  });

  it("keeps manifest-bound configuration infrastructure-only", () => {
    type Keys = keyof ManifestBoundInputSetUniquenessWorkflowConfig;
    expectTypeOf<Keys>().toEqualTypeOf<
      | "manifest"
      | "blueprintJson"
      | "deploymentInfo"
      | "headerHash"
      | "lucid"
      | "signer"
      | "referenceScripts"
      | "source"
      | "stateQueueMutationLeaseCoordinator"
    >();
    expect(
      ["verdict", "evidence", "outcome", "actuator"].some((key) =>
        [
          "manifest",
          "blueprintJson",
          "deploymentInfo",
          "headerHash",
          "lucid",
          "signer",
          "referenceScripts",
          "source",
          "stateQueueMutationLeaseCoordinator",
        ].includes(key),
      ),
    ).toBe(false);
  });
});
