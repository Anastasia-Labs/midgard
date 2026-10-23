import {
  castConfirmedStateToData,
  encodeLinkedListNodeView,
  FraudProofTokenDatum,
  makeGenesisConfirmedState,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveFraudProofRawL1CompletedTerminal,
  deriveFraudProofRawL1FamilyStage,
  FraudProofRawL1DerivationError,
  type FraudProofRawL1FamilyDefinition,
  type FraudProofRawL1Snapshot,
  type FraudProofRawL1Utxo,
} from "../src/workflow/index.js";
import {
  fixture,
  hash32,
  output,
  policy,
  PROVER,
  raw,
  releaseEconomics,
} from "./support/raw-l1-terminal-fixture.js";

const OTHER_PROVER = policy("ee");

const unitOf = (
  definition: FraudProofRawL1FamilyDefinition,
  policyId: string,
): string =>
  toUnit(policyId, `${definition.categoryId}${definition.headerHash}`);

/** A fraud-proof record of the header at `outRef`, naming `prover`. */
const record = (
  definition: FraudProofRawL1FamilyDefinition,
  outRef: string,
  prover: string,
): FraudProofRawL1Utxo =>
  raw(
    outRef,
    output({
      address: definition.proofToken.address,
      assets: {
        lovelace: 3_000_000n,
        [unitOf(definition, definition.proofToken.policyId)]: 1n,
      },
      datum: Data.to({ fraud_prover: prover }, FraudProofTokenDatum),
    }),
  );

const withRecords = (
  snapshot: FraudProofRawL1Snapshot,
  records: readonly FraudProofRawL1Utxo[],
): FraudProofRawL1Snapshot => ({
  ...snapshot,
  scopes: snapshot.scopes.map((scope) =>
    scope.role === "permanent_proof_token"
      ? { ...scope, utxos: [...records] }
      : scope,
  ) as FraudProofRawL1Snapshot["scopes"],
});

/** The removed fixture, its removal referencing the record it carries. */
const removed = async () => {
  const value = await fixture();
  const referenced =
    value.snapshot.transactions[0]!.resolvedReferenceInputs[0]!;
  return { ...value, referencedOutRef: referenced.outRef };
};

const derive = (
  snapshot: FraudProofRawL1Snapshot,
  definition: FraudProofRawL1FamilyDefinition,
) => ({
  stage: deriveFraudProofRawL1FamilyStage({
    snapshot,
    definition,
    releaseEconomics,
  }),
  completed: deriveFraudProofRawL1CompletedTerminal({
    snapshot,
    definition,
    releaseEconomics,
  }),
});

/** The fixture with its header live again, and `threads` standing on it. */
const liveWithThreads = async (
  threads: readonly { readonly step: number; readonly outRef: string }[],
) => {
  const value = await fixture();
  const { definition } = value;
  const target = value.snapshot.transactions[0]!.resolvedInputs[0]!;
  const root = raw(
    `${hash32("64")}#0`,
    output({
      address: definition.stateQueue.address,
      assets: {
        lovelace: 3_000_000n,
        [toUnit(definition.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
          1n,
      },
      datum: encodeLinkedListNodeView({
        key: "Empty",
        next: { Key: { key: definition.headerHash } },
        data: castConfirmedStateToData(makeGenesisConfirmedState(0n)) as never,
      }),
    }),
  );
  const steps = definition.computationThread.steps;
  const thread = (step: number, outRef: string) =>
    raw(
      outRef,
      output({
        address: steps[step - 1]!.address,
        assets: {
          lovelace: 3_000_000n,
          [unitOf(definition, definition.computationThread.policyId)]: 1n,
        },
        datum: Data.to({ fraud_prover: PROVER }, FraudProofTokenDatum),
      }),
    );
  const snapshot: FraudProofRawL1Snapshot = {
    ...value.snapshot,
    scopes: value.snapshot.scopes.map((scope) => {
      if (scope.role === "state_queue")
        return { ...scope, utxos: [root, target] };
      if (scope.role === "permanent_proof_token")
        return { ...scope, utxos: [] };
      const index = steps.findIndex((step) => step.role === scope.role);
      return index < 0
        ? scope
        : {
            ...scope,
            utxos: threads
              .filter(({ step }) => step === index + 1)
              .map(({ step, outRef }) => thread(step, outRef)),
          };
    }) as FraudProofRawL1Snapshot["scopes"],
  };
  return { snapshot, definition };
};

describe("raw L1 derivation with several records and threads on one header", () => {
  it("derives the terminal against the record the removal referenced, though ours holds a lower one too", async () => {
    const { snapshot, definition, referencedOutRef, removalTxHash } =
      await removed();
    // Both cases convicted on our key: two records, the removal citing the
    // higher out-ref.
    const lower = `${hash32("00")}#0`;
    expect(lower < referencedOutRef).toBe(true);
    const both = withRecords(snapshot, [
      record(definition, lower, PROVER),
      record(definition, referencedOutRef, PROVER),
    ]);
    const { stage, completed } = derive(both, definition);
    const terminal = {
      proofToken: { outRef: referencedOutRef },
      correction: {
        removalTxHash,
        referencedProofTokenOutRef: referencedOutRef,
      },
    };
    await expect(stage).resolves.toMatchObject({ kind: "removed", terminal });
    await expect(completed).resolves.toMatchObject({
      kind: "removed",
      terminal,
    });
  });

  it("reads the header as resolved by another record when another prover's record removed it", async () => {
    const { snapshot, definition, referencedOutRef, removalTxHash } =
      await removed();
    const foreign = record(definition, referencedOutRef, OTHER_PROVER);
    const resolution = {
      removalTxHash,
      referencedProofTokenOutRef: referencedOutRef,
    };
    // Only the other prover's record stands.
    const alone = derive(withRecords(snapshot, [foreign]), definition);
    await expect(alone.completed).resolves.toEqual({
      kind: "terminal_not_ours",
      reason: "the header is gone and no record of ours stands",
    });
    await expect(alone.stage).rejects.toMatchObject({
      name: "FraudProofResolvedByAnotherRecordError",
      resolution,
    });
    // Ours stands beside it, but the removal cited theirs.
    const beside = derive(
      withRecords(snapshot, [
        record(definition, `${hash32("00")}#0`, PROVER),
        foreign,
      ]),
      definition,
    );
    await expect(beside.completed).resolves.toEqual({
      kind: "terminal_not_ours",
      reason: "another prover's record removed the header",
    });
    await expect(beside.stage).rejects.toMatchObject({
      name: "FraudProofResolvedByAnotherRecordError",
      resolution,
    });
  });

  it("reads a header gone without a removal citing a record of it as a derivation failure, never as resolved", async () => {
    const { snapshot, definition, referencedOutRef, removalTxHash } =
      await removed();
    const [removal, ...rest] = snapshot.transactions;
    const cited = removal!.resolvedReferenceInputs[0]!;
    const citing = (reference: FraudProofRawL1Utxo | undefined) =>
      withRecords(
        {
          ...snapshot,
          transactions: [
            {
              ...removal!,
              resolvedReferenceInputs: removal!.resolvedReferenceInputs
                .map((candidate) =>
                  candidate === cited ? reference : candidate,
                )
                .filter((candidate) => candidate !== undefined),
            },
            ...rest,
          ],
        },
        [record(definition, referencedOutRef, OTHER_PROVER)],
      );
    const token = (headerHash: string, categoryId = definition.categoryId) =>
      toUnit(definition.proofToken.policyId, `${categoryId}${headerHash}`);
    const holding = (address: string, unit: string, quantity = 1n) =>
      raw(
        referencedOutRef,
        output({
          address,
          assets: { lovelace: 3_000_000n, [unit]: quantity },
          datum: Data.to({ fraud_prover: OTHER_PROVER }, FraudProofTokenDatum),
        }),
      );
    const otherCategory = `${definition.categoryId.slice(0, -1)}${
      definition.categoryId.endsWith("0") ? "1" : "0"
    }`;
    // The record of the header in another family resolves it too.
    await expect(
      derive(
        citing(
          holding(
            definition.proofToken.address,
            token(definition.headerHash, otherCategory),
          ),
        ),
        definition,
      ).stage,
    ).rejects.toMatchObject({
      name: "FraudProofResolvedByAnotherRecordError",
      resolution: {
        removalTxHash,
        referencedProofTokenOutRef: referencedOutRef,
      },
    });
    for (const reference of [
      // The burn cites nothing: a merge or any other exit.
      undefined,
      // It cites a record of another header.
      holding(
        definition.proofToken.address,
        token("ab".repeat(definition.headerHash.length / 2)),
      ),
      // It cites the header's token away from the fraud-proof address.
      holding(definition.stateQueue.address, token(definition.headerHash)),
    ]) {
      const { stage } = derive(citing(reference), definition);
      await expect(stage).rejects.toBeInstanceOf(
        FraudProofRawL1DerivationError,
      );
      await expect(stage).rejects.toThrow(
        /disappeared without a retained proof token/u,
      );
    }
  });

  it("acts on the furthest of two equally old threads of ours, then on the lower out-ref", async () => {
    const low = `${hash32("a1")}#0`;
    const high = `${hash32("a2")}#0`;
    for (const threads of [
      [
        { step: 1, outRef: low },
        { step: 3, outRef: high },
      ],
      [
        { step: 3, outRef: high },
        { step: 1, outRef: low },
      ],
    ]) {
      const { snapshot, definition } = await liveWithThreads(threads);
      await expect(
        deriveFraudProofRawL1FamilyStage({
          snapshot,
          definition,
          releaseEconomics,
        }),
      ).resolves.toMatchObject({ kind: "step", step: 3, threadOutRef: high });
    }
    for (const threads of [
      [
        { step: 2, outRef: high },
        { step: 2, outRef: low },
      ],
      [
        { step: 2, outRef: low },
        { step: 2, outRef: high },
      ],
    ]) {
      const { snapshot, definition } = await liveWithThreads(threads);
      await expect(
        deriveFraudProofRawL1FamilyStage({
          snapshot,
          definition,
          releaseEconomics,
        }),
      ).resolves.toMatchObject({ kind: "step", step: 2, threadOutRef: low });
    }
  });
});
