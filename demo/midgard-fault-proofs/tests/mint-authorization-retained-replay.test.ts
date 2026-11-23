import { encodeMidgardVersionedScript } from "@al-ft/midgard-core";
import { encodeMidgardMintPolicyItem } from "@al-ft/midgard-core/codec";
import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk/core";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { checkedLedgerAsLedger } from "../src/checked-ledger-store.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import {
  admitMintAuthorizationWorkflowArtifact,
  MINT_AUTHORIZATION_ARTIFACT,
} from "../src/mint-authorization/artifact.js";
import {
  detectMintAuthorizationReplay,
  mintAuthorizationDetectionId,
  mintAuthorizationInputProof,
  prepareMintAuthorizationReplay,
} from "../src/mint-authorization/replay.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { buildFixtureTransaction } from "./helpers/canonical-block-evidence-fixture.js";
import { block, OTHER, SPENT } from "./helpers/event-ledger-committed-steps.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { outRef } from "./helpers/event-ledger-seeded-blocks.js";
import {
  buildMintAuthorizationSubject,
  directionBNativeScript,
} from "./support/mint-authorization-emulator.js";
import { mintAuthorizationRetainedReferenceFixture } from "./support/mint-authorization-retained-reference.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.build-decoding-block-fixture.js";

const fixture = async (present: boolean) => {
  const policy = directionBNativeScript();
  const witness = encodeMidgardVersionedScript({
    language: "NativeCardano",
    nativeScript: policy.script,
    scriptBytes: Buffer.from(policy.scriptBytesHex, "hex"),
  });
  const subject = buildMintAuthorizationSubject({
    mintItemCbors: [policy.mintItemCbor],
    scriptWitnessItemCbors: present ? [witness.toString("hex")] : [],
  });
  return await buildDecodingBlockFixture({
    priorLedgerRoot: EMPTY_MERKLE_TREE_ROOT,
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
};

describe("mint authorization retained evidence", () => {
  it.each([false, true])(
    "reconstructs absent/present policy %s and JSON journal recovery",
    async (present) => {
      const block = await fixture(present);
      const findings = await prepareMintAuthorizationReplay({
        current: block.reconstruction,
        sourceIndex: 0,
      });
      expect(findings).toHaveLength(1);
      expect(findings[0]!.finding.direction).toBe(present ? 1n : 0n);
      const artifact = {
        schemaVersion: MINT_AUTHORIZATION_ARTIFACT,
        headerHash: block.reconstruction.headerHash,
        detectionId: mintAuthorizationDetectionId(findings[0]!.coordinate),
        coordinate: findings[0]!.coordinate,
        payloadEnvelopeCbor:
          block.reconstruction.payloadEnvelopeCbor.toString("hex"),
        predecessorLedger: null,
      };
      const restored = await admitMintAuthorizationWorkflowArtifact(
        JSON.parse(JSON.stringify(artifact)),
      );
      expect(restored.finding).toEqual(findings[0]!.finding);
      await expect(
        admitMintAuthorizationWorkflowArtifact({
          ...artifact,
          coordinate: { sourceIndex: 0, policyIndex: "1" },
        }),
      ).rejects.toThrow();
      await expect(
        admitMintAuthorizationWorkflowArtifact({
          ...artifact,
          headerHash: "00".repeat(28),
        }),
      ).rejects.toThrow();
      await expect(
        admitMintAuthorizationWorkflowArtifact({ ...artifact, direction: 1 }),
      ).rejects.toThrow();
    },
  );
});

/**
 * Phase B resolves a policy among the reference scripts of every resolved
 * output, spent and referenced alike. A policy whose only source is a spent
 * output is present: authorized when its native script holds, unauthorized
 * when it fails, and absent only when no resolved output carries it.
 */
describe("mint authorization policies sourced from spent outputs", () => {
  it.each([
    {
      inputField: "spend",
      present: true,
      satisfied: true,
      expected: [],
    },
    {
      inputField: "spend",
      present: true,
      satisfied: false,
      expected: [1n],
    },
    {
      inputField: "spend",
      present: false,
      satisfied: false,
      expected: [0n],
    },
    {
      inputField: "reference",
      present: true,
      satisfied: true,
      expected: [],
    },
  ] as const)(
    "$inputField source present=$present satisfied=$satisfied proves directions $expected",
    async ({ inputField, present, satisfied, expected }) => {
      const fixture = await mintAuthorizationRetainedReferenceFixture({
        now: 1_000_000,
        operatorVkey: "aa".repeat(28),
        inputField,
        present,
        satisfied,
      });
      const findings = await detectMintAuthorizationReplay({
        block: fixture.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger({
          evidence: fixture.evidence,
          context: fixture.context,
        }),
      });
      expect(
        findings.map(({ prepared }) => prepared.finding.direction),
      ).toEqual(expected);
      for (const { prepared } of findings) {
        expect(prepared.finding.policyIdHex).toBe(fixture.target.policyIdHex);
        expect(prepared.spendInputItemCbors).toHaveLength(
          inputField === "spend" ? 1 : 0,
        );
      }
    },
  );
});

/** A transaction that spends `spent` and mints one unit of a policy no
 * resolved output carries: an absent policy, proved at direction 0. */
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

describe("mint authorization resolved inputs", () => {
  it("reads and proves each resolved input against the ledger before its transaction", async () => {
    // Step 0 spends `SPENT` and creates the output step 1 references: the
    // ledger before step 1 differs from the predecessor in entries step 1
    // does not read as well as in one it does.
    const creator = {
      spendInputs: [SPENT.outRef],
      outputs: [makeOutput(1_999_993n)],
      fee: 7n,
    };
    const created = outRef(buildFixtureTransaction(creator).txId, 0);
    const fixture = await block([
      { input: creator, applied: true },
      { input: unauthorizedMint(OTHER.outRef, [created]), applied: true },
    ]);
    const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
    const before = await referenceLedgerBefore({
      block: fixture.evidence,
      predecessor,
      eventKey: fixture.key(1),
      stepIndex: 1n,
    });
    expect(before.utxosRoot).not.toBe(predecessor.utxosRoot);
    const expected = async (key: Buffer) => {
      const { value } = before.trie.entries.find((entry) =>
        entry.key.equals(key),
      )!;
      return {
        keyHex: key.toString("hex"),
        descriptorCbor: value.toString("hex"),
        proof: await keyValuePhasProof(before.trie, key, value),
      };
    };
    const findings = await prepareMintAuthorizationReplay({
      current: fixture.evidence.reconstruction,
      predecessor: await checkedLedgerAsLedger(predecessor),
      sourceIndex: Number(fixture.position(1)),
    });
    expect(findings.map(({ finding }) => finding.direction)).toEqual([0n]);
    const prepared = findings[0]!;
    const resolved = async (inputs: typeof prepared.references) =>
      await Promise.all(
        inputs.map(async (input) => ({
          keyHex: input.keyHex,
          descriptorCbor: input.descriptorCbor,
          proof: await mintAuthorizationInputProof(prepared, input),
        })),
      );
    expect(await resolved(prepared.references)).toStrictEqual([
      await expected(created),
    ]);
    expect(await resolved(prepared.spends)).toStrictEqual([
      await expected(OTHER.outRef),
    ]);
  });

  type Prepare = () => ReturnType<typeof prepareMintAuthorizationReplay>;
  it.each([
    [
      "reading its inputs",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        await dropLedger();
        await prepare();
      },
    ],
    [
      "proving an input",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        const [prepared] = await prepare();
        await dropLedger();
        await mintAuthorizationInputProof(prepared!, prepared!.spends[0]!);
      },
    ],
  ] as const)(
    "is this process's local fault %s when the store no longer holds the ledger",
    async (_, run) => {
      // The transaction is step 0, so it reads the predecessor itself, which
      // `retain` of nothing deletes.
      const fixture = await block([
        { input: unauthorizedMint(OTHER.outRef), applied: true },
      ]);
      const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
      const store = createLedgerStore(createMemoryLedgerStorage());
      const ledger = await store.admit(
        predecessor.entries,
        predecessor.utxosRoot,
      );
      const error: unknown = await run(
        async () => {
          await store.retain(new Set());
        },
        async () =>
          await prepareMintAuthorizationReplay({
            current: fixture.evidence.reconstruction,
            predecessor: ledger,
            sourceIndex: Number(fixture.position(0)),
          }),
      ).catch((cause: unknown) => cause);
      expect(isReplayLocalFault(error)).toBe(true);
      expect((error as { cause: unknown }).cause).toBeInstanceOf(
        LedgerNodeMissing,
      );
    },
  );
});
