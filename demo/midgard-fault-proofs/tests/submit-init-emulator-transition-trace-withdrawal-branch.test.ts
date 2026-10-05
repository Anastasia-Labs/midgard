import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  computeHash32,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  outRefLabel,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  buildTransitionFaultProof,
  buildValidWithdrawalTransitionWitness,
  resolveTransitionTraceDeploymentContracts,
  submitTransitionTraceProof,
  transitionTraceFinalIndex,
} from "../src/index.js";
import {
  alignedHeaderStart,
  removeAndAssertPermanentProof,
} from "./submit-init-emulator-transition-trace-subvariants.remove-and-assert-permanent-proof.js";
import {
  firstThreadUtxo,
  makeHarness,
  reconstruct,
  setupWithdrawalChallenge,
  withdrawalIdFor,
  withdrawalInfo,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import { FAMILY_HISTORY_HEADER_LEAD_MS } from "./support/emulator/family-history.js";
import {
  funderPaymentKeyHash,
  makeHeader,
  network,
  transitionTraceDaEntry,
} from "./support/submit-init-emulator-shared.js";

// A withdrawal step deletes the spent output from the ledger. When the delete
// proof ends in a Branch, withdrawal_v1 requires that branch to keep two
// other children (`terminal_branch_keeps_two_children`). An honest step is
// defended against a Branch standing in for the spent output's lone
// neighbour, and a genuinely wrong step is still convicted with an honest
// delete that sends a lone group's opening. A delete ending in a Leaf, and one
// that removes the last ledger entry, are held the same way.

const NULL = Buffer.alloc(32);
const nibble0 = (key: Buffer): number => computeHash32(key)[0]! >> 4;
const combine = (left: Uint8Array, right: Uint8Array): Buffer =>
  Buffer.from(computeHash32(Buffer.concat([left, right])));
const merkle = (hashes: readonly Buffer[]): Buffer =>
  hashes.length === 1
    ? hashes[0]!
    : merkle(
        hashes.flatMap((hash, index) =>
          index % 2 === 0 ? [combine(hash, hashes[index + 1]!)] : [],
        ),
      );

const output = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x22)]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});
const ledgerKey = (outRef: SDK.OutputReference): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });
const descriptor = (key: Buffer): Buffer =>
  Buffer.from(
    buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: key,
      outputCbor: output,
    }).descriptorCbor,
  );
const otherKey = (byte: number): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, byte),
    outputIndex: 0,
  });

const trieOf = async (keys: readonly Buffer[]): Promise<Trie> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const key of keys) await trie.insert(key, descriptor(key));
  return trie;
};
const rootHex = (trie: Trie): string => Buffer.from(trie.hash!).toString("hex");
const sdkProof = async (trie: Trie, key: Buffer): Promise<SDK.Proof> =>
  Data.from((await trie.prove(key)).toCBOR().toString("hex"), SDK.Proof);

type Harnessed = Awaited<ReturnType<typeof makeHarness>>;

// Commits a block whose one withdrawal step takes `preRoot` to `postRoot`,
// challenges it and returns what the final-2 submission needs.
const commitWithdrawalStep = async (
  harnessed: Harnessed,
  preRoot: string,
  postRoot: string,
) => {
  const { harness, history } = harnessed;
  const startTime = await alignedHeaderStart(
    harness,
    FAMILY_HISTORY_HEADER_LEAD_MS,
  );
  const withdrawalId = withdrawalIdFor(history);
  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: withdrawalId },
  };
  const withdrawals: SDK.DaPayloadEntry[] = [
    [
      Data.to(withdrawalId, SDK.OutputReference),
      SDK.committedWithdrawalValueBytes(withdrawalInfo("WithdrawalIsValid")),
    ],
  ];
  const transitionTrace = [
    transitionTraceDaEntry({
      key: 0n,
      keySchema: Data.Integer() as never,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "Withdrawal",
        pre_utxos_root: preRoot,
        post_utxos_root: postRoot,
      } satisfies SDK.TransitionStep,
      valueSchema: SDK.TransitionStepSchema,
    }),
  ];
  const eventToStep = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: SDK.EventKeySchema,
      value: {
        step_index: 0n,
        phase: "Withdrawal",
      } satisfies SDK.EventToStepValue,
      valueSchema: SDK.EventToStepValueSchema,
    }),
  ];
  const counted = (
    domain: Parameters<typeof buildCountedRoot>[0],
    entries: readonly SDK.DaPayloadEntry[],
  ) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const header: SDK.Header = {
    ...makeHeader(await funderPaymentKeyHash(harness.funderLucid), startTime),
    withdrawalsRoot: (await counted(SDK.ROOT_DOMAINS.withdrawals, withdrawals))
      .root,
    transitionTraceRoot: (
      await counted(SDK.ROOT_DOMAINS.transitionTrace, transitionTrace)
    ).root,
    eventToStepRoot: (await counted(SDK.ROOT_DOMAINS.eventToStep, eventToStep))
      .root,
    withdrawalCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const { lifecycle } = await setupWithdrawalChallenge({
    ...harnessed,
    header,
    inclusionTime: header.endTime,
  });
  const reconstruction = await reconstruct({
    header,
    withdrawals,
    transitionTrace,
    eventToStep,
  });
  const faultProof = async (spentUtxo: SDK.LedgerDeleteWitness) => {
    const proof = buildTransitionFaultProof({
      reconstruction,
      fault: SDK.invalidOneStepTransitionFault(
        await buildValidWithdrawalTransitionWitness({
          reconstruction,
          stepIndex: 0n,
          evidence: { spentUtxo },
        }),
      ),
    });
    expect(transitionTraceFinalIndex(proof)).toBe(2);
    return proof;
  };
  const submit = async (proof: SDK.TransitionFaultProof) =>
    submitTransitionTraceProof({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef: outRefLabel(
        await firstThreadUtxo({ harness, init: lifecycle.init }),
      ),
      proof,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
  return { lifecycle, faultProof, submit };
};

const spentKey = ledgerKey(withdrawalInfo("WithdrawalIsValid").body.l2_outref);

describe("transition-trace withdrawal delete ending in a Branch", () => {
  it("defends an honest withdrawal step against a Branch standing in for the spent output's lone neighbour", async () => {
    const harnessed = await makeHarness();
    const { harness } = harnessed;
    // One neighbour in a different root slot: the honest delete ends in a
    // Leaf step and leaves the neighbour alone.
    const neighbourKey = Array.from({ length: 256 }, (_, byte) =>
      otherKey(byte),
    ).find((key) => nibble0(key) !== nibble0(spentKey))!;
    const pre = await trieOf([spentKey, neighbourKey]);
    const post = await trieOf([neighbourKey]);
    const honestProof = await sdkProof(pre, spentKey);
    expect(honestProof.map((step) => Object.keys(step)[0])).toEqual(["Leaf"]);
    const neighbourNode = (await pre.childAt(
      nibble0(neighbourKey).toString(16),
    )) as { hash: Buffer };
    const me = nibble0(spentKey);
    const children = Array.from({ length: 16 }, (_, slot) =>
      slot === nibble0(neighbourKey) ? Buffer.from(neighbourNode.hash) : NULL,
    );
    const masquerade: SDK.Proof = [
      {
        Branch: {
          skip: 0n,
          neighbors: Buffer.concat(
            [8, 4, 2, 1].map((size) => {
              const start = (Math.floor(me / size) ^ 1) * size;
              return merkle(children.slice(start, start + size));
            }),
          ).toString("hex"),
        },
      },
    ];

    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(pre),
      rootHex(post),
    );
    const resolved = await resolveTransitionTraceDeploymentContracts({
      blueprint: harness.realBlueprint,
      deploymentInfo: lifecycle.deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
    await expect(
      submit(
        await faultProof({
          key: spentKey.toString("hex"),
          value: descriptor(spentKey).toString("hex"),
          opening: "",
          delete_proof: masquerade,
        }),
      ),
    ).rejects.toThrow(/failed script execution .* the validator crashed/u);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        resolved.contracts.transitionTrace.finals[2]!.spendingScriptAddress,
        lifecycle.init.computationThreadUnit,
      ),
    ).resolves.toHaveLength(1);
    await expect(
      harness.funderLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        lifecycle.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 240_000);

  it("convicts a wrong withdrawal step with an honest delete that opens a lone neighbour group", async () => {
    const harnessed = await makeHarness();
    const me = nibble0(spentKey);
    // Two neighbours in the other half of the root's slots: the honest delete
    // ends in a Branch whose only non-empty neighbour group is n8.
    const [first, second] = Array.from({ length: 256 }, (_, byte) =>
      otherKey(byte),
    )
      .filter((key) => nibble0(key) >> 3 !== me >> 3)
      .reduce<Buffer[]>(
        (chosen, key) =>
          chosen.length < 2 &&
          chosen.every((other) => nibble0(other) !== nibble0(key))
            ? [...chosen, key]
            : chosen,
        [],
      );
    const pre = await trieOf([spentKey, first!, second!]);
    const proof = await pre.prove(spentKey);
    const opening = await buildMidgardMpfDeletionOpening(
      pre,
      spentKey,
      parseMidgardMpfProofJson(proof.toJSON()),
    );
    expect(opening.length).toBeGreaterThanOrEqual(64);
    const deleteProof = Data.from(proof.toCBOR().toString("hex"), SDK.Proof);
    expect(deleteProof.map((step) => Object.keys(step)[0])).toEqual(["Branch"]);

    // The step claims the withdrawal left the ledger unchanged.
    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(pre),
      rootHex(pre),
    );
    const proofResult = await submit(
      await faultProof({
        key: spentKey.toString("hex"),
        value: descriptor(spentKey).toString("hex"),
        opening: opening.toString("hex"),
        delete_proof: deleteProof,
      }),
    );
    await removeAndAssertPermanentProof({
      harness: harnessed.harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 240_000);
});

// Both remaining refusals are asserted the same way: the final-2 submission
// fails, and the thread and the block are both still there.
const expectRefusedAndKept = async (
  harnessed: Harnessed,
  lifecycle: Awaited<ReturnType<typeof commitWithdrawalStep>>["lifecycle"],
  submission: Promise<unknown>,
) => {
  const { harness } = harnessed;
  const resolved = await resolveTransitionTraceDeploymentContracts({
    blueprint: harness.realBlueprint,
    deploymentInfo: lifecycle.deploymentInfo,
    network,
    requireFraudProofSpend: true,
  });
  await expect(submission).rejects.toThrow(/failed script execution/u);
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      resolved.contracts.transitionTrace.finals[2]!.spendingScriptAddress,
      lifecycle.init.computationThreadUnit,
    ),
  ).resolves.toHaveLength(1);
  await expect(
    harness.funderLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      lifecycle.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(1);
};

describe("transition-trace withdrawal delete ending in a Leaf", () => {
  it("defends an honest withdrawal step against a terminal Leaf whose skipped nibble is rewritten", async () => {
    const harnessed = await makeHarness();
    // One neighbour sharing the spent output's first path nibble: the honest
    // delete ends in a Leaf that skips that nibble.
    const neighbourKey = Array.from({ length: 256 }, (_, byte) =>
      otherKey(byte),
    ).find((key) => nibble0(key) === nibble0(spentKey))!;
    const pre = await trieOf([spentKey, neighbourKey]);
    const post = await trieOf([neighbourKey]);
    const honestProof = await sdkProof(pre, spentKey);
    const [terminal] = honestProof;
    if (terminal === undefined || !("Leaf" in terminal)) {
      throw new Error("expected the honest delete to end in a Leaf");
    }
    expect(terminal.Leaf.skip).toBeGreaterThanOrEqual(1n);
    const key = terminal.Leaf.key;
    const moved = ((Number.parseInt(key[0]!, 16) + 1) % 16).toString(16);
    const tampered: SDK.Proof = [
      { Leaf: { ...terminal.Leaf, key: `${moved}${key.slice(1)}` } },
    ];

    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(pre),
      rootHex(post),
    );
    await expectRefusedAndKept(
      harnessed,
      lifecycle,
      submit(
        await faultProof({
          key: spentKey.toString("hex"),
          value: descriptor(spentKey).toString("hex"),
          opening: "",
          delete_proof: tampered,
        }),
      ),
    );
  }, 240_000);
});

// A leaf at cursor 1 opens with 0x10, never with a branch's prefix nibble. A
// terminal Fork whose neighbour is that leaf, split into a "prefix" and a
// "root", is refused on the neighbour's first byte.
describe("transition-trace withdrawal delete ending in a Fork", () => {
  it("defends an honest withdrawal step against a terminal Fork standing in for the spent output's leaf neighbour", async () => {
    const harnessed = await makeHarness();
    // A neighbour in another root slot: the honest delete ends in a Leaf.
    const neighbourKey = Array.from({ length: 256 }, (_, byte) =>
      otherKey(byte),
    ).find((key) => nibble0(key) !== nibble0(spentKey))!;
    const pre = await trieOf([spentKey, neighbourKey]);
    const post = await trieOf([neighbourKey]);
    const honestProof = await sdkProof(pre, spentKey);
    expect(honestProof.map((step) => Object.keys(step)[0])).toEqual(["Leaf"]);
    const neighbourPath = Buffer.from(computeHash32(neighbourKey));
    const leafSuffix = Buffer.concat([
      Buffer.from([0x10, neighbourPath[0]! & 15]),
      neighbourPath.subarray(1),
    ]);
    const valueHash = Buffer.from(computeHash32(descriptor(neighbourKey)));
    const slot = nibble0(neighbourKey);
    const neighbourNode = await pre.childAt(slot.toString(16));
    expect(combine(leafSuffix, valueHash)).toEqual(neighbourNode?.hash);
    const neighbor = {
      nibble: BigInt(slot),
      prefix: leafSuffix.toString("hex"),
      root: valueHash.toString("hex"),
    };
    const masquerade: SDK.Proof = [{ Fork: { skip: 0n, neighbor } }];

    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(pre),
      rootHex(post),
    );
    await expectRefusedAndKept(
      harnessed,
      lifecycle,
      faultProof({
        key: spentKey.toString("hex"),
        value: descriptor(spentKey).toString("hex"),
        opening: "",
        delete_proof: masquerade,
      }).then(submit),
    );
  }, 240_000);
});

// Withdrawing the only ledger entry leaves an empty ledger, which a block
// commits as the protocol's empty root, never as the MPF library's all-zero
// null root.
describe("transition-trace withdrawal of the last ledger entry", () => {
  const lastEntryDelete = async (): Promise<SDK.LedgerDeleteWitness> => {
    const proof = await sdkProof(await trieOf([spentKey]), spentKey);
    expect(proof).toEqual([]);
    return {
      key: spentKey.toString("hex"),
      value: descriptor(spentKey).toString("hex"),
      opening: "",
      delete_proof: proof,
    };
  };

  it("defends an honest step that commits the empty ledger root", async () => {
    const harnessed = await makeHarness();
    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(await trieOf([spentKey])),
      SDK.EMPTY_MERKLE_TREE_ROOT,
    );
    await expectRefusedAndKept(
      harnessed,
      lifecycle,
      submit(await faultProof(await lastEntryDelete())),
    );
  }, 240_000);

  it("convicts a step that commits the all-zero root for the empty ledger", async () => {
    const harnessed = await makeHarness();
    const { lifecycle, faultProof, submit } = await commitWithdrawalStep(
      harnessed,
      rootHex(await trieOf([spentKey])),
      "00".repeat(32),
    );
    const proofResult = await submit(await faultProof(await lastEntryDelete()));
    await removeAndAssertPermanentProof({
      harness: harnessed.harness,
      setup: lifecycle.setup,
      deploymentInfo: lifecycle.deploymentInfo,
      proofResult,
    });
  }, 240_000);
});
