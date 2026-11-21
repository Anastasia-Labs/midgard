/**
 * Proof chunks are found by content. A chunk anyone paid with exactly the
 * chunk's bytes and nothing more is adopted, and a proof-chunk publication
 * survives the loss of everything the prover kept: after the chunks land, a
 * new process with a fresh view finds them, publishes
 * nothing, and its step carries the very outputs the first process paid.
 */
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { MAXIMUM_CHUNK_PROOF_STEP_COUNT, Proof } from "@al-ft/midgard-sdk/core";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { splitProofIntoChunkDatums } from "../src/publish-proof-chunks.js";
import {
  type ResolvedProverSigner,
  resolveProverSigner,
} from "../src/runtime.resolve-prover-signer.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../src/workflow/proof-chunk-prerequisite.js";
import { resolveDirectFirstProofChunks } from "../src/workflow/proof-chunk-prerequisite.route-action-identity.js";
import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
} from "../src/workflow/proof-materials-view.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  base,
  baseAction,
  context,
  headerHash,
  proofSigner,
} from "./proof-chunk-prerequisite.prerequisite.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const ADA = 1_000_000n;
const MAX_TX_SIZE = 16_384;

/** Two chunks' worth of proof steps, canonically encoded. */
const PROOF_CBOR = canonicalPlutusDataCbor(
  Data.to(
    Array.from({ length: MAXIMUM_CHUNK_PROOF_STEP_COUNT + 1 }, () => ({
      Branch: { skip: 0n, neighbors: "" },
    })),
    Proof,
  ),
);

const scene = async () => {
  const account = generateEmulatorAccount({ lovelace: 10_000n * ADA });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const signer = resolveProverSigner({
    network: "Custom",
    walletSeedPhrase: account.seedPhrase,
  });
  const funder = await Lucid(emulator, "Custom");
  funder.selectWallet.fromSeed(account.seedPhrase);
  const funding = await (
    await funder
      .newTx()
      .pay.ToAddress(signer.address, {
        lovelace: 1_000n * ADA,
      })
      .complete()
  ).sign
    .withWallet()
    .complete();
  await funding.submit();
  emulator.awaitBlock();
  return { emulator, signer };
};

/**
 * One process: its own Lucid, port and wrapper over the shared chain. The
 * base step is too large to carry its proof directly, and records the chunk
 * out-refs each preflight carries.
 */
const startProcess = async (
  { emulator }: Awaited<ReturnType<typeof scene>>,
  signer: ResolvedProverSigner,
) => {
  const lucid: LucidEvolution = await Lucid(emulator, "Custom");
  const carried: string[][] = [];
  const underlying = base();
  const step: FraudProofFamilyWorkflowAdapter = {
    ...underlying,
    preflight: async (input) => {
      const chunks = await resolveDirectFirstProofChunks({
        action: input.action,
        address: signer.proofMaterialsAddress,
        proofCbor: PROOF_CBOR,
      });
      carried.push(chunks.map(({ outRef }) => outRef));
      if (chunks.length === 0)
        throw new Error(
          `Max transaction size of ${MAX_TX_SIZE.toString()} exceeded. Found: 20000`,
        );
      return await underlying.preflight(input);
    },
  };
  const port = createAuthenticatedProofChunkPrerequisitePort({
    category: "invalidRange",
    maximumTransactionBytes: MAX_TX_SIZE,
    lucid,
    signer,
    publications: {
      observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
      observeExact: async ({ expectedOutRef, expectedDatumCbor }) => {
        const [txHash, index] = expectedOutRef.split("#");
        const [utxo] = await lucid.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(index) },
        ]);
        return utxo?.datum === expectedDatumCbor &&
          utxo.address === signer.proofMaterialsAddress
          ? { kind: "confirmed", outRef: expectedOutRef }
          : { kind: "not_found" };
      },
    },
    proofCborForAction: ({ action }) =>
      action.actionId === baseAction.actionId ? PROOF_CBOR : null,
  });
  return {
    carried,
    adapter: withProofChunkPrerequisite({
      category: "invalidRange",
      base: step,
      prerequisite: port,
    }),
  };
};

describe("proof chunks found by content", () => {
  it("adopts exact plain chunks anyone paid, and never a copy carrying more", async () => {
    const datums = splitProofIntoChunkDatums(PROOF_CBOR);
    const plain = (byte: string, index: number): UTxO => ({
      txHash: byte.repeat(32),
      outputIndex: index,
      address: proofSigner.proofMaterialsAddress,
      assets: { lovelace: 2_000_000n },
      datum: datums[index]!,
    });
    const planted = [plain("e1", 0), plain("e1", 1)];
    const carryingMore: UTxO[] = [
      {
        ...plain("e2", 0),
        assets: { lovelace: 2_000_000n, [`${"cc".repeat(28)}01`]: 1n },
      },
      {
        ...plain("e2", 1),
        scriptRef: {
          type: "PlutusV3",
          script: "4e4d01000033222220051200120011",
        },
      },
      { ...plain("e3", 0), datum: undefined, datumHash: "a1".repeat(32) },
    ];
    let chain: UTxO[] = [...carryingMore, planted[0]!];
    const observed: string[] = [];
    const port = createAuthenticatedProofChunkPrerequisitePort({
      category: "invalidRange",
      maximumTransactionBytes: 16_384,
      lucid: { utxosAt: async () => chain } as unknown as LucidEvolution,
      signer: proofSigner,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async ({ expectedOutRef }) => {
          observed.push(expectedOutRef);
          return { kind: "confirmed", outRef: expectedOutRef };
        },
      },
      proofCborForAction: () => PROOF_CBOR,
    });
    const step = {
      headerHash,
      workflowId: context.workflowId,
      baseAction,
      artifact: context.artifact,
    };
    // One chunk is plain; the other only has copies carrying more.
    await expect(port.inspect(step)).resolves.toMatchObject({
      kind: "required",
    });
    expect(observed).toEqual([]);
    chain = [...carryingMore, ...planted];
    const satisfied = await port.inspect(step);
    expect(satisfied).toMatchObject({ kind: "satisfied" });
    if (satisfied.kind !== "satisfied") throw new Error("not adopted");
    expect(satisfied.chunks.map(({ outRef }) => outRef)).toEqual([
      `${"e1".repeat(32)}#0`,
      `${"e1".repeat(32)}#1`,
    ]);
    expect(observed).toEqual(satisfied.chunks.map(({ outRef }) => outRef));
    const underlying = base();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: port,
    });
    // Nothing is published: the step itself is next, over the planted chunks.
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await adapter.preflight({ ...context, action: baseAction });
    expect(underlying.preflight).toHaveBeenCalledOnce();
  });

  it("are found by content with no process state: nothing is published again", async () => {
    const chain = await scene();
    const datums = splitProofIntoChunkDatums(PROOF_CBOR);
    expect(datums).toHaveLength(2);
    const materials = async () =>
      await chain.emulator.getUtxos(chain.signer.proofMaterialsAddress);

    const first = await startProcess(chain, chain.signer);
    const observed = await first.adapter.observe(context);
    if (observed.kind !== "action_required")
      throw new Error(`no publication required: ${observed.kind}`);
    const publication: FraudProofWorkflowAction = observed.action;
    expect(publication.actionId).not.toBe(baseAction.actionId);
    const preflight = await first.adapter.preflight({
      ...context,
      action: publication,
    });
    expect(preflight).toMatchObject({
      actionId: publication.actionId,
      scriptExecution: "none",
      referenceScripts: [],
    });
    const submitted = await first.adapter.submit({
      ...context,
      action: publication,
      preflight,
    });
    expect(submitted).toEqual({ kind: "submitted", txHash: preflight.txHash });
    expect(workflowPreflightTransaction(preflight)).toBeDefined();
    chain.emulator.awaitBlock();
    const published = (await materials()).map(
      (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`,
    );
    expect(published).toHaveLength(2);

    // Everything the first process held is gone: its Lucid, its view and
    // its prepared bodies.
    const reader = await Lucid(chain.emulator, "Custom");
    const second = await startProcess(chain, {
      ...chain.signer,
      proofMaterials: createProofMaterialsView({
        address: chain.signer.proofMaterialsAddress,
        reader: lucidProofMaterialsReader(
          reader,
          chain.signer.proofMaterialsAddress,
        ),
      }),
    });
    await expect(second.adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await second.adapter.preflight({ ...context, action: baseAction });
    // The step carries the first process's outputs, in proof order.
    expect(second.carried).toHaveLength(1);
    const [carried] = second.carried;
    expect([...carried!].sort()).toEqual([...published].sort());
    expect(
      carried!.map((outRef) => outRef.startsWith(`${preflight.txHash}#`)),
    ).toEqual([true, true]);
    const datumAt = new Map(
      (await materials()).map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        utxo.datum,
      ]),
    );
    expect(carried!.map((outRef) => datumAt.get(outRef))).toEqual(datums);
    expect(await materials()).toHaveLength(2);
  }, 300_000);
});
