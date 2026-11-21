/**
 * The double-spend workflow adapter when the prover's carriage objects are not
 * the only ones of their kind on chain. The adapter settles its own
 * transactions by their own outputs and treats a changed selection as a
 * changed action.
 */
import { Proof } from "@al-ft/midgard-sdk/core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { splitProofIntoChunkDatums } from "../src/publish-proof-chunks.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { addressFieldCarriageOwnOutputs } from "../src/workflow/bespoke-adapter-field-carriage.js";
import { h32, outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  certifiedOpening,
  doubleSpendHarness,
  LOWEST_TX_HASH,
  outRef,
  SIGNER,
  STATE_QUEUE_OUT_REF,
  THREAD_OUT_REF,
  withOtherAsset,
} from "./support/double-spend-shared-carriage-harness.js";
import { testProofMaterials } from "./support/proof-materials-holder.js";
import { repeatedFirstChunkItems } from "./support/repeated-first-chunk-items.js";
import { publishCarriageUnjournaled } from "./support/submit-unjournaled.js";

/** 800 spend inputs whose field repeats its first chunk as its second. */
const repeatedFirstChunkSpendInputs = () =>
  repeatedFirstChunkItems({
    count: 800,
    item: (fill) => outRefCbor(fill, 0n),
  });

/**
 * The ledger view and carriage claims the double-spend adapter publishes
 * with, over `wallet` as the prover's proof-materials address; the claims
 * are found by content.
 */
const adapterCarriage = async (
  plan: ReturnType<typeof certifiedOpening>["plan"],
  wallet: readonly UTxO[],
) => {
  const lucid = {
    utxosAt: async () => [...wallet],
    utxosByOutRef: async (
      refs: readonly { txHash: string; outputIndex: number }[],
    ) =>
      refs.flatMap((ref) =>
        wallet.filter(
          (utxo) =>
            utxo.txHash === ref.txHash && utxo.outputIndex === ref.outputIndex,
        ),
      ),
  } as unknown as LucidEvolution;
  return {
    lucid,
    ownOutputs: await addressFieldCarriageOwnOutputs({
      lucid,
      network: "Preview",
      publisherAddress: SIGNER.proofMaterialsAddress,
      planned: plan,
      materials: testProofMaterials(SIGNER, lucid),
    }),
  };
};

describe("double-spend carriage shared with other on-chain objects", () => {
  it("treats a certificate other than the scheduled one as a changed proof-step action", async () => {
    const { artifact, certificate, publications } = certifiedOpening();
    const certificates: UTxO[] = [certificate(h32(0x61))];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: publications,
      certificateUtxos: certificates,
    });
    const step = await harness.next();
    expect(step.input).toMatchObject({
      stage: "step_03",
      certificateOutRef: `${h32(0x61)}#0`,
    });

    // Another valid certificate for the same opening, ordered first.
    certificates.unshift(certificate(LOWEST_TX_HASH));
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    certificates.splice(0, certificates.length);
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("selects its own field publications over another output of a different shape with the same datum", async () => {
    const { artifact, publications } = certifiedOpening();
    const [first] = publications;
    const wallet: UTxO[] = [
      withOtherAsset(first!, LOWEST_TX_HASH),
      ...publications,
    ];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const certify = await harness.next();
    expect(certify.input).toMatchObject({
      stage: "certify-field",
      chunkOutRefs: publications.map(outRef),
    });
    await expect(harness.authenticatedBy(certify)).resolves.toEqual(
      publications.map(outRef),
    );

    // With only the other output for the first chunk, that chunk is still to
    // be published.
    wallet.splice(wallet.indexOf(first!), 1);
    await expect(harness.next()).resolves.toMatchObject({
      input: { stage: "publish-field", publicationDatumCbor: first!.datum },
    });
  });

  it("publishes a field chunk whose only output at the prover's proof-materials address has a different shape", async () => {
    const { plan, publications } = certifiedOpening();
    const [first, ...rest] = publications;
    const wallet = [withOtherAsset(first!, LOWEST_TX_HASH), ...rest];
    class PublicationStarted extends Error {}
    await expect(
      publishCarriageUnjournaled({
        ...(await adapterCarriage(plan, wallet)),
        signer: SIGNER,
        planned: plan,
        publisherAddress: SIGNER.proofMaterialsAddress,
        label: "double-spend shared carriage test",
        beforePublication: async () => {
          throw new PublicationStarted();
        },
      }),
    ).rejects.toBeInstanceOf(PublicationStarted);
  });

  it("selects its own proof chunks over another output of a different shape with the same datum", async () => {
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [chunkDatum] = splitProofIntoChunkDatums(proofCbor);
    const own: UTxO = {
      txHash: h32(0x6e),
      outputIndex: 0,
      address: SIGNER.proofMaterialsAddress,
      assets: { lovelace: 2_000_000n },
      datum: chunkDatum!,
    };
    const { artifact } = certifiedOpening(proofCbor);
    const wallet: UTxO[] = [withOtherAsset(own, LOWEST_TX_HASH), own];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_01",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const step = await harness.next();
    expect(step.input.stage).toBe("step_01");
    await expect(harness.authenticatedBy(step)).resolves.toEqual([outRef(own)]);

    // With only the other output, the proof is still to be published.
    wallet.splice(wallet.indexOf(own), 1);
    await expect(harness.next()).resolves.toMatchObject({
      input: { stage: "publish-proof" },
    });
  });

  it("treats a change in the observed field publications as a changed certification action", async () => {
    const { artifact, publications } = certifiedOpening();
    const wallet: UTxO[] = [...publications];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const certify = await harness.next();
    expect(certify.input).toMatchObject({
      stage: "certify-field",
      chunkOutRefs: publications.map(outRef),
    });

    // The first chunk is now also at an out-ref that sorts first.
    wallet.unshift({ ...publications[0]!, txHash: LOWEST_TX_HASH });
    await expect(harness.preflight(certify)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("treats a field chunk gone from L1 since scheduling as a changed certification action", async () => {
    const { artifact, publications } = certifiedOpening();
    const wallet: UTxO[] = [...publications];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const certify = await harness.next();
    expect(certify.input).toMatchObject({ stage: "certify-field" });

    // A rollback or a spend removes a chunk before the action runs.
    wallet.splice(wallet.indexOf(publications[0]!), 1);
    await expect(harness.preflight(certify)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
    expect(await harness.next()).toMatchObject({
      input: {
        stage: "publish-field",
        publicationDatumCbor: publications[0]!.datum,
      },
    });
  });

  it("treats a field chunk published since scheduling as a changed publication action", async () => {
    const { artifact, publications } = certifiedOpening();
    const [first, ...rest] = publications;
    const wallet: UTxO[] = [...rest];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const publish = await harness.next();
    expect(publish.input).toMatchObject({
      stage: "publish-field",
      publicationDatumCbor: first!.datum,
    });

    // The scheduled chunk is on chain before the action runs.
    wallet.unshift(first!);
    await expect(harness.preflight(publish)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it.each([
    ["uses a plain field chunk that another transaction paid to us", true],
    ["finds its landed publications by content after a restart", false],
  ] as const)("%s", async (_name, otherPaidFirst) => {
    const { plan, publications } = certifiedOpening();
    const [first, ...rest] = publications;
    const wallet = otherPaidFirst
      ? [{ ...first!, txHash: LOWEST_TX_HASH }, ...rest]
      : [...publications];
    await expect(
      publishCarriageUnjournaled({
        ...(await adapterCarriage(plan, wallet)),
        signer: SIGNER,
        planned: plan,
        publisherAddress: SIGNER.proofMaterialsAddress,
        label: "double-spend shared carriage test",
        beforePublication: async () => {
          throw new Error("every chunk is already on chain");
        },
      }).then((utxos) => utxos.map(outRef)),
    ).resolves.toEqual(wallet.map(outRef));
  });

  it("publishes every field chunk when two chunks are identical", async () => {
    const { plan, publications } = certifiedOpening(
      undefined,
      repeatedFirstChunkSpendInputs(),
    );
    expect(publications.map((utxo) => utxo.datum)).toEqual([
      publications[0]!.datum,
      publications[0]!.datum,
      publications[2]!.datum,
    ]);
    // One output carries the repeated chunk's bytes; the plan needs two.
    const wallet = [publications[0]!, publications[2]!];
    class PublicationStarted extends Error {}
    await expect(
      publishCarriageUnjournaled({
        ...(await adapterCarriage(plan, wallet)),
        signer: SIGNER,
        planned: plan,
        publisherAddress: SIGNER.proofMaterialsAddress,
        label: "double-spend shared carriage test",
        beforePublication: async () => {
          throw new PublicationStarted();
        },
      }),
    ).rejects.toBeInstanceOf(PublicationStarted);
  });

  it("uses a different output for each of two identical field chunks", async () => {
    const { plan, publications } = certifiedOpening(
      undefined,
      repeatedFirstChunkSpendInputs(),
    );
    const wallet = [...publications];
    await expect(
      publishCarriageUnjournaled({
        ...(await adapterCarriage(plan, wallet)),
        signer: SIGNER,
        planned: plan,
        publisherAddress: SIGNER.proofMaterialsAddress,
        label: "double-spend shared carriage test",
        beforePublication: async () => {
          throw new Error("every chunk is already on chain");
        },
      }).then((utxos) => utxos.map(outRef)),
    ).resolves.toEqual(publications.map(outRef));
  });

  it("treats a proof chunk gone from L1 since scheduling as a changed proof-step action", async () => {
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [chunkDatum] = splitProofIntoChunkDatums(proofCbor);
    const chunk: UTxO = {
      txHash: h32(0x6e),
      outputIndex: 0,
      address: SIGNER.proofMaterialsAddress,
      assets: { lovelace: 2_000_000n },
      datum: chunkDatum!,
    };
    const { artifact } = certifiedOpening(proofCbor);
    const wallet: UTxO[] = [chunk];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_01",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [],
    });
    const step = await harness.next();
    expect(step.input.stage).toBe("step_01");

    // A rollback or a spend removes the chunk before the action runs.
    wallet.splice(0, wallet.length);
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });

  it("treats a field chunk gone from L1 since scheduling as a changed proof-step action", async () => {
    const { artifact, certificate, publications } = certifiedOpening();
    const wallet: UTxO[] = [...publications];
    const harness = doubleSpendHarness({
      artifact,
      stage: {
        kind: "step_03",
        threadOutRef: THREAD_OUT_REF,
        stateQueueBlockOutRef: STATE_QUEUE_OUT_REF,
      },
      walletUtxos: wallet,
      certificateUtxos: [certificate(h32(0x61))],
    });
    const step = await harness.next();
    expect(step.input.stage).toBe("step_03");

    // A rollback or a spend removes a chunk before the action runs.
    wallet.splice(wallet.indexOf(publications[0]!), 1);
    await expect(harness.preflight(step)).rejects.toBeInstanceOf(
      WorkflowActionChangedError,
    );
  });
});
