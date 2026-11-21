/**
 * Every linear kind of proof material is found by content after a restart:
 * a new process with nothing in memory and a fresh view finds what an
 * earlier one landed at the proof-materials address and publishes nothing
 * again. Tier-2 field carriage stands for both field-carriage paths. The
 * dispute's staging outputs stay as they are found, but the workflow's pins
 * keep them out of every sweep until it is released.
 */
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { scriptDecodeStagingDatums } from "../src/script-decode/submit.js";
import { resolveTransitionTraceDataCarriage } from "../src/transition-trace/proof-carriage.js";
import { pinValidationTraceStagings } from "../src/validation-trace-dispute/one-step-staging-pins.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import {
  createProofMaterialsView,
  lucidProofMaterialsReader,
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "../src/workflow/proof-materials-view.js";
import { createRawDatumPreimageRequirement } from "../src/workflow/raw-datum-preimage.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { scriptDecodeStagedChunks } from "../src/workflow/script-decode-prerequisite.staged-chunks.js";
import {
  base,
  baseAction,
  context,
  headerHash,
  identity,
} from "./field-carriage-prerequisite.prerequisite.js";

const ADDRESS = "addr_test1_proof_materials";

const HOLDER: ProofMaterialsHolder = {
  workflowId: context.workflowId,
  objective: {
    category: identity.category,
    headerHash,
  },
};

/** An output at the proof-materials address that an earlier process paid. */
const landed = (byte: string, outputIndex: number, datum: string): UTxO => ({
  txHash: byte.repeat(32),
  outputIndex,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n },
  datum,
});

const label = ({ txHash, outputIndex }: UTxO) =>
  `${txHash}#${outputIndex.toString()}`;

/** A Lucid over `chain` and a signer whose view is fresh: a new process. */
const freshProcess = (chain: () => readonly UTxO[]) => {
  const lucid = {
    utxosAt: async (address: string) =>
      chain().filter((utxo) => utxo.address === address),
    utxosByOutRef: async (
      refs: readonly { txHash: string; outputIndex: number }[],
    ) =>
      chain().filter((utxo) =>
        refs.some(
          (ref) =>
            ref.txHash === utxo.txHash && ref.outputIndex === utxo.outputIndex,
        ),
      ),
  } as unknown as LucidEvolution;
  const view = createProofMaterialsView({
    address: ADDRESS,
    reader: lucidProofMaterialsReader(lucid, ADDRESS),
  });
  const signer = {
    source: "test",
    address: "addr_test1_funding",
    proofMaterialsAddress: ADDRESS,
    paymentKeyHash: "12".repeat(28),
    selectWallet: () => undefined,
    proofMaterials: view,
  };
  return { lucid, view, signer };
};

describe("proof materials found by content after a restart", () => {
  it("tier-2 field carriage: the landed publication is the step's, and nothing is published again", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("8101", "hex"),
    });
    const [datum] = requirement.publicationDatums;
    const chain = [landed("e1", 0, datum!)];
    const { lucid, view, signer } = freshProcess(() => chain);
    const observed: string[] = [];
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid,
      network: "Preview",
      signer,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async ({ expectedOutRef }) => {
          observed.push(expectedOutRef);
          return { kind: "confirmed", outRef: expectedOutRef };
        },
      },
      requirementForAction: () => requirement,
    });
    const underlying = base();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await expect(
      port.resolveAuthenticated({
        headerHash,
        holder: HOLDER,
        action: baseAction,
        artifact: context.artifact,
      }),
    ).resolves.toMatchObject({ publications: chain });
    // Its authentication is still by the out-ref it references.
    expect(new Set(observed)).toEqual(new Set(chain.map(label)));
    // The workflow needs it until it is released.
    expect(await view.sweepable(100)).toEqual([]);
    view.release(context.workflowId);
    expect((await view.sweepable(100)).map(label)).toEqual(chain.map(label));
  });

  it("transition-trace data carriage: the landed chunks are the step's, and nothing is published again", async () => {
    const datums = [Data.to("aa".repeat(40)), Data.to("bb".repeat(40))];
    const chain = [landed("e2", 1, datums[1]!), landed("e2", 0, datums[0]!)];
    const { lucid, signer } = freshProcess(() => chain);
    const resolved = await resolveTransitionTraceDataCarriage({
      lucid,
      address: ADDRESS,
      datums,
      ownOutputs: {
        ...createOwnOutputJournal(),
        materials: proofMaterialsAccess({ signer, lucid, holder: HOLDER }),
      },
      publish: true,
    });
    if (!Array.isArray(resolved)) throw new Error("published again");
    expect((resolved as readonly UTxO[]).map(label)).toEqual([
      `${"e2".repeat(32)}#0`,
      `${"e2".repeat(32)}#1`,
    ]);
  });

  it("script-decode staging: the landed chunks are Bind's, and nothing is staged again", async () => {
    const scriptBytes = new Uint8Array(20_000).fill(7);
    const datums = scriptDecodeStagingDatums(scriptBytes);
    expect(datums.length).toBeGreaterThan(1);
    const chain = datums.map((datum, index) => landed("e3", index, datum));
    const { lucid, signer } = freshProcess(() => chain);
    const staged = await scriptDecodeStagedChunks({
      lucid,
      address: ADDRESS,
      scriptBytes,
      materials: proofMaterialsAccess({ signer, lucid, holder: HOLDER }),
    });
    expect(staged.kind).toBe("ready");
    if (staged.kind !== "ready") return;
    expect(staged.utxos.map(label)).toEqual(chain.map(label));
  });

  it("dispute staging: every staging output the workflow pins stays out of the sweep until the workflow is released", async () => {
    const staged = "e4".repeat(32);
    const pending = "e5".repeat(32);
    // One staging landed, as the own view names it; the other is signed by
    // this process and not yet seen (its build pins its output), and lands
    // later.
    const signed = [staged, pending].map((txHash) => ({
      outRef: `${txHash}#0`,
      datumCbor: undefined,
    }));
    const chain = [
      landed("e4", 0, Data.to("cc".repeat(40))),
      landed("e6", 0, Data.to("dd".repeat(40))),
    ];
    const { lucid, view, signer } = freshProcess(() => chain);
    pinValidationTraceStagings(
      proofMaterialsAccess({ signer, lucid, holder: HOLDER }),
      signed,
    );
    expect((await view.sweepable(100)).map(label)).toEqual([
      `${"e6".repeat(32)}#0`,
    ]);
    chain.push(landed("e5", 0, Data.to("ee".repeat(40))));
    expect((await view.sweepable(100)).map(label)).toEqual([
      `${"e6".repeat(32)}#0`,
    ]);
    view.release(context.workflowId);
    expect((await view.sweepable(100)).map(label)).toEqual(
      [...chain].map(label).sort(),
    );
  });
});
