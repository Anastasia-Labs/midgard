/**
 * The prover's two addresses. Its money sits at the plain enterprise address
 * of its key (the funding address), where every pure-Ada coin without a
 * reference script is funding, a refund's tag datum included. Its in-flight
 * proof materials sit at the enterprise address of the native script
 * `sig(key)` (the proof-materials address), which only that key spends. A read of either by its full
 * address never returns an output of the other; a reservation's transaction
 * may pay and spend reserved outputs at both, but posts collateral from, and
 * returns collateral to, the funding address only.
 */
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import {
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isPlainFundingOutput } from "../src/funding/plain-funding-output.js";
import {
  isProverOwnAddress,
  proofMaterialsAddress,
  proofMaterialsAddressOfFundingAddress,
  proofMaterialsScript,
} from "../src/funding/proof-materials-address.js";
import { resolveProverSigner } from "../src/runtime.js";
import { assertManifestBoundWorkflowSigner } from "../src/workflow/deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import {
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
} from "../src/workflow/funding-reservation-permit.begin-workflow-funding-reservation-action.js";
import { createWorkflowFundingReservationPermit } from "../src/workflow/funding-reservation-permit.create-workflow-funding-reservation-permit.js";
import { restrictWorkflowFundingSigner } from "../src/workflow/funding-reservation-permit.restrict-workflow-funding-signer.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";
import {
  admittedActuation,
  DEPLOYMENT,
  discardingWorkflowJournal,
  fundingAddress,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const KEY = "a7".repeat(28);
/** A Close refund's tag: the OutputReference of the bond it refunds. */
const TAG = Data.to(new Constr(0, ["ab".repeat(32), 0n]));
const coin = (output: Partial<UTxO>): UTxO => ({
  txHash: "61".repeat(32),
  outputIndex: 0,
  address: credentialToAddress("Preview", { type: "Key", hash: KEY }),
  assets: { lovelace: 5_000_000n },
  ...output,
});

describe("the proof-materials address", () => {
  it("is the enterprise address of the key's sig native script, on the funding address's network", () => {
    for (const network of ["Preview", "Mainnet"] as const) {
      const funding = credentialToAddress(network, { type: "Key", hash: KEY });
      const materials = proofMaterialsAddress(network, KEY);
      const details = getAddressDetails(materials);
      expect(details.type).toBe("Enterprise");
      expect(details.networkId).toBe(getAddressDetails(funding).networkId);
      expect(details.paymentCredential).toEqual({
        type: "Script",
        hash: validatorToScriptHash(proofMaterialsScript(KEY)),
      });
      expect(details.stakeCredential).toBeUndefined();
      // As long as the funding address, so publications keep their size.
      expect(Buffer.from(details.address.hex, "hex").length).toBe(
        Buffer.from(getAddressDetails(funding).address.hex, "hex").length,
      );
      expect(materials).toBe(SDK.midgardCekStagingAddress(network, KEY));
      expect(materials).not.toBe(funding);
      expect(proofMaterialsAddressOfFundingAddress(funding)).toBe(materials);
      expect(isProverOwnAddress(funding, funding)).toBe(true);
      expect(isProverOwnAddress(funding, materials)).toBe(true);
    }
    const other = credentialToAddress("Preview", {
      type: "Key",
      hash: "a8".repeat(28),
    });
    expect(
      isProverOwnAddress(other, proofMaterialsAddress("Preview", KEY)),
    ).toBe(false);
  });

  it("is derived only from a plain enterprise key address", () => {
    for (const address of [
      proofMaterialsAddress("Preview", KEY),
      credentialToAddress("Preview", { type: "Script", hash: KEY }),
    ])
      expect(() => proofMaterialsAddressOfFundingAddress(address)).toThrow(
        "the funding address must be a plain enterprise key address",
      );
  });

  it("binds a workflow signer to both of its key's manifest-network addresses", () => {
    const address = credentialToAddress("Preview", { type: "Key", hash: KEY });
    const signer = {
      address,
      paymentKeyHash: KEY,
      proofMaterialsAddress: proofMaterialsAddress("Preview", KEY),
    };
    expect(() =>
      assertManifestBoundWorkflowSigner({ network: "Preview", ...signer }),
    ).not.toThrow();
    expect(() =>
      assertManifestBoundWorkflowSigner({ network: "Mainnet", ...signer }),
    ).toThrow("manifest-network enterprise address");
    for (const substituted of [
      address,
      proofMaterialsAddress("Preview", "a8".repeat(28)),
      credentialToAddress(
        "Preview",
        { type: "Key", hash: KEY },
        { type: "Key", hash: "a8".repeat(28) },
      ),
    ])
      expect(() =>
        assertManifestBoundWorkflowSigner({
          network: "Preview",
          ...signer,
          proofMaterialsAddress: substituted,
        }),
      ).toThrow("proof-materials address");
  });
});

describe("a funding coin", () => {
  it("is any pure-Ada output without a reference script, a datum or datum hash included", () => {
    expect(isPlainFundingOutput(coin({}))).toBe(true);
    expect(isPlainFundingOutput(coin({ datum: TAG }))).toBe(true);
    expect(isPlainFundingOutput(coin({ datumHash: "cd".repeat(32) }))).toBe(
      true,
    );
  });

  it("is never an output holding a token or a reference script", () => {
    expect(
      isPlainFundingOutput(
        coin({
          assets: { lovelace: 5_000_000n, [`${"aa".repeat(28)}00`]: 1n },
        }),
      ),
    ).toBe(false);
    expect(
      isPlainFundingOutput(
        coin({ scriptRef: { type: "PlutusV3", script: "46010000222601" } }),
      ),
    ).toBe(false);
  });
});

describe("the funding and proof-materials reads", () => {
  it("never return each other's outputs, and coin selection offers only reserved funding coins, a tagged refund included", async () => {
    const prover = generateEmulatorAccount({});
    const signer = resolveProverSigner({
      network: "Custom",
      walletSeedPhrase: prover.seedPhrase,
    });
    const genesis = (address: string, lovelace: bigint, datum?: string) => ({
      ...prover,
      address,
      assets: { lovelace },
      ...(datum === undefined ? {} : { outputData: { inline: datum } }),
    });
    const emulator = new Emulator([
      genesis(signer.address, 40_000_000n),
      genesis(signer.address, 60_000_000n, TAG),
      genesis(signer.proofMaterialsAddress, 80_000_000n, Data.to("cafe")),
    ]);
    const [plain, refund, material] = (
      await emulator.getUtxosByOutRef(
        [0, 1, 2].map((outputIndex) => ({
          txHash: "00".repeat(32),
          outputIndex,
        })),
      )
    ).sort((left, right) => left.outputIndex - right.outputIndex);
    const label = ({ txHash, outputIndex }: UTxO) =>
      `${txHash}#${outputIndex.toString()}`;
    const byLabel = (utxos: readonly UTxO[]) => utxos.map(label).sort();

    const lucid = await Lucid(emulator, "Custom");
    expect(byLabel(await lucid.utxosAt(signer.address))).toEqual(
      byLabel([plain!, refund!]),
    );
    expect(byLabel(await lucid.utxosAt(signer.proofMaterialsAddress))).toEqual([
      label(material!),
    ]);

    // A reservation holding all three: the restricted wallet funds and posts
    // collateral from the funding coins alone.
    const actuation = await admittedActuation();
    const { runner, policy } = runtimeFundingPolicyFixture({
      deploymentFingerprint: DEPLOYMENT,
      fundingPaymentKeyHash: signer.paymentKeyHash,
    });
    const unused = async () => {
      throw new Error("the action records no transition");
    };
    const reserved = [plain!, refund!, material!];
    const permit = await createWorkflowFundingReservationPermit({
      category: "doubleSpend",
      runner,
      policy,
      actuationPermit: actuation.actuationPermit,
      rollbackGeneration: "7",
      port: {
        load: async () =>
          Object.freeze({
            reservationId: "c1".repeat(32),
            deploymentFingerprint: DEPLOYMENT,
            decisionDigest: actuation.decisionDigest,
            policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
            reservationBasisDigest: "c2".repeat(32),
            rollbackGeneration: "7",
            revision: "0",
            walletAddress: signer.address,
            fundingPaymentKeyHash: signer.paymentKeyHash,
            state: "active" as const,
            activeInputs: Object.freeze(
              reserved
                .map((utxo) =>
                  Object.freeze({
                    outRef: label(utxo),
                    lovelace: utxo.assets.lovelace!.toString(),
                    assets: Object.freeze([]),
                  }),
                )
                .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
            ),
          }),
        refreshIdle: unused,
        resolveInputs: async (outRefs) =>
          await emulator.getUtxosByOutRef(
            outRefs.map((outRef) => {
              const [txHash, outputIndex] = outRef.split("#");
              return { txHash: txHash!, outputIndex: Number(outputIndex) };
            }),
          ),
        resolveConfirmedInput: async () => null,
        readPendingTransition: async () => null,
        readLandedSupersededAttempt: async () => null,
        readRecordedSignedTransaction: async () => null,
        resolveProtocolInputAuthority: unused,
        prepare: unused,
        confirm: unused,
        abandon: unused,
        markConflict: unused,
        release: unused,
      },
    });
    const journal = discardingWorkflowJournal();
    bindWorkflowFundingReservationJournal({ journal, permit });
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "step-one", input: { actionKind: "step-one" } },
    });
    restrictWorkflowFundingSigner({ signer, permit }).selectWallet(lucid);
    expect(byLabel(await lucid.wallet().getUtxos())).toEqual(
      byLabel([plain!, refund!]),
    );
  });
});

describe("a reservation's transaction at admission", () => {
  const materials = proofMaterialsAddressOfFundingAddress(fundingAddress);
  /** The reservation, its `71` coin held at the proof-materials address. */
  const reservation = () =>
    runtimeFunding("step-one", {
      additionalInputs: [
        {
          txHash: "71".repeat(32),
          outputIndex: 0,
          address: materials,
          assets: { lovelace: 3_000_000n },
          datum: Data.to("cafe"),
        },
      ],
    });
  const admit = async (
    shape: Omit<
      Parameters<typeof signedFundingTransaction>[0],
      "inputOutRefs" | "outputLovelace"
    >,
  ) => {
    const runtime = await reservation();
    expect(runtime.selected.fundingOutRefs).toContain(`${"71".repeat(32)}#0`);
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: 14_800_000n,
      ...shape,
    });
    return runtime.prepareTransaction({
      action: { actionId: "step-one", input: { actionKind: "step-one" } },
      preflight: bindWorkflowPreflightTransaction(
        Object.freeze({ txHash: signed.toHash() }),
        signed,
      ),
    });
  };

  it("spends a reserved proof-materials output and pays our proof-materials address", async () => {
    await expect(admit({ outputAddress: materials })).resolves.toBeUndefined();
  });

  it("embeds our proof-materials lock only to spend a proof material", async () => {
    const lock = (key: string) =>
      CML.NativeScript.new_script_pubkey(CML.Ed25519KeyHash.from_hex(key));
    const ours = lock(fundingKey.to_public().hash().to_hex());
    await expect(admit({ nativeScripts: [ours] })).resolves.toBeUndefined();
    const runtime = await reservation();
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs.filter(
        (outRef) => outRef !== `${"71".repeat(32)}#0`,
      ),
      outputLovelace: 11_800_000n,
      nativeScripts: [ours],
    });
    await expect(
      runtime.prepareTransaction({
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight: bindWorkflowPreflightTransaction(
          Object.freeze({ txHash: signed.toHash() }),
          signed,
        ),
      }),
    ).rejects.toThrow(
      "funding transaction embeds our proof-materials script without spending a proof material",
    );
    for (const nativeScripts of [[lock("a8".repeat(28))], [ours, ours]])
      await expect(admit({ nativeScripts })).rejects.toThrow(
        "funding transaction embeds executable script witnesses",
      );
  });

  it("pays no other address of a key", async () => {
    const stranger = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(
        CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x52))
          .to_public()
          .hash(),
      ),
    )
      .to_address()
      .to_bech32();
    // The funding key staked to another key is not ours either.
    const restaked = credentialToAddress(
      "Preprod",
      { type: "Key", hash: fundingKey.to_public().hash().to_hex() },
      { type: "Key", hash: "a8".repeat(28) },
    );
    for (const outputAddress of [stranger, restaked])
      await expect(admit({ outputAddress })).rejects.toThrow(
        "funding output escapes the governed contract roster",
      );
  });

  it("posts collateral from, and returns it to, the funding address only", async () => {
    const collateral = (outRef: string, returned: bigint) => ({
      outRefs: [outRef],
      total: 300_000n,
      returned,
    });
    await expect(
      admit({
        redeemerMemory: 1n,
        collateral: collateral(`${"72".repeat(32)}#0`, 1_700_000n),
      }),
    ).resolves.toBeUndefined();
    await expect(
      admit({
        redeemerMemory: 1n,
        collateral: collateral(`${"71".repeat(32)}#0`, 2_700_000n),
      }),
    ).rejects.toThrow("funding collateral is not reserved-wallet pure Ada");
    await expect(
      admit({
        redeemerMemory: 1n,
        collateral: {
          ...collateral(`${"72".repeat(32)}#0`, 1_700_000n),
          returnAddress: materials,
        },
      }),
    ).rejects.toThrow("funding collateral return escapes the reserved wallet");
  });
});
