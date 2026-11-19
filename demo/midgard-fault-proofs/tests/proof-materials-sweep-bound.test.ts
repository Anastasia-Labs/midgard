/**
 * The runtime bound of a proof-materials sweep. A reservation's sweep spends
 * exactly its page of our proof materials, each one reserved or an output
 * of a transaction its workflow confirmed, embeds no script but the
 * materials' own `sig(key)` lock, and pays one plain coin to the funding
 * address; a reserved funding coin joins it only when the materials alone
 * fall under min-ADA after the fee.
 */
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { proofMaterialsAddressOfFundingAddress } from "../src/funding/proof-materials-address.js";
import { proofMaterialsSweepAction } from "../src/workflow/proof-materials-sweep.action.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  fundingAddress,
  fundingKey,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const materials = proofMaterialsAddressOfFundingAddress(fundingAddress);
const lock = (key: string) =>
  CML.NativeScript.new_script_pubkey(CML.Ed25519KeyHash.from_hex(key));
const ours = lock(fundingKey.to_public().hash().to_hex());
const material = (txHash: string, lovelace: bigint): UTxO => ({
  txHash,
  outputIndex: 0,
  address: materials,
  assets: { lovelace },
  datum: Data.to("cafe"),
});
/** `71` is a reserved material; `75` and `7a` of transactions we
 * confirmed; `79` one at our materials address that is neither. */
const RESERVED = material("71".repeat(32), 3_000_000n);
const LINEAGE = material("75".repeat(32), 4_000_000n);
const FOREIGN = material("79".repeat(32), 4_000_000n);
/** One of ours too small to pay its own fee and a min-ADA coin. */
const SMALL = material("7a".repeat(32), 1_000_000n);
const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const sweep = async (
  page: readonly UTxO[],
  shape: Omit<Parameters<typeof signedFundingTransaction>[0], "inputOutRefs">,
  inputs: readonly string[] = page.map(outRef),
) => {
  const action = proofMaterialsSweepAction({
    category: "doubleSpend",
    outRefs: page.map(outRef),
  });
  const runtime = await runtimeFunding("step-one", {
    action,
    additionalInputs: [RESERVED, LINEAGE, FOREIGN, SMALL],
    confirmedInputs: [LINEAGE, SMALL],
  });
  const signed = signedFundingTransaction({
    inputOutRefs: inputs,
    nativeScripts: [ours],
    ...shape,
  });
  return await runtime.prepareTransaction({
    action,
    preflight: bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: signed.toHash() }),
      signed,
    ),
  });
};

describe("a proof-materials sweep at admission", () => {
  it("spends a reserved material and one of its workflow's lineage back to funding as one plain coin", async () => {
    await expect(
      sweep([RESERVED, LINEAGE], { outputLovelace: 6_800_000n }),
    ).resolves.toBeUndefined();
  });

  it("never spends a material that is neither reserved nor of its workflow", async () => {
    await expect(
      sweep([FOREIGN], { outputLovelace: 3_800_000n }),
    ).rejects.toThrow(
      "proof-materials sweep spends an output that is not a material of its workflow",
    );
  });

  it("pays nothing but its one coin to the funding address", async () => {
    await expect(
      sweep([LINEAGE], {
        outputLovelace: 1_800_000n,
        additionalOutputs: [
          CML.TransactionOutput.new(
            CML.Address.from_bech32(materials),
            CML.Value.from_coin(2_000_000n),
          ),
        ],
      }),
    ).rejects.toThrow(
      "a proof-materials sweep pays one plain coin to the funding address",
    );
    await expect(
      sweep([LINEAGE], {
        outputLovelace: 3_800_000n,
        outputAddress: materials,
      }),
    ).rejects.toThrow(
      "a proof-materials sweep pays one plain coin to the funding address",
    );
  });

  it("adds a reserved funding coin only when its materials fall under min-ADA", async () => {
    await expect(
      sweep([SMALL], { outputLovelace: 2_800_000n }, [
        outRef(SMALL),
        `${"72".repeat(32)}#0`,
      ]),
    ).resolves.toBeUndefined();
    await expect(
      sweep([LINEAGE], { outputLovelace: 5_800_000n }, [
        outRef(LINEAGE),
        `${"72".repeat(32)}#0`,
      ]),
    ).rejects.toThrow(
      "proof-materials sweep adds funding its materials do not need",
    );
  });

  it("embeds no script but the materials' own lock", async () => {
    await expect(
      sweep([LINEAGE], {
        outputLovelace: 3_800_000n,
        nativeScripts: [lock("a8".repeat(28))],
      }),
    ).rejects.toThrow("funding transaction embeds executable script witnesses");
  });
});
