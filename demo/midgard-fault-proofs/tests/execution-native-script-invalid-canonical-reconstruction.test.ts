import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import { reconstructExecutionNativeScriptPurposes } from "../src/execution-native-script-invalid/canonical-reconstruction-v1.js";

describe("executionNativeScriptInvalid canonical source reconstruction", () => {
  it("derives spend, mint, observe, and receive execution order independently of witness order", () => {
    const script = nativeScriptWitness({ type: "any", scripts: [] });
    const scriptHash = Buffer.from(hashScriptWitness(script), "hex");
    const scriptAddress = Buffer.from(
      CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_script(CML.ScriptHash.from_raw_bytes(scriptHash)),
      )
        .to_address()
        .to_raw_bytes(),
    );
    const spent = outRefFromByte(0x61);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE, scriptAddress);
    const protectedReceive = protectMidgardAddress(scriptAddress);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE, protectedReceive);
    const assetName = Buffer.from("31", "hex");
    const tx = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
      scriptWitnesses: [script],
      requiredObserverItems: [scriptHash],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[scriptHash, new Map([[assetName, 1n]])]]),
      ),
    });
    const reconstructed = reconstructExecutionNativeScriptPurposes({
      canonicalTransactionCbor: tx.txCbor,
      resolvedOutputsByOutRef: new Map([[spent.toString("hex"), spentOutput]]),
    });
    expect(reconstructed.transactionId).toBe(tx.txId.toString("hex"));
    expect(
      reconstructed.purposes.map(({ purposeKind }) => purposeKind),
    ).toEqual(["spend", "mint", "observe", "receive"]);
    expect(
      reconstructed.purposes.map(({ executionIndex }) => executionIndex),
    ).toEqual([0, 1, 2, 3]);
    expect(
      reconstructed.purposes.map(({ purposeIndex }) => purposeIndex),
    ).toEqual([0n, 0n, 0n, 0n]);
    expect(
      reconstructed.purposes.every(({ source }) => source.originKind === 0),
    ).toBe(true);
  });

  it("selects an authenticated resolved-reference source when no inline source exists", () => {
    const script = nativeScriptWitness({ type: "any", scripts: [] });
    const scriptHash = Buffer.from(hashScriptWitness(script), "hex");
    const spent = outRefFromByte(0x62);
    const reference = outRefFromByte(0x63, 7n);
    const spentOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const referenceOutput = encodeMidgardTxOutput({
      address: Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(
            CML.PrivateKey.generate_ed25519().to_public().hash(),
          ),
        )
          .to_address()
          .to_raw_bytes(),
      ),
      value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
      script_ref: script,
    });
    const assetName = Buffer.from("31", "hex");
    const tx = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      referenceInputs: [reference],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
      scriptWitnesses: [],
      mintPreimageCbor: makeMintPreimageCbor(
        new Map([[scriptHash, new Map([[assetName, 1n]])]]),
      ),
    });
    const reconstructed = reconstructExecutionNativeScriptPurposes({
      canonicalTransactionCbor: tx.txCbor,
      resolvedOutputsByOutRef: new Map([
        [spent.toString("hex"), spentOutput],
        [reference.toString("hex"), referenceOutput],
      ]),
    });
    expect(reconstructed.purposes).toHaveLength(1);
    expect(reconstructed.purposes[0]).toMatchObject({
      purposeKind: "mint",
      purposeIndex: 0n,
      scriptHash: scriptHash.toString("hex"),
      source: {
        originKind: 1,
        sourceKey: reference.toString("hex"),
      },
    });
  });

  it("fails closed when canonical prior-ledger material is absent", () => {
    const spent = outRefFromByte(0x64);
    const tx = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    });
    expect(() =>
      reconstructExecutionNativeScriptPurposes({
        canonicalTransactionCbor: tx.txCbor,
        resolvedOutputsByOutRef: new Map(),
      }),
    ).toThrow(/unresolved spend input/u);
  });
});
