import "./reference-script-sweep.retired-reference-script-sweep-batching.js";

import {
  CML,
  type Script,
  toUnit,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { decideRetiredAuthPolicyDisposition } from "../src/transactions/reference-script-sweep.js";
import {
  plan,
  refusalCheck,
  refUtxo,
  RETIRED,
  SIGNER,
  tokenName,
} from "./reference-script-sweep.plan.js";

describe("retired auth policy disposition", () => {
  const authPolicy = (signer: string, invalidHereafter: number): Script => {
    const conditions = CML.NativeScriptList.new();
    conditions.add(
      CML.NativeScript.new_script_pubkey(CML.Ed25519KeyHash.from_hex(signer)),
    );
    conditions.add(
      CML.NativeScript.new_script_invalid_hereafter(BigInt(invalidHereafter)),
    );
    const script = CML.NativeScript.new_script_all(conditions);
    return { type: "Native", script: script.to_cbor_hex() };
  };
  const policyIdOf = (script: Script): string => validatorToScriptHash(script);

  it("burns while the wallet can still satisfy the policy", () => {
    const script = authPolicy(SIGNER, 10_000);
    const disposition = decideRetiredAuthPolicyDisposition({
      retiredAuthPolicyId: policyIdOf(script),
      policyScript: script,
      signerKeyHash: SIGNER,
      currentSlot: 9_000,
    });

    expect(disposition).toMatchObject({ kind: "burn", validToSlot: 9_600 });
  });

  it("quarantines once the policy's validity window has closed", () => {
    const script = authPolicy(SIGNER, 10_000);

    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: policyIdOf(script),
        policyScript: script,
        signerKeyHash: SIGNER,
        currentSlot: 9_401,
      }).kind,
    ).toBe("quarantine");
  });

  it("quarantines when the reference wallet is not the policy signer", () => {
    const script = authPolicy("aa".repeat(28), 10_000);

    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: policyIdOf(script),
        policyScript: script,
        signerKeyHash: SIGNER,
        currentSlot: 1,
      }).kind,
    ).toBe("quarantine");
  });

  it("quarantines when no policy script is supplied", () => {
    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: RETIRED,
        signerKeyHash: SIGNER,
        currentSlot: 1,
      }),
    ).toMatchObject({ kind: "quarantine" });
  });

  it("refuses a policy script that does not hash to the retired policy", () => {
    const script = authPolicy(SIGNER, 10_000);

    expect(
      refusalCheck(() =>
        decideRetiredAuthPolicyDisposition({
          retiredAuthPolicyId: RETIRED,
          policyScript: script,
          signerKeyHash: SIGNER,
          currentSlot: 1,
        }),
      ),
    ).toBe("policy-script-mismatch");
  });

  it("burns every retired token in place of quarantine outputs", () => {
    const script = authPolicy(SIGNER, 10_000);
    const policyId = policyIdOf(script);
    const utxos = [
      refUtxo({ index: 1, policyId }),
      refUtxo({ index: 2, policyId }),
    ];
    const disposition = decideRetiredAuthPolicyDisposition({
      retiredAuthPolicyId: policyId,
      policyScript: script,
      signerKeyHash: SIGNER,
      currentSlot: 1,
    });
    const [batch] = plan({
      utxos,
      retiredAuthPolicyId: policyId,
      disposition,
    }).batches;

    expect(batch!.quarantineOutputs).toEqual([]);
    expect(batch!.quarantineLovelace).toBe(0n);
    expect(batch!.burnedAssets).toEqual({
      [toUnit(policyId, tokenName(1))]: 1n,
      [toUnit(policyId, tokenName(2))]: 1n,
    });
  });
});
