import {
  credentialToAddress,
  type Native,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { verifyReferenceScriptPublicationAuthority } from "../src/deployment-manifest-identity.js";

const publisherKeyHash = "ab".repeat(28);
const otherKeyHash = "cd".repeat(28);
const expiresAtSlot = 123_456;
const signature: Native = { type: "sig", keyHash: publisherKeyHash };
const expiry: Native = { type: "before", slot: expiresAtSlot };
const authority = (script: Native, required = false) => ({
  cborHex: scriptFromNative(script).script,
  expiresAtSlot,
  publisherAddress: credentialToAddress("Preprod", {
    type: "Key",
    hash: publisherKeyHash,
  }),
  postTimelockAuditRequired: required,
});
const signedAuthority = () =>
  authority({ type: "all", scripts: [signature, expiry] });

describe("reference-script publication authority", () => {
  it("permits immediate audit only for the exact publisher signature AND expiry policy", () => {
    expect(
      verifyReferenceScriptPublicationAuthority(signedAuthority()),
    ).toEqual({
      kind: "publisher-signature",
      expiresAtSlot,
      publisherKeyHash,
    });
  });

  it("derives publisher identity from the policy without an expected wallet", () => {
    const { publisherAddress: _publisherAddress, ...input } = signedAuthority();
    expect(verifyReferenceScriptPublicationAuthority(input)).toEqual({
      kind: "publisher-signature",
      expiresAtSlot,
      publisherKeyHash,
    });
  });

  it("keeps deployed time-only policies readable with their expiry audit requirement", () => {
    expect(
      verifyReferenceScriptPublicationAuthority(authority(expiry, true)),
    ).toEqual({ kind: "time-only", expiresAtSlot });
    expect(() =>
      verifyReferenceScriptPublicationAuthority(authority(expiry)),
    ).toThrow(/time-only.*required to be true/u);
  });

  it.each([
    { type: "any", scripts: [signature, expiry] },
    { type: "all", scripts: [expiry, signature] },
    { type: "all", scripts: [signature] },
    { type: "all", scripts: [signature, expiry, signature] },
    { type: "all", scripts: [{ type: "any", scripts: [signature] }, expiry] },
    { type: "atLeast", required: 1, scripts: [signature, expiry] },
    signature,
  ] satisfies Native[])("rejects unsupported policy shape %#", (script) => {
    expect(() =>
      verifyReferenceScriptPublicationAuthority(authority(script)),
    ).toThrow(/must be an exact publisher signature AND expiry policy/u);
  });

  it("derives expiry from the policy rather than trusting manifest metadata", () => {
    for (const input of [signedAuthority(), authority(expiry, true)]) {
      expect(() =>
        verifyReferenceScriptPublicationAuthority({
          ...input,
          expiresAtSlot: expiresAtSlot + 1,
        }),
      ).toThrow(/expiresAtSlot must match the native policy CBOR/u);
    }
  });

  it("binds the signing key to the deployment publisher payment credential", () => {
    for (const credential of [
      { type: "Key", hash: otherKeyHash },
      { type: "Script", hash: publisherKeyHash },
    ] as const) {
      expect(() =>
        verifyReferenceScriptPublicationAuthority({
          ...signedAuthority(),
          publisherAddress: credentialToAddress("Preprod", credential),
        }),
      ).toThrow(/signer must match.*payment key/u);
    }
  });

  it("does not allow metadata to restore an expiry wait for the signed policy", () => {
    expect(() =>
      verifyReferenceScriptPublicationAuthority({
        ...signedAuthority(),
        postTimelockAuditRequired: true,
      }),
    ).toThrow(/publisher-signed.*required to be false/u);
  });

  it("rejects malformed policy CBOR and trailing data", () => {
    for (const cborHex of ["ff", `${signedAuthority().cborHex}00`]) {
      expect(() =>
        verifyReferenceScriptPublicationAuthority({
          ...signedAuthority(),
          cborHex,
        }),
      ).toThrow();
    }
  });
});
