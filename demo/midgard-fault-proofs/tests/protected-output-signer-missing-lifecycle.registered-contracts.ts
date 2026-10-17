import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import { materializeMidgardNativeTxFromCanonical } from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardAddressWitnessItem,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  encodeMidgardForcedTxCompact,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import {
  AddressData,
  addressDataFromBech32,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  applyProtectedOutputSignerMissingScripts,
  prepareProtectedOutputSignerMissingEvidence,
  PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  type ProtectedOutputSignerMissingContracts,
  type ProtectedOutputSignerMissingEvidence,
} from "../src/protected-output-signer-missing/index.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  expectRegisteredChainParity,
  familyStepsFromRegisteredChain,
} from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  makeNativeTx,
  network,
} from "./support/submit-init-emulator-shared.js";

export const REASON = "ProtectedOutputSignerMissing";

/** Every seam a step authenticates before it reads or commits anything. */
export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf",
  "compact_tx",
  "witness_set_anchor",
  "field_preimage",
  "field_certificate",
  "checkpoint",
  "credential",
] as const;

/** The five physical scripts, in chain order; every one carries a cancel arm. */
export const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

export const FORCED_ORDER_KEY = {
  transactionId: "ab".repeat(32),
  outputIndex: 0n,
};

export const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

// ---------------------------------------------------------------------------
// A real Ed25519 signer, so the frontier admits a genuine signature over the
// transaction id and refuses a forged one.
// ---------------------------------------------------------------------------

const signerSeed = Buffer.alloc(32, 0x2b);

const signerPrivateKey = createPrivateKey({
  key: Buffer.concat([
    Buffer.from("302e020100300506032b657004220420", "hex"),
    signerSeed,
  ]),
  format: "der",
  type: "pkcs8",
});

const signerVerificationKey = createPublicKey(signerPrivateKey)
  .export({ format: "der", type: "spki" })
  .subarray(-32);

export const signerCredentialHex = missingSignatureVkeyHash(
  signerVerificationKey.toString("hex"),
);

/**
 * Address header `0x68` is a protected pub-key enterprise address; `0x60` is
 * its unprotected form and `0x78` a protected script enterprise address.
 */
export const protectedOutputCbor = (
  credentialHex: string,
  addressHeader = 0x68,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([addressHeader]),
      Buffer.from(credentialHex, "hex"),
    ]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });

export const validWitness = (txId: Buffer): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey: signerVerificationKey,
    signature: sign(null, txId, signerPrivateKey),
  });

/** The right key with a signature that does not verify. */
export const forgedWitness = (): Buffer =>
  encodeMidgardAddressWitnessItem({
    verificationKey: signerVerificationKey,
    signature: Buffer.alloc(64, 0xff),
  });

/** A decoy key nobody holds, with an unverifiable signature. */
export const decoyWitness = (index: number): Buffer => {
  const key = Buffer.alloc(32);
  key.writeUInt32BE(index + 1, 28);
  return encodeMidgardAddressWitnessItem({
    verificationKey: key,
    signature: Buffer.alloc(64, 0xff),
  });
};

/**
 * One protected pub-key output and a field-7 witness collection built after
 * the body is fixed: the transaction id commits the body alone, so witnesses
 * can sign it.
 */
export const nativeTxWith = ({
  fee,
  credentialHex = signerCredentialHex,
  addressHeader = 0x68,
  witnesses,
}: {
  readonly fee: bigint;
  readonly credentialHex?: string;
  readonly addressHeader?: number;
  readonly witnesses: (txId: Buffer) => readonly Buffer[];
}): MidgardNativeTxFull => {
  const unsigned = makeNativeTx({
    spendInputCbors: [],
    fee,
    outputCbor: protectedOutputCbor(credentialHex, addressHeader),
  });
  const txId = computeMidgardNativeTxId(unsigned);
  return makeNativeTx({
    spendInputCbors: [],
    fee,
    outputCbor: protectedOutputCbor(credentialHex, addressHeader),
    addrTxWitsPreimageCbor: encodeCbor([...witnesses(txId)]),
  });
};

export const compactHex = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): string =>
  ("validity" in nativeTx
    ? encodeMidgardNativeTxCompact(nativeTx.compact)
    : encodeMidgardForcedTxCompact(nativeTx.compact)
  ).toString("hex");

export const witnessSetCompactHex = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

export const witnessSetOf = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): SDK.NativeTxWitnessSetCompact => {
  const derived = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    addr_tx_wits_hash: Buffer.from(derived.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(derived.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(derived.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

const fixtureBytes = (
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
  sourceKind: bigint,
) =>
  sourceKind === 1n
    ? encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(nativeTx),
      )
    : encodeMidgardNativeTxCanonical(
        materializeMidgardNativeTxFromCanonical({
          ...nativeTx,
          validity: "TxIsValid",
        }),
      );

export const evidenceFor = (
  subject: SDK.VerdictSubject,
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): ProtectedOutputSignerMissingEvidence =>
  prepareProtectedOutputSignerMissingEvidence({
    subject,
    outputIndex: 0,
    canonicalTransactionCbor: fixtureBytes(nativeTx, subject.source_kind),
  });

/**
 * Evidence for an honest block: the production preparer refuses evidence
 * that agrees with the operator, so the honest suites prepare the closing
 * polarity and carry the honest subject instead. Every later step derives
 * its datum from the subject, so the chain sees exactly the honest claim.
 */
export const honestEvidenceFor = (
  honestSubject: SDK.VerdictSubject,
  closingSubject: SDK.VerdictSubject,
  nativeTx: MidgardNativeTxFull | MidgardForcedTxFull,
): ProtectedOutputSignerMissingEvidence =>
  Object.freeze({
    ...evidenceFor(closingSubject, nativeTx),
    subject: honestSubject,
    canonicalTransactionCborHex: fixtureBytes(
      nativeTx,
      honestSubject.source_kind,
    ).toString("hex"),
  });

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. The family-side application must reproduce it
 * step for step before the suite drives it.
 */
export const registeredContracts = async (harness: Harness) => {
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const registered =
    harness.contracts.fraudProofContracts.protectedOutputSignerMissing;
  const category = harness.catalogue.categories.protectedOutputSignerMissing;
  expectRegisteredChainParity({
    registered,
    applied: applyProtectedOutputSignerMissingScripts({
      blueprint: harness.realBlueprint,
      network,
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    }),
    category,
  });
  const steps = familyStepsFromRegisteredChain(
    registered.steps,
    PROTECTED_OUTPUT_SIGNER_MISSING_BLUEPRINT_TITLES,
  );
  const contracts: ProtectedOutputSignerMissingContracts = {
    steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
  };
  return { steps, contracts, catalogue: harness.catalogue, category };
};

export const publishFamilyReferences = async (
  harness: Harness,
  steps: ProtectedOutputSignerMissingContracts["steps"],
  label: string,
) => {
  const refs: UTxO[] = [];
  for (const [index, step] of steps.entries())
    refs.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `${label} step ${(index + 1).toString()}`,
        })
      ).utxo,
    );
  const certificateRef = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: `${label} certificate`,
    })
  ).utxo;
  return { refs, certificateRef };
};
